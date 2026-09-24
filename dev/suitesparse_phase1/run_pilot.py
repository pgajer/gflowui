"""Supervised, content-keyed serial experiment runner."""
import argparse
import importlib.metadata
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time
import psutil
from common import atomic_json, identity, read_json, sha256

METHODS=['metric_mds','metric_mds_edge_kk','weighted_grip','isomap_graph','umap','lle']
SEEDS=[17,29,43]


class MonitoringUnavailable(RuntimeError):
    """A live owned process cannot be reliably measured within the poll budget."""


def owned_process_rss(process):
    """One bounded POSIX ps fallback; never substitute zero for denied live RSS."""
    try:
        return process.memory_info().rss,'psutil'
    except psutil.NoSuchProcess:
        return 0,'vanished'
    except psutil.AccessDenied:
        try:
            if not process.is_running() or process.status()==psutil.STATUS_ZOMBIE:
                return 0,'vanished'
        except psutil.NoSuchProcess:
            return 0,'vanished'
        except psutil.AccessDenied:
            pass
        try:
            raw=subprocess.check_output(['ps','-o','rss=','-p',str(process.pid)],
                text=True,stderr=subprocess.DEVNULL,timeout=.5).strip()
            # A single PID must produce exactly one nonnegative integer in KiB.
            if not raw.isascii() or not raw.isdecimal(): raise ValueError('invalid RSS response')
            rss=int(raw)*1024
            if not process.is_running(): return 0,'vanished'
            return rss,'ps_fallback'
        except psutil.NoSuchProcess:
            return 0,'vanished'
        except (OSError,subprocess.SubprocessError,ValueError,psutil.AccessDenied) as exc:
            try:
                if not process.is_running(): return 0,'vanished'
            except psutil.NoSuchProcess:
                return 0,'vanished'
            except psutil.AccessDenied:
                pass
            raise MonitoringUnavailable(f'pid {process.pid}: bounded RSS fallback unavailable ({type(exc).__name__})') from exc


def environment():
    return dict(python=sys.version,packages={x:importlib.metadata.version(x) for x in
                ['numpy','scipy','scikit-learn','umap-learn','numba','networkx','psutil']},
                threads=1,platform=sys.platform)


def supervise(command, dest, seconds=600, memory=2*1024**3):
    dest=Path(dest)
    env=os.environ.copy()
    env.update({key:'1' for key in ['OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS',
                                  'VECLIB_MAXIMUM_THREADS','NUMBA_NUM_THREADS']})
    env['NUMBA_CACHE_DIR']=str(dest/'numba_cache')
    start=time.monotonic()
    peak=0
    failure=None
    failure_status=None
    fallback_reads=vanished_reads=0
    complete_samples=0
    with (dest/'process.log').open('w') as log:
        proc=subprocess.Popen(command,stdout=log,stderr=subprocess.STDOUT,env=env,start_new_session=True)
        try:
            while proc.poll() is None:
                try:
                    parent=psutil.Process(proc.pid)
                    processes=[parent]+parent.children(recursive=True)
                    rss=0
                    live_reads=0
                    for child in processes:
                        value,method=owned_process_rss(child)
                        rss+=value
                        fallback_reads+=int(method=='ps_fallback')
                        vanished_reads+=int(method=='vanished')
                        live_reads+=int(method!='vanished')
                    if live_reads:
                        peak=max(peak,rss)
                        complete_samples+=1
                except psutil.NoSuchProcess:
                    pass
                except (psutil.AccessDenied,MonitoringUnavailable) as exc:
                    failure='memory_telemetry_unavailable: '+str(exc)
                    failure_status='failed'
                if time.monotonic()-start>seconds:
                    failure='timeout'
                    failure_status='resource_limited'
                elif peak>memory:
                    failure='memory_limit'
                    failure_status='resource_limited'
                if failure:
                    try: os.killpg(proc.pid,signal.SIGKILL)
                    except ProcessLookupError: pass
                    break
                time.sleep(.1)
        except BaseException:
            try: os.killpg(proc.pid,signal.SIGKILL)
            except ProcessLookupError: pass
            proc.wait()
            raise
        code=proc.wait()
    return dict(status=failure_status if failure else ('completed' if code==0 else 'failed'),
                reason=failure,exit_code=code,elapsed_seconds=time.monotonic()-start,
                peak_rss_bytes=peak if complete_samples else None,
                limits=dict(seconds=seconds,memory_bytes=memory),
                memory_fallback_reads=fallback_reads,vanished_process_reads=vanished_reads,
                memory_complete_samples=complete_samples,
                memory_measurement='sum parent/descendant RSS sampled every 0.1s; one ps fallback per denied read (0.5s limit); overshoot possible')


def verified_cached(dest,key):
    dest=Path(dest)
    path=dest/'manifest.json'
    if not path.exists(): return False
    try:
        data=read_json(path)
        artifacts=data['artifacts']
        required={'request.json','result.json','coords_raw.csv','coords_display.csv','vertices.json'}
        if (data.get('run_key')!=key or data.get('status')!='completed'
                or not isinstance(artifacts,dict) or not required.issubset(artifacts)):
            return False
        for name,digest in artifacts.items():
            relative=Path(name)
            file=dest/relative
            if (relative.is_absolute() or '..' in relative.parts or '\\' in name
                    or not file.resolve().is_relative_to(dest.resolve())
                    or not file.is_file() or sha256(file)!=digest):
                return False
        return True
    except (OSError,ValueError,KeyError,TypeError,AttributeError):
        return False


def allocation_preflight(info,landmarks,memory):
    """Reject known prepared inputs alone exceeding budget; not a peak-RSS forecast."""
    n=max(info['component_sizes'],default=0)
    # Exact float64 distances, int32 predecessors, and float64 landmark features.
    distance=8*n*n
    predecessor=4*n*n
    features=8*n*min(n,landmarks)
    known=distance+predecessor+features
    return dict(component_vertices=n,distance_bytes=distance,predecessor_bytes=predecessor,
                feature_bytes=features,known_prepared_bytes=known,admitted=known<=memory,
                caveat='Necessary allocation check only; excludes optimizer, scoring, imports and copies. RSS supervision remains mandatory.')


def run(root,methods=METHODS,landmarks=64,result_name='pilot_results.json'):
    root=Path(root)
    cohort=read_json(root/'cohort.json')
    source=Path(__file__).parent
    code={f.name:sha256(f) for f in sorted(source.iterdir()) if f.suffix in ('.py','.R')}
    revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    dirty=subprocess.check_output(['git','status','--porcelain'],text=True).strip()
    if dirty:
        raise RuntimeError('commit implementation before pilot run')
    env=environment()
    grip=subprocess.check_output(['Rscript','-e',
        'cat(jsonlite::toJSON(list(version=as.character(packageVersion("grip")), smacof_version=as.character(packageVersion("smacof")), R=R.version.string, path=find.package("grip")),auto_unbox=TRUE))'],text=True)
    env['grip']=json.loads(grip)
    install=Path(env['grip']['path'])
    env['grip']['installed_files_sha256']={str(f.relative_to(install)):sha256(f) for f in sorted(install.rglob('*')) if f.is_file()}
    rows=[]
    previous={}
    for record in cohort['records']:
        if record['status']!='completed': continue
        token=record['graph_id'].replace('/','__')
        graph_dir=root/'graphs'/token
        info=read_json(graph_dir/'graph.json')
        for method in methods:
            seeds=SEEDS if method in ['metric_mds','metric_mds_edge_kk','weighted_grip','umap'] else [17]
            for seed in seeds:
                payload=dict(schema_version=1,graph_dir=str(graph_dir),graph_sha256=info['graph_sha256'],
                        method=method,seed=seed,landmarks=landmarks,code=code,commit=revision,environment=env,
                        limits=dict(seconds=600,memory_bytes=2*1024**3))
                payload['allocation_preflight']=allocation_preflight(info,landmarks,payload['limits']['memory_bytes'])
                initial=previous.get((token,'metric_mds',seed))
                if method=='metric_mds_edge_kk':
                    if not initial or read_json(initial/'manifest.json')['status']!='completed':
                        rows.append(dict(graph_id=record['graph_id'],method=method,seed=seed,
                                         status='unavailable',reason='matching MDS run not completed'))
                        continue
                    payload['initial_run']=str(initial)
                    payload['initial_manifest_sha256']=sha256(initial/'manifest.json')
                key=identity(payload)
                dest=root/'runs'/token/f'{method}_seed{seed}_{key[:12]}'
                if not verified_cached(dest,key):
                    if dest.exists():
                        # Do not overwrite an interrupted, corrupt, or failed historical run.
                        dest=dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
                    dest.mkdir(parents=True)
                    payload['output']=str(dest)
                    atomic_json(dest/'request.json',payload)
                    print(record['graph_id'],method,seed,'running',flush=True)
                    if payload['allocation_preflight']['admitted']:
                        timing=supervise([sys.executable,str(source/'worker.py'),str(dest/'request.json')],dest)
                    else:
                        timing=dict(status='resource_limited',reason='prepared_allocation_preflight',
                                    elapsed_seconds=0.,peak_rss_bytes=0,limits=payload['limits'])
                    if timing['status']=='completed' and not (dest/'result.json').exists():
                        timing.update(status='failed',reason='missing result')
                    artifacts={str(f.relative_to(dest)):sha256(f) for f in sorted(dest.rglob('*'))
                               if f.is_file() and 'numba_cache' not in f.parts and f.name!='manifest.json'}
                    atomic_json(dest/'manifest.json',dict(run_key=key,request=payload,artifacts=artifacts,**timing))
                previous[token,method,seed]=dest
                manifest=read_json(dest/'manifest.json')
                row=dict(graph_id=record['graph_id'],method=method,seed=seed,status=manifest['status'],
                         reason=manifest.get('reason'),run_dir=str(dest),elapsed_seconds=manifest['elapsed_seconds'],
                         peak_rss_bytes=manifest['peak_rss_bytes'])
                if row['status']=='completed': row['scores']=read_json(dest/'result.json')['summary']
                rows.append(row)
                atomic_json(root/result_name,dict(schema_version=1,commit=revision,runs=rows))
                print(record['graph_id'],method,seed,row['status'],round(row['elapsed_seconds'],2),flush=True)
    atomic_json(root/result_name,dict(schema_version=1,commit=revision,runs=rows))


if __name__=='__main__':
    parser=argparse.ArgumentParser()
    parser.add_argument('root')
    parser.add_argument('--methods',nargs='+',choices=METHODS,default=METHODS)
    parser.add_argument('--landmarks',type=int,choices=[16,32,64],default=64)
    parser.add_argument('--result-name',default='pilot_results.json')
    args=parser.parse_args()
    if Path(args.result_name).name!=args.result_name:
        parser.error('result-name must be a plain filename')
    run(args.root,args.methods,args.landmarks,args.result_name)
