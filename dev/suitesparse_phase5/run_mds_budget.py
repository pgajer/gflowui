"""Explicit serial MDS retries: 30 GiB, no elapsed-time limit, unchanged science."""
import argparse
from contextlib import contextmanager
import fcntl
from pathlib import Path
import subprocess
import sys
import time

HERE=Path(__file__).resolve().parent
PILOT=HERE.parent/'suitesparse_phase1'
sys.path.insert(0,str(PILOT))
from common import atomic_json,read_json,sha256,identity
from run_pilot import supervise,allocation_preflight,verified_cached,environment

GRAPHS=['Meszaros/nemscem','HB/sstmodel','Bomhof/circuit_2']
SEEDS=[17,29,43]
LIMITS=dict(seconds=None,memory_bytes=30*1024**3)
INDEX='mds_30gib_results.json'

@contextmanager
def serial_lock(root):
    # Persistent lock inode; OS releases ownership on exit, including exceptions.
    with (Path(root)/'.mds_30gib.lock').open('a') as stream:
        try: fcntl.flock(stream,fcntl.LOCK_EX|fcntl.LOCK_NB)
        except BlockingIOError as exc: raise RuntimeError('another MDS budget runner is active') from exc
        try: yield
        finally: fcntl.flock(stream,fcntl.LOCK_UN)

def capture_environment():
    import json
    env=environment()
    env['grip']=json.loads(subprocess.check_output(['Rscript','-e',
        'cat(jsonlite::toJSON(list(version=as.character(packageVersion("grip")),smacof_version=as.character(packageVersion("smacof")),R=R.version.string,path=find.package("grip")),auto_unbox=TRUE))'],text=True))
    install=Path(env['grip']['path'])
    env['grip']['installed_files_sha256']={str(p.relative_to(install)):sha256(p) for p in sorted(install.rglob('*')) if p.is_file()}
    return env

def history(root):
    rows=[]
    for filename in ['pilot_results.json','phase05_results.json']:
        rows.extend(dict(r,source_index=filename) for r in read_json(root/filename)['runs'])
    return rows

def run(root,graphs=GRAPHS):
    root=Path(root).resolve()
    if not graphs or len(set(graphs))!=len(graphs) or any(g not in GRAPHS for g in graphs):
        raise ValueError('select distinct graphs from the three authorized MDS retry cases')
    with serial_lock(root):
        return run_locked(root,graphs)

def run_locked(root,graphs):
    if subprocess.check_output(['git','status','--porcelain'],text=True).strip():
        raise RuntimeError('commit source before experiment')
    commit=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    env=capture_environment()
    code={str(p.relative_to(HERE.parent)):sha256(p) for directory in [PILOT,HERE,HERE.parent/'suitesparse_phase3']
          for p in sorted(directory.iterdir()) if p.suffix in ('.py','.R','.txt')}
    old=history(root)
    index=root/INDEX
    rows=read_json(index)['runs'] if index.exists() else []
    # Never overwrite a partial/failed/historical attempt or silently retry it.
    if any(r['graph_id'] in graphs for r in rows):
        raise ValueError('requested graph already recorded; choose unrecorded graphs or a separately identified corrective run')
    for graph in graphs:
        folder=root/'graphs'/graph.replace('/','__');info=read_json(folder/'graph.json')
        evaluation='exact' if graph=='Meszaros/nemscem' else 'suitesparse-uniform-pairs-v1'
        worker=PILOT/'worker.py' if evaluation=='exact' else HERE/'expanded_worker.py'
        previous={}
        for method in ['metric_mds','metric_mds_edge_kk']:
            for seed in SEEDS:
                prior=[r for r in old if (r['graph_id'],r['method'],r['seed'])==(graph,method,seed)]
                if len(prior)!=1 or prior[0]['status'] not in ('resource_limited','unavailable'):
                    raise ValueError('retry requires exactly one original resource-limited or dependent-unavailable case')
                earlier=prior[0]
                lineage=dict(index=earlier['source_index'],index_sha256=sha256(root/earlier['source_index']),
                    status=earlier['status'],run_dir=earlier.get('run_dir'))
                if earlier.get('run_dir'):
                    lineage['manifest_sha256']=sha256(Path(earlier['run_dir'])/'manifest.json')
                request=dict(schema_version=1,method=method,seed=seed,dimension=3,landmarks=64,
                    graph_dir=str(folder),graph_sha256=info['graph_sha256'],commit=commit,code=code,
                    environment=env,limits=LIMITS,evaluation=evaluation,previous_attempt=lineage,
                    attempt_label='30 GiB; no time limit; serial',
                    resource_admission=dict(policy='user-authorized higher-budget MDS retry; historical time projections are not admission gates',
                        memory_bytes=LIMITS['memory_bytes'],seconds=None,serial=True))
                request['allocation_preflight']=allocation_preflight(info,64,LIMITS['memory_bytes'])
                reason=None
                if not request['allocation_preflight']['admitted']: reason='prepared_allocation_preflight'
                if method=='metric_mds_edge_kk':
                    if seed not in previous: reason='matching higher-budget MDS run not completed; no edge-KK optimizer attempted'
                    else:
                        parent=previous[seed];manifest=read_json(parent/'manifest.json')
                        if not verified_cached(parent,manifest['run_key']):raise ValueError('invalid parent MDS assets')
                        request.update(initial_run=str(parent),initial_manifest_sha256=sha256(parent/'manifest.json'))
                key=identity(request);dest=root/'runs'/graph.replace('/','__')/f'{method}_seed{seed}_{key[:12]}'
                # An interrupted directory is immutable even without an index row.
                if dest.exists():dest=dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
                dest.mkdir(parents=True);request['output']=str(dest);atomic_json(dest/'request.json',request)
                print(graph,method,seed,'excluded' if reason else 'running',flush=True)
                if reason:
                    timing=dict(status='unavailable' if method=='metric_mds_edge_kk' else 'resource_limited',
                        reason=reason,elapsed_seconds=0.,peak_rss_bytes=None,limits=LIMITS)
                else:
                    start=time.monotonic()
                    try:timing=supervise([sys.executable,str(worker),str(dest/'request.json')],dest,
                        seconds=LIMITS['seconds'],memory=LIMITS['memory_bytes'])
                    except Exception as exc:timing=dict(status='failed',reason=f'supervisor_error: {type(exc).__name__}: {exc}',
                        elapsed_seconds=time.monotonic()-start,peak_rss_bytes=None,limits=LIMITS)
                if timing['status']=='completed' and not (dest/'result.json').exists():
                    timing.update(status='failed',reason='missing result')
                if timing['status']=='failed' and not timing.get('reason'):
                    timing['reason']=f"worker exit {timing.get('exit_code')}; see process.log"
                artifacts={str(p.relative_to(dest)):sha256(p) for p in sorted(dest.rglob('*'))
                    if p.is_file() and p.name!='manifest.json' and 'numba_cache' not in p.parts}
                atomic_json(dest/'manifest.json',dict(run_key=key,request=request,artifacts=artifacts,**timing))
                if method=='metric_mds' and timing['status']=='completed':
                    if not verified_cached(dest,key):raise ValueError('completed MDS failed artifact validation')
                    previous[seed]=dest
                row=dict(graph_id=graph,method=method,seed=seed,run_dir=str(dest),
                    **{k:timing.get(k) for k in ['status','reason','elapsed_seconds','peak_rss_bytes']})
                if timing['status']=='completed':row['scores']=read_json(dest/'result.json')['summary']
                rows.append(row);atomic_json(index,dict(schema_version=1,commit=commit,limits=LIMITS,serial=True,runs=rows))
                print(graph,method,seed,row['status'],round(row['elapsed_seconds'],2),flush=True)
    return rows

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('root')
    parser.add_argument('--graphs',nargs='+',choices=GRAPHS,default=GRAPHS)
    args=parser.parse_args();run(args.root,args.graphs)
