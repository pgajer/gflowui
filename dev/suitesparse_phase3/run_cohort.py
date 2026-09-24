"""Frozen small-cohort expansion: one supervised subprocess per method/seed."""
import argparse
import importlib.metadata
import os
from pathlib import Path
import subprocess
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256,identity
from run_pilot import supervise,verified_cached,allocation_preflight
from adapters import METHODS

def run(root,contracts,methods,native_manifests=()):
    root=Path(root).resolve();source=Path(__file__).resolve().parent
    if subprocess.check_output(['git','status','--porcelain'],text=True).strip():
        raise RuntimeError('commit source before cohort execution')
    revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    code={str(p.relative_to(source.parent)):sha256(p)
          for folder in (source,source.parent/'suitesparse_phase1')
          for p in sorted(folder.iterdir()) if p.suffix in ('.py','.R','.txt')}
    packages={dist.metadata['Name']:dist.version for dist in importlib.metadata.distributions()}
    env=dict(python=sys.version,packages=dict(sorted(packages.items())),threads=1,platform=sys.platform)
    env['native_builds']=[dict(path=str(Path(p).resolve()),sha256=sha256(p),manifest=read_json(p))
                          for p in native_manifests]
    binary=os.environ.get('GFLOWUI_LARGEVIS_BINARY')
    if binary: env['largevis_binary_sha256']=sha256(binary)
    validated={}
    for path in contracts:
        for record in read_json(path)['results']:
            if record['method'] in validated: raise ValueError('duplicate method contract')
            validated[record['method']]=record
    rows=[]
    for graph in read_json(root/'cohort.json')['records']:
        if graph['status']!='completed': continue
        folder=root/'graphs'/graph['graph_id'].replace('/','__')
        info=read_json(folder/'graph.json')
        for method in methods:
            contract=validated.get(method)
            for seed in (17,29,43):
                row=dict(graph_id=graph['graph_id'],method=method,seed=seed)
                if not contract or contract['status']!='validated':
                    rows.append(dict(**row,status='unsupported',reason='3D adapter contract not validated'))
                    continue
                request=dict(schema_version=1,method=method,seed=seed,landmarks=64,dimension=3,
                    graph_dir=str(folder),graph_sha256=info['graph_sha256'],code=code,commit=revision,
                    environment=env,adapter_contract=contract,
                    limits=dict(seconds=600,memory_bytes=2*1024**3))
                request['allocation_preflight']=allocation_preflight(info,64,2*1024**3)
                key=identity(request)
                dest=root/'runs'/graph['graph_id'].replace('/','__')/f'{method}_seed{seed}_{key[:12]}'
                if not verified_cached(dest,key):
                    if dest.exists(): dest=dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
                    dest.mkdir(parents=True);request['output']=str(dest)
                    atomic_json(dest/'request.json',request)
                    print(graph['graph_id'],method,seed,'running',flush=True)
                    if request['allocation_preflight']['admitted']:
                        timing=supervise([sys.executable,str(source/'feature_worker.py'),str(dest/'request.json')],dest)
                    else:
                        timing=dict(status='resource_limited',reason='prepared_allocation_preflight',
                                    elapsed_seconds=0.,peak_rss_bytes=0,limits=request['limits'])
                    if timing['status']=='completed' and not (dest/'result.json').exists():
                        timing.update(status='failed',reason='missing result')
                    artifacts={str(p.relative_to(dest)):sha256(p) for p in sorted(dest.rglob('*'))
                               if p.is_file() and 'numba_cache' not in p.parts and p.name!='manifest.json'}
                    atomic_json(dest/'manifest.json',dict(run_key=key,request=request,artifacts=artifacts,**timing))
                manifest=read_json(dest/'manifest.json')
                row.update(status=manifest['status'],reason=manifest.get('reason'),run_dir=str(dest),
                           elapsed_seconds=manifest['elapsed_seconds'],peak_rss_bytes=manifest['peak_rss_bytes'])
                if row['status']=='completed': row['scores']=read_json(dest/'result.json')['summary']
                rows.append(row)
                atomic_json(root/'phase03_results.json',dict(schema_version=1,commit=revision,runs=rows))
                print(graph['graph_id'],method,seed,row['status'],round(row['elapsed_seconds'],2),flush=True)
    atomic_json(root/'phase03_results.json',dict(schema_version=1,commit=revision,runs=rows))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');p.add_argument('--contracts',nargs='+',required=True)
    p.add_argument('--methods',choices=METHODS,nargs='+',default=METHODS)
    p.add_argument('--native-manifests',nargs='*',default=[])
    a=p.parse_args();run(a.root,a.contracts,a.methods,a.native_manifests)
