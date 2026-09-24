"""Measured method-specific admission; serial jobs, immutable historical indexes."""
import argparse
import importlib.metadata
import os
from pathlib import Path
import subprocess
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json,atomic_json,identity,sha256
from run_pilot import supervise,verified_cached,allocation_preflight,METHODS as BASE
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase3'))
from adapters import METHODS as EXTRA

INDEXES=['pilot_results.json','phase03_results.json','phase03_trimap_graph_results.json']
LIMITS=dict(seconds=600,memory_bytes=2*1024**3)

def admission(info,method,references,root):
    candidates=[]
    for r in references:
        if r['method']!=method or r['status']!='completed' or not r.get('peak_rss_bytes'):continue
        g=read_json(root/'graphs'/r['graph_id'].replace('/','__')/'graph.json')
        candidates.append((max(g['component_sizes']),r,g))
    if not candidates:return dict(admitted=False,reason='no completed same-method calibration')
    largest=max(n for n,_,_ in candidates);chosen=[(r,g) for n,r,g in candidates if n==largest]
    memory=max(r['peak_rss_bytes'] for r,g in chosen)*1.1*(max(info['component_sizes'])/largest)**2
    seconds=max(r['elapsed_seconds']/sum(n**3 for n in g['component_sizes'] if n>1) for r,g in chosen)*1.25*sum(n**3 for n in info['component_sizes'] if n>1)
    ok=memory<=LIMITS['memory_bytes'] and seconds<=LIMITS['seconds']
    return dict(admitted=ok,reason='admitted by measured same-method projection' if ok else 'conservative same-method time/memory projection exceeds job budget',
        projected_memory_bytes=memory,projected_seconds=seconds,reference_component_vertices=largest,
        references=[dict(graph_id=r['graph_id'],seed=r['seed'],run_dir=r['run_dir'],manifest_sha256=sha256(Path(r['run_dir'])/'manifest.json')) for r,g in chosen],
        policy='largest completed same-method graph, worst replicate; quadratic RSS x1.1, cubic component-summed time x1.25; sampled scorer may reduce actual cost, not assumed in admission')

def run(root,graph,contracts,native_manifests):
    root=Path(root);source=Path(__file__).resolve().parent
    if subprocess.check_output(['git','status','--porcelain'],text=True).strip():raise RuntimeError('commit before experiment')
    commit=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    references=sum([read_json(root/name)['runs'] for name in INDEXES],[])
    index_path=root/'phase05_results.json'
    previous=read_json(index_path)['runs'] if index_path.exists() else []
    if any(r['graph_id']==graph for r in previous):raise ValueError('graph already recorded; use a separately identified corrective run')
    references+=previous
    contract_rows={}
    for file in contracts:
        c=read_json(file)
        for name in ('adapters.py','feature_worker.py','probe.py'):
            if c['code'][name]!=sha256(source.parent/'suitesparse_phase3'/name):raise ValueError('adapter contract changed')
        for r in c['results']:
            if r['status']=='validated':contract_rows[r['method']]=r
    folder=root/'graphs'/graph.replace('/','__');info=read_json(folder/'graph.json')
    code={str(p.relative_to(source.parent)):sha256(p) for parent in [source,source.parent/'suitesparse_phase1',source.parent/'suitesparse_phase3']
          for p in sorted(parent.iterdir()) if p.suffix in ('.py','.R','.txt')}
    env=dict(python=sys.version,packages={d.metadata['Name']:d.version for d in importlib.metadata.distributions()},threads=1,
        native_builds=[dict(sha256=sha256(p),manifest=read_json(p)) for p in native_manifests])
    if os.environ.get('GFLOWUI_LARGEVIS_BINARY'):env['largevis_binary_sha256']=sha256(os.environ['GFLOWUI_LARGEVIS_BINARY'])
    grip=subprocess.check_output(['Rscript','-e','cat(jsonlite::toJSON(list(version=as.character(packageVersion("grip")),R=R.version.string,path=find.package("grip")),auto_unbox=TRUE))'],text=True)
    import json
    env['grip']=json.loads(grip)
    install=Path(env['grip']['path']);env['grip']['installed_files_sha256']={str(f.relative_to(install)):sha256(f) for f in sorted(install.rglob('*')) if f.is_file()}
    rows=list(previous);initial={}
    for method in BASE+EXTRA+['lgs_paper']:
        policy=admission(info,method,references,root)
        if method in EXTRA and method not in contract_rows:policy.update(admitted=False,reason='missing validated 3D adapter contract')
        if method=='lgs_paper':policy=dict(admitted=False,reason='component exceeds accepted LGS 2000-vertex contract; no optimizer attempted')
        for seed in ([17] if method in ('isomap_graph','lle') else [17,29,43]):
            request=dict(schema_version=1,method=method,seed=seed,landmarks=64,dimension=3,graph_dir=str(folder),
                graph_sha256=info['graph_sha256'],commit=commit,code=code,environment=env,
                limits=LIMITS,resource_admission=policy,adapter_contract=contract_rows.get(method),
                evaluation=dict(version='suitesparse-uniform-pairs-v1',pairs_per_component=20000,seed=314159,bootstraps=200),
                small_component_cutoff=63 if method=='trimap_graph' else 5)
            admitted=policy['admitted'];reason=policy['reason']
            if method=='metric_mds_edge_kk':
                if seed not in initial:admitted=False;reason='matching MDS run unavailable; no edge-KK optimizer attempted'
                else:request['initial_run']=str(initial[seed])
            alloc=allocation_preflight(info,64,LIMITS['memory_bytes']);request['allocation_preflight']=alloc
            if not alloc['admitted']:admitted=False;reason='prepared allocation exceeds memory budget'
            key=identity(request);dest=root/'runs'/graph.replace('/','__')/f'{method}_seed{seed}_{key[:12]}'
            if not verified_cached(dest,key):
                if dest.exists():dest=dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
                dest.mkdir(parents=True);request['output']=str(dest);atomic_json(dest/'request.json',request)
                print(graph,method,seed,'running' if admitted else 'excluded',flush=True)
                if admitted:
                    started=time.monotonic()
                    try:timing=supervise([sys.executable,str(source/'expanded_worker.py'),str(dest/'request.json')],dest,**dict(seconds=600,memory=2*1024**3))
                    except Exception as exc:timing=dict(status='failed',reason=f'supervisor_error: {type(exc).__name__}: {exc}',elapsed_seconds=time.monotonic()-started,peak_rss_bytes=None)
                else:timing=dict(status='resource_limited' if method!='metric_mds_edge_kk' else 'unavailable',reason=reason,elapsed_seconds=0.,peak_rss_bytes=None)
                if timing['status']=='completed' and not (dest/'result.json').exists():timing.update(status='failed',reason='missing result')
                if timing['status']=='failed' and not timing.get('reason'):timing['reason']=f"worker exit {timing.get('exit_code')}; see process.log"
                artifacts={str(p.relative_to(dest)):sha256(p) for p in sorted(dest.rglob('*')) if p.is_file() and p.name!='manifest.json' and 'numba_cache' not in p.parts}
                atomic_json(dest/'manifest.json',dict(run_key=key,request=request,artifacts=artifacts,**timing))
            manifest=read_json(dest/'manifest.json')
            if method=='metric_mds' and manifest['status']=='completed':initial[seed]=dest
            row=dict(graph_id=graph,method=method,seed=seed,run_dir=str(dest),
                **{k:manifest.get(k) for k in ('status','reason','elapsed_seconds','peak_rss_bytes')})
            if row['status']=='completed':row['scores']=read_json(dest/'result.json')['summary']
            rows.append(row);atomic_json(index_path,dict(schema_version=1,commit=commit,runs=rows))
            print(method,seed,row['status'],round(row['elapsed_seconds'],2),flush=True)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');p.add_argument('--graph',required=True)
    p.add_argument('--contracts',nargs='+',required=True);p.add_argument('--native-manifests',nargs='+',required=True)
    a=p.parse_args();run(a.root,a.graph,a.contracts,a.native_manifests)
