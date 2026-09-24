"""Serial main-project LGS integration fixtures and gallery admission matrix."""
import argparse
import importlib.metadata
from pathlib import Path
import subprocess
import sys
import time
import numpy as np
from scipy.sparse import coo_matrix,save_npz
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256,identity
from graphs import convert
from run_pilot import supervise,verified_cached
sys.path.insert(0,str(Path(__file__).resolve().parent))
from lgs_adapter import dependency,locality_settings,PARAMETERS

def fixture(root,name,n,edges):
    folder=Path(root)/'graphs'/name
    a=coo_matrix((np.ones(len(edges)),np.asarray(edges).T),shape=(n,n))
    a,info=convert(a,name)
    if (folder/'graph.json').exists():
        if read_json(folder/'graph.json')['graph_sha256']!=info['graph_sha256']: raise ValueError('fixture identity changed')
        return read_json(folder/'graph.json')
    folder.mkdir(parents=True)
    save_npz(folder/'adjacency.npz',a);info['adjacency_sha256']=sha256(folder/'adjacency.npz')
    info['source']=dict(kind='synthetic integration validation; NOT SuiteSparse gallery',name=name)
    atomic_json(folder/'graph.json',info)
    return info

def make_fixtures(root):
    graphs=[fixture(root,'validation_path48',48,[(i,i+1) for i in range(47)]),
        fixture(root,'validation_grid49',49,[(i,j) for i in range(49) for j in range(i+1,49)
                                           if j==i+7 or (j==i+1 and i//7==j//7)]),
        fixture(root,'validation_cliques48',48,[(i,j) for i in range(48) for j in range(i+1,48)
                                               if i//24==j//24]+[(23,24)]),
        fixture(root,'validation_tetrahedron',4,[(i,j) for i in range(4) for j in range(i+1,4)]),
        fixture(root,'validation_path128',128,[(i,i+1) for i in range(127)]),
        fixture(root,'validation_components',10,[(i,j) for i in range(4) for j in range(i+1,4)]+
                [(i,i+1) for i in range(4,8)])]
    atomic_json(Path(root)/'cohort.json',dict(schema_version=1,records=[dict(graph_id=g['graph_id'],
        graph_sha256=g['graph_sha256'],status='completed') for g in graphs]))
    return graphs

def admission(info,calibration,seconds=600,memory_bytes=2*1024**3):
    sizes=info['component_sizes'];n=max(sizes)
    # Sequential component times add. Isolates use the explicit outer placement.
    rates=[r['elapsed_seconds']/r['component_sizes'][0]**3 for r in calibration
           if r['status']=='completed' and r.get('elapsed_seconds',0)>0
           and len(r['component_sizes'])==1 and r['component_sizes'][0]>=32]
    estimate=2*max(rates)*sum(s**3 for s in sizes if s>1) if rates else None
    memory=128*1024**2+128*n*n+512*info['n_edges']
    reason=('accepted LGS contract admits at most 2000 vertices per component' if n>2000 else
            'no completed runtime calibration' if estimate is None else
            'conservative cubic time projection exceeds 600 seconds' if estimate>seconds else
            'LGS dense-memory preflight exceeds 2 GiB' if memory>memory_bytes else None)
    return dict(admitted=reason is None,reason=reason,projected_seconds=estimate,
                preflight_memory_bytes=memory,largest_component=n,
                time_rule='2 * max(measured total seconds / connected calibration n^3, n>=32) * sum(nontrivial component n^3)',
                measured_runtime=False)

def run_one(root,info,setting,seed,dep,code,commit,preflight=None):
    root=Path(root);folder=root/'graphs'/info['graph_id'].replace('/','__')
    request=dict(schema_version=1,method='lgs_paper',seed=seed,dimension=3,landmarks=64,
        graph_id=info['graph_id'],graph_dir=str(folder),graph_sha256=info['graph_sha256'],
        locality_requested=setting['requested'],locality=setting,small_component_cutoff=2,
        lgs_dependency=dep,parameters=PARAMETERS,code=code,commit=commit,
        environment=dict(python=sys.version,platform=sys.platform,
            packages={name:importlib.metadata.version(name) for name in ['numpy','scipy','networkx','scikit-learn']}),
        limits=dict(seconds=600,memory_bytes=2*1024**3),admission=preflight)
    key=identity(request);dest=root/'runs'/info['graph_id'].replace('/','__')/f'lgs_k{setting["requested"]}_seed{seed}_{key[:12]}'
    if not verified_cached(dest,key):
        if dest.exists(): dest=dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
        dest.mkdir(parents=True);request['output']=str(dest);atomic_json(dest/'request.json',request)
        if preflight is not None and not preflight['admitted']:
            timing=dict(status='resource_limited',reason=preflight['reason'],elapsed_seconds=0.,peak_rss_bytes=0,limits=request['limits'])
        else:
            timing=supervise([sys.executable,str(Path(__file__).with_name('lgs_worker.py')),str(dest/'request.json')],dest)
            if timing['status']=='completed' and not (dest/'result.json').exists():
                timing.update(status='failed',reason='missing main-project result')
            if timing['status']=='failed':
                for path in dest.glob('component_*/lgs_stdout.json'):
                    try: response=read_json(path)
                    except (ValueError,OSError): continue
                    if response.get('status') in ('resource_limited','unsupported','invalid_input','cancelled'):
                        timing.update(status=response['status'],reason=response.get('termination'))
        artifacts={str(p.relative_to(dest)):sha256(p) for p in sorted(dest.rglob('*')) if p.is_file() and p.name!='manifest.json'}
        atomic_json(dest/'manifest.json',dict(run_key=key,request=request,artifacts=artifacts,**timing))
    manifest=read_json(dest/'manifest.json')
    row=dict(graph_id=info['graph_id'],method='lgs_paper',seed=seed,locality=setting,
        component_sizes=info['component_sizes'],run_dir=str(dest),
        **{k:manifest.get(k) for k in ['status','reason','elapsed_seconds','peak_rss_bytes']})
    if row['status']=='completed': row['scores']=read_json(dest/'result.json')['summary']
    print(info['graph_id'],setting['requested'],seed,row['status'],round(row['elapsed_seconds'],2),flush=True)
    return row

def run(root,dep,mode,calibration_path=None):
    root=Path(root).resolve();root.mkdir(parents=True,exist_ok=True)
    if subprocess.check_output(['git','status','--porcelain'],text=True).strip(): raise ValueError('commit main source first')
    commit=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    source=Path(__file__).resolve().parent
    code={str(p.relative_to(source.parent)):sha256(p) for directory in [source,source.parent/'suitesparse_phase1']
          for p in sorted(directory.glob('*.py'))}
    graphs=make_fixtures(root) if mode=='validation' else [read_json(root/'graphs'/r['graph_id'].replace('/','__')/'graph.json')
        for r in read_json(root/'cohort.json')['records'] if r['status']=='completed']
    calibration=read_json(calibration_path)['runs'] if calibration_path else None
    rows=[];result_name='phase04_validation_results.json' if mode=='validation' else 'phase04_results.json'
    for info in graphs:
        settings=locality_settings(info['component_sizes'])
        if info['graph_id']=='validation_path128': settings=settings[:1]
        for setting in settings:
            for seed in ([17] if info['graph_id']=='validation_path128' else [17,29,43]):
                rows.append(run_one(root,info,setting,seed,dep,code,commit,
                    admission(info,calibration) if mode=='gallery' else None))
                atomic_json(root/result_name,dict(schema_version=1,commit=commit,
                    mode=mode,dependency=dep,calibration_source=str(calibration_path) if calibration_path else None,
                    calibration_sha256=sha256(calibration_path) if calibration_path else None,runs=rows))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');p.add_argument('--dependency',required=True)
    p.add_argument('--python',required=True);p.add_argument('--mode',choices=['validation','gallery'],required=True)
    p.add_argument('--calibration');a=p.parse_args()
    if a.mode=='gallery' and not a.calibration: p.error('gallery mode requires measured calibration')
    run(a.root,dependency(a.dependency,a.python),a.mode,a.calibration)
