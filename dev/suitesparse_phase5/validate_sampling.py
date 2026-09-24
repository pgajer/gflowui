"""Retained exact pilot layouts: agreement and conditional interval coverage."""
import argparse
from pathlib import Path
import sys
import numpy as np
from scipy.sparse import load_npz,triu
sys.path.insert(0,str(Path(__file__).resolve().parent))
import sampled_metrics as sample
from common import read_json,atomic_json,sha256
from graphs import prepare

def run(root,output,repetitions=20):
    root=Path(root);rows=read_json(root/'pilot_results.json')['runs'];records=[]
    for graph in ['HB/lock_700','HB/illc1033','Meszaros/nemscem']:
        info=read_json(root/'graphs'/graph.replace('/','__')/'graph.json')
        a=load_npz(root/'graphs'/graph.replace('/','__')/'adjacency.npz');labels=np.array(info['component_labels'])
        c=int(np.argmax(info['component_sizes']));idx=np.flatnonzero(labels==c);sub=a[idx][:,idx]
        ids=[info['vertex_ids'][i] for i in idx];d,p,_,_=prepare(sub,ids)
        u=triu(sub,k=1).tocoo();edges=np.column_stack((u.row,u.col))
        for method in ['weighted_grip','isomap_graph']:
            row=next(r for r in rows if r['graph_id']==graph and r['method']==method and r['seed']==17)
            dest=Path(row['run_dir']);result=read_json(dest/'result.json');exact=result['components'][c]
            z=np.loadtxt(dest/'coords_raw.csv',delimiter=',',skiprows=1)[idx]
            trials=[]
            for seed in range(100,100+repetitions):
                x=sample.score_component(z,d,p,edges,ids,seed=seed,neighborhood=seed==100)
                x['n_vertices']=len(idx);summary=sample.aggregate([x])
                errors={k:dict(exact=exact[k],estimate=x[k],absolute_error=abs(x[k]-exact[k]),
                    interval=summary['intervals'].get(k),covered=summary['intervals'][k]['lower']<=exact[k]<=summary['intervals'][k]['upper'])
                    for k in ['chord_error','relative_stress','path_error','distance_rank_correlation'] if exact[k] is not None}
                if seed==100:
                    for key,value in x['neighborhood'].items():
                        if value is not None:assert abs(value-exact['neighborhood'][key])<1e-12
                    assert abs(x['edge_error']-exact['edge_error'])<1e-12
                trials.append(dict(seed=seed,pair_count=x['evaluation']['pair_count'],pairs_sha256=x['evaluation']['pairs_sha256'],metrics=errors))
            records.append(dict(graph_id=graph,component=c,n_vertices=len(idx),method=method,seed=17,
                original_result_sha256=sha256(dest/'result.json'),original_run_dir=str(dest),trials=trials))
            atomic_json(output,dict(version=sample.VERSION,repetitions=repetitions,bootstrap_replicates=sample.BOOTSTRAPS,
                purpose='conditional pair-sampling accuracy on six fixed original layouts; not optimization reruns',runs=records))
            print(graph,method,'validated',flush=True)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');p.add_argument('output');p.add_argument('--repetitions',type=int,default=20)
    a=p.parse_args();run(a.root,a.output,a.repetitions)
