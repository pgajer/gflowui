"""Sensitivity to reversing lexical ID tie priority, without changing layouts."""
import argparse
from pathlib import Path
import numpy as np
from scipy.sparse import load_npz
from scipy.sparse.csgraph import shortest_path
from scipy.spatial.distance import pdist, squareform
from common import atomic_json, read_json, sha256
from metrics import rank_quality
from run_pilot import verified_cached


def reversed_priorities(ids):
    ordered=sorted(ids)
    reverse=dict(zip(ordered,reversed(ordered)))
    return [reverse[x] for x in ids]


def eligible_runs(index):
    return [r for r in index['runs'] if r['status']=='completed' and r['seed']==17]


def validate_coverage(data,index,index_hash):
    def key(row): return (row['graph_id'],row['method'],row['seed'])
    expected=[key(row) for row in eligible_runs(index)]
    actual=[key(row) for row in data.get('runs',[])]
    if (data.get('status')!='completed' or data.get('schema_version')!=2
            or data.get('source_index_sha256')!=index_hash
            or len(expected)!=len(set(expected)) or len(actual)!=len(set(actual))
            or set(actual)!=set(expected)):
        raise ValueError('incomplete, duplicate, unexpected or stale tie diagnostic coverage')
    return data


def diagnostic(root,index_name='pilot_results.json',output_name='tie_sensitivity.json'):
    root=Path(root)
    if any(Path(name).name!=name or not name.endswith('.json') for name in (index_name,output_name)) or index_name==output_name:
        raise ValueError('index/output must be distinct plain JSON filenames')
    index=read_json(root/index_name)
    rows=[]
    index_hash=sha256(root/index_name)
    for run in eligible_runs(index):
        dest=Path(run['run_dir']); manifest=read_json(dest/'manifest.json')
        if not verified_cached(dest,manifest['run_key']):
            raise ValueError('invalid run artifacts: '+str(dest))
        graph_dir=root/'graphs'/run['graph_id'].replace('/','__')
        graph=read_json(graph_dir/'graph.json')
        if sha256(graph_dir/'adjacency.npz')!=graph['adjacency_sha256']:
            raise ValueError('graph adjacency checksum mismatch')
        a=load_npz(graph_dir/'adjacency.npz')
        labels=np.array(graph['component_labels'])
        original={};reverse={};weight={}
        result=read_json(dest/'result.json')
        for comp in result['components']:
            idx=np.flatnonzero(labels==comp['component'])
            if len(idx)<3: continue
            ids=[graph['vertex_ids'][i] for i in idx]
            z=np.loadtxt(dest/f"component_{comp['component']:03d}"/'coords.csv',delimiter=',',skiprows=1,ndmin=2)
            d=shortest_path(a[idx][:,idx],directed=False,unweighted=True,method='D')
            r=squareform(pdist(z))
            for k in [5,10,20,50]:
                if k>=len(idx)/2: continue
                forward=rank_quality(d,r,ids,k)
                backward=rank_quality(d,r,reversed_priorities(ids),k)
                for name,x,y in zip(['trustworthiness','continuity'],forward,backward):
                    key=f'{name}_{k}'
                    if not np.isclose(x,comp['neighborhood'][key],rtol=1e-10,atol=1e-12):
                        raise ValueError('canonical score mismatch')
                    original[key]=original.get(key,0)+x*len(idx)
                    reverse[key]=reverse.get(key,0)+y*len(idx)
                    weight[key]=weight.get(key,0)+len(idx)
        original={k:v/weight[k] for k,v in original.items()}
        reverse={k:v/weight[k] for k,v in reverse.items()}
        delta={k:reverse[k]-original[k] for k in original}
        rows.append(dict(graph_id=run['graph_id'],method=run['method'],seed=run['seed'],
            run_key=manifest['run_key'],coordinates_sha256=result['coords_sha256'],
            original=original,reversed_id_priority=reverse,delta=delta,
            max_absolute_change=max(map(abs,delta.values()),default=None)))
        print(run['graph_id'],run['method'],'checked',flush=True)
    complete=dict(schema_version=2,status='completed',source_index_sha256=index_hash,
        diagnostic='reverse lexical vertex-ID tie priority; fixed layouts and graph distances',
        coverage='completed main-cohort seed-17 runs only; not a new embedding experiment',runs=rows)
    validate_coverage(complete,index,index_hash)
    if sha256(root/index_name)!=index_hash:
        raise ValueError('result index changed during diagnostic')
    # Do not publish a prefix. Cancellation preserves any prior complete artifact.
    atomic_json(root/output_name,complete)


if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('root')
    parser.add_argument('--index',default='pilot_results.json')
    parser.add_argument('--output',default='tie_sensitivity.json')
    args=parser.parse_args();diagnostic(args.root,args.index,args.output)
