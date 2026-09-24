"""Reproduce saved TriMAP constraints, without rerunning its optimizer."""
import argparse
import hashlib
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import numpy as np
from scipy.sparse import load_npz
from graphs import prepare
from common import read_json,atomic_json,sha256
from adapters import exact_feature_neighbors,seed_numba

def diagnose(root):
    from trimap.trimap_ import generate_triplets_known_knn
    root=Path(root);rows=[]
    for run in read_json(root/'phase03_results.json')['runs']:
        if run['method']!='trimap' or run['seed']!=17 or run['status']!='completed': continue
        folder=root/'graphs'/run['graph_id'].replace('/','__')
        graph=read_json(folder/'graph.json');a=load_npz(folder/'adjacency.npz')
        if sha256(folder/'adjacency.npz')!=graph['adjacency_sha256']: raise ValueError('graph checksum changed')
        labels=np.array(graph['component_labels']);component=int(np.argmax(graph['component_sizes']))
        idx=np.flatnonzero(labels==component);ids=[graph['vertex_ids'][i] for i in idx]
        if len(ids)<63: raise ValueError('diagnostic expects the pilot large component')
        d,p,x,prep=prepare(a[idx][:,idx],ids);x=x.astype(np.float32)
        neighbors,distances=exact_feature_neighbors(x,ids,62)
        raw_sig=distances[:,3:6].mean(axis=1)
        seed_numba(17);np.random.seed(17)
        triplets,weights=generate_triplets_known_knn(x,neighbors,distances,12,4,3,None,'euclidean',False,.5)
        result=read_json(Path(run['run_dir'])/'result.json');detail=result['components'][component]['details']
        hashes_match=(hashlib.sha256(triplets.tobytes()).hexdigest()==detail['triplets_sha256'] and
                      hashlib.sha256(weights.tobytes()).hexdigest()==detail['weights_sha256'])
        if not hashes_match: raise ValueError('constraints differ from saved run')
        coords=Path(run['run_dir'])/'coords_raw.csv'
        if sha256(coords)!=result['coords_sha256']: raise ValueError('coordinates changed')
        z=np.loadtxt(coords,delimiter=',',skiprows=1)
        rows.append(dict(graph_id=run['graph_id'],seed=17,component=component,vertices=len(ids),
            distinct_feature_rows=len(np.unique(x,axis=0)),sigma_floor_vertices=int(np.sum(raw_sig<1e-10)),
            weight_quantiles=np.quantile(weights,[0,.5,.9,.99,1]).tolist(),
            degenerate_triplets=int(np.sum((triplets[:,0]==triplets[:,1])|(triplets[:,0]==triplets[:,2])|(triplets[:,1]==triplets[:,2]))),
            max_absolute_coordinate=float(np.abs(z).max()),exact_saved_triplet_weight_hashes_match=hashes_match))
        print(run['graph_id'],'constraints reproduced',flush=True)
    atomic_json(root/'phase03_trimap_scale_diagnosis.json',dict(schema_version=1,results=rows,
        source_index_sha256=sha256(root/'phase03_results.json'),
        interpretation='Repeated feature rows cause local scale floors and extreme weights; these retained runs are not evidence of useful layout quality.'))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');diagnose(p.parse_args().root)
