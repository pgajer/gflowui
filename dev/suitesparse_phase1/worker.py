"""One embedding job in a supervised subprocess (including its GRIP child)."""
import argparse
import os
from pathlib import Path
import subprocess
import warnings
import numpy as np
from scipy.linalg import eigh
from scipy.sparse import load_npz
from sklearn.manifold import LocallyLinearEmbedding
from common import atomic_json, read_json, sha256
from graphs import prepare
from metrics import validate_coords, score_component, aggregate


def embed(method, adjacency, ids, d, features, seed, dest, initial=None):
    n=len(ids)
    detail={}
    if method.startswith('metric_mds') or method=='weighted_grip':
        from scipy.sparse import triu
        upper=triu(adjacency,k=1).tocoo()
        edges=np.column_stack((upper.row,upper.col))
        edge_file=dest/'r_edges.csv'
        np.savetxt(edge_file,edges,fmt='%d',delimiter=',',header='source,target',comments='')
        req=dict(method=method,n=n,edge_file=str(edge_file),seed=seed)
        if initial is not None:
            req['initial_file']=str(initial)
        atomic_json(dest/'r_request.json',req)
        subprocess.run(['Rscript',str(Path(__file__).with_name('grip_adapter.R')),
                        str(dest/'r_request.json'),str(dest/'r_coords.csv')],check=True)
        coords=np.loadtxt(dest/'r_coords.csv',delimiter=',',skiprows=1,ndmin=2)
        detail=read_json(dest/'r_coords.csv.json')
    elif method=='isomap_graph':
        # Classical scaling of ORIGINAL graph distances, not an inferred kNN graph.
        squared=d*d
        gram=-.5*(squared-squared.mean(axis=0)[None,:]-squared.mean(axis=1)[:,None]+squared.mean())
        values,vectors=eigh(gram,subset_by_index=[max(0,n-3),n-1])
        order=np.argsort(values)[::-1]
        coords=vectors[:,order]*np.sqrt(np.maximum(values[order],0))[None,:]
        coords=np.pad(coords,((0,0),(0,3-coords.shape[1])))
        detail=dict(algorithm='classical scaling of original graph shortest paths',eigenvalues=values[order].tolist(),
                    termination='eigensolve completed')
    elif method=='umap':
        from umap import UMAP
        model=UMAP(n_components=3,metric='precomputed',n_neighbors=min(15,n-1),
                   min_dist=.1,random_state=seed,n_jobs=1,init='random',n_epochs=300)
        coords=model.fit_transform(d)
        detail=dict(n_neighbors=min(15,n-1),min_dist=.1,n_epochs=300,init='random',
                    termination='fixed epoch budget; no convergence claim')
    elif method=='lle':
        model=LocallyLinearEmbedding(n_components=3,n_neighbors=min(15,n-1),
                      method='standard',eigen_solver='dense',reg=.001,random_state=seed)
        coords=model.fit_transform(features)
        detail=dict(n_neighbors=min(15,n-1),reg=.001,eigen_solver='dense',
                    reconstruction_error=float(model.reconstruction_error_),termination='eigensolve completed')
    else:
        raise ValueError('unsupported method')
    return validate_coords(coords,ids,ids),detail


def run(request, embedder=embed, input_type=None):
    graph_dir=Path(request['graph_dir'])
    info=read_json(graph_dir/'graph.json')
    if sha256(graph_dir/'adjacency.npz')!=info['adjacency_sha256']:
        raise ValueError('graph adjacency checksum mismatch')
    adjacency=load_npz(graph_dir/'adjacency.npz')
    labels=np.array(info['component_labels'])
    root=Path(request['output'])
    method,seed=request['method'],request['seed']
    components=[]
    full=np.zeros((len(labels),3))
    packed=np.zeros_like(full)
    offset=0.
    for component in range(info['n_components']):
        indices=np.flatnonzero(labels==component)
        ids=[info['vertex_ids'][i] for i in indices]
        sub=adjacency[indices][:,indices].tocsr()
        dest=root/f'component_{component:03d}'
        dest.mkdir(parents=True,exist_ok=True)
        d,p,f,prep=prepare(sub,ids,request.get('landmarks',64))
        atomic_json(dest/'preparation.json',prep)
        initial=None
        if method=='metric_mds_edge_kk':
            initial=Path(request['initial_run'])/f'component_{component:03d}'/'coords.csv'
        small=len(ids)<request.get('small_component_cutoff',5)
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter('always')
            if small:
                z,detail=embed('isomap_graph',sub,ids,d,f,seed,dest)
                detail['small_component_placement']='classical scaling; not attributed to requested method'
            else:
                z,detail=embedder(method,sub,ids,d,f,seed,dest,initial)
                z=validate_coords(z,ids,ids)
        coord_file=dest/'coords.csv'
        np.savetxt(coord_file,z,delimiter=',',header='x,y,z',comments='')
        atomic_json(dest/'vertices.json',ids)
        from scipy.sparse import triu
        upper=triu(sub,k=1).tocoo()
        edges=np.column_stack((upper.row,upper.col))
        score=score_component(z,d,p,edges,ids)
        score['n_vertices']=len(ids)
        score['component']=component
        score['small_component_placement']=small
        score['input_sha256']=prep['input_sha256']
        score['coordinates_sha256']=sha256(coord_file)
        score['details']=detail
        score['warnings']=[str(w.message) for w in captured]
        if initial is not None:
            score['initial_coordinates_sha256']=sha256(initial)
        components.append(score)
        full[indices]=z
        disp=z-z.mean(axis=0)
        disp[:,0]-=disp[:,0].min()
        disp[:,0]+=offset
        offset=float(disp[:,0].max()+2)
        packed[indices]=disp
    np.savetxt(root/'coords_raw.csv',full,delimiter=',',header='x,y,z',comments='')
    np.savetxt(root/'coords_display.csv',packed,delimiter=',',header='x,y,z',comments='')
    atomic_json(root/'vertices.json',info['vertex_ids'])
    result=dict(schema_version=1,status='completed',method=method,seed=seed,dimension=3,
                graph_sha256=info['graph_sha256'],components=components,summary=aggregate(components),
                cross_component_pairs_excluded=int(len(labels)*(len(labels)-1)//2-sum(x['n_pairs'] for x in components)),
                packing='x separated components; display only; not used in scores',
                coords_sha256=sha256(root/'coords_raw.csv'),display_sha256=sha256(root/'coords_display.csv'),
                cluster_distance_status='deferred optional metric; no frozen partition',
                input_type=input_type or ('landmark_distance_features' if method=='lle' else 'original_graph_distances_or_edges'))
    atomic_json(root/'result.json',result)


if __name__=='__main__':
    parser=argparse.ArgumentParser()
    parser.add_argument('request')
    args=parser.parse_args()
    run(read_json(args.request))
