"""Build a checksummed, read-only Shiny view of completed scientific assets."""
import argparse
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import numpy as np
from scipy.sparse import load_npz
from scipy.sparse.csgraph import shortest_path
from common import atomic_json,read_json,sha256,identity
from run_pilot import verified_cached

PLANNED=['pacmap','localmap','trimap','phate','largevis','ncvis','lgs_paper']


def pair_sample(n,limit=2000,seed=2718):
    count=n*(n-1)//2
    ranks=np.sort(np.random.default_rng(seed).choice(count,min(limit,count),replace=False))
    starts=np.arange(n,dtype=np.int64)*(2*n-np.arange(n,dtype=np.int64)-1)//2
    i=np.searchsorted(starts,ranks,side='right')-1
    return np.column_stack((i,i+1+ranks-starts[i])).astype(int)


def build(root,index_names,cohort_file='cohort.json'):
    root=Path(root).resolve()
    def asset(path):
        path=Path(path).resolve()
        if not path.is_relative_to(root): raise ValueError('asset outside project root')
        return dict(path=str(path.relative_to(root)),sha256=sha256(path))
    if Path(cohort_file).name!=cohort_file:raise ValueError('cohort must be a plain filename')
    cohort=read_json(root/cohort_file)
    graphs=[];by_graph={};inputs={}
    for rec in cohort['records']:
        if rec['status']!='completed': continue
        folder=root/'graphs'/rec['graph_id'].replace('/','__')
        info=read_json(folder/'graph.json')
        if info['graph_sha256']!=rec['graph_sha256'] or sha256(folder/'adjacency.npz')!=info['adjacency_sha256']:
            raise ValueError('graph identity mismatch')
        graph=dict(id=rec['graph_id'],graph_sha256=info['graph_sha256'],file=asset(folder/'graph.json'))
        graphs.append(graph);by_graph[rec['graph_id']]=info
        a=load_npz(folder/'adjacency.npz');labels=np.array(info['component_labels'])
        prepared=[]
        for c in range(info['n_components']):
            idx=np.flatnonzero(labels==c)
            pairs=pair_sample(len(idx))
            d=shortest_path(a[idx][:,idx],directed=False,unweighted=True,method='D')
            prepared.append(dict(indices=idx,pairs=pairs,targets=d[pairs[:,0],pairs[:,1]]))
        inputs[rec['graph_id']]=prepared
    runs=[];seen=set()
    for filename in index_names:
        if not (root/filename).exists(): raise ValueError('missing requested index '+filename)
        for original in read_json(root/filename)['runs']:
            row={k:original.get(k) for k in ['graph_id','method','seed','status','reason','elapsed_seconds','peak_rss_bytes']}
            if row['graph_id'] not in by_graph: raise ValueError('unknown graph')
            row['source_index']=filename
            if original.get('locality') is not None: row['locality']=original['locality']
            if original.get('run_dir'):
                dest=Path(original['run_dir'])
                manifest=read_json(dest/'manifest.json')
                key=manifest['run_key']
                row.update(id=key,manifest=asset(dest/'manifest.json'),parameters=manifest['request'].get('landmarks'),
                           reason=manifest.get('reason') or row.get('reason'))
                if manifest['request'].get('attempt_label'):
                    row['attempt_label']=manifest['request']['attempt_label']
                if row['status']=='completed':
                    if not verified_cached(dest,key): raise ValueError('invalid completed run '+str(dest))
                    result=read_json(dest/'result.json')
                    if result['graph_sha256']!=by_graph[row['graph_id']]['graph_sha256']:
                        raise ValueError('run graph identity mismatch')
                    row.update(result=asset(dest/'result.json'),raw_coordinates=asset(dest/'coords_raw.csv'),
                               display_coordinates=asset(dest/'coords_display.csv'),vertices=asset(dest/'vertices.json'))
                    z=np.loadtxt(dest/'coords_raw.csv',delimiter=',',skiprows=1,ndmin=2)
                    samples=[]
                    for comp,prep in zip(result['components'],inputs[row['graph_id']]):
                        idx,pairs,targets=prep['indices'],prep['pairs'],prep['targets']
                        scale=comp.get('chord_scale')
                        if scale is None: continue
                        for (i,j),target in zip(pairs,targets):
                            samples.append(dict(component=comp['component'],source=int(idx[i]),target=int(idx[j]),
                                                original_distance=float(target),
                                                fitted_chord=float(scale*np.linalg.norm(z[idx[i]]-z[idx[j]]))))
                    edges=np.array(by_graph[row['graph_id']]['edges'],dtype=int).reshape(-1,2)
                    residual=np.linalg.norm(z[edges[:,0]]-z[edges[:,1]],axis=1)-1
                    diag=root/'viewer_diagnostics'/f'{key}.json'
                    atomic_json(diag,dict(schema_version=1,coordinates_sha256=result['coords_sha256'],
                        shepard=samples,edge_residuals=residual.tolist(),
                        sampling='up to 2000 uniform unordered pairs per component, seed 2718; display only',
                        scale='chord fitted separately per component; edge residuals identity scale',
                        exact_pair_count=result['summary'].get('n_pairs',0),
                        evaluation=result['summary'].get('evaluation',dict(mode='exact',pair_count=result['summary'].get('n_pairs',0)))))
                    row['diagnostics']=asset(diag)
            else:
                row['id']=identity(dict(index=filename,**row))
            if row['id'] in seen: continue
            seen.add(row['id']);runs.append(row)
    available={(r['graph_id'],r['method']) for r in runs}
    for graph in graphs:
        for method in PLANNED:
            if (graph['id'],method) not in available:
                runs.append(dict(id=identity(dict(graph=graph['id'],method=method,status='unsupported')),
                    graph_id=graph['id'],method=method,seed=None,status='unsupported',
                    reason='Not integrated in this project phase; no validated layout available.'))
    extras={}
    for name in ['FINDINGS.md','scores.csv','replicate_summary.json','tie_sensitivity.json','landmark_sensitivity.json',
                 'catalog/gallery.json','cohort.json','deliverables.json',
                 'PHASE03_FINDINGS.md','phase03_scores.csv','phase03_replicate_summary.json',
                 'phase03_capabilities.json','phase03_tie_sensitivity.json','phase03_trimap_graph_ties.json',
                 'phase03_trimap_scale_diagnosis.json','PHASE04_FINDINGS.md','phase04_lgs_summary.json',
                 'lgs_validation/phase04_validation_results.json',cohort_file,'phase05_admission.json',
                 'phase05_sampling_validation.json','phase05_summary.json','PHASE05_FINDINGS.md',
                 'MDS_30GIB_FINDINGS.md']:
        if (root/name).exists(): extras[name]=asset(root/name)
    for path in sorted((root/'lgs_dependency_documents').glob('*')):
        if path.is_file(): extras[str(path.relative_to(root))]=asset(path)
    if (root/'phase04_lgs_summary.json').exists():
        for row in read_json(root/'phase04_lgs_summary.json')['validation_rows']:
            for key in ['result_path','coordinates_path','vertices_path','manifest_path','graph_path']:
                if row.get(key): extras[row[key]]=asset(root/row[key])
    atomic_json(root/'viewer_manifest.json',dict(schema_version=1,kind='gflowui_embedding_comparison',
        title='SuiteSparse 3D Embedding Comparison',graphs=graphs,runs=runs,artifacts=extras,
        indexes=[asset(root/name) for name in index_names],
        notes='Components packed only for display; exact pilot scores unchanged. Expanded distance scores use labeled shared uniform pair samples; edge/neighborhood scores remain exact.'))
    print(len(graphs),'graphs;',len(runs),'run/availability rows;',len([r for r in runs if r['status']=='completed']),'completed layouts')


if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('root')
    parser.add_argument('--indexes',nargs='+',default=['pilot_results.json','lle_landmarks16.json','lle_landmarks32.json'])
    parser.add_argument('--cohort-file',default='cohort.json')
    args=parser.parse_args();build(args.root,args.indexes,args.cohort_file)
