import numpy as np
import pytest
from adapters import exact_feature_neighbors,embed

def test_exact_neighbors_self_and_ties():
    x=np.array([[0.],[1.],[-1.],[0.]])
    ids=['b','c','a','d']
    indices,distances=exact_feature_neighbors(x,ids,4)
    assert indices[0].tolist()==[0,3,2,1]
    assert indices[3].tolist()==[3,0,2,1]
    assert distances[0].tolist()==[0,0,1,1]

def test_invalid_adapters_do_not_silently_fallback(tmp_path):
    with pytest.raises(ValueError,match='unsupported'):
        embed('imaginary',None,['a']*8,None,np.ones((8,3)),17,tmp_path)
    with pytest.raises(ValueError,match='small'):
        embed('pacmap',None,['a']*4,None,np.ones((4,3)),17,tmp_path)
    with pytest.raises(ValueError,match='constant'):
        embed('pacmap',None,['a']*8,None,np.ones((8,3)),17,tmp_path)

def test_trimap_graph_small_input_is_not_silently_misinterpreted(tmp_path):
    with pytest.raises(ValueError,match='at least 63'):
        embed('trimap_graph',None,[str(i) for i in range(6)],np.ones((6,6)),np.arange(18).reshape(6,3),17,tmp_path)

def test_shared_component_pipeline_and_isolate(tmp_path):
    import sys,importlib.util
    from pathlib import Path
    pilot=Path(__file__).resolve().parents[1]/'suitesparse_phase1'
    sys.path.insert(0,str(pilot))
    from scipy.sparse import coo_matrix,save_npz
    from graphs import convert
    from common import atomic_json,read_json,sha256
    spec=importlib.util.spec_from_file_location('component_pipeline_test',pilot/'worker.py')
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
    a,info=convert(coo_matrix((np.ones(6),(np.arange(6),(np.arange(6)+1)%6)),shape=(7,7)),'test/cycle-isolate')
    graph=tmp_path/'graph';graph.mkdir();save_npz(graph/'adjacency.npz',a)
    info['adjacency_sha256']=sha256(graph/'adjacency.npz');atomic_json(graph/'graph.json',info)
    output=tmp_path/'output';output.mkdir();calls=[]
    def fake(method,adj,ids,d,x,seed,dest,initial):
        calls.append((method,len(ids)))
        return np.column_stack((np.arange(len(ids)),np.zeros((len(ids),2)))),dict(termination='test fixture')
    module.run(dict(graph_dir=str(graph),output=str(output),method='fixture',seed=17),
               embedder=fake,input_type='landmark_distance_features')
    result=read_json(output/'result.json')
    assert calls==[('fixture',6)]
    assert result['input_type']=='landmark_distance_features'
    assert result['cross_component_pairs_excluded']==6
    assert [c['small_component_placement'] for c in result['components']]==[False,True]
    assert read_json(output/'vertices.json')==info['vertex_ids']
