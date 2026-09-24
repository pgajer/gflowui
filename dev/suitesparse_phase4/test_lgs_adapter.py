import copy
import csv
import hashlib
import json
from pathlib import Path
import sys
import numpy as np
import pytest
sys.path.insert(0,str(Path(__file__).resolve().parent))
from lgs_adapter import ACCEPTED,PARAMETERS,graph_hash,locality_settings,validate_response
from run_lgs import admission

def test_locality_clips_each_component_and_deduplicates_vectors():
    rows=locality_settings([35,18,1])
    assert [r['component_k'] for r in rows]==[[16,16,0],[32,17,0],[34,17,0]]
    assert rows[-1]['component_fraction']==[1.,1.,None]
    assert locality_settings([4,1])[0]['component_k']==[3,0]

def test_serial_component_resource_preflight():
    info=dict(component_sizes=[100,50,1],n_edges=200)
    calibration=[dict(status='completed',elapsed_seconds=1,component_sizes=[50])]
    assert admission(info,calibration)['projected_seconds']==18
    assert admission(info,calibration)['admitted']
    assert not admission(info,[])['admitted']
    assert not admission(dict(component_sizes=[691],n_edges=2000),calibration)['admitted']
    assert '2000' in admission(dict(component_sizes=[2363],n_edges=3840),calibration)['reason']

def response_fixture(tmp_path):
    folder=tmp_path/'lgs_artifacts'/'results';folder.mkdir(parents=True)
    path=folder/'coordinates.csv'
    ids=['z','α','a']
    with path.open('w',newline='') as f:
        w=csv.writer(f);w.writerow(['vertex_id','x','y','z']);w.writerows([[i,j,0,1] for j,i in enumerate(ids)])
    request=dict(graph_id='g',graph_sha256='a'*64,dimension=3,seed=17,locality_k=2,
                 parameters=PARAMETERS,vertex_sha256='b'*64,edge_sha256='c'*64)
    dep=dict(runtime_source_sha256='d'*64,environment=dict(python='3.12.10',numpy='2.1.0'))
    response=dict(**{k:v for k,v in request.items() if k not in ['vertex_sha256','edge_sha256']},
        status='completed',variant='lgs-paper-union-v1',
        implementation=dict(commit=ACCEPTED,source_sha256=dep['runtime_source_sha256']),
        input_hashes={k:request[k] for k in ['vertex_sha256','edge_sha256']},environment=dep['environment'],
        coordinate_path=str(path),coordinate_sha256=hashlib.sha256(path.read_bytes()).hexdigest())
    return response,request,ids,dep

def test_strict_portable_output(tmp_path):
    response,request,ids,dep=response_fixture(tmp_path)
    z=validate_response(response,request,ids,tmp_path,dep,0)
    assert z.shape==(3,3) and np.array_equal(z[:,0],[0,1,2])
    for field,value in [('dimension',2),('seed',29),('variant','upstream-2d'),('graph_sha256','x')]:
        bad=copy.deepcopy(response);bad[field]=value
        with pytest.raises(ValueError): validate_response(bad,request,ids,tmp_path,dep,0)
    with pytest.raises(RuntimeError): validate_response(response,request,ids,tmp_path,dep,1)
    with pytest.raises(ValueError): validate_response(response,request,ids[::-1],tmp_path,dep,0)
    path=Path(response['coordinate_path']);path.write_text(path.read_text().replace(',0,1',',nan,1'))
    with pytest.raises(ValueError): validate_response(response,request,ids,tmp_path,dep,0)
    response['coordinate_sha256']=hashlib.sha256(path.read_bytes()).hexdigest()
    with pytest.raises(ValueError): validate_response(response,request,ids,tmp_path,dep,0)

def test_versioned_graph_hash():
    payload=json.dumps(dict(edge_sha256='e',format='csv-graph-v1',vertex_sha256='v'),
                       sort_keys=True,separators=(',',':')).encode()
    assert graph_hash('v','e')==hashlib.sha256(payload).hexdigest()
