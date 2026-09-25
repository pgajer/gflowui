import importlib.util
import json
from pathlib import Path
import sys
import numpy as np
import pytest
from scipy.sparse import csr_matrix, save_npz

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
from adapter import embed, parse_positions
spec = importlib.util.spec_from_file_location('sfdp_runner', HERE/'run.py')
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)
from common import read_json, atomic_json, sha256


def test_position_identity_dimension_finiteness_and_units():
    d = dict(objects=[dict(name='1',pos='72,144,-72'),dict(name='0',pos='0,0,0')])
    np.testing.assert_array_equal(parse_positions(d,2),[[0,0,0],[1,2,-1]])
    for bad in [[], [dict(name='0',pos='1,2')], [dict(name='0',pos='1,2,nan')],
                [dict(name='extra',pos='1,2,3')], [dict(name='0',pos='1,2,3')]*2]:
        with pytest.raises(ValueError): parse_positions(dict(objects=bad),1)


def test_real_backend_is_3d_reproducible_and_not_using_distance_features(tmp_path):
    a = csr_matrix(np.ones((8,8))-np.eye(8))
    output = []
    for i, seed in enumerate([17,17,29]):
        dest=tmp_path/str(i);dest.mkdir()
        z, detail = embed('sfdp',a,list(map(str,range(8))),None,None,seed,dest)
        assert z.shape==(8,3) and np.isfinite(z).all()
        assert detail['centered_rank']==3
        assert '-Ksfdp' in detail['command']
        assert '-Gdim=3' in detail['command'] and '-Gdimen=3' in detail['command']
        doc=read_json(dest/'graphviz.json')
        np.testing.assert_array_equal(z,parse_positions(doc,8))
        output.append(z)
    np.testing.assert_array_equal(output[0],output[1])
    assert not np.allclose(output[0],output[2])
    with pytest.raises(ValueError): embed('wrong',a,list(range(8)),None,None,17,tmp_path)


def test_worker_keeps_forest_ids_and_isolates(tmp_path):
    import subprocess
    sys.path.insert(0,str(HERE.parent/'suitesparse_phase1'))
    from graphs import convert
    a=np.zeros((7,7));a[:5,:5]=1-np.eye(5)
    adj,info=convert(csr_matrix(a),'fixture')
    graph=tmp_path/'graph';graph.mkdir()
    save_npz(graph/'adjacency.npz',adj)
    info['adjacency_sha256']=sha256(graph/'adjacency.npz')
    atomic_json(graph/'graph.json',info)
    for evaluation in ['exact','suitesparse-uniform-pairs-v1']:
        dest=tmp_path/evaluation;dest.mkdir()
        request=dict(method='sfdp',seed=17,graph_dir=str(graph),output=str(dest),evaluation=evaluation)
        atomic_json(dest/'request.json',request)
        subprocess.run([sys.executable,str(HERE/'worker.py'),str(dest/'request.json')],check=True)
        result=read_json(dest/'result.json')
        assert result['dimension']==3 and len(result['components'])==3
        assert result['components'][0]['details']['centered_rank']==3
        assert all(c['small_component_placement'] for c in result['components'][1:])
        assert result['cross_component_pairs_excluded']==11
        assert read_json(dest/'vertices.json')==info['vertex_ids']
        raw=np.loadtxt(dest/'coords_raw.csv',delimiter=',',skiprows=1)
        display=np.loadtxt(dest/'coords_display.csv',delimiter=',',skiprows=1)
        assert raw.shape==display.shape==(7,3)
        delta=display[:5]-raw[:5]
        np.testing.assert_allclose(delta,np.tile(delta[0],(5,1)),atol=1e-14)


def test_sparse_multilevel_coarsening_regression(tmp_path):
    # Graphviz 15.1.1 crashes here although its small complete-graph tests pass.
    # This deterministic sparse graph exercises the multilevel coarsener.
    n=256
    a=np.zeros((n,n));a[np.arange(n),np.roll(np.arange(n),1)]=1
    i,j=np.random.default_rng(281).integers(n,size=(2,n*2));a[i,j]=1
    a=np.maximum(a,a.T);np.fill_diagonal(a,0)
    z,detail=embed('sfdp',csr_matrix(a),list(range(n)),None,None,17,tmp_path)
    assert z.shape==(n,3) and np.isfinite(z).all() and detail['centered_rank']==3


@pytest.mark.parametrize('fail_seed',[None,29])
def test_serial_runner_preserves_history_and_records_failures(tmp_path,monkeypatch,fail_seed):
    monkeypatch.setattr(runner.subprocess,'check_output',lambda args,**kw:'' if 'status' in args else 'commit')
    monkeypatch.setattr(runner,'capture_environment',lambda:{'fixture':True})
    records=[]
    for graph in runner.GRAPHS:
        atomic_json(tmp_path/'graphs'/graph.replace('/','__')/'graph.json',dict(graph_sha256=graph,component_sizes=[8]))
        records.append(dict(graph_id=graph,graph_sha256=graph,status='completed'))
    atomic_json(tmp_path/'combined_cohort.json',dict(records=records))
    atomic_json(tmp_path/'pilot_results.json',dict(historical=True))
    old=sha256(tmp_path/'pilot_results.json');calls=[]
    def supervise(cmd,dest,seconds,memory):
        req=read_json(dest/'request.json');calls.append((req['graph_sha256'],req['seed']))
        assert seconds is None and memory==30*1024**3
        assert req['evaluation']==('suitesparse-uniform-pairs-v1' if req['graph_sha256'] in runner.GRAPHS[-2:] else 'exact')
        failed=req['seed']==fail_seed
        if not failed:
            atomic_json(dest/'result.json',dict(summary={}))
            for name in ['coords_raw.csv','coords_display.csv','vertices.json']:(dest/name).write_text('fixture')
        return dict(status='failed' if failed else 'completed',reason='fixture' if failed else None,
                    elapsed_seconds=.2,peak_rss_bytes=42,limits=runner.LIMITS)
    monkeypatch.setattr(runner,'supervise',supervise)
    rows=runner.run(tmp_path)
    assert calls==[(g,s) for g in runner.GRAPHS for s in runner.SEEDS]
    assert len(rows)==18 and sum(r['status']=='completed' for r in rows)==(18 if fail_seed is None else 12)
    assert sha256(tmp_path/'pilot_results.json')==old
    with pytest.raises(ValueError,match='already recorded'):runner.run(tmp_path)
    with runner.serial_lock(tmp_path):
        with pytest.raises(RuntimeError):
            with runner.serial_lock(tmp_path):pass
    with runner.serial_lock(tmp_path):pass
    snapshot=sha256(tmp_path/'sfdp_results.json')
    runner.run(tmp_path,graphs=[runner.GRAPHS[0]],index_name='sfdp_graphviz16_results.json')
    assert sha256(tmp_path/'sfdp_results.json')==snapshot
    assert len(read_json(tmp_path/'sfdp_graphviz16_results.json')['runs'])==3
    with pytest.raises(ValueError,match='plain SFDP'):
        runner.run(tmp_path,index_name='../sfdp_results.json')


def test_report_preserves_failed_cases_and_rejects_incomplete_matrix(tmp_path):
    spec=importlib.util.spec_from_file_location('sfdp_report',HERE/'report.py')
    report=importlib.util.module_from_spec(spec);spec.loader.exec_module(report)
    rows=[dict(graph_id=g,method='sfdp',seed=s,status='completed',elapsed_seconds=1,
               peak_rss_bytes=1024**3,scores=dict(chord_error=.1,edge_error=.2))
          for g in runner.GRAPHS for s in runner.SEEDS]
    rows[0].update(status='failed',reason='test failure',peak_rss_bytes=None,scores={})
    atomic_json(tmp_path/'sfdp_results.json',dict(runs=rows,limits=runner.LIMITS,serial=True))
    report.build(tmp_path)
    original=(tmp_path/'SFDP_FINDINGS.md').read_text()
    assert '17 of 18' in original and 'test failure' in original and 'unavailable' in original
    atomic_json(tmp_path/'sfdp_results.json',dict(runs=rows[:-1],limits=runner.LIMITS,serial=True))
    with pytest.raises(ValueError):report.build(tmp_path)
    assert (tmp_path/'SFDP_FINDINGS.md').read_text()==original
