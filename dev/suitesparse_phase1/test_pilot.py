"""Independent small-case tests; run with pytest from this directory."""
import io
import json
import tarfile
from pathlib import Path
import numpy as np
import pytest
from scipy.sparse import coo_matrix, csr_matrix
from scipy.spatial.distance import squareform,pdist
from sklearn.manifold import trustworthiness
from graphs import convert,prepare,matrix_from_archive
from metrics import validate_coords,score_component,fixed_path_lengths,rank_quality,aggregate
from catalog import admission,parse_metadata
from common import atomic_json,sha256
from run_pilot import supervise,verified_cached,allocation_preflight


def path(n):
    a=csr_matrix((np.ones(2*(n-1)),(np.r_[np.arange(n-1),np.arange(1,n)],
                                  np.r_[np.arange(1,n),np.arange(n-1)])),shape=(n,n))
    return a,[f'v:{i}' for i in range(n)],np.column_stack((np.arange(n-1),np.arange(1,n)))


def test_admission():
    assert admission(2999,2999,100000)[0]
    assert not admission(3000,3000,1)[0]
    assert not admission(1500,1501,1)[0]
    assert not admission(3,3,100001)[0]


def test_matrix_support():
    m=coo_matrix(([2,-2,0,7,1,4],([0,0,0,1,1,2],[1,1,2,1,2,1])),shape=(4,4))
    a,g=convert(m,'fixture')
    assert g['edges']==[[1,2]]
    assert g['removed_loops']==1 and g['canceled_duplicate_positions']==1
    assert g['n_isolates']==2 and g['n_components']==3
    a,g=convert(coo_matrix(([1,2],([0,1],[2,0])),shape=(2,3)),'rectangular')
    assert g['edges']==[[0,4],[1,2]] and len(g['vertex_ids'])==5
    with pytest.raises(ValueError): convert(coo_matrix(([np.nan],([0],[0])),shape=(2,2)),'bad')
    with pytest.raises(ValueError): convert(coo_matrix(np.eye(2)),'bad',3)


@pytest.mark.parametrize('name,link', [('../evil.mtx',False),('/evil',False),('ok',True)])
def test_archive_safety(tmp_path,name,link):
    archive=tmp_path/'bad.tar.gz'
    with tarfile.open(archive,'w:gz') as out:
        item=tarfile.TarInfo(name)
        if link:
            item.type=tarfile.SYMTYPE
            item.linkname='/etc/passwd'
        out.addfile(item)
    with pytest.raises(ValueError): matrix_from_archive(archive,'x',dict(rows=2,columns=2))


def test_archive_size(tmp_path):
    archive=tmp_path/'large.tar.gz'
    with tarfile.open(archive,'w:gz') as out:
        item=tarfile.TarInfo('x.mtx'); item.size=100
        out.addfile(item,io.BytesIO(b' '*100))
    with pytest.raises(ValueError,match='size'): matrix_from_archive(archive,'x',dict(rows=2,columns=2),50)


def test_valid_archive_and_metadata(tmp_path):
    data=b'%%MatrixMarket matrix coordinate real symmetric\n2 2 1\n2 1 1\n'
    archive=tmp_path/'good.tar.gz'
    with tarfile.open(archive,'w:gz') as out:
        item=tarfile.TarInfo('x/x.mtx');item.size=len(data);out.addfile(item,io.BytesIO(data))
    m=matrix_from_archive(archive,'x',dict(rows=2,columns=2))
    assert m.nnz==2
    with pytest.raises(ValueError,match='dimensions'): matrix_from_archive(archive,'x',dict(rows=3,columns=3))
    html='<table>'+''.join(f'<tr><th>{k}</th><td>{v}</td></tr>' for k,v in
          [('Num Rows','2'),('Num Cols','2'),('Nonzeros','2')])+'</table>'
    assert parse_metadata(html,'https://sparse.tamu.edu/HB/x')['eligible']


def test_bounded_transfer_cleanup(tmp_path,monkeypatch):
    from catalog import bounded_download
    class Response:
        headers={}
        def __enter__(self): return self
        def __exit__(self,*args): pass
        def raise_for_status(self): pass
        def iter_content(self,n): return iter([b'1234',b'5678'])
    monkeypatch.setattr('catalog.requests.get',lambda *args,**kwargs:Response())
    with pytest.raises(ValueError,match='cap'): bounded_download('https://example.test',tmp_path/'a',5)
    assert not (tmp_path/'a').exists() and not (tmp_path/'a.partial').exists()


def test_coords():
    with pytest.raises(ValueError): validate_coords([[1,2]],['a'],['a'])
    with pytest.raises(ValueError): validate_coords([[1,2,np.inf]],['a'],['a'])
    with pytest.raises(ValueError): validate_coords(np.ones((2,3)),['a','b'],['b','a'])


def test_exact_and_folded():
    a,ids,e=path(3); d,p,f,meta=prepare(a,ids)
    z=np.column_stack(([0,1,2],[0,0,0],[0,0,0]))
    score=score_component(z,d,p,e,ids,ks=(1,))
    assert score['chord_error']==0 and score['path_error']==0 and score['edge_error']==0
    folded=z.copy(); folded[2]=0
    bad=score_component(folded,d,p,e,ids,ks=(1,))
    assert bad['path_error']==0 and bad['chord_error']>0
    collapsed=score_component(np.zeros((3,3)),d,p,e,ids)
    assert collapsed['chord_error'] is None and collapsed['path_error']==1


def test_invariances_and_reference_paths():
    a,ids,e=path(9); d,p,f,meta=prepare(a,ids,4)
    rng=np.random.default_rng(17); z=rng.normal(size=(9,3))
    q,_=np.linalg.qr(rng.normal(size=(3,3)))
    base=score_component(z,d,p,e,ids)
    moved=score_component(z@q+42,d,p,e,ids)
    scaled=score_component(7*z,d,p,e,ids)
    for key in ['chord_error','relative_stress','path_error','edge_error']:
        assert moved[key]==pytest.approx(base[key],abs=1e-12)
    assert scaled['chord_error']==pytest.approx(base['chord_error'])
    assert scaled['relative_stress']==pytest.approx(base['relative_stress'])
    assert scaled['path_error']!=pytest.approx(base['path_error'])
    actual=fixed_path_lengths(z,d,p)
    # Brute-force predecessor walk, independent from distance-sorted accumulation.
    for i in range(9):
        for j in range(9):
            length=0; at=j
            while at!=i:
                parent=p[i,at]; length+=np.linalg.norm(z[at]-z[parent]); at=parent
            assert actual[i,j]==pytest.approx(length)
    r=pdist(z); target=squareform(d,checks=False)
    s=sum(x*y for x,y in zip(r,target))/sum(x*x for x in r)
    expected=(sum((s*x-y)**2 for x,y in zip(r,target))/sum(y*y for y in target))**.5
    assert base['chord_error']==pytest.approx(expected)
    assert prepare(a,ids,4)[3]==meta


def test_rank_scores_independent():
    rng=np.random.default_rng(8)
    x=rng.normal(size=(25,5)); z=rng.normal(size=(25,3))
    d=squareform(pdist(x)); r=squareform(pdist(z)); ids=[str(i) for i in range(25)]
    for k in [1,3,5,10]:
        t,c=rank_quality(d,r,ids,k)
        assert t==pytest.approx(trustworthiness(d,z,n_neighbors=k,metric='precomputed'))
        assert c==pytest.approx(trustworthiness(r,x,n_neighbors=k,metric='precomputed'))
    assert rank_quality(d,r,ids,13)==(None,None)


def test_uniform_pair_sampling_check():
    # Phase 1 uses exact pairs; validate a diagnostic sampling calculation independently.
    rng=np.random.default_rng(15)
    n=80; d=pdist(np.arange(n)[:,None]);r=pdist(rng.normal(size=(n,3)))
    s=np.dot(r,d)/np.dot(r,r)
    all_error=np.mean((s*r-d)**2)
    estimates=[np.mean((s*r[idx]-d[idx])**2) for idx in
               [rng.choice(len(d),500,replace=False) for _ in range(100)]]
    assert abs(np.mean(estimates)-all_error)/all_error<.03


def test_stable_ids_break_distance_ties():
    from metrics import ordered_neighbors
    distances=np.ones((4,4))-np.eye(4)
    ids=['d','c','b','a']
    assert ordered_neighbors(distances,ids)[0].tolist()==[3,2,1]
    permutation=np.array([2,0,3,1])
    reordered=ordered_neighbors(distances[permutation][:,permutation],[ids[i] for i in permutation])
    row=int(np.flatnonzero(permutation==0)[0])
    assert [ids[permutation[j]] for j in reordered[row]]==['a','b','c']


def test_components_and_ties():
    a,ids,e=path(4)
    a=a.tolil(); a[0,3]=a[3,0]=1; a=a.tocsr()
    d,p,f,meta=prepare(a,ids)
    assert np.array_equal(p,prepare(a,ids)[1])
    z=np.eye(4,3)
    s=score_component(z,d,p,e,ids);s['n_vertices']=4
    one=aggregate([s]);two=aggregate([s,s])
    assert one['chord_error']==two['chord_error']
    assert two['n_pairs']==2*one['n_pairs']
    with pytest.raises(ValueError): prepare(csr_matrix((2,2)),['a','b'])
    single=score_component(np.zeros((1,3)),np.zeros((1,1)),np.array([[-9999]]),np.empty((0,2),int),['a'])
    assert single['status']=='unavailable'


def test_supervisor_and_cache(tmp_path):
    import sys
    result=supervise([sys.executable,'-c','import time; time.sleep(5)'],tmp_path,seconds=.1)
    assert result['status']=='resource_limited' and result['reason']=='timeout'
    artifact=tmp_path/'result.json';atomic_json(artifact,{'value':1})
    names=['result.json','request.json','coords_raw.csv','coords_display.csv','vertices.json']
    for name in names[1:]: (tmp_path/name).write_text('fixture')
    artifacts={name:sha256(tmp_path/name) for name in names}
    manifest=dict(status='completed',run_key='x',artifacts=artifacts)
    atomic_json(tmp_path/'manifest.json',manifest)
    assert verified_cached(tmp_path,'x') and not verified_cached(tmp_path,'y')
    atomic_json(artifact,{'value':2});assert not verified_cached(tmp_path,'x')
    for invalid in [{},dict(manifest,artifacts={}),dict(manifest,artifacts=dict(artifacts,**{'../outside':'x'}))]:
        atomic_json(tmp_path/'manifest.json',invalid)
        assert not verified_cached(tmp_path,'x')
    (tmp_path/'manifest.json').write_text('{invalid')
    assert not verified_cached(tmp_path,'x')


def test_allocation_preflight():
    check=allocation_preflight(dict(component_sizes=[100,1000]),64,2*1024**3)
    assert check['known_prepared_bytes']==12*1000**2+8*1000*64
    assert check['admitted']
    assert not allocation_preflight(dict(component_sizes=[20000]),64,2*1024**3)['admitted']


def test_reporting_ranges_and_tie_priority():
    from report import replicate_summary,format_replicates
    from tie_diagnostics import reversed_priorities
    assert replicate_summary([3,None,1,2])==dict(count=3,mean=2.,minimum=1.,maximum=3.)
    assert format_replicates([3,1,2])=='2 [1, 3]'
    assert format_replicates([2])=='2 (single run)'
    assert format_replicates([None])=='unavailable'
    ids=['v:2','v:10','v:1']
    assert reversed_priorities(ids)==['v:1','v:10','v:2']
    assert reversed_priorities(reversed_priorities(ids))==ids


def test_interrupted_tie_publication_and_report_boundary(tmp_path,monkeypatch):
    import tie_diagnostics as diagnostic_module
    from report import report
    from scipy.sparse import save_npz
    a,info=convert(coo_matrix([[0,1],[1,0]]),'fixture')
    graph_dir=tmp_path/'graphs'/'fixture';graph_dir.mkdir(parents=True)
    save_npz(graph_dir/'adjacency.npz',a)
    info['adjacency_sha256']=sha256(graph_dir/'adjacency.npz')
    atomic_json(graph_dir/'graph.json',info)
    d,p,f,meta=prepare(a,info['vertex_ids'])
    z=np.array([[0.,0,0],[1.,0,0]])
    score=score_component(z,d,p,np.array([[0,1]]),info['vertex_ids'])
    score.update(n_vertices=2,component=0,small_component_placement=True,details={})
    rows=[]
    for method in ['isomap_graph','lle']:
        dest=tmp_path/method;comp=dest/'component_000';comp.mkdir(parents=True)
        np.savetxt(comp/'coords.csv',z,delimiter=',',header='x,y,z',comments='')
        for name in ['coords_raw.csv','coords_display.csv']:
            np.savetxt(dest/name,z,delimiter=',',header='x,y,z',comments='')
        atomic_json(dest/'request.json',{})
        atomic_json(dest/'vertices.json',info['vertex_ids'])
        atomic_json(dest/'result.json',dict(components=[score],coords_sha256=sha256(dest/'coords_raw.csv')))
        files=[f for f in dest.rglob('*') if f.is_file()]
        atomic_json(dest/'manifest.json',dict(status='completed',run_key=method,
            artifacts={str(f.relative_to(dest)):sha256(f) for f in files}))
        rows.append(dict(graph_id='fixture',method=method,seed=17,status='completed',
                         scores=aggregate([score]),run_dir=str(dest)))
    index=dict(commit='fixture',runs=rows)
    atomic_json(tmp_path/'pilot_results.json',index)
    (tmp_path/'catalog').mkdir()
    atomic_json(tmp_path/'catalog'/'gallery.json',dict(gallery_count=1))
    atomic_json(tmp_path/'cohort.json',dict(records=[dict(graph_id='fixture')]))
    diagnostic_module.diagnostic(tmp_path)
    complete=(tmp_path/'tie_sensitivity.json').read_bytes()
    report(tmp_path)  # Complete control is accepted by the real report boundary.
    original_verified=diagnostic_module.verified_cached
    calls=[]
    def interrupt_before_second(*args):
        calls.append(1)
        if len(calls)==2: raise KeyboardInterrupt('after first diagnostic row')
        return original_verified(*args)
    monkeypatch.setattr(diagnostic_module,'verified_cached',interrupt_before_second)
    with pytest.raises(KeyboardInterrupt): diagnostic_module.diagnostic(tmp_path)
    assert (tmp_path/'tie_sensitivity.json').read_bytes()==complete
    report(tmp_path)
    good=json.loads(complete)
    for bad in [dict(good,runs=good['runs'][:1]),dict(good,runs=good['runs']*2),
                dict(good,runs=[dict(good['runs'][0],method='unexpected'),good['runs'][1]]),
                dict(good,status='partial'),dict(good,schema_version=1)]:
        atomic_json(tmp_path/'tie_sensitivity.json',bad)
        old_report=(tmp_path/'FINDINGS.md').read_bytes()
        with pytest.raises(ValueError,match='coverage'): report(tmp_path)
        assert (tmp_path/'FINDINGS.md').read_bytes()==old_report


def test_six_real_adapters(tmp_path):
    from worker import embed
    # Octahedral graph: genuinely three-dimensional distance configuration.
    n=6; a=np.ones((n,n))-np.eye(n)
    for i,j in [(0,1),(2,3),(4,5)]: a[i,j]=a[j,i]=0
    a=csr_matrix(a);ids=[str(i) for i in range(n)]
    d,p,f,meta=prepare(a,ids)
    initial=None
    for method in ['metric_mds','metric_mds_edge_kk','weighted_grip','isomap_graph','umap','lle']:
        dest=tmp_path/method;dest.mkdir()
        z,detail=embed(method,a,ids,d,f,17,dest,initial)
        assert z.shape==(6,3) and np.isfinite(z).all()
        assert np.linalg.matrix_rank(z-z.mean(axis=0),tol=1e-7)==3
        if method=='metric_mds':
            initial=dest/'initial.csv'
            np.savetxt(initial,z,delimiter=',',header='x,y,z',comments='')
