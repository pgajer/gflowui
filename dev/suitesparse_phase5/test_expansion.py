import sys
from pathlib import Path
import numpy as np
import pytest
from scipy.sparse import csr_matrix
sys.path.insert(0,str(Path(__file__).resolve().parent))
import sampled_metrics as sample
from metrics import score_component as exact,aggregate
from graphs import prepare,convert
from catalog import admission

def fixture(n=24):
    a=np.zeros((n,n))
    for i in range(n):a[i,(i+1)%n]=a[(i+1)%n,i]=1
    edges=np.transpose(np.triu(a,1).nonzero())
    ids=[f'v:{i}' for i in range(n)];d,p,_,_=prepare(csr_matrix(a),ids)
    return d,p,edges,ids

def test_full_sample_matches_every_exact_score_and_paths():
    d,p,e,ids=fixture();z=np.random.default_rng(4).normal(size=(len(ids),3))
    s=sample.score_component(z,d,p,e,ids);x=exact(z,d,p,e,ids)
    for key in ['chord_error','relative_stress','path_error','edge_error','distance_rank_correlation','chord_scale','relative_scale','nonedge_separation_ratio']:
        assert s[key]==pytest.approx(x[key],abs=1e-12)
    assert s['neighborhood']==pytest.approx(x['neighborhood'])
    assert s['evaluation']['mode']=='exact' and not s['bootstrap_sums']

def test_frozen_pairs_and_streamed_ties_degeneracies():
    d,p,e,ids=fixture(12);z=np.zeros((12,3));z[::2]=1
    a=sample.score_component(z,d,p,e,ids,limit=20)
    b=sample.score_component(z*3,d,p,e,ids,limit=20)
    assert a['evaluation']['pairs_sha256']==b['evaluation']['pairs_sha256']
    np.testing.assert_allclose(a['chord_error'],b['chord_error'])
    assert a['neighborhood']==pytest.approx(exact(z,d,p,e,ids)['neighborhood'])
    c=sample.score_component(z*0,d,p,e,ids,limit=20)
    assert c['chord_error'] is None and c['relative_stress'] is None
    with pytest.raises(ValueError):sample.score_component(z*np.nan,d,p,e,ids)

def test_population_weighted_components_and_intervals():
    comps=[]
    for n,seed in [(13,10),(29,20)]:
        d,p,e,ids=fixture(n);z=np.random.default_rng(seed).normal(size=(n,3))
        c=sample.score_component(z,d,p,e,ids,limit=40,seed=seed)
        c['n_vertices']=n;comps.append(c)
        pairs=np.array(c['evaluation']['pairs']);target=d[pairs[:,0],pairs[:,1]]
        assert c['sums']['target_ss']==pytest.approx(np.sum(target**2)*n*(n-1)/2/40)
    s=sample.aggregate(comps)
    assert s['n_pairs']==13*12//2+29*28//2
    assert s['evaluation']['pair_count']==80 and s['distance_rank_correlation'] is None
    assert s['chord_error']==pytest.approx(np.sqrt(sum(c['sums']['chord_ss'] for c in comps)/sum(c['sums']['target_ss'] for c in comps)))
    assert s['intervals']['chord_error']['valid_replicates']==200
    assert s['intervals']['distance_rank_correlation'] is None

def test_explicit_expansion_preserves_original_admission():
    assert not admission(3000,3000,100)[0]
    assert admission(3000,3000,100,max_vertices=10000)[0]
    assert not admission(6000,5000,100,max_vertices=10000)[0]
    assert not admission(4000,4000,100001,max_vertices=10000)[0]
    with pytest.raises(ValueError):convert(csr_matrix((3000,3000)),'large')
    _,g=convert(csr_matrix((3000,3000)),'large',max_vertices=10000)
    assert g['n_isolates']==3000

def test_admission_uses_verified_largest_same_method_reference(tmp_path):
    from run_expansion import admission as resource_admission
    from common import atomic_json,sha256,read_json
    rows=[]
    for name,n,memory,seconds,method in [('small',30,900,8,'test'),('large',50,200,3,'test'),('other',60,999,99,'different')]:
        folder=tmp_path/'graphs'/name;folder.mkdir(parents=True)
        atomic_json(folder/'graph.json',dict(component_sizes=[n],graph_sha256=name))
        dest=tmp_path/'runs'/name;dest.mkdir(parents=True)
        for filename in ['request.json','result.json','coords_raw.csv','coords_display.csv','vertices.json']:
            (dest/filename).write_text('fixture')
        atomic_json(dest/'manifest.json',dict(run_key=name,status='completed',peak_rss_bytes=memory,
            elapsed_seconds=seconds,request=dict(graph_sha256=name),
            artifacts={p.name:sha256(p) for p in dest.iterdir()}))
        rows.append(dict(method=method,status='completed',graph_id=name,seed=17,run_dir=str(dest),peak_rss_bytes=memory,elapsed_seconds=seconds))
    info=dict(component_sizes=[100,1])
    a=resource_admission(info,'test',rows,tmp_path)
    assert a['admitted'] and a['reference_component_vertices']==50
    assert a['projected_memory_bytes']==pytest.approx(880)
    assert a['projected_seconds']==pytest.approx(30)
    assert not resource_admission(info,'missing',rows,tmp_path)['admitted']
    manifest=tmp_path/'runs/large/manifest.json';changed=read_json(manifest);changed['peak_rss_bytes']=201
    atomic_json(manifest,changed)
    with pytest.raises(ValueError,match='calibration'):resource_admission(info,'test',rows,tmp_path)
