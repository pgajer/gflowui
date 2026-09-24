import itertools
import numpy as np
import pytest
from export_viewer import pair_sample
from report_expansion import require_complete_matrix


def test_pair_sample_exact_small_graphs():
    for n in range(0,30):
        expected=np.array(list(itertools.combinations(range(n),2)),dtype=int).reshape(-1,2)
        np.testing.assert_array_equal(pair_sample(n),expected)


def test_pair_sample_uniform_reproducible_unordered_indices():
    a=pair_sample(3000,limit=123)
    np.testing.assert_array_equal(a,pair_sample(3000,limit=123))
    assert len(np.unique(a,axis=0))==123
    assert np.all(a[:,0]<a[:,1])
    assert np.min(a)>=0 and np.max(a)<3000
    assert not np.array_equal(a,pair_sample(3000,limit=123,seed=19))

def test_expansion_requires_the_whole_matrix_not_a_successful_prefix():
    rows=[dict(graph_id='g',method=m,seed=s) for m in
          ['pacmap','localmap','trimap','phate','largevis','ncvis'] for s in (17,29,43)]
    require_complete_matrix(rows,['g'])
    with pytest.raises(ValueError): require_complete_matrix(rows[:-1],['g'])
    with pytest.raises(ValueError): require_complete_matrix(rows+[rows[0]],['g'])
    with pytest.raises(ValueError): require_complete_matrix(rows,['g','h'])
