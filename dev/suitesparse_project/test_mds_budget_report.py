import pytest
from report_mds_budget import report
from common import atomic_json

def test_report_requires_all_attempts_and_retains_failures(tmp_path):
    rows=[dict(graph_id=g,method=m,seed=s,status='completed',elapsed_seconds=12.,peak_rss_bytes=3*1024**3)
        for g in ['Meszaros/nemscem','HB/sstmodel','Bomhof/circuit_2']
        for m in ['metric_mds','metric_mds_edge_kk'] for s in [17,29,43]]
    def put(rows):atomic_json(tmp_path/'mds_30gib_results.json',dict(runs=rows,limits=dict(seconds=None,memory_bytes=30*1024**3),serial=True))
    put(rows[:-1])
    with pytest.raises(ValueError,match='incomplete'):report(tmp_path)
    rows[0].update(status='failed',reason='measurement unavailable',peak_rss_bytes=None)
    rows[3].update(status='unavailable',reason='matching MDS unavailable',peak_rss_bytes=None,elapsed_seconds=0.)
    put(rows);assert report(tmp_path)==16
    text=(tmp_path/'MDS_30GIB_FINDINGS.md').read_text()
    assert '16 of 18' in text and 'measurement unavailable' in text and 'unknown' in text
    assert 'no elapsed-time limit' in text and 'not convergence' in text
