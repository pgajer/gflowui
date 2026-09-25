"""Report the complete higher-budget retry matrix without replacing old outcomes."""
import argparse
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json,atomic_json

def report(root):
    root=Path(root);data=read_json(root/'mds_30gib_results.json');rows=data['runs']
    expected={(g,m,s) for g in ['Meszaros/nemscem','HB/sstmodel','Bomhof/circuit_2']
        for m in ['metric_mds','metric_mds_edge_kk'] for s in [17,29,43]}
    keys=[(r['graph_id'],r['method'],r['seed']) for r in rows]
    if len(keys)!=len(expected) or set(keys)!=expected:raise ValueError('incomplete or duplicate higher-budget matrix')
    if data['limits']!=dict(seconds=None,memory_bytes=30*1024**3) or data['serial'] is not True:
        raise ValueError('unexpected retry policy')
    names=dict(metric_mds='Metric MDS',metric_mds_edge_kk='Metric MDS + edge-KK')
    completed=[r for r in rows if r['status']=='completed']
    lines=['# Metric-MDS reruns with a higher memory allowance','',
        'These are new attempts with a 30-GiB memory ceiling, no elapsed-time limit, and one job at a time. '
        'The original 2-GiB/10-minute outcomes remain unchanged. Optimizer settings, seeds and graph inputs are unchanged; '
        'nemscem retains exact evaluation and the two larger graphs retain shared-pair evaluation.','',
        f'{len(completed)} of {len(rows)} requested layouts completed. Completion means finite saved coordinates, not convergence or good fit.','',
        '| Graph | Method | Seed | Outcome | Seconds | Peak sampled GiB | Reason |',
        '|---|---|---:|---|---:|---:|---|']
    for r in rows:
        rss=r.get('peak_rss_bytes');memory='unknown' if rss is None else f'{rss/1024**3:.3f}'
        reason=(r.get('reason') or '').replace('|','/').replace('\n',' ')
        lines.append(f"| {r['graph_id']} | {names[r['method']]} | {r['seed']} | {r['status']} | {r['elapsed_seconds']:.1f} | {memory} | {reason} |")
    lines+=['','Each edge-KK refinement uses the matching successful new MDS seed. A failed MDS attempt leaves its refinement unavailable; no replacement seed is substituted.',
        '', 'Memory is sampled across the worker and its child processes, so short peaks may be missed and the ceiling can be briefly exceeded. '
        'Elapsed time includes preparation and evaluation, not only optimization. No elapsed-time cutoff was applied; algorithmic iteration limits still apply. '
        'Other methods and all prior resource policies remain as recorded.']
    (root/'MDS_30GIB_FINDINGS.md').write_text('\n'.join(lines)+'\n')
    return len(completed)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');a=p.parse_args()
    print(report(a.root),'completed higher-budget layouts')
