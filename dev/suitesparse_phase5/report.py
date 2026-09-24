"""Report coverage, calibration and sampling diagnostics without selecting winners."""
import argparse
from collections import Counter
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256

def report(root):
    root=Path(root);index=read_json(root/'phase05_results.json')
    cohort=read_json(root/'phase05_cohort.json');validation=read_json(root/'phase05_sampling_validation.json')
    summaries=[]
    for r in validation['runs']:
        for key in r['trials'][0]['metrics']:
            vals=[t['metrics'][key] for t in r['trials']]
            summaries.append(dict(graph_id=r['graph_id'],method=r['method'],metric=key,
                replicates=len(vals),covered=sum(v['covered'] for v in vals),
                max_absolute_error=max(v['absolute_error'] for v in vals)))
    rows=index['runs'];coverage=[];graphs=[]
    for rec in cohort['records']:
        g=read_json(root/'graphs'/rec['graph_id'].replace('/','__')/'graph.json')
        graphs.append({k:g[k] for k in ('graph_id','n_vertices','n_edges','n_components','n_isolates','graph_sha256')})
        methods=sorted(set(r['method'] for r in rows if r['graph_id']==rec['graph_id']))
        for method in methods:
            rr=[r for r in rows if r['graph_id']==rec['graph_id'] and r['method']==method]
            coverage.append(dict(graph_id=rec['graph_id'],method=method,counts=dict(Counter(r['status'] for r in rr)),
                reasons=sorted(set(r['reason'] or '' for r in rr if r['status']!='completed'))))
    atomic_json(root/'phase05_summary.json',dict(source_commit=index['commit'],graphs=graphs,coverage=coverage,
        sampling_validation=summaries,source_indexes={n:sha256(root/n) for n in ('phase05_results.json','phase05_admission.json','phase05_sampling_validation.json')}))
    lines=['# Controlled larger-graph expansion','',
        'Two additional gallery graphs were selected by increasing vertex count, after metadata screening. '
        'This is a deliberately small feasibility expansion, not a representative performance benchmark.',
        '', '| Graph | Vertices | Edges | Components | Isolates |','|---|---:|---:|---:|---:|']
    lines += [f"| {g['graph_id']} | {g['n_vertices']} | {g['n_edges']} | {g['n_components']} | {g['n_isolates']} |" for g in graphs]
    lines += ['', 'All vertices, including isolates, are retained. Unit edges are derived from numerical matrix support; '
        'matrix coefficients are not distances. No graph repair or subsampling was used.',
        '', '## Method coverage','', '| Graph | Method | Terminal outcomes | Reasons for gaps |','|---|---|---|---|']
    lines += [f"| {r['graph_id']} | {r['method']} | {r['counts']} | {'; '.join(r['reasons'])} |" for r in coverage]
    lines += ['', 'Every job retains the 600-second/2-GiB allowance. Admission uses the largest completed '
        'same-method graph, worst replicate, quadratic memory scaling with a 1.1 factor and cubic time scaling '
        'with a 1.25 factor. These admission heuristics are not guaranteed upper bounds or measured failures. Expanded runs may '
        'calibrate the next graph; results record the exact references. LGS components above 2,000 vertices '
        'remain outside its accepted contract. No LGS optimizer was attempted here.',
        '', '## Sampled versus exact evaluation','',
        'Expanded distance scores use up to 20,000 shared uniform unordered pairs per component. '
        'Both scales are refitted on that sample; component sums are weighted by the population/sample '
        'pair ratio before aggregation. Neighborhood ranks are streamed exactly; edge errors are exact. '
        'Original pilot coordinates and exact scores are unchanged.',
        '', 'Validation used six fixed pilot layouts (weighted GRIP and original-graph Isomap on three '
        'graphs), 20 independent pair samples each. Every result is retained. These are evaluation reruns, '
        'not new optimization results. Approximate 95% intervals use 200 paired bootstrap draws and a '
        'finite-population correction. Their coverage is empirical, not guaranteed.',
        '', '| Graph | Method | Metric | Largest absolute difference | Intervals covering exact value |',
        '|---|---|---|---:|---:|']
    lines += [f"| {r['graph_id']} | {r['method']} | {r['metric']} | {r['max_absolute_error']:.6g} | {r['covered']}/{r['replicates']} |" for r in summaries]
    lines += ['', 'Intervals condition on one fixed embedding and are distinct from differences across optimizer '
        'seeds. Full-population small-graph equivalence, tied-neighborhood ranks and disconnected population '
        'weighting are separately covered by tests. The agreement study checks larger connected components; '
        'it is not a universal coverage theorem.',
        '', '## Exports and limitations','',
        'The app ZIP includes all tables, saved coordinates, graph identities, settings, metric definitions '
        'and PDF/SVG figures for the selected graph. A standalone R script rebuilds those figures from '
        'the exported table and settings. Sample intervals and active-run guides are labeled. The ZIP '
        'is an analysis bundle, not a self-contained installation of every optimizer and its environment.',
        '', 'No graph above 10,000 vertices or 100,000 edges was downloaded. Only two additional eligible '
        'graphs were selected; other eligible cases remain untested. Sample intervals are approximate, '
        'distance-band summaries are not computed for this evaluator, optional cluster diagnostics '
        'remain deferred, and exact neighborhood scores still depend on the declared lexical-ID tie rule. '
        'RSS is sampled and may miss short peaks. Finite output does not establish optimality or useful '
        'geometry; no method is declared universally best.']
    (root/'PHASE05_FINDINGS.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');report(p.parse_args().root)
