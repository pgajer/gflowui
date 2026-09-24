"""Generate compact human-readable findings and flat scores from saved results."""
import argparse
import csv
from collections import Counter,defaultdict
from pathlib import Path
from statistics import mean
from common import read_json,sha256,atomic_json


def report(root):
    root=Path(root)
    runs=read_json(root/'pilot_results.json')
    catalog=read_json(root/'catalog/gallery.json')
    cohort=read_json(root/'cohort.json')
    fields=['graph_id','method','seed','status','elapsed_seconds','peak_rss_bytes',
            'chord_error','relative_stress','path_error','edge_error','run_dir']
    with (root/'scores.csv').open('w') as stream:
        writer=csv.DictWriter(stream,fields);writer.writeheader()
        for run in runs['runs']:
            row={k:run.get(k) for k in fields}
            row.update({k:run.get('scores',{}).get(k) for k in fields[6:10]})
            writer.writerow(row)
    lines=['# SuiteSparse Phase 1 pilot: findings and limitations','',
      'Purpose: validate six 3D adapters and common quality measures on the eligible gallery subset.',
      '',f"The metadata inventory contains {catalog['gallery_count']} gallery matrices. "
      f"Only {len(cohort['records'])} meet the frozen bounds (fewer than 3,000 vertices and a conservative 100,000-edge bound). "
      'All eligible matrices were included; the six-to-eight target could not be met. No outside-gallery substitutes were used.',
      '', '## Graphs and interpretation','',
      '| Graph | Vertices | Edges | Components | Isolates | Max degree | Mean clustering |',
      '|---|---:|---:|---:|---:|---:|---:|']
    for rec in cohort['records']:
        g=read_json(root/'graphs'/rec['graph_id'].replace('/','__')/'graph.json')
        lines.append(f"| {g['graph_id']} | {g['n_vertices']} | {g['n_edges']} | {g['n_components']} | {g['n_isolates']} | {g['degree_max']:.0f} | {g['mean_clustering']:.3f} |")
    lines+=['','The two lock matrices are structural-engineering problems, represented by undirected numerical-support graphs. '
       'Their clustering coefficients exceed 0.6. The two rectangular matrices are represented as bipartite graphs; '
       'their zero triangle clustering follows from that representation, not proof of absent community structure. '
       'The rectangular examples have more heterogeneous degrees and include hubs. Isolates are retained without repair.',
       '', 'This cohort covers structural and bipartite/hub-rich patterns, but it is not a balanced collection of all '
       'desired graph types. There is no independently established pure-tree or community-structure example. '
       'The synthetic paths, cycles, and octahedral graph are correctness fixtures, not extra benchmark subjects.',
       '', '## Run coverage','',str(dict(Counter(x['status'] for x in runs['runs']))),
       '', 'Stochastic MDS, its paired edge-KK refinement, weighted GRIP, and UMAP use seeds 17, 29, 43. '
       'Classical scaling (Isomap on the original graph) and dense LLE run once. '
       'Every job includes preparation, embedding and exact evaluation, with a 600-second and 2-GiB RSS budget. '
       'Process startup and package loading are included in elapsed time. Peak RSS is sampled; short peaks may be missed.',
       '', '## Scores (means over completed replicates only; lower is better)','',
       '| Graph | Method | Completed | Euclidean error | Relative stress | Fixed-path error | Edge error |',
       '|---|---|---:|---:|---:|---:|---:|']
    groups=defaultdict(list)
    for run in runs['runs']: groups[run['graph_id'],run['method']].append(run)
    for (graph,method),rr in groups.items():
        done=[x for x in rr if x['status']=='completed']
        vals=[]
        for metric in ['chord_error','relative_stress','path_error','edge_error']:
            v=[x['scores'][metric] for x in done if x['scores'].get(metric) is not None]
            vals.append(f'{mean(v):.5g}' if v else 'unavailable')
        lines.append(f'| {graph} | {method} | {len(done)}/{len(rr)} | '+ ' | '.join(vals)+' |')
    lines+=['','Euclidean error and relative stress fit a scale separately per connected component. '
        'Fixed-path and edge errors use identity scale. Consequently, large identity-scale errors can reflect '
        'layout scale as well as shape; these columns are not a single ranking. '
        'Path lengths follow frozen original-graph shortest routes, not recomputed paths in the drawing. '
        'Component scores and neighborhood curves are retained in each result.json. Cross-component pairs are excluded.',
        '', '## Limitations and unverified claims','',
        '- No LGS implementation or live app panel is included; project registration and visualization are Phase 2.',
        '- No hyperparameter tuning or universal superiority claim is made. Fixed optimization budgets need not imply convergence.',
        '- Small components use explicitly labeled classical placement, not purported output of the requested optimizer.',
        '- Optional cluster-distance diagnostics are deferred: no community partition was frozen.',
        '- Landmark sensitivity is recorded separately; one feature representation does not make LLE identical to graph-distance methods.',
        '- No larger graphs, approximate neighbor rankings, or population-level quality conclusions are validated.',
        '', '## Reproduction and sources','',
        f"Source commit: `{runs['commit']}`. Exact per-run settings, code hashes, environments, artifact checksums, "
        'and process logs are in each run directory.',
        '', '[SuiteSparse gallery and matrix licensing](https://sparse.tamu.edu/about). '
        'Preserve the downloaded Matrix Market headers and their matrix-specific citations. '
        'Converted graphs are explicitly derived assets, not original collection matrices.',
        '', 'See dev/suitesparse_phase1/README.md in the source repository for formulas and reproduction commands.']
    (root/'FINDINGS.md').write_text('\n'.join(lines)+'\n')
    atomic_json(root/'deliverables.json',dict(schema_version=1,source_commit=runs['commit'],
        artifacts={p:sha256(root/p) for p in ['catalog/gallery.json','cohort.json','pilot_results.json','scores.csv','FINDINGS.md']}))


if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');report(p.parse_args().root)
