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
    terminations=[]
    failures=[]
    for run in runs['runs']:
        if run.get('run_dir'):
            manifest=read_json(Path(run['run_dir'])/'manifest.json')
            if run['status']!='completed':
                failures.append(dict(graph_id=run['graph_id'],method=run['method'],seed=run['seed'],
                                     reason=manifest.get('reason'),peak_rss_bytes=manifest['peak_rss_bytes']))
            else:
                result=read_json(Path(run['run_dir'])/'result.json')
                for comp in result['components']:
                    if comp['small_component_placement']: continue
                    detail=comp['details'].get('metadata',comp['details'])
                    terminations.append(dict(graph_id=run['graph_id'],method=run['method'],seed=run['seed'],
                        component=comp['component'],termination=detail.get('termination','not reported'),
                        converged=detail.get('converged')))
    atomic_json(root/'termination_diagnostics.json',dict(components=terminations,stopped_runs=failures))
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
       '',f"{sum(t['termination']=='iteration_limit' for t in terminations)} component optimizations reached their iteration limit. "
       'Completed means that finite coordinates and scores were saved, not that an optimum was established. '
       'Full optimizer termination records are in termination_diagnostics.json.',
       '', *[f"- {x['graph_id']}, {x['method']}, seed {x['seed']}: {x['reason']}; sampled peak RSS {x['peak_rss_bytes']/1024**3:.3f} GiB." for x in failures],
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
    sensitivity=[]
    for count,filename in [(16,'lle_landmarks16.json'),(32,'lle_landmarks32.json'),(64,'pilot_results.json')]:
        if not (root/filename).exists(): continue
        for run in read_json(root/filename)['runs']:
            if run['method']=='lle':
                sensitivity.append(dict(graph_id=run['graph_id'],landmarks=count,status=run['status'],
                    chord_error=run.get('scores',{}).get('chord_error'),
                    neighborhood=run.get('scores',{}).get('neighborhood'),run_dir=run.get('run_dir')))
    atomic_json(root/'landmark_sensitivity.json',dict(runs=sensitivity))
    lines+=['','Euclidean error and relative stress fit a scale separately per connected component. '
        'Fixed-path and edge errors use identity scale. Consequently, large identity-scale errors can reflect '
        'layout scale as well as shape; these columns are not a single ranking. '
        'Path lengths follow frozen original-graph shortest routes, not recomputed paths in the drawing. '
        'Component scores and neighborhood curves are retained in each result.json. Cross-component pairs are excluded.',
        '', '## LLE landmark sensitivity','',
        'The same graphs and settings were evaluated with 16, 32 and 64 farthest-point landmark-distance features. '
        'This is a diagnostic sensitivity comparison, not a search for the best setting. '
        'Coordinates and all scores remain available for every setting. Changes in error show that '
        '64 landmarks cannot yet be treated as a representation-independent LLE benchmark.',
        '', '| Graph | Landmarks | Status | Euclidean error |','|---|---:|---|---:|',
        *[f"| {x['graph_id']} | {x['landmarks']} | {x['status']} | "+
          (f"{x['chord_error']:.5g}" if x['chord_error'] is not None else 'unavailable')+' |' for x in sensitivity],
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
        artifacts={p:sha256(root/p) for p in ['catalog/gallery.json','cohort.json','pilot_results.json','scores.csv','FINDINGS.md',
            'termination_diagnostics.json','landmark_sensitivity.json','lle_landmarks16.json','lle_landmarks32.json'] if (root/p).exists()}))


if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');report(p.parse_args().root)
