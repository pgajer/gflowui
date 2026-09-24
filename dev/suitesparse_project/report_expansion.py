"""Additional-method findings from saved results; never rerun an optimizer."""
import argparse
from collections import defaultdict,Counter
import csv
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json,atomic_json,sha256
from report import format_replicates,replicate_summary
from run_pilot import verified_cached

METRICS=['chord_error','relative_stress','path_error','edge_error']

def require_complete_matrix(runs,graph_ids,methods=None):
    if methods is None: methods=['pacmap','localmap','trimap','phate','largevis','ncvis']
    expected={(g,m,s) for g in graph_ids for m in methods for s in (17,29,43)}
    actual=[(r['graph_id'],r['method'],r['seed']) for r in runs]
    if len(actual)!=len(set(actual)) or set(actual)!=expected:
        raise ValueError('incomplete, duplicate or unexpected Phase 03 run matrix')

def report(root):
    root=Path(root);index=read_json(root/'phase03_results.json')
    graph_ids=[r['graph_id'] for r in read_json(root/'cohort.json')['records'] if r['status']=='completed']
    require_complete_matrix(index['runs'],graph_ids)
    source_indexes={'phase03_results.json':sha256(root/'phase03_results.json')}
    if (root/'phase03_trimap_graph_results.json').exists():
        extra=read_json(root/'phase03_trimap_graph_results.json')
        require_complete_matrix(extra['runs'],graph_ids,['trimap_graph'])
        index['runs']+=extra['runs']
        source_indexes['phase03_trimap_graph_results.json']=sha256(root/'phase03_trimap_graph_results.json')
    baseline={r['graph_id']:r for r in read_json(root/'pilot_results.json')['runs']
              if r['method']=='isomap_graph' and r['status']=='completed'}
    records=[];groups=defaultdict(list);capabilities={};warnings=[]
    for run in index['runs']:
        groups[run['graph_id'],run['method']].append(run)
        record={k:run.get(k) for k in ['graph_id','method','seed','status','reason','elapsed_seconds','peak_rss_bytes','run_dir']}
        if run.get('run_dir'):
            dest=Path(run['run_dir']);manifest=read_json(dest/'manifest.json')
            contract=manifest['request']['adapter_contract']
            capabilities[run['method']]=dict(status=contract['status'],dimension=3,
                same_seed_max_abs=contract['same_seed_max_abs'],different_seed_max_abs=contract['different_seed_max_abs'],
                input_type=('original graph distances' if run['method']=='trimap_graph' else '64-landmark graph-distance features; one component scale'),
                numerical_source=manifest['request']['commit'],environment=manifest['request']['environment'])
            if run['status']=='completed':
                if not verified_cached(dest,manifest['run_key']): raise ValueError('corrupt completed run')
                result=read_json(dest/'result.json')
                record.update({k:result['summary'].get(k) for k in METRICS})
                base=Path(baseline[run['graph_id']]['run_dir'])
                for comp in result['components']:
                    name=f"component_{comp['component']:03d}"
                    prep=read_json(dest/name/'preparation.json');old=read_json(base/name/'preparation.json')
                    for key in ['features_sha256','distances_sha256','predecessors_sha256','vertex_ids','landmark_ids','landmark_scale']:
                        if prep[key]!=old[key]: raise ValueError('prepared inputs changed from baseline: '+key)
                    if comp.get('warnings'):
                        warnings.append(dict(graph_id=run['graph_id'],method=run['method'],seed=run['seed'],
                                             component=comp['component'],warnings=comp['warnings']))
        records.append(record)
    fields=['graph_id','method','seed','status','reason',*METRICS,'elapsed_seconds','peak_rss_bytes','run_dir']
    with (root/'phase03_scores.csv').open('w') as stream:
        writer=csv.DictWriter(stream,fields);writer.writeheader();writer.writerows(records)
    summaries=[]
    lines=['# Additional 3D methods on the fixed SuiteSparse pilot','',
        'Purpose: compare six additional public 3D backends on the same four gallery graphs, without changing graph conversion or quality measures.',
        '', '## Methods and conditions','',
        'PaCMAP, LocalMAP, TriMAP, PHATE, LargeVis and NCVis each use seeds 17, 29 and 43. '
        'Their shared input is distance to up to 64 frozen graph landmarks, divided by one component distance scale. '
        'Every completed preparation was checked against the accepted original-graph baseline: vertex order, landmarks, '
        'prepared float64 feature values, graph distances and fixed path predecessors match exactly. '
        'The six new adapters convert these features to float32 at their backend boundary. Components remain separate; isolates are retained.',
        '', 'A separately identified TriMAP variant uses the original graph shortest-path distances through the public precomputed-distance interface. '
        'It was added after the feature-based runs exposed repeated feature rows, extreme triplet weights and very large coordinates. '
        'The original feature-based results remain in the comparison; the new variant is not a replacement or a best-seed selection.',
        '', 'The saved diagnostic phase03_trimap_scale_diagnosis.json reconstructs seed-17 triplets and weights '
        'and matches their saved hashes. In graph order illc1033, lock1074, lock_700, nemscem, the largest '
        'components contain 1353, 1038, 691, 2363 vertices but only 293, 226, 234, 997 distinct float32 '
        'feature rows. Local bandwidths reach the backend floor for 919, 797, 240, 1107 vertices; '
        'maximum triplet weights are approximately 5.9e10–8.5e10. The retained feature layouts have '
        'raw coordinate magnitudes of approximately 17–125 million. These are poor outputs, not successful '
        'quality results merely because their coordinates are finite. The original-distance variant changes '
        'the input geometry as well as removing these repeated-feature targets; quality differences cannot '
        'be attributed solely to the degeneracy.',
        '', 'Exact metrics still use original graph targets, not internal neighbor or diffusion distances. '
        'Euclidean error and relative stress fit a component scale; fixed-path and edge errors use identity scale. '
        'These answer different questions and are not combined into a winner score. '
        'Runtime includes imports, preparation and scoring; sampled peak memory can miss brief peaks. '
        'The limits are 600 seconds and 2 GiB per job, with one benchmark job at a time.',
        '', '## Coverage','', str(dict(Counter(r['status'] for r in records))),
        '', 'A completed job has finite 3D coordinates and saved scores, not a demonstrated optimum. '
        'PaCMAP/LocalMAP, TriMAP, NCVis and LargeVis use explicitly fixed budgets. '
        'PHATE uses automatic diffusion time and SGD-MDS; its actual stopping iteration is not exposed. '
        'Backend logs retain convergence warnings. Seed ranges below are observed variability, not confidence intervals.',
        '', '| Graph | Method | Completed | Euclidean error | Relative stress | Fixed-path error | Edge error |',
        '| --- | --- | ---: | ---: | ---: | ---: | ---: |']
    for (graph,method),runs in groups.items():
        done=[r for r in runs if r['status']=='completed'];values=[]
        for metric in METRICS:
            scores=[r.get('scores',{}).get(metric) for r in done]
            values.append(format_replicates(scores))
            summaries.append(dict(graph_id=graph,method=method,metric=metric,planned=len(runs),**replicate_summary(scores)))
        lines.append(f'| {graph} | {method} | {len(done)}/{len(runs)} | '+' | '.join(values)+' |')
    lines+=['','## Limitations','',
        'This is the same limited gallery cohort, not a balanced test of every graph type. '
        'Feature-based runs and distance/edge-based baselines have different input tracks. '
        'The nonplanar contract test establishes genuine 3D output and repeatability on one synthetic example, '
        'not universal quality. TriMAP uses exact feature neighbors and a pinned legacy backend; '
        'LargeVis uses a documented seed override and 20 million edge samples, below its actual default billion-sample budget. '
        'Native macOS compatibility changes are documented in the source README. '
        'No original baseline optimizer was rerun or silently replaced.',
        '', 'Per-run settings, selected PHATE time, warnings and termination are in the component results and process logs. '
        'The full score table is phase03_scores.csv; contract/environment evidence is phase03_capabilities.json.']
    for r in records:
        if r['status']!='completed': lines.append(f"- {r['graph_id']}, {r['method']}, seed {r['seed']}: {r['status']}; {r.get('reason') or 'see process log'}.")
    atomic_json(root/'phase03_replicate_summary.json',dict(runs=summaries))
    atomic_json(root/'phase03_capabilities.json',dict(schema_version=1,methods=capabilities,warnings=warnings,
                                                   source_indexes_sha256=source_indexes))
    (root/'PHASE03_FINDINGS.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');report(p.parse_args().root)
