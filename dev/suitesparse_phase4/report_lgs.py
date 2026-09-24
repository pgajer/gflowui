"""Publish measured LGS integration evidence, keeping synthetic/gallery apart."""
import argparse
from collections import Counter
from pathlib import Path
import shutil
import sys
import numpy as np
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256
from run_pilot import verified_cached
sys.path.insert(0,str(Path(__file__).resolve().parent))
from lgs_adapter import locality_settings

def report(root):
    root=Path(root);validation=root/'lgs_validation/phase04_validation_results.json'
    data=read_json(validation);gallery=read_json(root/'phase04_results.json')
    if len(data['runs'])!=34 or len(gallery['runs'])!=72:
        raise ValueError('expected complete fixed integration matrices (34 synthetic, 72 gallery)')
    if len({(r['graph_id'],r['locality']['requested'],r['seed']) for r in data['runs']})!=34:
        raise ValueError('duplicate validation cases')
    expected=set()
    for record in read_json(root/'cohort.json')['records']:
        if record['status']!='completed': continue
        info=read_json(root/'graphs'/record['graph_id'].replace('/','__')/'graph.json')
        expected.update((record['graph_id'],setting['requested'],seed)
            for setting in locality_settings(info['component_sizes']) for seed in [17,29,43])
    actual=[(r['graph_id'],r['locality']['requested'],r['seed']) for r in gallery['runs']]
    if len(set(actual))!=len(actual) or set(actual)!=expected: raise ValueError('incomplete or duplicate gallery locality matrix')
    rows=[];comparisons=0
    for r in data['runs']:
        row={k:r[k] for k in ['graph_id','seed','status','locality','elapsed_seconds','peak_rss_bytes','reason']}
        row.update(scores=r.get('scores',{}),warnings=[],objective_increasing_epochs=0)
        if r['status']=='completed':
            dest=Path(r['run_dir']);m=read_json(dest/'manifest.json')
            if not verified_cached(dest,m['run_key']): raise ValueError('invalid LGS result cache')
            result=read_json(dest/'result.json')
            for comp in result['components']:
                if comp['small_component_placement']: continue
                response=comp['details']['response'];quality=response['quality']
                for main,lgs in [('chord_error','euclidean_distance_error'),('relative_stress','relative_distance_stress'),
                                 ('edge_error','edge_length_error_identity'),('chord_scale','euclidean_fitted_scale'),
                                 ('relative_scale','relative_fitted_scale')]:
                    if not np.isclose(comp[main],quality[lgs],rtol=1e-10,atol=1e-12):
                        raise ValueError('independent metric implementations disagree: '+main)
                    comparisons+=1
                row['warnings'].extend(response['warnings'])
                hist=response['objective']['history']
                row['objective_increasing_epochs']+=sum(b['objective']>a['objective'] for a,b in zip(hist,hist[1:]))
            row['result_path']=str((dest/'result.json').relative_to(root))
            row['result_sha256']=sha256(dest/'result.json')
            row['coordinates_path']=str((dest/'coords_raw.csv').relative_to(root))
            row['coordinates_sha256']=sha256(dest/'coords_raw.csv')
            for key,path in [('vertices',dest/'vertices.json'),('manifest',dest/'manifest.json'),
                ('graph',root/'lgs_validation/graphs'/r['graph_id'].replace('/','__')/'graph.json')]:
                row[key+'_path']=str(path.relative_to(root));row[key+'_sha256']=sha256(path)
        rows.append(row)
    cases=[]
    for r in gallery['runs']:
        m=read_json(Path(r['run_dir'])/'manifest.json')
        cases.append(dict(graph_id=r['graph_id'],locality=r['locality'],seed=r['seed'],status=r['status'],
                          reason=r['reason'],admission=m['request']['admission']))
    payload=dict(schema_version=1,method='lgs-paper-union-v1',
        validation_population='Synthetic integration fixtures; not SuiteSparse gallery graphs.',
        validation_index_sha256=sha256(validation),gallery_index_sha256=sha256(root/'phase04_results.json'),
        dependency=data['dependency'],validation_rows=rows,gallery_rows=cases,
        metric_cross_checks=comparisons,
        limits='600 seconds / 2 GiB; at most 2000 vertices per component in accepted LGS contract.')
    atomic_json(root/'phase04_lgs_summary.json',payload)
    doc=root/'lgs_dependency_documents';doc.mkdir(exist_ok=True)
    source=Path(data['dependency']['root'])/'research/lgs3d'
    for name in ['METHOD.md','CONTRACT.md','NOTICE.md','EXPERIMENTS.md','requirements.lock']:
        shutil.copyfile(source/name,doc/name)
    shutil.copyfile(source/'vendor/L2G/LICENSE',doc/'LICENSE')
    lines=['# Experimental LGS integration','',
        'The accepted paper-form 3D LGS kernel is connected to the same original-graph evaluation pipeline. '
        'It is a deliberately different method from the original 2D L2G optimizer. No kernel expression was changed.',
        '', '## Examples and conditions','',
        'Synthetic path (48 vertices), grid (49), joined cliques (48), complete tetrahedron (4), '
        'and a two-component graph with an isolate use seeds 17, 29 and 43; the 128-vertex path uses seed 17. '
        'All runs use 60 epochs, walk depth 10, decay 0.1 and repulsion 0.2. Locality caps 16,32,64,128,256 '
        'and all-neighbors are clipped/deduplicated per component. These are NOT gallery graph results. '
        'Main-project vertex IDs and initialization are retained; they differ from the standalone worker fixtures, '
        'so this is a new integration experiment, not a claim of identical coordinates to that earlier sweep.',
        '', '## Measured results','',
        f'Synthetic statuses: {dict(Counter(r["status"] for r in rows))}. '
        f'{comparisons} component metric/scale comparisons agree between the separate dependency and main-project implementations.',
        '', '| Synthetic graph | Requested cap | Seed | Status | Euclidean error | Relative stress | Fixed-path error | Edge error | Seconds |',
        '| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |']
    for r in rows:
        scores=r['scores'];values=[format(scores[k],'.6g') if scores.get(k) is not None else 'unavailable'
                                  for k in ['chord_error','relative_stress','path_error','edge_error']]
        lines.append(f'| {r["graph_id"]} | {r["locality"]["requested"]} | {r["seed"]} | {r["status"]} | '+
                     ' | '.join(values)+f' | {r["elapsed_seconds"]:.3f} |')
    lines+=['','## Gallery admission','',
        f'Gallery statuses: {dict(Counter(r["status"] for r in cases))}. '
        'Each of the four gallery graphs has six locality settings and three planned seeds. '
        'A time-preflight exclusion is not an attempted optimizer failure or measured gallery runtime. '
        'No gallery layout is claimed where the method was excluded. The time projection doubles the largest '
        'measured connected-fixture seconds/n^3 (n>=32), then sums component cubes.',
        '', '| Gallery graph | Largest component | Projected seconds | Reason |',
        '| --- | ---: | ---: | --- |']
    for g in dict.fromkeys(r['graph_id'] for r in cases):
        r=next(r for r in cases if r['graph_id']==g);a=r['admission']
        lines.append(f'| {g} | {a["largest_component"]} | {a["projected_seconds"]:.1f} | {r["reason"] or "admitted"} |')
    lines+=['','## Recorded warning cases','']
    for graph in dict.fromkeys(r['graph_id'] for r in rows):
        for requested in dict.fromkeys(r['locality']['requested'] for r in rows if r['graph_id']==graph):
            subset=[r for r in rows if r['graph_id']==graph and r['locality']['requested']==requested]
            warnings=sorted({w for r in subset for w in r['warnings']})
            if warnings: lines.append(f'- {graph}, requested cap {requested}: '+ '; '.join(warnings)+'.')
    lines+=['','## Interpretation and limitations','',
        f'The synthetic runs contain {sum(r["objective_increasing_epochs"] for r in rows)} objective-increasing epochs. '
        'Safeguarded pair descent does not guarantee full-objective descent; finite output does not prove convergence. '
        'Disconnected attractive constraints with positive repulsion can yield an unbounded-below objective. '
        'Warnings and complete objective histories remain in component results. Locality changes the objective; '
        'compare external graph-quality measures, not raw objective values across k.',
        '', 'The separately rebuilt dependency demonstration validates its analytical 2D/3D distinction and portable '
        'contract. It does not establish useful layouts at gallery scale. The accepted 2000-vertex contract and '
        'conservative runtime exclusions remain limits, not a claim that an optimized future kernel cannot scale. '
        'No weighted graph support, approximate LGS replacement or convergence improvement is supplied.',
        '', 'The app shows synthetic locality plots in a separately labeled subsection. '
        'phase04_lgs_summary.json retains all seeds, statuses, actual per-component localities/fractions, hashes and timings. '
        'The copied dependency documents define its mathematics, BSD attribution and portable contract.']
    (root/'PHASE04_FINDINGS.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');report(p.parse_args().root)
