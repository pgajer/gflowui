"""Factual SFDP cohort summary; completion does not establish convergence."""
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json


def build(root, index_name='sfdp_results.json'):
    from run import GRAPHS, SEEDS, LIMITS
    root=Path(root)
    if Path(index_name).name != index_name:
        raise ValueError('index must be a plain filename')
    doc=read_json(root/index_name)
    rows=doc['runs']
    expected={(g,s) for g in GRAPHS for s in SEEDS}
    if len(rows)!=18 or {(r['graph_id'],r['seed']) for r in rows}!=expected:
        raise ValueError('incomplete/duplicate SFDP matrix')
    if doc['limits']!=LIMITS or doc['serial'] is not True or any(r['method']!='sfdp' for r in rows):
        raise ValueError('unexpected SFDP policy or method')
    complete=[r for r in rows if r['status']=='completed']
    versions=sorted({read_json(Path(r['run_dir'])/'request.json')['environment']['graphviz']['version']
                     for r in rows if r.get('run_dir')})
    lines=['# SFDP three-dimensional graph layouts','',
        'Result index: '+index_name+'. Backend: '+(', '.join(versions) or 'fixture/unavailable')+'.','',
        'Graphviz SFDP (Yifan Hu), with dim=3, dimen=3, K=1, seeded initialization, '+
        'no overlap removal or smoothing. All components retained. Components with fewer '+
        'than five vertices use the existing explicitly labeled classical placement; isolates '+
        'remain present. Component packing is translation-only and used only for viewing.', '',
        f'{len(complete)} of 18 requested layouts completed. Backend success is not a convergence claim.', '',
        '30 GiB sampled-RSS allowance; no elapsed-time cutoff; one supervised job at a time. '+
        'Times include graph preparation and evaluation, not only SFDP.', '',
        'Coordinates use Graphviz layout inches (output points divided by 72), with no fitted '+
        'normalization. Identity-scale edge/path errors depend on that native scale; fitted '+
        'chord/relative errors have their existing separate fitted scales.', '',
        '| Graph | Seed | Outcome | Seconds | Peak GiB | Euclidean error | Edge error |',
        '|---|---:|---|---:|---:|---:|---:|']
    def num(x):return 'unavailable' if x is None else f'{x:.5g}'
    for row in rows:
        score=row.get('scores',{});memory=row.get('peak_rss_bytes')
        lines.append('| '+ ' | '.join([row['graph_id'],str(row['seed']),row['status'],
            num(row.get('elapsed_seconds')),num(None if memory is None else memory/1024**3),
            num(score.get('chord_error')),num(score.get('edge_error'))])+' |')
    lines += ['', 'Failures or exclusions:']
    lines += [f"- {r['graph_id']}, seed {r['seed']}: {r.get('reason') or r['status']}" for r in rows if r['status']!='completed'] or ['- None.']
    if index_name!='sfdp_results.json' and (root/'sfdp_results.json').exists():
        old=read_json(root/'sfdp_results.json')['runs']
        lines += ['', 'Earlier backend attempts (not replaced):',
            f"- sfdp_results.json: {sum(r['status']=='completed' for r in old)} completed, "+
            f"{sum(r['status']!='completed' for r in old)} unsuccessful of {len(old)} requests. "+
            'Their outcomes, request manifests, and any completed layout remain available in the viewer and bundle. '+
            'Original process logs and raw Graphviz files remain in the external run directories.',
            '- Graphviz 15.1.1 aborted in multilevel coarsening on 17 cohort attempts. '+
            'Graphviz 16.1.0 documents an SFDP crash fix; the new index is a separately identified '+
            'full-cohort run, not a silent replacement of those failures.']
    lines += ['', 'The four original pilot graphs retain exact evaluation; sstmodel and circuit_2 '+
        'retain shared-pair distance evaluation and exact edge/neighborhood scores. Existing '+
        'results are unchanged. Optional warm-to-cool edge coloring is display-only and uses '+
        'the active layout’s edge-length range; colors are not comparable across different layouts.', '',
        'Sources: [SFDP](https://graphviz.org/docs/layouts/sfdp/), '+
        '[3D coordinates](https://graphviz.org/docs/attrs/dimen/), '+
        '[Hu’s gallery](https://yifanhu.net/GALLERY/GRAPHS/index73.html). '+
        'This is a 3D application of the method, not a reproduction of a particular gallery image.']
    (root/'SFDP_FINDINGS.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':build(*sys.argv[1:])
