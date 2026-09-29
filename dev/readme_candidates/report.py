from pathlib import Path
import sys,json,hashlib,csv
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json
r=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
rows=[]
admission=[]
for graph in read_json(r/'readme_candidates/selection.json')['selected']:
 info=read_json(r/'graphs'/graph.replace('/','__')/'graph.json')
 admission.append(dict(graph=graph,n=info['n_vertices'],edges=info['n_edges'],components=info['n_components'],admitted=info['n_components']==1,kind=info['source']['metadata'].get('Kind')))
(r/'readme_candidates/admission.json').write_text(json.dumps(admission,indent=2))
for row in read_json(r/'readme_candidate_results.json')['runs']:
 result=read_json(Path(row['run_dir'])/'result.json');c=result['components'][0];d=c['details'];start=d['starts'][0]
 summary=result['summary']
 rows.append(dict(graph=row['graph_id'],seed=row['seed'],elapsed_seconds=row['elapsed_seconds'],fit_seconds=d['fit_elapsed_seconds'],termination=start['termination'],iterations=start['iterations'],**{k:summary.get(k) for k in ['chord_error','relative_stress','edge_error','path_error']}))
f=r/'readme_candidates/results.csv'
with f.open('w') as o:w=csv.DictWriter(o,fieldnames=rows[0]);w.writeheader();w.writerows(rows)
text='''# README graph candidates\n\nThese connected, unit-edge graphs were selected from SuiteSparse metadata for application diversity, not to establish representative performance. Three random-start full-SGD fits per graph use uniform pair weights and 1,000 iterations. All pairwise graph distances are evaluated. Time includes preparation, fitting and scoring; fit-only time is separately recorded. Reaching an iteration limit is not convergence.\n\nOpen **SuiteSparse 3D Embedding Comparison** in gflowui and select the new graph, then **Metric MDS — SGD (README candidates)**. The Inspector retains all seeds even if the menu displays only one example per configuration. Reload the project to refresh its saved manifest.\n\n| Graph | Seed | Fit seconds | Termination | Scale-fitted chord error |\n|---|---:|---:|---|---:|\n'''
for row in rows:text+=f"| {row['graph']} | {row['seed']} | {row['fit_seconds']:.1f} | {row['termination']} | {row['chord_error']:.4f} |\n"
text+='\n## Graph admission\n\n| Source | Application | Vertices | Edges | Components | Admitted |\n|---|---|---:|---:|---:|---|\n'
for g in admission:text+=f"| {g['graph']} | {g['kind']} | {g['n']} | {g['edges']} | {g['components']} | {g['admitted']} |\n"
text+='\nThe disconnected candidates dwt_492 and oscil_dcop_01 were excluded without modification. Matrix coefficients were discarded after numerical support conversion; all retained edges have unit length. Original archives and attribution are retained.\n'
text+='\nChord error is the root sum-squared Euclidean distance error divided by the root sum-squared graph distance, after fitting one scale; zero is exact agreement. The full CSV and run records retain other diagnostics. Selection for visual appeal is a showcase decision, not evidence that a method outperforms alternatives. The current README uses the bundled 494_bus graph with seed 11 and default initialization, separately from these random-start trials.\n'
(r/'readme_candidates/README.md').write_text(text)
env=read_json(r/'readme_candidates/environment.json');repo=Path('/Users/pgajer/current_projects/grip')
assert all(hashlib.sha256((repo/p).read_bytes()).hexdigest()==h for p,h in env['grip_source_sha256'].items()),'grip source changed during runs'
print('Recorded',len(rows),'runs; fitting source hashes unchanged')
