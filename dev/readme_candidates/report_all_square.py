from pathlib import Path
import json,csv,hashlib,collections,subprocess
r=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison');out=r/'square_400_500'
runs=json.loads((r/'square_candidate_results.json').read_text())['runs'];assert len(runs)==93 and all(x['status']=='completed' for x in runs)
rows=[]
for row in runs:
 dest=Path(row['run_dir']);m=json.loads((dest/'manifest.json').read_text())
 for p,h in m['artifacts'].items():assert hashlib.sha256((dest/p).read_bytes()).hexdigest()==h,(dest,p)
 result=json.loads((dest/'result.json').read_text());d=result['components'][0]['details'];start=d['starts'][0]
 rows.append(dict(graph=row['graph_id'],seed=row['seed'],reused=row.get('reused',False),fit_seconds=d['fit_elapsed_seconds'],termination=start['termination'],iterations=start['iterations'],**{k:result['summary'].get(k) for k in ['chord_error','relative_stress','edge_error','path_error']}))
for p,h in json.loads((out/'fitting_source_snapshot.json').read_text()).items():assert hashlib.sha256(Path(p).read_bytes()).hexdigest()==h,p
rows.sort(key=lambda r:(r['graph'],r['seed']))
with (out/'embedding_results.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=rows[0]);w.writeheader();w.writerows(rows)
counts=collections.Counter(x['termination'] for x in rows)
s='''# 3D SGD embeddings of the connected SuiteSparse candidates

All 31 admitted graphs have three full metric-MDS fits in three dimensions, using the SGD backend, uniform pair weights, random initialization, seeds 11, 29 and 43, and a 1,000-iteration budget. Targets are ordinary shortest-path distances on unweighted undirected graphs. No edge-KK refinement is applied.

Twelve matching fits from the initial four-graph cohort are reused; 81 fits are newly computed. New fits ran with up to four concurrent jobs, so runtimes are descriptive and should not be treated as a controlled comparison with earlier timings. Fit time excludes preparation and scoring. All unordered vertex pairs are evaluated.

Open **SuiteSparse 3D Embedding Comparison** in gflowui and select a graph, then **Metric MDS — SGD (README candidates)**. All three seeds are retained in the Inspector; the menu may show one representative per configuration. Reload the project to read the updated manifest.

An iteration limit is not a convergence claim. These are candidates for visual inspection, not a method-ranking experiment. Chord error is normalized Euclidean-versus-graph distance error after fitting one scale; zero means exact agreement. Detailed run records also retain edge and path diagnostics.

'''
s+='Stopping status: '+str(dict(counts))+'.\n\n| Graph | Seed | Fit seconds | Stopping status | Chord error |\n|---|---:|---:|---|---:|\n'
for x in rows:s+=f"| {x['graph']} | {x['seed']} | {x['fit_seconds']:.1f} | {x['termination']} | {x['chord_error']:.4f} |\n"
s+='\n[Graph admission inventory]('+str(out/'README.md')+') · [HTML]('+str(out/'README.html')+').\n'
(out/'embeddings.md').write_text(s)
subprocess.run(['pandoc',str(out/'embeddings.md'),'--standalone','--metadata','title=SuiteSparse SGD embeddings','-o',str(out/'embeddings.html')],check=True)
print('Verified 93 manifests, artifact hashes, and unchanged fitting sources;',dict(counts))
