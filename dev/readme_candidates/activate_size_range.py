"""Add one verified size cohort to the current (possibly curated) viewer."""
from pathlib import Path
import sys,datetime,hashlib,csv,collections,subprocess
base=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(base/'suitesparse_project'))
import export_viewer as exp
from common import read_json,atomic_json
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
lower,upper=map(int,sys.argv[1:3]);prefix=f'square_{lower}_{upper}';out=root/prefix
runs=read_json(root/(prefix+'_results.json'))['runs']
cohort=read_json(out/'embedding_candidates.json')['records']
assert len(runs)==3*len(cohort) and all(r['status']=='completed' for r in runs)
rows=[]
for r in runs:
 dest=Path(r['run_dir']);m=read_json(dest/'manifest.json')
 for p,h in m['artifacts'].items():assert hashlib.sha256((dest/p).read_bytes()).hexdigest()==h
 result=read_json(dest/'result.json');d=result['components'][0]['details'];start=d['starts'][0]
 rows.append(dict(graph=r['graph_id'],seed=r['seed'],fit_seconds=d['fit_elapsed_seconds'],termination=start['termination'],iterations=start['iterations'],**{k:result['summary'].get(k) for k in ['chord_error','relative_stress','edge_error','path_error']}))
rows.sort(key=lambda r:(r['graph'],r['seed']))
if rows:
 with (out/'embedding_results.csv').open('w') as f:
  w=csv.DictWriter(f,fieldnames=rows[0]);w.writeheader();w.writerows(rows)
original=read_json(root/'viewer_manifest.json')
backup=root/'viewer_manifests'/('before_'+prefix+'_'+datetime.datetime.now().strftime('%Y%m%d_%H%M%S')+'.json');atomic_json(backup,original)
save=exp.atomic_json
def staged(path,obj):
 if Path(path).name=='viewer_manifest.json':path=out/'viewer_manifest.json'
 save(path,obj)
exp.atomic_json=staged
exp.build(root,[prefix+'_results.json'],prefix+'_cohort.json')
new=read_json(out/'viewer_manifest.json')
for field,key in [('graphs','id'),('runs','id'),('indexes','path')]:
 merged={r[key]:r for r in original[field]};merged.update({r[key]:r for r in new[field]});original[field]=list(merged.values())
original['artifacts'].update(new['artifacts'])
original['notes']+=f' Added connected unique square {lower+1}–{upper} vertex candidates, three full SGD fits each.'
atomic_json(root/'viewer_manifest.json',original)
text=f'# SuiteSparse {lower+1}–{upper} vertex SGD embeddings\n\n{len(cohort)} connected graphs; {len(rows)} completed fits. Targets are ordinary shortest-path distances on unweighted undirected numerical support. Full metric-MDS uses SGD, uniform pair weights, random starts, seeds 11, 29, 43, and a 1,000-iteration budget. Four jobs run concurrently; fit timings exclude preparation and scoring and are descriptive. No edge-KK refinement. Diagnostics evaluate all unordered vertex pairs. An iteration limit does not establish convergence.\n\n'
text+='Stopping status: '+str(dict(collections.Counter(r['termination'] for r in rows)))+'\n\n| Graph | Seed | Fit seconds | Stopping status | Chord error |\n|---|---:|---:|---|---:|\n'
for r in rows:text+=f"| {r['graph']} | {r['seed']} | {r['fit_seconds']:.2f} | {r['termination']} | {r['chord_error']:.4f} |\n"
text+='\nChord error compares embedded Euclidean distances with graph distances after fitting one scale; zero is exact agreement.\n'
(out/'embeddings.md').write_text(text)
for name in ['README','embeddings']:
 subprocess.run(['pandoc',str(out/(name+'.md')),'--standalone','--metadata','title=SuiteSparse graph candidates','-o',str(out/(name+'.html'))],check=True)
print('Activated',prefix,len(cohort),'graphs;',len(rows),'verified runs; total graphs',len(original['graphs']),flush=True)
