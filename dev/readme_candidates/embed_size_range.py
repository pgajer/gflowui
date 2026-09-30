"""Fit admitted square graphs with the existing README comparison protocol."""
from pathlib import Path
import sys,subprocess,time,shutil
from concurrent.futures import ThreadPoolExecutor,as_completed
import numpy as np
base=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(base/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256,identity
from worker import run
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
lower,upper=map(int,sys.argv[1:3])
folder=root/f'square_{lower}_{upper}'
prefix=f'square_{lower}_{upper}'
def embed(method,a,ids,d,f,seed,dest,initial):
 from scipy.sparse import triu
 u=triu(a,k=1).tocoo();edge=dest/'edges.csv'
 np.savetxt(edge,np.column_stack((u.row,u.col)),delimiter=',',fmt='%d',header='source,target',comments='')
 req=dest/'fit.json';out=dest/'fit_coords.csv';atomic_json(req,dict(n=len(ids),seed=seed,edge_file=str(edge)))
 with (dest/'R.log').open('w') as log:subprocess.run(['Rscript',str(Path(__file__).with_name('fit.R')),str(req),str(out)],stdout=log,stderr=subprocess.STDOUT,check=True)
 return np.loadtxt(out,delimiter=',',skiprows=1),read_json(str(out)+'.json')
cohort=read_json(folder/'embedding_candidates.json')['records']
old=read_json(root/'readme_candidate_results.json')['runs']
reuse={(r['graph_id'],r['seed']):r for r in old if r['status']=='completed'}
for row in cohort:
 token=row['graph_id'].replace('/','__');src=folder/'graphs'/token;dst=root/'graphs'/token
 if dst.exists():assert read_json(dst/'graph.json')['graph_sha256']==row['graph_sha256']
 else:shutil.copytree(src,dst)
atomic_json(root/(prefix+'_cohort.json'),dict(records=[dict(graph_id=r['graph_id'],status='completed',graph_sha256=r['graph_sha256']) for r in cohort]))
def one(gid,seed):
 if (gid,seed) in reuse:return dict(reuse[(gid,seed)],reused=True)
 req=dict(graph_id=gid,graph_dir=str(root/'graphs'/gid.replace('/','__')),method='metric_mds_sgd_readme',seed=seed,landmarks=64,attempt_label='README candidates: full SGD, uniform weights, random start, 1000 iterations',source_sha256=sha256(Path(__file__)),adapter_sha256=sha256(Path(__file__).with_name('fit.R')))
 key=identity(req);out=root/'runs'/key;out.mkdir(exist_ok=True);req['output']=str(out)
 try:
  if not (out/'manifest.json').exists():
   atomic_json(out/'request.json',req);start=time.monotonic();run(req,embedder=embed)
   artifacts={str(p.relative_to(out)):sha256(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json'}
   atomic_json(out/'manifest.json',dict(run_key=key,request=req,status='completed',elapsed_seconds=time.monotonic()-start,peak_rss_bytes=None,artifacts=artifacts))
  m=read_json(out/'manifest.json')
  return dict(graph_id=gid,method=req['method'],seed=seed,status='completed',run_dir=str(out),elapsed_seconds=m['elapsed_seconds'],peak_rss_bytes=None)
 except Exception as e:return dict(graph_id=gid,method=req['method'],seed=seed,status='failed',run_dir=str(out),error=str(e))
results=[]
with ThreadPoolExecutor(max_workers=4) as pool:
 for f in as_completed([pool.submit(one,r['graph_id'],s) for r in cohort for s in [11,29,43]]):
  row=f.result();results.append(row);atomic_json(root/(prefix+'_results.json'),dict(runs=results))
  print(len(results),'of '+str(3*len(cohort)),row['graph_id'],row['seed'],row['status'],'reused' if row.get('reused') else '',flush=True)
assert len(results)==3*len(cohort) and all(r['status']=='completed' for r in results)
