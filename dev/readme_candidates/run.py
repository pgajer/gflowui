from pathlib import Path
import sys,csv,time,subprocess,shutil
import numpy as np
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256,identity
from catalog import parse_metadata
from graphs import import_record
from worker import run
import requests
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
folder=root/'readme_candidates';folder.mkdir(exist_ok=True)
selected=['HB/494_bus','HB/bcsstk06','HB/dwt_492','HB/lshp_406','HB/west0479','Sandia/oscil_dcop_01']
url='https://sparse.tamu.edu/files/ssstats.csv'
p=folder/'ssstats.csv'
if not p.exists():p.write_bytes(requests.get(url,timeout=60).content)
rows=list(csv.reader(p.open()))[2:]
eligible=[x for x in rows if 400<=int(x[2])<=500 and x[2]==x[3]]
atomic_json(folder/'selection.json',dict(metadata_url=url,metadata_sha256=sha256(p),all_square_400_500=eligible,selected=selected,rationale='Purposive category diversity; exclude known duplicate bcsstk07 and repeated circuit sequence members. Connectivity verified after conversion.'))
def embed(method,a,ids,d,f,seed,dest,initial):
 from scipy.sparse import triu
 u=triu(a,k=1).tocoo();edge=dest/'edges.csv'
 np.savetxt(edge,np.column_stack((u.row,u.col)),delimiter=',',fmt='%d',header='source,target',comments='')
 req=dest/'fit.json';out=dest/'fit_coords.csv'
 atomic_json(req,dict(n=len(ids),seed=seed,edge_file=str(edge)))
 with (dest/'R.log').open('w') as log:subprocess.run(['Rscript',str(Path(__file__).with_name('fit.R')),str(req),str(out)],stdout=log,stderr=subprocess.STDOUT,check=True)
 return np.loadtxt(out,delimiter=',',skiprows=1),read_json(str(out)+'.json')
cohort=[];results=[]
for graph in selected:
 dest=root/'graphs'/graph.replace('/','__')
 if (dest/'graph.json').exists():info=read_json(dest/'graph.json')
 else:
  url='https://sparse.tamu.edu/'+graph;html=requests.get(url,timeout=60);html.raise_for_status()
  (folder/(graph.replace('/','__')+'.html')).write_text(html.text)
  rec=parse_metadata(html.text,url)
  # Collection's canonical Matrix Market distribution.
  rec['archive_url']='https://sparse.tamu.edu/MM/'+graph+'.tar.gz'
  atomic_json(folder/(graph.replace('/','__')+'.metadata.json'),rec)
  info=import_record(rec,root,max_vertices=500)
 if info['n_components']!=1:
  print('EXCLUDED disconnected',graph,flush=True);continue
 cohort.append(dict(graph_id=graph,status='completed',graph_sha256=info['graph_sha256']))
 for seed in [11,29,43]:
  req=dict(graph_id=graph,graph_dir=str(dest),method='metric_mds_sgd_readme',seed=seed,landmarks=64,attempt_label='README candidates: full SGD, uniform weights, random start, 1000 iterations',source_sha256=sha256(Path(__file__)),adapter_sha256=sha256(Path(__file__).with_name('fit.R')))
  key=identity(req);out=root/'runs'/key;out.mkdir(exist_ok=True);req['output']=str(out)
  if not (out/'manifest.json').exists():
   atomic_json(out/'request.json',req);start=time.monotonic();run(req,embedder=embed)
   elapsed=time.monotonic()-start
   artifacts={str(p.relative_to(out)):sha256(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json'}
   atomic_json(out/'manifest.json',dict(run_key=key,request=req,status='completed',elapsed_seconds=elapsed,peak_rss_bytes=None,artifacts=artifacts))
  m=read_json(out/'manifest.json')
  results.append(dict(graph_id=graph,method=req['method'],seed=seed,status='completed',run_dir=str(out),elapsed_seconds=m['elapsed_seconds'],peak_rss_bytes=None))
  atomic_json(root/'readme_candidate_results.json',dict(runs=results))
  print(graph,seed,'completed',round(m['elapsed_seconds'],1),flush=True)
atomic_json(root/'readme_candidate_cohort.json',dict(records=cohort))
old=read_json(root/'combined_cohort.json')['records'];byid={r['graph_id']:r for r in old+cohort}
atomic_json(root/'readme_combined_cohort.json',dict(records=list(byid.values())))
