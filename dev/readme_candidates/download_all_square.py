"""Acquire the frozen 400–500 square cohort; no embedding or viewer mutation."""
from pathlib import Path
import sys,csv,time,json
from concurrent.futures import ThreadPoolExecutor,as_completed
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256
from catalog import bounded_download
from graphs import matrix_from_archive,convert
from scipy.sparse import save_npz
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
out=root/'square_400_500';out.mkdir(exist_ok=True)
metadata=root/'readme_candidates/ssstats.csv'
rows=[r for r in list(csv.reader(metadata.open()))[2:] if r[2]==r[3] and 400<=int(r[2])<=500]
assert len(rows)==108
atomic_json(out/'selection.json',dict(metadata_file=str(metadata),metadata_sha256=sha256(metadata),rows=rows,conversion='Sum duplicate coefficients, remove numerical zeros and diagonal, symmetrize support; retain all vertices; unit edge lengths.'))
def one(r):
 gid='/'.join(r[:2]);token=gid.replace('/','__');dest=out/'graphs'/token;dest.mkdir(parents=True,exist_ok=True)
 if (dest/'graph.json').exists():return read_json(dest/'graph.json')
 rec=dict(graph_id=gid,rows=int(r[2]),columns=int(r[3]),nonzeros=int(r[4]),archive_url='https://sparse.tamu.edu/MM/'+gid+'.tar.gz')
 archive=root/'archives'/(token+'.tar.gz')
 for attempt in range(3):
  try:
   if not archive.exists():bounded_download(rec['archive_url'],archive)
   matrix=matrix_from_archive(archive,r[1],rec)
   a,info=convert(matrix,gid,rec['nonzeros'],max_vertices=500)
   save_npz(dest/'adjacency.npz',a)
   info.update(source=rec,archive_file=str(archive),archive_sha256=sha256(archive),archive_bytes=archive.stat().st_size,adjacency_sha256=sha256(dest/'adjacency.npz'))
   atomic_json(dest/'graph.json',info)
   return info
  except Exception as e:
   if attempt==2:return dict(graph_id=gid,error=str(e))
   time.sleep(2)
results=[]
with ThreadPoolExecutor(max_workers=4) as pool:
 for f in as_completed([pool.submit(one,r) for r in rows]):
  item=f.result();results.append(item)
  atomic_json(out/'progress.json',dict(target=108,finished=len(results),records=results))
  print(len(results),item['graph_id'],item.get('error',str(item.get('n_components'))+' components'),flush=True)
# Exact adjacency-pattern duplicates, preserving source vertex order. Never delete archives.
byhash={};records=[]
for info in sorted(results,key=lambda x:x['graph_id']):
 rec={k:info[k] for k in ['graph_id','n_vertices','n_edges','n_components','component_sizes','graph_sha256','archive_sha256','error'] if k in info}
 if 'error' in info:rec['status']='failed'
 elif info['n_components']!=1:rec['status']='disconnected'
 elif info['graph_sha256'] in byhash:rec.update(status='duplicate',representative=byhash[info['graph_sha256']])
 else:rec['status']='eligible';byhash[info['graph_sha256']]=info['graph_id']
 records.append(rec)
atomic_json(out/'admission.json',dict(records=records,deduplication='Exact unweighted undirected adjacency with original vertex order; not graph-isomorphism equivalence.',embeddings_generated=False))
with (out/'admission.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=['graph_id','status','n_vertices','n_edges','n_components','representative','error'],extrasaction='ignore');w.writeheader();w.writerows(records)
from collections import Counter
counts=Counter(r['status'] for r in records);print(dict(counts),flush=True)
text='# SuiteSparse square graphs with 400–500 vertices\n\nDownloaded from the frozen SuiteSparse metadata cohort on 29_sept_2026. Conversion uses unweighted undirected numerical support, drops loops, and preserves all vertices. No embeddings generated.\n\n'+ '\n'.join('- '+k+': '+str(v) for k,v in counts.items())+'\n\nDuplicate means identical adjacency in the source vertex order; isomorphic graphs with reordered vertices are not necessarily removed. Original archives and duplicate graph records are retained; only the embedding candidate list excludes duplicates and disconnected graphs.\n\n| Matrix | Status | Vertices | Edges | Components | Representative |\n|---|---|---:|---:|---:|---|\n'
for r in records:text+='| '+' | '.join(str(r.get(k,'')) for k in ['graph_id','status','n_vertices','n_edges','n_components','representative'])+' |\n'
(out/'README.md').write_text(text)
