"""Extract >=95% components, retain vertex maps, and deduplicate exact patterns."""
from pathlib import Path
import sys,collections
import numpy as np
from scipy.sparse import load_npz,save_npz
from scipy.sparse.csgraph import connected_components
base=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(base/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256,identity
from graphs import convert
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison');out=root/'largest_components_95';out.mkdir(exist_ok=True)
def pattern(g):return identity(dict(n=g['n_vertices'],edges=g['edges']))
known={}
for p in sorted((root/'graphs').glob('*/graph.json')):
 g=read_json(p)
 if g['n_components']==1 and '__LCC_' not in g['graph_id']:known.setdefault(pattern(g),g['graph_id'])
records=[];candidates=[]
for scope in ['square_400_500','square_500_600','square_600_700','square_700_800']:
 for row in read_json(root/scope/'admission.json')['records']:
  if row['status']!='disconnected':continue
  n=row['n_vertices'];largest=max(row['component_sizes'])
  if 100*largest<95*n:continue
  source=root/scope/'graphs'/row['graph_id'].replace('/','__');old=read_json(source/'graph.json')
  assert sha256(source/'adjacency.npz')==old['adjacency_sha256']
  a=load_npz(source/'adjacency.npz');nc,labels=connected_components(a,directed=False)
  sizes=np.bincount(labels);vertices=np.flatnonzero(labels==sizes.argmax());assert len(vertices)==largest and a.shape==(n,n)
  sub=a[vertices][:,vertices].tocoo();gid=f"{row['graph_id']}__LCC_{largest}_of_{n}"
  adj,g=convert(sub,gid,None,max_vertices=800)
  assert g['n_components']==1 and g['n_vertices']==largest
  mapping=[dict(vertex=i+1,original_vertex=int(v)+1,original_vertex_id=old['vertex_ids'][v]) for i,v in enumerate(vertices)]
  dest=out/'graphs'/gid.replace('/','__');dest.mkdir(parents=True,exist_ok=True)
  save_npz(dest/'adjacency.npz',adj);atomic_json(dest/'vertex_mapping.json',mapping)
  g.update(source=old['source'],archive_file=old['archive_file'],archive_sha256=old['archive_sha256'],adjacency_sha256=sha256(dest/'adjacency.npz'),
    component_extraction=dict(original_graph_id=old['graph_id'],original_graph_file=str(source/'graph.json'),original_graph_sha256=old['graph_sha256'],original_vertices=n,retained_vertices=largest,retained_fraction=largest/n,original_vertex_indices_one_based=(vertices+1).tolist(),vertex_mapping_file=str(dest/'vertex_mapping.json'),vertex_mapping_sha256=sha256(dest/'vertex_mapping.json')))
  atomic_json(dest/'graph.json',g)
  key=pattern(g);representative=known.get(key)
  rec=dict(graph_id=gid,original_graph_id=old['graph_id'],n_vertices=largest,original_vertices=n,retained_percent=100*largest/n,n_edges=g['n_edges'],graph_sha256=g['graph_sha256'],status='duplicate' if representative else 'eligible',representative=representative)
  if not representative:known[key]=gid;candidates.append(rec)
  records.append(rec)
atomic_json(out/'admission.json',dict(records=records,threshold=0.95,deduplication='Exact adjacency in retained source vertex order after consecutive reindexing; not graph isomorphism. Compared against all existing connected graph assets, including previously unselected graphs.'))
atomic_json(out/'embedding_candidates.json',dict(records=candidates))
s='# Recovered largest components\n\nExtracted components retain at least 95% of the original vertices. Original graph assets are unchanged; each extracted graph has a one-based vertex mapping to its source. Deduplication compares exact unweighted adjacency after consecutive reindexing, against the existing collection and earlier extracted components. It is not an isomorphism test. Duplicates of previously unselected graphs are recorded but not reintroduced.\n\n'
s+=f'{len(records)} qualifying source graphs; {len(candidates)} distinct new components; {len(records)-len(candidates)} duplicate patterns.\n\n'
s+='| Source | Retained vertices | Original vertices | Percent | Status | Representative |\n|---|---:|---:|---:|---|---|\n'
for r in records:s+=f"| {r['original_graph_id']} | {r['n_vertices']} | {r['original_vertices']} | {r['retained_percent']:.2f} | {r['status']} | {r['representative'] or r['graph_id']} |\n"
s+=f'\nEmbedding results: [Markdown]({out}/embeddings.md) · [HTML]({out}/embeddings.html). Viewer labels show retained and original vertex counts. Stable IDs use LCC for largest connected component. Each graph folder contains vertex_mapping.json with one-based source vertex indices and source labels.\n'
(out/'README.md').write_text(s)
print(len(records),'qualifying;',len(candidates),'new;',dict(collections.Counter(r['status'] for r in records)),flush=True)
