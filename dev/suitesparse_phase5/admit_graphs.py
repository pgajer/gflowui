"""Explicit two-graph expansion; original cohort and gallery snapshot stay frozen."""
import argparse
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json,atomic_json,sha256
from catalog import admission
from graphs import import_record

SELECTED=['HB/sstmodel','Bomhof/circuit_2']

def run(root):
    root=Path(root);catalog=read_json(root/'catalog/gallery.json');old=read_json(root/'cohort.json')
    rows=[]
    for r in catalog['records']:
        ok,reason=admission(r['rows'],r['columns'],r['nonzeros'],max_vertices=10000)
        rows.append(dict(r,eligible=ok,admission_reason=reason,selected=r['graph_id'] in SELECTED))
    atomic_json(root/'phase05_admission.json',dict(source_catalog_sha256=sha256(root/'catalog/gallery.json'),
        max_vertices=10000,max_edges=100000,archive_limit_bytes=100*1024**2,extracted_limit_bytes=1024**3,
        rationale='Two smallest additional eligible gallery graphs: structural engineering and circuit simulation; not a random or representative graph sample.',records=rows))
    new=[]
    for graph in SELECTED:
        row=next(r for r in rows if r['graph_id']==graph)
        existing=root/'graphs'/graph.replace('/','__')/'graph.json'
        if existing.exists():
            info=read_json(existing)
            if sha256(existing.with_name('adjacency.npz'))!=info['adjacency_sha256']:raise ValueError('changed existing graph')
        else:info=import_record(row,root,max_vertices=10000)
        new.append(dict(graph_id=graph,status='completed',graph_sha256=info['graph_sha256']))
        print(graph,info['n_vertices'],info['n_edges'],info['component_sizes'],flush=True)
    atomic_json(root/'phase05_cohort.json',dict(schema_version=1,records=new))
    atomic_json(root/'combined_cohort.json',dict(schema_version=1,original_cohort_sha256=sha256(root/'cohort.json'),records=old['records']+new))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');run(p.parse_args().root)
