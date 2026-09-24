"""Nonplanar adapter contract probe, run independently for each method/seed."""
from pathlib import Path
import sys
import argparse
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import numpy as np
import networkx as nx
from scipy.sparse import csr_matrix
from graphs import prepare
from common import atomic_json,sha256
from adapters import embed

def probe(method,seed,output):
    output=Path(output);output.mkdir(parents=True,exist_ok=True)
    graph=nx.grid_graph(dim=(5,5,5))
    a=csr_matrix(nx.to_scipy_sparse_array(graph,dtype=float))
    ids=[str(node) for node in graph]
    d,p,x,prep=prepare(a,ids)
    before=x.copy()
    z,details=embed(method,a,ids,d,x,seed,output)
    if not np.array_equal(x,before): raise AssertionError('adapter mutated shared features')
    singular=np.linalg.svd(z-z.mean(axis=0),compute_uv=False)
    if singular[-1]<=1e-6*singular[0]: raise AssertionError('output is not genuinely three-dimensional')
    np.savetxt(output/'coords.csv',z,delimiter=',')
    atomic_json(output/'probe.json',dict(method=method,seed=seed,n=125,input=prep,
        coordinate_sha256=sha256(output/'coords.csv'),singular_values=singular.tolist(),details=details))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('method');p.add_argument('seed',type=int);p.add_argument('output')
    a=p.parse_args();probe(a.method,a.seed,a.output)
