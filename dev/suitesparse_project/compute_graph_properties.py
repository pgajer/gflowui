"""Exact structural measures for the saved unit-length gallery graphs.

Run with the SuiteSparse pipeline Python environment and DATA_ROOT. Publishes
hash-checked properties, preserving graph/run entries and the active selection.
Infinity is JSON null only in detour_ratio; bridge provides an explicit flag.
"""
import os
for key in ('OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS', 'MKL_NUM_THREADS'):
    os.environ[key] = '1'
import argparse
from collections import deque
from concurrent.futures import ProcessPoolExecutor
import hashlib
import json
from pathlib import Path
import time
import networkx as nx
import numpy as np
import scipy
from scipy.linalg import cho_factor, cho_solve

VERSION = 'unit-graph-properties-v1'


def replacement_distance(adj, u, v):
    seen = {u}; queue = deque([(u, 0)])
    while queue:
        a, depth = queue.popleft()
        for b in adj[a]:
            if (a == u and b == v) or (a == v and b == u):
                continue
            if b == v:
                return depth + 1
            if b not in seen:
                seen.add(b); queue.append((b, depth + 1))
    return None


def compute(n, edges):
    g = nx.Graph(); g.add_nodes_from(range(n)); g.add_edges_from(edges)
    if g.number_of_edges() != len(edges) or nx.number_of_selfloops(g):
        raise ValueError('Expected a simple graph')
    if any(not 0 <= v < n for e in edges for v in e):
        raise ValueError('Invalid vertex index')
    adj = [set(g[v]) for v in range(n)]
    components = sorted((sorted(c) for c in nx.connected_components(g)), key=lambda c:c[0])
    nc = {}; component_ids = [0]*n
    for cid, c in enumerate(components):
        for v in c: nc[v] = len(c); component_ids[v] = cid
    bridges = {tuple(sorted(e)) for e in nx.bridges(g)}
    articulation = set(nx.articulation_points(g))
    core = nx.core_number(g)
    vb = nx.betweenness_centrality(g, normalized=False, weight=None, endpoints=False)
    eb = nx.edge_betweenness_centrality(g, normalized=False, weight=None)
    triangles = [len(adj[u] & adj[v]) for u,v in edges]
    tv = [0]*n
    for (u,v), t in zip(edges,triangles): tv[u] += t; tv[v] += t
    # Solve a grounded Laplacian per component; no approximation or sampling.
    resistance = {}
    for c in components:
        k = len(c)
        if k < 2: continue
        h = g.subgraph(c)
        lap = nx.laplacian_matrix(h, nodelist=c).toarray().astype(float)
        inv = np.zeros((k,k))
        inv[:-1,:-1] = cho_solve(cho_factor(lap[:-1,:-1]), np.eye(k-1))
        loc = {v:i for i,v in enumerate(c)}
        for u,v in h.edges:
            i,j = loc[u],loc[v]
            resistance[tuple(sorted((u,v)))] = float(inv[i,i]+inv[j,j]-2*inv[i,j])
    ep = dict(detour_ratio=[], betweenness=[], betweenness_normalized=[], triangle_count=triangles,
              effective_resistance=[], bridge=[], bridge_smaller_side=[], bridge_larger_side=[], bridge_pairs=[])
    for u,v in edges:
        key = tuple(sorted((u,v))); bridge = key in bridges
        b = float(eb.get((u,v), eb.get((v,u))))
        r = resistance[key]
        assert -1e-8 < r <= 1+1e-8
        if bridge:
            reached = {u}; todo = [u]
            for a in todo:
                for z in adj[a]:
                    if (a==u and z==v) or (a==v and z==u): continue
                    if z not in reached: reached.add(z); todo.append(z)
            small = min(len(reached), nc[u]-len(reached)); large = nc[u]-small
            assert abs(b-small*large) < 1e-7
            detour = None
        else:
            small = large = None
            detour = 2 if adj[u] & adj[v] else replacement_distance(adj,u,v)
            assert detour is not None
        ep['detour_ratio'].append(detour)
        ep['betweenness'].append(b)
        ep['betweenness_normalized'].append(b/(nc[u]*(nc[u]-1)/2))
        ep['effective_resistance'].append(1.0 if bridge else min(1.0,max(0.0,r)))
        ep['bridge'].append(int(bridge))
        ep['bridge_smaller_side'].append(small); ep['bridge_larger_side'].append(large)
        ep['bridge_pairs'].append(small*large if bridge else 0)
    vp = dict(degree=[len(a) for a in adj], betweenness=[float(vb[v]) for v in range(n)],
        betweenness_normalized=[float(vb[v])/((nc[v]-1)*(nc[v]-2)/2) if nc[v]>2 else 0.0 for v in range(n)],
        core_number=[core[v] for v in range(n)],
        clustering=[tv[v]/(len(adj[v])*(len(adj[v])-1)) if len(adj[v])>1 else 0.0 for v in range(n)],
        articulation=[int(v in articulation) for v in range(n)])
    # Foster's identity is an independent aggregate check of the linear solves.
    assert abs(sum(ep['effective_resistance'])-(n-len(components))) < 1e-6*max(1,n)
    assert all(0 <= x <= 1+1e-8 for x in ep['betweenness_normalized']+vp['betweenness_normalized'])
    return dict(vertex=vp, edge=ep, component_ids=component_ids,
                component_sizes=[nc[v] for v in range(n)])


def encoded(data):
    return (json.dumps(data,ensure_ascii=False,allow_nan=False,separators=(',',':'))+'\n').encode()


def save_asset(root, content):
    sha = hashlib.sha256(content).hexdigest()
    path = f'graph_properties/{sha}.json'
    (root/'graph_properties').mkdir(exist_ok=True)
    (root/path).write_bytes(content)
    return dict(path=path,sha256=sha)


def worker(task):
    root, spec = task; started = time.monotonic()
    raw = (root/spec['file']['path']).read_bytes()
    assert hashlib.sha256(raw).hexdigest() == spec['file']['sha256']
    graph = json.loads(raw)
    assert graph['graph_sha256'] == spec['graph_sha256'] and graph['graph_id'] == spec['id']
    assert graph['edge_length'] == 1
    props = compute(graph['n_vertices'],graph['edges'])
    props.update(schema_version=1,algorithm=VERSION,graph_id=spec['id'],graph_sha256=spec['graph_sha256'],
                 graph_file_sha256=spec['file']['sha256'], vertex_ids=graph['vertex_ids'],edges=graph['edges'])
    asset = save_asset(root,encoded(props))
    return spec['id'],asset,round(time.monotonic()-started,3)


def main(root, workers):
    original = (root/'viewer_manifest.json').read_bytes(); manifest = json.loads(original)
    records = {}; timings = {}
    with ProcessPoolExecutor(max_workers=workers) as pool:
        for gid,asset,seconds in pool.map(worker,[(root,s) for s in manifest['graphs']]):
            records[gid] = asset; timings[gid] = seconds
            print(f'{len(records)}/{len(manifest["graphs"])} {gid}: {seconds}s',flush=True)
    index = dict(schema_version=1,algorithm=VERSION,graphs=records,
                 software=dict(networkx=nx.__version__,scipy=scipy.__version__,numpy=np.__version__),
                 conventions=dict(edge_lengths='unit',betweenness='exact unordered reachable pairs',
                   normalized='within connected component; vertex endpoints excluded',
                   detour_null='infinity for bridges',bridge_side_null='not applicable for nonbridges'),timings=timings)
    manifest.setdefault('artifacts',{})['graph_properties.json'] = save_asset(root,encoded(index))
    # List every property asset so comparison exports remain self-contained.
    for gid,spec in records.items(): manifest['artifacts']['properties:'+gid] = spec
    backup = root/'viewer_manifests'/('before_properties_'+hashlib.sha256(original).hexdigest()+'.json')
    backup.parent.mkdir(exist_ok=True)
    if not backup.exists(): backup.write_bytes(original)
    assert (root/'viewer_manifest.json').read_bytes()==original, 'Manifest changed during computation; rerun publication'
    stage = root/'viewer_manifest.properties.tmp'; stage.write_bytes(encoded(manifest)); stage.replace(root/'viewer_manifest.json')
    print('Published',len(records),'graph property assets.',flush=True)


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('data_root',type=Path)
    p.add_argument('--workers',type=int,default=4)
    a=p.parse_args(); main(a.data_root.resolve(),a.workers)
