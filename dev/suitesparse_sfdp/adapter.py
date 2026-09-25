"""Graphviz SFDP in three dimensions; graph edges only, no distance fitting."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import numpy as np
from scipy.sparse import triu


def executable():
    path = os.environ.get('GFLOWUI_SFDP') or shutil.which('sfdp')
    if not path or not Path(path).is_file():
        raise RuntimeError('Graphviz sfdp not found; install Graphviz or set GFLOWUI_SFDP')
    return str(Path(path).resolve())


def parse_positions(document, n):
    objects = document.get('objects', [])
    by_id = {}
    for obj in objects:
        name = obj.get('name')
        if name not in {str(i) for i in range(n)} or name in by_id:
            raise ValueError('unexpected or duplicate SFDP vertex')
        try:
            xyz = [float(x) for x in obj['pos'].split(',')]
        except (KeyError, ValueError, AttributeError) as exc:
            raise ValueError('missing/invalid SFDP position') from exc
        if len(xyz) != 3 or not np.isfinite(xyz).all():
            raise ValueError('SFDP must return three finite coordinates per vertex')
        by_id[name] = xyz
    if len(by_id) != n:
        raise ValueError('missing SFDP vertices')
    # Graphviz pos is in points; 72 points = one native layout inch. This is a
    # fixed unit conversion, not a per-graph fitted or edge-normalizing scale.
    return np.array([by_id[str(i)] for i in range(n)], dtype=float) / 72.


def embed(method, adjacency, ids, d, features, seed, dest, initial=None):
    if method != 'sfdp' or initial is not None:
        raise ValueError('SFDP adapter accepts only fresh sfdp layouts')
    n = len(ids)
    if adjacency.shape != (n, n) or (adjacency != adjacency.T).nnz:
        raise ValueError('SFDP requires a symmetric adjacency matching vertex IDs')
    edges = triu(adjacency, k=1).tocoo()
    graph = ['strict graph {', 'node [shape=point,width=0,height=0,label=""];']
    graph.extend(f'{i};' for i in range(n))
    graph.extend(f'{i} -- {j};' for i, j in zip(edges.row, edges.col))
    graph.append('}')
    dest = Path(dest)
    (dest/'input.dot').write_text('\n'.join(graph)+'\n')
    # sfdp may be a symlink to dot; select the engine explicitly after resolving it.
    command = [executable(), '-Ksfdp', '-Gdim=3', '-Gdimen=3', '-Gstart='+str(int(seed)),
               '-GK=1', '-Goverlap=true', '-Gsmoothing=none', '-Gnormalize=false',
               '-Tjson', str(dest/'input.dot')]
    # Owned by the outer memory supervisor; deliberately no elapsed-time limit.
    with (dest/'graphviz.json').open('w') as out, (dest/'graphviz.stderr').open('w') as err:
        subprocess.run(command, stdout=out, stderr=err, check=True)
    coords = parse_positions(json.loads((dest/'graphviz.json').read_text()), n)
    centered = coords-coords.mean(axis=0)
    detail = dict(algorithm='Graphviz SFDP (Yifan Hu), native three-dimensional force-directed layout',
        command=command, dimension=3, rendering_dimension=3, K=1,
        overlap=True, smoothing='none', normalize=False,
        coordinate_units='Graphviz layout inches; output points divided by 72; no fitted rescaling',
        centered_rank=int(np.linalg.matrix_rank(centered)),
        coordinate_ranges=np.ptp(coords, axis=0).tolist(),
        termination='backend returned successfully; convergence not reported',
        stderr=(dest/'graphviz.stderr').read_text())
    return coords, detail
