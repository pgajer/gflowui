"""Load exact upstream functions with narrowly scoped dependency substitutes.

No graph-tool installation is required: adjacency and neighbor iteration on
explicit tiny fixtures are provided here. AST extraction retains the original
function bodies, choosing the last definition as normal module execution does.
It avoids importing unrelated graph-tool/sklearn/scipy features. This is not an
end-to-end graph-tool or upstream command-line reproduction.
"""
import ast
from pathlib import Path
from types import SimpleNamespace
import numpy as np

ROOT = Path(__file__).resolve().parents[1]


class TinyGraph:
    def __init__(self, adjacency):
        self.adjacency = np.asarray(adjacency, dtype=np.int64)

    def num_vertices(self):
        return len(self.adjacency)

    def iter_vertices(self):
        return range(len(self.adjacency))

    def iter_all_neighbors(self, v):
        return np.flatnonzero(self.adjacency[v])

    def edge(self, i, j):
        return bool(self.adjacency[i, j])


def pairwise_distances(X):
    X = np.asarray(X)
    return np.sqrt(np.sum((X[:, None] - X[None, :])**2, axis=2))


def load_functions():
    gt = SimpleNamespace(Graph=TinyGraph, adjacency=lambda g:
                         SimpleNamespace(toarray=lambda: g.adjacency.copy()))
    namespace = {'np': np, 'gt': gt, 'pairwise_distances': pairwise_distances}
    requested = {
        'L2G.py': ['norm_counts', 'find_neighbors_small', 'find_neighbors_large', 'find_neighbors'],
        'metrics.py': ['get_cost', 'get_stress', 'get_neighborhood', 'get_cluster_distances'],
    }
    for filename, names in requested.items():
        path = ROOT / 'vendor/L2G/modules' / filename
        tree = ast.parse(path.read_text(), filename=str(path))
        selected = {node.name: node for node in tree.body
                    if isinstance(node, ast.FunctionDef) and node.name in names}
        for name in names:
            exec(compile(ast.Module(body=[selected[name]], type_ignores=[]), str(path), 'exec'), namespace)
    return SimpleNamespace(**{name: namespace[name] for names in requested.values() for name in names})


def load_native():
    import sys
    sys.path.insert(0, str(ROOT / '.cache/oracle'))
    try:
        import lgs_upstream_oracle
    except ImportError as exc:
        raise RuntimeError('Run scripts/build_oracle.py with this Python first') from exc
    return lgs_upstream_oracle
