"""Numerical-support graph conversion and deterministic component preparations."""
from pathlib import Path, PurePosixPath
import tarfile
import tempfile
import numpy as np
import scipy
from scipy.io import mmread, mminfo
from scipy.sparse import coo_matrix, csr_matrix, save_npz
from scipy.sparse.csgraph import connected_components, shortest_path
import networkx as nx
from catalog import admission, bounded_download
from common import atomic_json, identity, sha256


def matrix_from_archive(path, name, metadata, extracted_limit=1024**3):
    """Read only the named matrix, never extract paths supplied by an archive."""
    matrix = None
    total = 0
    with tarfile.open(path, 'r|gz') as archive:
        for member in archive:
            parts = PurePosixPath(member.name)
            if parts.is_absolute() or '..' in parts.parts or '\\' in member.name:
                raise ValueError('unsafe archive path')
            if not (member.isfile() or member.isdir()):
                raise ValueError('archive links or special members are forbidden')
            total += member.size
            if total > extracted_limit:
                raise ValueError('expanded archive size exceeds cap')
            if member.isfile() and parts.name == name + '.mtx':
                if matrix is not None:
                    raise ValueError('duplicate primary matrix')
                with tempfile.TemporaryFile() as tmp:
                    stream = archive.extractfile(member)
                    while chunk := stream.read(65536):
                        tmp.write(chunk)
                    tmp.seek(0)
                    rows, cols, entries, fmt, field, symmetry = mminfo(tmp)
                    if (rows, cols) != (metadata['rows'], metadata['columns']):
                        raise ValueError('matrix dimensions differ from catalog')
                    if fmt != 'coordinate' or entries > 200000:
                        raise ValueError('unsupported format or excessive stored entries')
                    tmp.seek(0)
                    matrix = mmread(tmp, spmatrix=True).tocoo()
    if matrix is None:
        raise ValueError('primary Matrix Market member missing')
    return matrix


def convert(matrix, graph_id, expected_nnz=None):
    matrix = coo_matrix(matrix)
    if not np.isfinite(matrix.data).all():
        raise ValueError('nonfinite matrix coefficient')
    rows, cols = matrix.shape
    admitted, reason = admission(rows, cols, 0)
    if not admitted:
        raise ValueError(reason)
    stored = int(matrix.nnz)
    positions = matrix.row.astype(np.int64) * cols + matrix.col
    unique, counts = np.unique(positions, return_counts=True)
    matrix.sum_duplicates()
    merged_positions = matrix.row.astype(np.int64) * cols + matrix.col
    duplicate_positions = set(unique[counts > 1].tolist())
    canceled = sum(int(pos) in duplicate_positions and val == 0
                   for pos, val in zip(merged_positions, matrix.data))
    zeros = int(np.sum(matrix.data == 0))
    matrix.eliminate_zeros()
    if expected_nnz is not None and matrix.nnz != expected_nnz:
        raise ValueError('numerical nonzero count differs from catalog')
    loops = int(np.sum(matrix.row == matrix.col)) if rows == cols else 0
    if rows == cols:
        ids = [f'v:{i+1}' for i in range(rows)]
        mask = matrix.row != matrix.col
        u, v = matrix.row[mask], matrix.col[mask]
    else:
        ids = [f'row:{i+1}' for i in range(rows)] + [f'col:{i+1}' for i in range(cols)]
        u, v = matrix.row, matrix.col + rows
    n = len(ids)
    pairs = sorted(set((int(min(a,b)), int(max(a,b))) for a,b in zip(u,v)))
    if len(pairs) > 100000:
        raise ValueError('converted edge count exceeds cap')
    edges = np.array(pairs, dtype=np.int64).reshape(-1, 2)
    adjacency = csr_matrix((np.ones(2*len(edges)),
                            (np.r_[edges[:,0], edges[:,1]], np.r_[edges[:,1], edges[:,0]])),
                           shape=(n,n))
    count, labels = connected_components(adjacency, directed=False)
    graph = nx.from_scipy_sparse_array(adjacency)
    degrees = np.asarray(adjacency.sum(axis=1)).ravel()
    info = dict(schema_version=1, graph_id=graph_id, recipe='numerical_support_union_or_bipartite_v1',
                vertex_ids=ids, edges=edges.tolist(), edge_length=1., n_vertices=n,
                n_edges=len(edges), component_labels=labels.tolist(), n_components=int(count),
                stored_entries=stored, numerical_nonzeros=int(matrix.nnz), removed_loops=loops,
                merged_duplicate_entries=stored-len(unique), canceled_duplicate_positions=int(canceled),
                zero_positions_after_sum=zeros, n_isolates=int(np.sum(degrees==0)),
                degree_min=float(degrees.min()), degree_max=float(degrees.max()),
                degree_mean=float(degrees.mean()), degree_cv=float(degrees.std()/degrees.mean()) if degrees.mean() else None,
                mean_clustering=float(nx.average_clustering(graph)),
                component_sizes=np.bincount(labels).tolist())
    info['graph_sha256'] = identity({k: info[k] for k in ('recipe','vertex_ids','edges','edge_length')})
    return adjacency, info


def prepare(adjacency, vertex_ids, landmark_count=64):
    n = adjacency.shape[0]
    if len(vertex_ids) != n or len(set(vertex_ids)) != n:
        raise ValueError('vertex IDs do not match adjacency')
    distances, predecessors = shortest_path(adjacency, directed=False, unweighted=True,
                                            method='D', return_predecessors=True)
    if not np.isfinite(distances).all():
        raise ValueError('prepare requires one connected component')
    # Source ID ordering is the frozen tie policy, including landmark ties.
    ids_order = np.argsort(np.array(vertex_ids), kind='stable')
    selected = [int(ids_order[0])]
    near = distances[selected[0]].copy()
    while len(selected) < min(landmark_count, n):
        candidates = [i for i in ids_order if i not in selected]
        nxt = int(max(candidates, key=lambda i: near[i]))
        selected.append(nxt)
        near = np.minimum(near, distances[nxt])
    scale = float(distances.max()) or 1.
    features = distances[:, selected] / scale
    policy = dict(schema_version=1, path_policy='scipy_D_sorted_CSR_fixed_predecessors',
                  scipy_version=scipy.__version__, pair_policy='all_unordered_within_component',
                  vertex_ids=vertex_ids, landmark_ids=[vertex_ids[i] for i in selected],
                  landmark_scale=scale, landmark_ties='lexicographic_stable_vertex_id')
    for key, array in [('distances', distances), ('predecessors', predecessors), ('features', features)]:
        policy[key+'_sha256'] = __import__('hashlib').sha256(array.tobytes()).hexdigest()
    policy['input_sha256'] = identity(policy)
    return distances, predecessors, features, policy


def import_record(record, root):
    ok, reason = admission(record['rows'], record['columns'], record['nonzeros'])
    if not ok or not record.get('eligible'):
        raise ValueError(reason)
    token = record['graph_id'].replace('/', '__')
    root = Path(root)
    archive = root/'archives'/(token+'.tar.gz')
    if not record.get('archive_url'):
        raise ValueError('catalog has no archive URL')
    bounded_download(record['archive_url'], archive)
    matrix = matrix_from_archive(archive, record['graph_id'].split('/')[-1], record)
    adjacency, info = convert(matrix, record['graph_id'], record['nonzeros'])
    info.update(source=record, archive_sha256=sha256(archive), archive_file=str(archive),
                archive_bytes=archive.stat().st_size)
    dest = root/'graphs'/token
    dest.mkdir(parents=True, exist_ok=True)
    save_npz(dest/'adjacency.npz', adjacency)
    info['adjacency_sha256'] = sha256(dest/'adjacency.npz')
    atomic_json(dest/'graph.json', info)
    return info
