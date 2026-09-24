"""Small exact diagnostics matching the shared 2026-09-24 project definitions.

No community partition, fixed-path target, or alternative metric is substituted.
"""
import math
import numpy as np

METRIC_VERSION = 'suitesparse-design-2026-09-24-basic-v1'


def evaluate_quality(ids, adjacency, graph_distances, coordinates):
    x = np.asarray(coordinates,dtype=float)
    n = len(ids)
    if x.ndim != 2 or len(x) != n or not np.isfinite(x).all():
        raise ValueError('finite coordinates with declared vertex count required')
    radius = np.linalg.norm(x[:,None]-x[None,:],axis=2)
    i,j = np.triu_indices(n,1)
    r,d = radius[i,j],np.asarray(graph_distances)[i,j]
    if not len(d) or not np.all(np.isfinite(d)&(d>0)):
        raise ValueError('positive finite connected pair targets required')
    result = {'formula_version':METRIC_VERSION,'pair_population':'all unordered within-component pairs',
              'pair_count':len(d),'zero_embedded_pair_count':int(np.sum(r==0))}
    if not np.isfinite(radius).all() or not np.isfinite(np.sum(r*r)):
        return {**result,'status':'unavailable','reason':'nonfinite_distance_arithmetic'}
    if np.sum(r*r)==0:
        return {**result,'status':'unavailable','reason':'collapsed_layout'}
    scale = float(np.sum(r*d)/np.sum(r*r))
    ratios = r/d
    relative_scale = float(np.sum(ratios)/np.sum(ratios*ratios))
    result.update(status='available',
                  euclidean_distance_error=float(np.sqrt(np.sum((scale*r-d)**2)/np.sum(d*d))),
                  euclidean_fitted_scale=scale,
                  relative_distance_stress=float(np.mean((relative_scale*ratios-1)**2)),
                  relative_fitted_scale=relative_scale)
    edge_mask = np.asarray(adjacency)[i,j].astype(bool)
    result['edge_length_error_identity'] = float(np.sqrt(np.mean((r[edge_mask]-1)**2)))
    for hops in (1,2):
        errors=[]
        for v in range(n):
            original={u for u in range(n) if u!=v and graph_distances[v,u]<=hops}
            chosen=sorted((u for u in range(n) if u!=v),key=lambda u:(radius[v,u],ids[u]))[:len(original)]
            embedded=set(chosen)
            errors.append(1-len(original&embedded)/len(original|embedded))
        result[f'neighborhood_error_hops_{hops}']=float(np.mean(errors))
    return result
