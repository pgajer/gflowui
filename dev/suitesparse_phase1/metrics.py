"""Versioned graph embedding scores; independent of optimization backends."""
import numpy as np
from scipy.spatial.distance import pdist, squareform
from scipy.stats import spearmanr

METRICS_VERSION = 'suitesparse-pilot-v1'


def validate_coords(coords, vertex_ids, expected_ids):
    z = np.asarray(coords, dtype=float)
    if z.shape != (len(expected_ids), 3) or not np.isfinite(z).all():
        raise ValueError('coordinates must be finite n-by-3')
    if list(vertex_ids) != list(expected_ids) or len(set(vertex_ids)) != len(vertex_ids):
        raise ValueError('coordinate vertex IDs/order mismatch')
    return z


def fixed_path_lengths(coords, distances, predecessors):
    n = len(coords)
    result = np.zeros((n,n))
    for source in range(n):
        for target in np.argsort(distances[source], kind='stable'):
            if source == target:
                continue
            parent = int(predecessors[source,target])
            if parent < 0 or distances[source,parent] >= distances[source,target]:
                raise ValueError('invalid retained predecessor')
            result[source,target] = result[source,parent] + np.linalg.norm(coords[target]-coords[parent])
    return result


def ordered_neighbors(matrix, vertex_ids):
    # lexsort uses ID to break ties and always excludes self.
    ids = np.array(vertex_ids)
    return np.array([np.lexsort((ids, row))[np.lexsort((ids, row)) != i]
                     for i,row in enumerate(matrix)])


def rank_quality(d, r, vertex_ids, k):
    n = len(d)
    if not 0 < k < n/2:
        return None, None
    original, embedded = ordered_neighbors(d, vertex_ids), ordered_neighbors(r, vertex_ids)
    ranks_d, ranks_r = np.zeros((n,n), int), np.zeros((n,n), int)
    for i in range(n):
        ranks_d[i,original[i]] = np.arange(1,n)
        ranks_r[i,embedded[i]] = np.arange(1,n)
    intrusion = omission = 0
    for i in range(n):
        a, b = set(original[i,:k]), set(embedded[i,:k])
        intrusion += sum(ranks_d[i,j]-k for j in b-a)
        omission += sum(ranks_r[i,j]-k for j in a-b)
    factor = 2/(n*k*(2*n-3*k-1))
    return float(1-factor*intrusion), float(1-factor*omission)


def score_component(coords, distances, predecessors, edges, ids, ks=(5,10,20,50)):
    z = validate_coords(coords, ids, ids)
    n = len(z)
    if n < 2:
        return dict(status='unavailable', reason='no within-component pairs', n_pairs=0)
    rmat = squareform(pdist(z))
    mask = np.triu_indices(n,1)
    d, r = distances[mask], rmat[mask]
    if not np.isfinite(d).all() or np.any(d <= 0):
        raise ValueError('invalid pair targets')
    p = fixed_path_lengths(z, distances, predecessors)[mask]
    collapsed = not np.any(r > 0)
    s = float(np.dot(r,d)/np.dot(r,r)) if not collapsed else None
    relative_scale = float(np.sum(r/d)/np.sum((r/d)**2)) if not collapsed else None
    edge_r = np.linalg.norm(z[edges[:,0]]-z[edges[:,1]],axis=1)
    sums = dict(target_ss=float(np.dot(d,d)),
                chord_ss=float(np.sum((s*r-d)**2)) if s is not None else None,
                relative_ss=float(np.sum(((relative_scale*r-d)/d)**2)) if relative_scale is not None else None,
                path_ss=float(np.sum((p-d)**2)), edge_ss=float(np.sum((edge_r-1)**2)))
    result = dict(status='completed', metrics_version=METRICS_VERSION, n_pairs=len(d), n_edges=len(edges),
                  scale_mode='chord_and_relative_profiled_per_component;path_and_edge_identity',
                  chord_scale=s, relative_scale=relative_scale, sums=sums,
                  chord_error=float(np.sqrt(sums['chord_ss']/sums['target_ss'])) if s is not None else None,
                  relative_stress=sums['relative_ss']/len(d) if relative_scale is not None else None,
                  path_error=float(np.sqrt(sums['path_ss']/sums['target_ss'])),
                  edge_error=float(np.sqrt(sums['edge_ss']/len(edges))) if len(edges) else None,
                  distance_rank_correlation=float(spearmanr(d,r).statistic) if np.ptp(d)>0 and np.ptp(r)>0 else None,
                  unavailable_reasons={}, neighborhood={}, bands={})
    if collapsed:
        result['unavailable_reasons'].update(chord_error='all coordinates coincide; scale undefined',
                                             relative_stress='all coordinates coincide; scale undefined')
    if result['distance_rank_correlation'] is None:
        result['unavailable_reasons']['distance_rank_correlation']='constant distance ranks'
    embedded = ordered_neighbors(rmat, ids)
    for radius in (1,2):
        errs = []
        for i in range(n):
            a=set(np.flatnonzero((distances[i]>0)&(distances[i]<=radius)))
            b=set(embedded[i,:len(a)])
            if a:
                errs.append(1-len(a&b)/len(a|b))
        result['neighborhood'][f'hop{radius}_jaccard_error'] = float(np.mean(errs)) if errs else None
    for k in ks:
        t,c = rank_quality(distances,rmat,ids,k)
        result['neighborhood'][f'trustworthiness_{k}']=t
        result['neighborhood'][f'continuity_{k}']=c
        if t is None:
            result['unavailable_reasons'][f'rank_neighborhood_{k}']='requires 0 < k < component_size/2'
    nonedge=r[distances[mask]>1]
    median_edge=float(np.median(edge_r)) if len(edge_r) else 0
    result['nonedge_separation_ratio']=float(nonedge.min()/median_edge) if len(nonedge) and median_edge>0 else None
    result['edge_length_quantiles']=np.quantile(edge_r,[0,.25,.5,.75,1]).tolist() if len(edge_r) else []
    q1,q2=np.quantile(d,[1/3,2/3])
    for name,band in [('short',d<=q1),('mid',(d>q1)&(d<=q2)),('long',d>q2)]:
        result['bands'][name] = dict(n_pairs=int(band.sum()),
            path_error=float(np.sqrt(np.sum((p[band]-d[band])**2)/np.sum(d[band]**2))) if band.any() else None)
    return result


def aggregate(components):
    rows=[x for x in components if x.get('n_pairs',0)>0]
    if not rows:
        return dict(status='unavailable', reason='no eligible component pairs')
    total=sum(x['n_pairs'] for x in rows)
    denom=sum(x['sums']['target_ss'] for x in rows)
    result=dict(status='completed', n_pairs=total, n_edges=sum(x['n_edges'] for x in rows))
    for name,key in [('chord_error','chord_ss'),('path_error','path_ss')]:
        result[name]=float(np.sqrt(sum(x['sums'][key] for x in rows)/denom)) if all(x['sums'][key] is not None for x in rows) else None
    result['relative_stress']=sum(x['sums']['relative_ss'] for x in rows)/total if all(x['sums']['relative_ss'] is not None for x in rows) else None
    result['edge_error']=float(np.sqrt(sum(x['sums']['edge_ss'] for x in rows)/result['n_edges'])) if result['n_edges'] else None
    # Preserve component correlations rather than average incompatible ranks.
    result['distance_rank_correlation']=rows[0]['distance_rank_correlation'] if len(rows)==1 else None
    result['rank_correlation_note']='per-component only when disconnected'
    result['neighborhood']={}
    for key in rows[0]['neighborhood']:
        valid=[(x['neighborhood'][key],x['n_vertices']) for x in rows if x['neighborhood'][key] is not None]
        result['neighborhood'][key]=sum(v*n for v,n in valid)/sum(n for v,n in valid) if valid else None
    return result
