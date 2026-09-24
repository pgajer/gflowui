"""Uniform-pair distance estimates; exact streamed neighborhoods and edges."""
import hashlib
from pathlib import Path
import sys
import numpy as np
from scipy.stats import spearmanr
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from metrics import validate_coords,aggregate as exact_aggregate

VERSION='suitesparse-uniform-pairs-v1'
LIMIT=20000
SEED=314159
BOOTSTRAPS=200
SUM_KEYS=('target_ss','chord_ss','relative_ss','path_ss')


def pairs_for(n,limit=LIMIT,seed=SEED):
    if limit<2: raise ValueError('at least two evaluated pairs required')
    total=n*(n-1)//2
    ranks=np.sort(np.random.default_rng(seed).choice(total,min(limit,total),replace=False))
    starts=np.arange(n,dtype=np.int64)*(2*n-np.arange(n,dtype=np.int64)-1)//2
    i=np.searchsorted(starts,ranks,side='right')-1
    return np.column_stack((i,i+1+ranks-starts[i])).astype(np.int64)


def sampled_paths(z,d,p,pairs):
    source=pairs[:,0];current=pairs[:,1].copy();length=np.zeros(len(pairs))
    for _ in range(len(z)):
        active=np.flatnonzero(current!=source)
        if not len(active): return length
        parent=p[source[active],current[active]]
        if np.any(parent<0) or np.any(d[source[active],parent]>=d[source[active],current[active]]):
            raise ValueError('invalid retained predecessor')
        length[active]+=np.linalg.norm(z[current[active]]-z[parent],axis=1)
        current[active]=parent
    raise ValueError('predecessor cycle')


def terms(d,r,p):
    s=float(np.dot(r,d)/np.dot(r,r)) if np.any(r>0) else None
    u=r/d;t=float(np.sum(u)/np.dot(u,u)) if np.any(u>0) else None
    return dict(target_ss=float(np.dot(d,d)),
        chord_ss=float(np.sum((s*r-d)**2)) if s is not None else None,
        relative_ss=float(np.sum((t*u-1)**2)) if t is not None else None,
        path_ss=float(np.sum((p-d)**2))),s,t


def streamed_neighborhood(z,d,ids,ks=(5,10,20,50)):
    """Exact lexical-ID ranks, one distance/rank row at a time."""
    n=len(z);ids=np.asarray(ids);valid=[k for k in ks if 0<k<n/2]
    intrusion={k:0 for k in valid};omission=intrusion.copy();hop={1:[],2:[]}
    nonedge=np.inf
    for i in range(n):
        r=np.linalg.norm(z-z[i],axis=1)
        a=np.lexsort((ids,d[i]));a=a[a!=i]
        b=np.lexsort((ids,r));b=b[b!=i]
        rd=np.zeros(n,int);rr=np.zeros(n,int)
        rd[a]=np.arange(1,n);rr[b]=np.arange(1,n)
        for k in valid:
            aa,bb=set(a[:k]),set(b[:k])
            intrusion[k]+=sum(rd[j]-k for j in bb-aa)
            omission[k]+=sum(rr[j]-k for j in aa-bb)
        for radius in hop:
            aa=set(np.flatnonzero((d[i]>0)&(d[i]<=radius)));bb=set(b[:len(aa)])
            if aa: hop[radius].append(1-len(aa&bb)/len(aa|bb))
        non=r[d[i]>1]
        if len(non):nonedge=min(nonedge,float(non.min()))
    out={f'hop{radius}_jaccard_error':float(np.mean(v)) if v else None for radius,v in hop.items()}
    for k in ks:
        factor=2/(n*k*(2*n-3*k-1)) if k in valid else None
        out[f'trustworthiness_{k}']=float(1-factor*intrusion[k]) if factor else None
        out[f'continuity_{k}']=float(1-factor*omission[k]) if factor else None
    return out,nonedge


def score_component(z,d,p,edges,ids,limit=LIMIT,seed=SEED,bootstraps=BOOTSTRAPS,neighborhood=True):
    z=validate_coords(z,ids,ids);n=len(z);total=n*(n-1)//2
    if not total:return dict(status='unavailable',reason='no within-component pairs',n_pairs=0)
    pairs=pairs_for(n,limit,seed);m=len(pairs)
    targets=d[pairs[:,0],pairs[:,1]]
    if not np.isfinite(targets).all() or np.any(targets<=0):raise ValueError('invalid pair targets')
    chords=np.linalg.norm(z[pairs[:,0]]-z[pairs[:,1]],axis=1)
    paths=sampled_paths(z,d,p,pairs)
    base,s,t=terms(targets,chords,paths);weight=total/m
    sums={k:v*weight if v is not None else None for k,v in base.items()}
    er=np.linalg.norm(z[edges[:,0]]-z[edges[:,1]],axis=1)
    sums['edge_ss']=float(np.sum((er-1)**2))
    neigh,nonedge=streamed_neighborhood(z,d,ids) if neighborhood else ({},np.inf)
    corr=float(spearmanr(targets,chords).statistic) if np.ptp(targets)>0 and np.ptp(chords)>0 else None
    result=dict(status='completed',metrics_version=VERSION,n_pairs=total,n_edges=len(edges),sums=sums,
        chord_scale=s,relative_scale=t,
        scale_mode='chord_and_relative_profiled_per_component_on_shared_pairs;path_and_edge_identity',
        chord_error=float(np.sqrt(sums['chord_ss']/sums['target_ss'])) if s is not None else None,
        relative_stress=sums['relative_ss']/total if t is not None else None,
        path_error=float(np.sqrt(sums['path_ss']/sums['target_ss'])),
        edge_error=float(np.sqrt(sums['edge_ss']/len(edges))) if len(edges) else None,
        distance_rank_correlation=corr,neighborhood=neigh,
        edge_length_quantiles=np.quantile(er,[0,.25,.5,.75,1]).tolist() if len(er) else [],
        nonedge_separation_ratio=float(nonedge/np.median(er)) if np.isfinite(nonedge) and len(er) and np.median(er)>0 else None,
        bands={},unavailable_reasons={'bands':'distance-stratified diagnostics not computed in this evaluator'},
        evaluation=dict(mode='exact' if m==total else 'uniform_pair_sample',pair_count=m,population_pairs=total,
            seed=seed,pairs_sha256=hashlib.sha256(pairs.astype('<i8').tobytes()).hexdigest(),
            pairs=pairs.tolist(),edge_evaluation='exact',neighborhood_evaluation='exact_streamed' if neighborhood else 'not_computed',
            component_population_weight=weight),bootstrap_sums=[],bootstrap_correlations=[])
    if s is None:result['unavailable_reasons'].update(chord_error='sampled chord scale undefined',relative_stress='sampled relative scale undefined')
    if corr is None:result['unavailable_reasons']['distance_rank_correlation']='constant sampled distance ranks'
    # Resample pairs jointly and refit both scales; approximate finite-population correction.
    rng=np.random.default_rng(seed+1000003)
    fpc=np.sqrt((1-m/total)*m/(m-1)) if m<total else 0.
    for _ in range(bootstraps if m<total else 0):
        ix=rng.integers(m,size=m);v,_,_=terms(targets[ix],chords[ix],paths[ix])
        adjusted={k:max(0.,sums[k]+fpc*(v[k]*weight-sums[k])) if v[k] is not None and sums[k] is not None else None for k in SUM_KEYS}
        result['bootstrap_sums'].append(adjusted)
        c=float(spearmanr(targets[ix],chords[ix]).statistic) if np.ptp(targets[ix])>0 and np.ptp(chords[ix])>0 else None
        result['bootstrap_correlations'].append(float(np.clip(corr+fpc*(c-corr),-1,1)) if corr is not None and c is not None else None)
    result['evaluation'].update(bootstrap_replicates=bootstraps if m<total else 0,bootstrap_seed=seed+1000003,
        uncertainty='approximate paired percentile bootstrap; scales refitted; component-sum deviations corrected by sqrt((1-m/N)*m/(m-1)); conditional on fixed coordinates, not optimizer variability')
    return result


def aggregate(components):
    out=exact_aggregate(components);rows=[r for r in components if r.get('n_pairs',0)]
    if not rows:return out
    sampled=[r for r in rows if r['evaluation']['mode']!='exact']
    out['evaluation']=dict(mode='uniform_pair_sample' if sampled else 'exact',
        pair_count=sum(r['evaluation']['pair_count'] for r in rows),population_pairs=out['n_pairs'],
        version=VERSION,edges='exact',neighborhoods='exact_streamed',
        uncertainty='approximate 95% conditional pair-sampling intervals; not optimizer-seed ranges')
    out['intervals']={}
    if not sampled:return out
    counts={len(r['bootstrap_sums']) for r in sampled}
    if len(counts)!=1:raise ValueError('inconsistent bootstrap counts')
    values={k:[] for k in ('chord_error','relative_stress','path_error','distance_rank_correlation')}
    for i in range(next(iter(counts))):
        copies=[]
        for r in rows:
            c=dict(r)
            if r['bootstrap_sums']:
                c['sums']=dict(r['sums'],**r['bootstrap_sums'][i])
                c['distance_rank_correlation']=r['bootstrap_correlations'][i]
            copies.append(c)
        b=exact_aggregate(copies)
        for k in values:
            if b[k] is not None:values[k].append(b[k])
    for key,v in values.items():
        out['intervals'][key]=dict(lower=float(np.quantile(v,.025)),upper=float(np.quantile(v,.975)),valid_replicates=len(v)) if v else None
    return out
