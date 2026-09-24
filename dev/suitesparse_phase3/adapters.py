"""Explicit 3D public-backend adapters; original graph evaluation is external."""
import os
from pathlib import Path
import subprocess
import numpy as np
from scipy.spatial.distance import cdist

METHODS=['pacmap','localmap','trimap','phate','largevis','ncvis']


def exact_feature_neighbors(x, ids, count):
    """Self first, then distance and lexicographic stable vertex ID ties."""
    d=cdist(x,x)
    order=np.argsort(np.asarray(ids),kind='stable')
    neighbors=np.empty((len(ids),count),dtype=np.int32)
    for i in range(len(ids)):
        other=order[order!=i]
        other=other[np.argsort(d[i,other],kind='stable')]
        neighbors[i]=np.r_[i,other][:count]
    return neighbors,np.take_along_axis(d,neighbors,axis=1).astype(np.float32)


def seed_numba(seed):
    # NumPy and Numba maintain distinct RNG states. This runs inside compiled code.
    import numba
    numba.set_num_threads(1)
    @numba.njit
    def set_seed(value):
        np.random.seed(value)
    set_seed(seed)
    @numba.njit(parallel=True)
    def set_parallel_seed(value):
        for i in numba.prange(numba.get_num_threads()):
            np.random.seed(value+i)
    set_parallel_seed(seed)


def embed(method, adjacency, ids, d, features, seed, dest, initial=None):
    if method not in METHODS: raise ValueError('unsupported Phase 03 method')
    n=len(ids)
    if n<5: raise ValueError('small components must use the declared outer placement')
    x=np.array(features,dtype=np.float32,order='C',copy=True)
    if x.shape[0]!=n or not np.isfinite(x).all() or np.ptp(x)==0:
        raise ValueError('invalid or constant feature input')
    np.random.seed(seed)
    detail=dict(input='shared graph landmark distances',dimension=3,seed=int(seed),threads=1)
    if method in ('pacmap','localmap'):
        from pacmap import PaCMAP,LocalMAP
        cls=PaCMAP if method=='pacmap' else LocalMAP
        settings=dict(n_components=3,n_neighbors=min(10,n-2),MN_ratio=.5,FP_ratio=2.,
                      distance='euclidean',lr=1.,num_iters=(100,100,250),apply_pca=False,
                      random_state=int(seed),knn_backend='annoy')
        model=cls(**settings)
        z=model.fit_transform(x,init='random')
        detail.update(settings=settings,initialization='seeded random',
                      effective_pair_counts=dict(neighbors=int(model.n_neighbors),
                                                 mid_near=int(model.n_MN),far=int(model.n_FP)),
                      termination='fixed 450-iteration schedule; no convergence claim',
                      internal_preprocessing='global min/max scalar normalization, then feature centering')
        if method=='localmap': detail['low_dist_threshold']=float(model.low_dist_thres)
    elif method=='trimap':
        from trimap import TRIMAP
        seed_numba(seed)
        inliers=min(12,n-2)
        neighbors,distances=exact_feature_neighbors(x,ids,min(n,inliers+50))
        settings=dict(n_dims=3,n_inliers=inliers,n_outliers=4,n_random=3,
                      distance='euclidean',lr=.1,n_iters=400,weight_temp=.5,
                      apply_pca=False,opt_method='dbd')
        model=TRIMAP(knn_tuple=(neighbors,distances),**settings)
        # Import/JIT initialization may consume the Python RNG; reset immediately
        # before fitting rather than relying on the seed before backend import.
        np.random.seed(seed)
        z=model.fit_transform(x,init='random')
        detail.update(settings=settings,initialization='seeded random; NumPy and serial/parallel Numba RNGs seeded',
                      neighbors='exact feature distances; stable-ID ties; public knn_tuple route',
                      termination='fixed 400-iteration schedule; no convergence claim')
        import hashlib
        detail['triplets_sha256']=hashlib.sha256(model.triplets.tobytes()).hexdigest()
        detail['weights_sha256']=hashlib.sha256(model.weights.tobytes()).hexdigest()
    elif method=='phate':
        from phate import PHATE
        settings=dict(n_components=3,knn=min(5,n-1),decay=40,t='auto',n_landmark=None,
                      n_pca=None,knn_dist='euclidean',mds='metric',mds_solver='sgd',
                      random_state=int(seed),n_jobs=1,verbose=0)
        model=PHATE(**settings)
        z=model.fit_transform(x)
        detail.update(settings=settings,optimal_t=int(model.optimal_t),
                      termination='SGD-MDS at most 500 iterations; actual stopping iteration not exposed',
                      evaluation_target='original graph distances, not PHATE diffusion/potential distances')
    elif method=='ncvis':
        from ncvis import NCVis
        settings=dict(d=3,n_threads=1,n_neighbors=min(15,n-1),M=8,ef_construction=100,
                      random_seed=int(seed),n_epochs=50,n_init_epochs=20,spread=1.,min_dist=.4,
                      alpha=1.,alpha_Q=1.,distance='euclidean')
        z=NCVis(**settings).fit_transform(x)
        detail.update(settings=settings,termination='fixed epoch budget; no convergence claim')
    else:
        executable=os.environ.get('GFLOWUI_LARGEVIS_BINARY')
        if not executable or not Path(executable).is_file():
            raise RuntimeError('validated LargeVis binary not configured')
        source=Path(dest)/'largevis_features.txt'
        output=Path(dest)/'largevis_coords.txt'
        np.savetxt(source,x,header=f'{n} {x.shape[1]}',comments='')
        settings=dict(outdim=3,threads=1,samples=20,neigh=min(150,n-1),
                      perp=min(50,max(2,(n-1)//3)),trees=50,prop=3,neg=5,alpha=1.,gamma=7.)
        command=[executable,'-input',str(source),'-output',str(output),'-fea','1']
        for key,value in settings.items(): command.extend(['-'+key,str(value)])
        env=os.environ.copy();env['GFLOWUI_LARGEVIS_SEED']=str(seed)
        subprocess.run(command,check=True,env=env)
        with output.open() as stream:
            if tuple(map(int,stream.readline().split()))!=(n,3):
                raise ValueError('LargeVis output dimension/count mismatch')
        z=np.loadtxt(output,skiprows=1,ndmin=2)
        detail.update(settings=settings,seed_patch='gflowui-env-seed-v1; unchanged default when absent',
                      termination='fixed 20 million sampled edges; no convergence claim',
                      internal_preprocessing='LargeVis feature kNN affinities; original graph retained for evaluation')
    z=np.asarray(z,dtype=float)
    if z.shape!=(n,3) or not np.isfinite(z).all(): raise ValueError('backend returned invalid 3D coordinates')
    return z,detail
