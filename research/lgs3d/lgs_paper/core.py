"""lgs-paper-union-v1: equation (1), exact dense locality, explicit SGD variant.

Paper and upstream attribution: ../NOTICE.md. No upstream kernel is used here;
the accepted reference reproduction remains in lgs_reference/.
"""
from dataclasses import dataclass, field
from collections import deque
import math
import numbers
import numpy as np

VARIANT = 'lgs-paper-union-v1'
# Relative to the displacement cap, never to absolute coordinate magnitudes.
MOVEMENT_RTOL = 8*np.finfo(np.float64).eps


class NumericalFailure(ValueError):
    """A reasoned failure; no valid result is returned."""


def integer(value, name, low, high=None):
    if isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Integral):
        raise ValueError(f'{name} must be an integer')
    if value < low or (high is not None and value > high):
        raise ValueError(f'{name} out of range')
    return int(value)


def real(value, name, low=0., strict=False):
    if isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Real):
        raise ValueError(f'{name} must be real')
    if not math.isfinite(value) or (value <= low if strict else value < low):
        raise ValueError(f'{name} out of range')
    return float(value)


@dataclass(frozen=True)
class Problem:
    vertex_ids: tuple
    distances: np.ndarray
    scores: np.ndarray
    directed: np.ndarray
    attractive: np.ndarray
    pairs: np.ndarray
    canonical_order: np.ndarray
    locality_k: int
    walk_depth: int
    walk_decay: float
    attractive_components: int


@dataclass(frozen=True)
class Controls:
    epochs: int = 60
    repulsion_alpha: float = 0.2
    transition_epochs: int = 30
    schedule_epsilon: float = 0.01
    movement_tolerance: float = 1e-7
    collision_distance: float = 1e-12
    max_pair_displacement: float = 1.0
    armijo: float = 1e-4
    max_backtracks: int = 60

    def __post_init__(self):
        integer(self.epochs,'epochs',1)
        integer(self.transition_epochs,'transition_epochs',2)
        integer(self.max_backtracks,'max_backtracks',0)
        real(self.repulsion_alpha,'repulsion_alpha')
        real(self.movement_tolerance,'movement_tolerance')
        real(self.collision_distance,'collision_distance',strict=True)
        real(self.max_pair_displacement,'max_pair_displacement',strict=True)
        if not 0 < real(self.schedule_epsilon,'schedule_epsilon') < 1:
            raise ValueError('schedule_epsilon must lie in (0,1)')
        if not 0 < real(self.armijo,'armijo') < 1:
            raise ValueError('armijo must lie in (0,1)')


@dataclass
class Result:
    coordinates: np.ndarray
    initial_coordinates: np.ndarray
    history: list
    termination: str
    epochs_completed: int
    seed: int
    dimension: int
    controls: Controls
    warnings: list = field(default_factory=list)
    variant: str = VARIANT


def component_count(adjacency):
    remaining = set(range(len(adjacency)))
    count = 0
    while remaining:
        frontier = [remaining.pop()]
        count += 1
        while frontier:
            i = frontier.pop()
            new = remaining.intersection(np.flatnonzero(adjacency[i]).tolist())
            remaining.difference_update(new)
            frontier.extend(new)
    return count


def prepare(vertex_ids, adjacency, locality_k, walk_depth=10, walk_decay=0.1):
    """Prepare exact small-graph constraints. Arrays retain input vertex order.

    Multiplication and pair order use canonical stable-ID order, making even
    floating summation order independent of input vertex ordering.
    """
    ids = tuple(vertex_ids)
    if len(ids) < 2:
        raise ValueError('unsupported_small_component: at least two vertices required')
    if any(not isinstance(v,str) or not v for v in ids) or len(set(ids)) != len(ids):
        raise ValueError('vertex IDs must be unique nonempty strings')
    n = len(ids)
    k = integer(locality_k,'locality_k',1,n-1)
    depth = integer(walk_depth,'walk_depth',1)
    decay = real(walk_decay,'walk_decay')
    if not 0 < decay < 1:
        raise ValueError('walk_decay must lie in (0,1)')
    raw = np.asarray(adjacency)
    if raw.shape != (n,n) or raw.dtype.kind not in 'biuf':
        raise ValueError('adjacency must be a real square matrix matching vertex IDs')
    a = raw.astype(np.float64)
    if not np.isfinite(a).all() or not np.isin(a,[0,1]).all():
        raise ValueError('only finite unweighted binary adjacency is supported')
    if not np.array_equal(a,a.T) or np.any(np.diag(a)):
        raise ValueError('graph must be simple and undirected without self loops')
    if component_count(a) != 1:
        raise ValueError('graph must be connected')
    order = np.array(sorted(range(n),key=ids.__getitem__),dtype=np.int64)
    inv = np.argsort(order)
    canonical = a[np.ix_(order,order)]
    # Float64 weighted recurrence avoids integer powers and int64 overflow.
    # B_p = (s A)^p. No normalization or size-dependent algorithm switch.
    weighted = decay*canonical
    power = weighted.copy()
    scores = power.copy()
    try:
        with np.errstate(over='raise',invalid='raise'):
            for _ in range(1,depth):
                power = power @ weighted
                scores += power
    except FloatingPointError as exc:
        raise NumericalFailure('nonfinite_walk_scores') from exc
    if not np.isfinite(scores).all():
        raise NumericalFailure('nonfinite_walk_scores')
    directed_c = np.zeros((n,n),dtype=bool)
    for i in range(n):
        candidates = [j for j in range(n) if j != i]
        chosen = sorted(candidates,key=lambda j:(-scores[i,j],ids[order[j]]))[:k]
        directed_c[i,chosen] = True
    directed = directed_c[np.ix_(inv,inv)]
    attractive = directed | directed.T
    d = np.full((n,n),np.inf)
    for start in range(n):
        d[start,start] = 0.
        queue = deque([start])
        while queue:
            i = queue.popleft()
            for j in np.flatnonzero(a[i]):
                if not np.isfinite(d[start,j]):
                    d[start,j] = d[start,i]+1
                    queue.append(int(j))
    pairs = np.array([(order[i],order[j]) for i in range(n) for j in range(i+1,n)],dtype=np.int64)
    arrays = [d,scores[np.ix_(inv,inv)],directed,attractive,pairs,order]
    for array in arrays:
        array.setflags(write=False)
    return Problem(ids,*arrays,k,depth,decay,component_count(attractive))


def coordinates(points, n):
    raw = np.asarray(points)
    if raw.dtype.kind not in 'iuf' or raw.ndim != 2 or raw.shape[0] != n or raw.shape[1] < 1:
        raise ValueError('coordinates must be a real (n,dimension) matrix')
    x = raw.astype(np.float64,copy=True)
    if not np.isfinite(x).all():
        raise ValueError('coordinates must be finite')
    return x


def objective_gradient(problem, points, alpha=0.2, collision_distance=1e-12):
    """Vectorized exact unordered paper objective and dimension-general gradient."""
    alpha = real(alpha,'repulsion_alpha')
    floor = real(collision_distance,'collision_distance',strict=True)
    x = coordinates(points,len(problem.vertex_ids))
    i,j = problem.pairs.T
    try:
        with np.errstate(over='raise',invalid='raise',divide='raise'):
            q = x[i]-x[j]
            radius = np.sqrt(np.sum(q*q,axis=1))
            if np.any(radius <= floor):
                raise NumericalFailure('collision_or_near_collision')
            mask = problem.attractive[i,j]
            residual = radius-problem.distances[i,j]
            values = np.where(mask,residual**2,-alpha*np.log(radius))
            factor = np.where(mask,2*residual/radius,-alpha/(radius**2))
            gradient = np.zeros_like(x)
            pair_gradient = factor[:,None]*q
            np.add.at(gradient,i,pair_gradient)
            np.add.at(gradient,j,-pair_gradient)
            value = float(np.sum(values))
    except FloatingPointError as exc:
        raise NumericalFailure('nonfinite_objective_or_gradient') from exc
    if not math.isfinite(value) or not np.isfinite(gradient).all():
        raise NumericalFailure('nonfinite_objective_or_gradient')
    return value,gradient


def schedule(problem, controls):
    """Exponential-to-reciprocal schedule with a budget-independent switch."""
    i,j = problem.pairs.T
    targets = problem.distances[i,j]
    eta_max = float(np.max(targets)**2)
    switch = float(np.min(targets)**2)
    eta_min = controls.schedule_epsilon*switch
    rate = math.log(eta_max/eta_min)/(controls.transition_epochs-1)
    # Define the first crossing from an infinite exponential sequence. A short
    # budget must be a prefix of a longer one, unlike the native short-run bug.
    tau = 0
    while eta_max*math.exp(-rate*tau) >= switch:
        tau += 1
    return np.array([eta_max*math.exp(-rate*t) if t < tau else
                     switch/(1+rate*(t-tau)) for t in range(controls.epochs)])


def _pair_value(radius, target, attractive, alpha):
    return (radius-target)**2 if attractive else -alpha*math.log(radius)


def safeguarded_pair(points, i, j, target, attractive, eta, controls):
    """Update two rows in place, after pair Armijo and all-point collision checks.

    Returns accepted endpoint movement, effective step, and backtrack count.
    A failed search raises without changing any coordinates.
    """
    q = points[i]-points[j]
    radius = float(np.linalg.norm(q))
    if not math.isfinite(radius) or radius <= controls.collision_distance:
        raise NumericalFailure('collision_or_near_collision')
    scalar = 2*(radius-target) if attractive else -controls.repulsion_alpha/radius
    gradient = scalar*(q/radius)
    norm = abs(scalar)
    if not math.isfinite(norm) or not np.isfinite(gradient).all():
        raise NumericalFailure('nonfinite_pair_gradient')
    if norm == 0:
        return 0.,float(eta),0
    step = min(float(eta),controls.max_pair_displacement/norm)
    old = _pair_value(radius,target,attractive,controls.repulsion_alpha)
    for attempt in range(controls.max_backtracks+1):
        delta = step*gradient
        left,right = points[i]-delta,points[j]+delta
        separation = float(np.linalg.norm(left-right))
        valid = np.isfinite(left).all() and np.isfinite(right).all() and math.isfinite(separation)
        valid = valid and separation > controls.collision_distance
        movement = max(float(np.linalg.norm(left-points[i])),float(np.linalg.norm(right-points[j])))
        # The intended delta cap does not bound rounded coordinate changes.
        # Inspect actual stored endpoint motion before accepting or mutating.
        if (not math.isfinite(movement) or
            (movement > controls.max_pair_displacement and
             movement-controls.max_pair_displacement > MOVEMENT_RTOL*controls.max_pair_displacement)):
            valid = False
        # Check third vertices too: reducing a pair's cost can otherwise create
        # a singularity in another constraint. This exact guard costs O(n D).
        if valid:
            keep = np.ones(len(points),dtype=bool)
            keep[[i,j]] = False
            for trial in (left,right):
                distances = np.linalg.norm(points[keep]-trial,axis=1)
                if not np.isfinite(distances).all() or np.any(distances <= controls.collision_distance):
                    valid = False
                    break
        if valid:
            cost = _pair_value(separation,target,attractive,controls.repulsion_alpha)
            required = old-controls.armijo*step*(2*norm*norm)
            if math.isfinite(cost) and cost <= required:
                if movement == 0:
                    raise NumericalFailure('floating_point_stagnation')
                points[i],points[j] = left,right
                return movement,step,attempt
        step *= 0.5
    raise NumericalFailure('pair_line_search_failed')


def optimize(problem, dimension=3, seed=0, initial=None, controls=None):
    """Finite shuffled-pair optimization; returns raw coordinates in input order."""
    dimension = integer(dimension,'dimension',2,3)
    seed = integer(seed,'seed',0,2**64-1)
    controls = Controls() if controls is None else controls
    if not isinstance(controls,Controls):
        raise ValueError('controls must be Controls')
    n = len(problem.vertex_ids)
    # Separate RNG streams ensure supplied starts do not alter pair schedules.
    init_seed,order_seed = np.random.SeedSequence(seed).spawn(2)
    if initial is None:
        x = np.empty((n,dimension))
        x[problem.canonical_order] = np.random.Generator(np.random.PCG64(init_seed)).uniform(-1,1,(n,dimension))
    else:
        x = coordinates(initial,n)
        if x.shape != (n,dimension):
            raise ValueError('initial coordinate dimension mismatch')
    initial_copy = x.copy()
    value,gradient = objective_gradient(problem,x,controls.repulsion_alpha,controls.collision_distance)
    history = [{'epoch':0,'objective':value,'gradient_norm':float(np.linalg.norm(gradient[problem.canonical_order])),
                'max_pair_movement':0.,'backtracks':0,'roundoff_skipped_pairs':0}]
    rng = np.random.Generator(np.random.PCG64(order_seed))
    warnings = []
    if problem.attractive_components > 1:
        warnings.append('attraction_graph_disconnected: relative component placement is unconstrained')
        if controls.repulsion_alpha > 0:
            warnings.append('objective_unbounded_below: disconnected attractive components can separate indefinitely')
    termination = 'epoch_budget'
    for epoch,eta in enumerate(schedule(problem,controls),1):
        movement,backtracks,roundoff_pairs = 0.,0,0
        for pair_index in rng.permutation(len(problem.pairs)):
            i,j = problem.pairs[pair_index]
            try:
                moved,_,count = safeguarded_pair(x,i,j,problem.distances[i,j],bool(problem.attractive[i,j]),eta,controls)
            except NumericalFailure as exc:
                if str(exc) != 'floating_point_stagnation':
                    raise
                # A locally unrepresentable update need not stop other pairs.
                # Preserve the state and disclose each skipped pair.
                roundoff_pairs += 1
                continue
            movement = max(movement,moved)
            backtracks += count
        value,gradient = objective_gradient(problem,x,controls.repulsion_alpha,controls.collision_distance)
        history.append({'epoch':epoch,'objective':value,'gradient_norm':float(np.linalg.norm(gradient[problem.canonical_order])),
                        'max_pair_movement':movement,'backtracks':backtracks,'roundoff_skipped_pairs':roundoff_pairs})
        if movement <= controls.movement_tolerance:
            termination = 'floating_point_stagnation' if roundoff_pairs else 'movement_tolerance'
            break
    return Result(x,initial_copy,history,termination,len(history)-1,seed,dimension,controls,warnings)
