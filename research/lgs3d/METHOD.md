# LGS specification and 2D source interpretation

This is an experimental research component, outside the gflowui runtime. Phase
01 records the specification; phase 02 reproduces the pinned 2D implementation.
No dimension-general optimizer or portable production adapter is implemented
at this stage. Nothing here establishes acceptance for app integration.

Sources: Miller, Huroyan and Kobourov, *Balancing between the Local and Global
Structures (LGS) in Graph Embedding*, [arXiv v2](https://arxiv.org/html/2308.16403v2),
sections 3.1–3.2 and appendices A–D; [JacobLMiller/L2G](https://github.com/JacobLMiller/L2G)
commit `9af0e3be0d30c40bf46cc3b6f952187268c3e22d`. Vendored files are unchanged,
with their BSD-3-Clause license. Our reference translation and compiled test
wrappers use that source: this is not a clean-room implementation.

## Domain and explicit names

The initial domain is a connected, simple, undirected graph with at least two
vertices, unique stable string IDs, and unit edge lengths. Let A be its binary
symmetric adjacency matrix and d the exact all-pairs shortest-path lengths.
Weights, disconnected graphs, loops, parallel edges and singletons are outside
this scope. The eventual adapter must reject them rather than repair them.
`locality_k` is a count from 1 through n-1; `walk_depth` is a positive integer;
`walk_decay` is in (0,1); `repulsion_alpha` is nonnegative and finite.
The upstream CLI calls walk depth `--alpha`, calls repulsion `--tau`, and does
not pass tau into its optimizer. These ambiguous names are not adopted here.

## Published model and proposed precise paper variant

Section 3.1 ranks each row of S = sum_{p=1}^c s^p A^p, with c=10 and s=0.1 as
published defaults. There is no normalization of individual powers in this
formula, and no probability transition matrix for the graph case. Finite c is
the truncation. Appendix D uses a separately normalized affinity for feature
data; it is not the graph method. Appendix B's displayed spectral equality
mixes weighted and unweighted sums and leaves a free p in its final factor;
the weighted identity is Q diag(sum_p s^p lambda^p) Q^T, with eigenvectors in
columns. We do not use that display to silently remove the decay.

For a future variant named `lgs-paper-union-v1` we specify the previously
ambiguous pair convention as follows. Exclude self before sorting, sort by
score descending then stable string ID ascending (Unicode code-point order),
and select exactly k other vertices, including zero scores if c is too small
to reach them. Floating scores are compared as computed, without a tie tolerance.
An unordered pair is attractive if either directed selection contains the other
vertex (union symmetrization); mutual selections still contribute only once.
The complement among i<j is repulsive. The paper defines directed neighborhoods
but does not fully specify symmetrization, self exclusion, zero-score selection
or ties; these are declared variant choices, not established published details.

For displacement q = X_i-X_j, radius r=||q||, the paper-form objective under
this convention is

    F_paper(X) = sum_attractive (r-d_ij)^2 - alpha sum_repulsive log(r).

Each attractive pair contributes gradient 2(r-d_ij)q/r at i, and each repulsive
pair contributes -alpha q/r^2 at i; the gradient at j is its negative. There is
no inverse-square graph-distance weight, division by pair count, or double
counting. At k=n-1 this reduces exactly to unordered raw metric-MDS stress;
that identity does not imply identical output from different finite optimizers.
Translation, rotation and reflection preserve this objective. Uniform scaling
generally does not. Coincident points are singular (including an undefined
attraction direction for positive d), so a gradient is not claimed there.

The paper uses shuffled unordered pairs each epoch, a learning rate with
exponential then reciprocal decay, and a movement-based stopping threshold
10^-7 or 60 epochs. It does not give sufficient numerical detail to uniquely
reconstruct every optimizer setting or initialization from the paper alone.
The named future variant will use float64, NumPy PCG64(seed) uniform(-1,1)
initial coordinates of shape (n,dimension), or a validated supplied start,
an unbiased permutation of all unordered pairs per epoch, and the exponential/
reciprocal schedule below with exposed transition_epochs=30, schedule_epsilon=0.01.
Pair updates will use the stated paper gradients and explicit backtracking on
the pair cost if necessary; a controlled full-objective descent check is a
separate diagnostic. It will terminate on the maximum single pair displacement
within an epoch <= movement_tolerance or an epoch budget, recording which one.
These are proposed optimizer choices, not a claim to reproduce the paper's runs.

Safeguards proposed for this future variant: reject nonfinite inputs, powers,
objectives and updates; reject exact or near collisions (r <= 10^-12 in raw
coordinate units) with a reason, rather than substituting a different smooth
objective silently. Validate initial shape and retain raw coordinates without
rescaling. Bounds, atomic output and resource supervision belong to phase 04.
Handling all-coincident input by a reasoned failure is intentional. No 3D
implementation of this proposal is present in phases 01–02.

## Observed reference neighborhood construction

`modules/L2G.py` has two different paths:

* n<=1000: sum_p [(0.1^p A^p)/max(0.1^p A^p)]. In exact arithmetic the decay
  cancels within each nonzero power. This is sum_p A^p/max(A^p), not the paper
  score. Integer powers may overflow before conversion on large inputs.
* n>1000: symmetric eigendecomposition followed by Q diag(sum_p lambda^p) Q^T,
  with no decay and no power normalization. Small tests exercise this function
  directly; they do not establish behavior at scale or across the threshold.

Both paths use `np.argsort(row)[::-1][:k+1]` on the full row, including self.
They stop at an exactly zero score and set a symmetric binary flag only if
`i != v and v`. Thus index zero cannot be selected as a target; it may still
attract through its own outgoing selections. Self need not be among the k+1
candidates, so a row may propose k+1 other vertices. A zero-score stop can yield
fewer neighbors. Ties depend on NumPy's default argsort and input order, not
stable IDs. Union symmetrization does not repair these directed-selection
issues. Even k=n-1 need not include every pair if finite-depth scores are zero.
Reference mode retains these behaviors and does not claim permutation invariance.

## Observed reference optimizer, gradients and objectives

`modules/cython_l2g.pyx` stores each unordered pair once, in order
(1,0),(2,0),(2,1),... . Let w be the symmetric binary attraction flag and eta
an epoch step. For each pair, the actual displacement subtracted at i is

    m_a = min(eta*w/d^2, 1)
    m_r = min(eta, 1)
    R = m_a*(r-d)*q/(2*r) - alpha*m_r*(1-w)*q/r^3
    X_i <- X_i-R; X_j <- X_j+R.

This is inverse-distance-potential repulsion: the extra division by r after
computing q/r^2 changes the force. It is not the gradient of -alpha log r.
When eta<=1 and eta<=d^2 (true for all pairs of an unweighted graph if eta<=1),
R/eta equals the gradient of the following **inferred** effective potential:

    F_code(X) = sum_attractive (r-d)^2/(4*d^2)
                + alpha sum_repulsive 1/r.

This formula is derived from the observed updates; no upstream function reports
it. Its pair gradients are (r-d)q/(2*d^2*r) and -alpha*q/r^3 respectively.
At larger eta, attraction and repulsion have different caps; one must not
interpret a capped step as -eta times that fixed objective's gradient.
At all attractive pairs F_code is one quarter of unordered relative-distance
stress, unlike the raw-stress reduction of F_paper. A single global coefficient
cannot reconcile both models across pairs with different d and r.

Upstream `metrics.get_cost` is a third quantity. It computes ordered attraction
plus logarithmic repulsion over **every matrix entry**, including attractive
pairs and the diagonal, then divides by (1+alpha)n^2. Specifically, with
rho_ij=sqrt(max(||X_i-X_j||^2,10^-13)), it returns

    [sum_ij w_ij*(d_ij-rho_ij)^2
     - alpha sum_ij log(rho_ij+2*10^-13)] / [(1+alpha)*n^2].

It therefore matches neither the paper objective nor the optimizer's effective
potential. The floor and repeated eps additions are retained in the diagnostic
reproduction; its gradient is tested only away from its floor boundary. There
is no upstream analytic gradient API. Tests compare independently derived
scalar gradients to finite differences and the compiled optimizer's small
steps, not to a nonexistent upstream gradient function.

Schedule: w_min=min(1/d^2), w_max=max(1/d^2), eta_max=1/w_min,
eta_min=eps/w_max, lambda=log(eta_max/eta_min)/(30-1), eta_switch=1/w_max.
Steps initially equal eta_max exp(-lambda*t). At the first t whose value is
below eta_switch, set tau=t and use eta_switch/[1+lambda*(t-tau)] thereafter.
If the first loop never crosses the switch before the requested budget, the
code nevertheless sets tau to its last index and overwrites that last step
with eta_switch. The Python translation preserves and tests this short-budget
behavior. There is no movement stopping rule in the Cython optimizer: it runs
all n_iter steps (default 200); eps controls the schedule (default 0.01).
Initialization is NumPy's global uniform(-1,1) in a flat 2*n array. The
`if init_pos` check rejects an ordinary multi-element NumPy array through
ambiguous truth evaluation. Our compiled wrapper exposes the unchanged internal
SGD with explicit starts, and tests that public-entry failure separately.

The shuffle swaps index i with `rand() % i`, never i itself: a Sattolo-type
cyclic permutation, not an unbiased Fisher–Yates permutation. Pair arrays are
shuffled in place across epochs. C rand is seeded at import using Python random;
NumPy initialization has a separate random state. Our test wrapper supplies a
C seed and extracts the realized schedules; repeatability is claimed only for
the pinned local C runtime. alpha is narrowed to C float (float32) in the
upstream optimizer, though coordinates, distances and steps use float64.
There are no collision, overflow, allocation, finite-value or singleton guards.
Tests only call unsafe native code on tiny valid matrices, plus a controlled
collision example that produces nonfinite results. A wrapper is not a safe
production adapter.

## Exact changes required for arbitrary output dimension

Mathematically, replace X_i in R^2 by X_i in R^D and compute r from the sum of
all D squared coordinate differences. Pair flags, shortest paths, coefficients
and schedule do not change solely because D changes. Each scalar coordinate
update becomes the corresponding D-vector update; initialization must sample
all D coordinates and CSV/shape validation must preserve that dimension. A
planar initialization remains planar for distance-only gradient updates, so a
nonplanar initial configuration is necessary for a genuine 3D diagnostic.
Replacing only a flag or zero-padding does not alter the upstream hardcoded
`2*i`, dx/dy storage and `(n,2)` reshape. Invariances and 3D gradient/rank tests
are deferred to phase 03. Neither the paper variant nor reference optimizer is
extended to 3D in this initial submission.

## Evaluation definitions and discrepancies

Paper section 3.2 defines neighborhood error as one minus average Jaccard overlap
between a full graph hop-radius ball (excluding self) and an equally sized
embedded nearest-neighbor set. It reports relative-distance stress as a sum,
with embedding scaling, and cluster stress on distances 1 minus the fraction
of all edges connecting each pair of frozen clusters. Its displayed sums do
not resolve ordered versus unordered pairs or exclusion of zero diagonals.

Upstream `get_stress` uses unordered pairs, fits s=sum(r/d)/sum((r/d)^2), and
returns mean((s*r/d-1)^2). This agrees with the project's scale-fitted relative
stress on valid noncollapsed inputs, not raw stress. A collapsed layout gives
an undefined scale. Upstream's later `get_neighborhood` definition shadows the
earlier matrix-based one: at radius >=2 it can include the source vertex in
the expanded graph neighbor set. Its embedded set excludes self by slicing,
which also mishandles ties at coincident points. We reproduce the latter
function with a minimal graph interface to exhibit the radius-2 discrepancy.

The graph cluster-distance helper exponentiates negative squared intercluster
edge counts, normalizes by column maxima, complements and symmetrizes; this is
not the paper's global-edge-count denominator. Community construction calls
`minimize_blockmodel_dl`, whereas the paper says modularity clustering. Finally
`get_metrics` computes cd but returns tst (a cluster-neighborhood score) as its
second value. No cluster quality claim is made from these routines, and no
replacement community partition is defined here. Main-project metric definitions
and partitions remain authoritative for any future comparison.

## Identity and limits of this stage

`upstream-9af0e3b-reference-2d` names the reproduced code behavior; it is not
presented as an implementation of equation (1). `lgs-paper-union-v1` names a
proposed paper-form interpretation and is not labeled simply “LGS”. The
objective, locality, tie and optimizer differences are scientifically material.
Choosing which model should be carried into 3D and whether it may bear the
unqualified LGS name requires an explicit scientific decision. This submission
provides 2D evidence for that choice; it does not silently correct the reference.
