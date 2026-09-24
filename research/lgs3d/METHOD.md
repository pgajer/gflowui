# LGS specification and 2D source interpretation

This is an experimental research component, outside the gflowui runtime. Phase
01 records the specification; phase 02 reproduces the pinned 2D implementation.
Phase 03 adds the explicitly selected paper-form objective and safeguarded
dimension-general optimizer. The portable adapter remains future work. Nothing
here establishes acceptance for app integration.

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

## Published model and selected precise paper variant

Section 3.1 ranks each row of S = sum_{p=1}^c s^p A^p, with c=10 and s=0.1 as
published defaults. There is no normalization of individual powers in this
formula, and no probability transition matrix for the graph case. Finite c is
the truncation. Appendix D uses a separately normalized affinity for feature
data; it is not the graph method. Appendix B's displayed spectral equality
mixes weighted and unweighted sums and leaves a free p in its final factor;
the weighted identity is Q diag(sum_p s^p lambda^p) Q^T, with eigenvectors in
columns. We do not use that display to silently remove the decay.

For the selected variant named `lgs-paper-union-v1` we specify the previously
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
The implemented variant uses float64 and NumPy PCG64. A SeedSequence(seed)
spawns independent initialization and pair-order streams; this means supplying
the same saved start does not change the pair schedule. Initialization draws
uniform(-1,1) in (n,dimension) shape in stable-ID order, then restores declared
input order. Every epoch uses a fresh NumPy permutation of all unordered pairs
listed in stable-ID order. This is unbiased pseudorandom permutation rather than
the upstream self-swap-excluding shuffle. All raw coordinates remain unscaled.

The schedule uses eta_max=max(d)^2, eta_switch=min(d)^2,
eta_min=schedule_epsilon*eta_switch and
lambda=log(eta_max/eta_min)/(transition_epochs-1). Define tau as the first
nonnegative integer for which eta_max*exp(-lambda*tau)<eta_switch, independent
of requested epoch budget. Before tau use the exponential value; from tau use
eta_switch/[1+lambda*(t-tau)]. Unlike the reference, short budgets are exact
prefixes of longer schedules. Defaults: 60 epochs, transition_epochs=30,
schedule_epsilon=0.01, repulsion_alpha=0.2, movement_tolerance=1e-7.
These schedule and optimizer details are declared variant choices, not a claim
to reproduce the paper's runs or the reference optimizer.

For pair gradient g at i (negative at j), trial endpoints are X_i-t*g and
X_j+t*g. Start t=min(eta, max_pair_displacement/||g||), with maximum endpoint
displacement default 1 graph-length unit. Before acceptance, the actual rounded
endpoint displacements must also obey this cap, allowing only 8*float64_epsilon
relative to the cap for norm rounding (about 1.78e-15). This tolerance does not
scale with the absolute coordinates or their ULP spacing. A trial that exceeds
the cap is backtracked before any mutation. When no nonzero representable step
fits, the existing stagnation policy applies. A zero gradient causes no movement.
Halve t until the pair objective f satisfies
f(trial)<=f(current)-armijo*t*2*||g||^2, with armijo=1e-4 by default.
The factor two includes both endpoints. Every trial must also keep both endpoints
more than collision_distance=1e-12 from each other and every third vertex.
There are at most max_backtracks=60 halvings; exhaustion raises
pair_line_search_failed without applying the failed step. Nonzero-gradient
updates that round to zero displacement are skipped without changing coordinates;
the optimizer counts these as roundoff_skipped_pairs so other pairs can continue.
These are safeguards on optimization, not changes to the scalar objective.
Pairwise descent does not imply full-objective descent after each pair or epoch.

The optimizer computes the full objective and gradient initially and after every
epoch, recording the maximum accepted single-pair endpoint movement and the
number of halvings for accepted pair steps. Searches ending in roundoff skips
contribute to roundoff_skipped_pairs, but their discarded halvings are not
included in the backtracks diagnostic. It stops if that maximum is <= movement_tolerance or the
epoch budget is exhausted. The termination strings are movement_tolerance and
epoch_budget. If the movement condition holds in an epoch with roundoff-skipped
pairs, termination is instead floating_point_stagnation, even if gradients are
small. None is a claim of a global optimum or certified stationarity.

Nonfinite inputs, scores, objectives and gradients fail explicitly. Coincident
and near-coincident starts (any pair r<=1e-12) fail; there is no jitter, distance
floor in the objective, or silent replacement by a smooth model. Accepted steps
are displacement-limited and checked against all vertices. A numerical failure
raises and returns no Result; the caller's initial array is never mutated.
External request/resource/atomic-output handling remains phase04 work.

Walk scores use the dense float64 recurrence B_1=s*A, B_p=B_(p-1)*(s*A),
S=sum B_p. Multiplication occurs after sorting vertices by stable ID, fixing
summation order across input permutations. There is no normalization, integer
matrix-power overflow, or n=1000 algorithm switch. Scores are unpermuted for
output. Overflow is rejected; underflow is ordinary float64 behavior, not exact
rational arithmetic. Independent tiny tests count walks using Python integers.
All k candidates, including zero scores, are considered; there is no minimum
score threshold. No promise of treating merely near-equal scores as exact ties
is made. The stable-ID policy governs equal computed scores.

A connected input graph can yield a disconnected attractive-pair graph after
neighborhood selection. For positive alpha, translating these attractive groups
arbitrarily far apart leaves their internal stress unchanged and sends the
repulsive logarithmic term to minus infinity. The optimizer emits an explicit
objective_unbounded_below warning and returns only a finite-budget trajectory;
it does not add repair edges, confine coordinates or claim a finite minimum.
At alpha=0 their relative placement is unconstrained. Both cases record the
number of attractive components. All-neighbors attraction is connected for the
accepted input domain and reduces to raw stress.

The implementation is exact over all pairs, subject to float64 arithmetic.
Dense walk multiplication costs O(c*n^3), preparation stores O(n^2), and the
all-vertex collision guard makes each full epoch O(n^3*D) in the worst case.
No scaling claim is made: this correctness-first implementation is tested on
tiny fixtures, before resource-limited adapter work.

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
`2*i`, dx/dy storage and `(n,2)` reshape. The new lgs_paper evaluator performs these vector operations in arbitrary
positive dimension; the embedding entry point accepts dimensions 2 and 3.
Invariance, 3D gradients and tetrahedron rank are tested in phase03. The pinned
reference optimizer remains unchanged and 2D-only.

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

The selected model is `lgs-paper-union-v1`: paper-form raw attraction and
logarithmic repulsion, decayed walk scores, union constraints, declared stable-ID
ties, and the explicit safeguarded optimizer above. It is not called simply
“LGS” and does not claim reference-code equivalence. The model selection was
made explicitly before phase03. The accepted phase01–02 reproduction remains
available as `upstream-9af0e3b-reference-2d`, including its documented anomalies.

Phase03 tests the numerical method in 2D and 3D on small graphs. Portable request
schemas, external file validation, resumable cache, enforced job limits, locality
quality comparisons and scaling remain phase04 work. Neither independent numerical
review nor model selection authorizes app integration or merging this research
folder into the package.
