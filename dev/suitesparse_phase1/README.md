# SuiteSparse Phase 1: small-graph scientific pilot

This is an offline experiment runner, not a new Shiny panel. It inventories the
SuiteSparse sample gallery, admits matrices below 3,000 graph vertices and a
conservative 100,000-edge bound, converts numerical support to graphs, and compares
six real 3D adapters. LGS is deliberately excluded. `dev/` is excluded from the
gflowui R package build; no app runtime or global package installation is changed.

## Reproduction

Use Python 3.12 and an isolated environment with `requirements.txt`. The reference
run uses an existing installed GRIP 0.2.0.9001 with public underscore-style APIs,
`smacof`, and `jsonlite`. `run_pilot.py` records the R version, installed GRIP file
hashes, smacof version, Python dependencies, source hashes, and exact Git commit.
An API-incompatible installation fails visibly, rather than changing algorithms.

From the repository root, with `python` referring to the isolated environment:

```sh
python -m pip install -r dev/suitesparse_phase1/requirements.txt
python -m pytest dev/suitesparse_phase1/test_pilot.py -q
python dev/suitesparse_phase1/catalog.py /path/to/project-data
python dev/suitesparse_phase1/ingest.py /path/to/project-data
# Commit the implementation before the run; the runner requires a clean source tree.
python dev/suitesparse_phase1/run_pilot.py /path/to/project-data
python dev/suitesparse_phase1/run_pilot.py /path/to/project-data --methods lle --landmarks 16 --result-name lle_landmarks16.json
python dev/suitesparse_phase1/run_pilot.py /path/to/project-data --methods lle --landmarks 32 --result-name lle_landmarks32.json
python dev/suitesparse_phase1/tie_diagnostics.py /path/to/project-data
python dev/suitesparse_phase1/report.py /path/to/project-data
```

All downloaded archives and generated assets are outside the package source.
The initial data root is `/Users/pgajer/current_projects/suitesparse_embedding_comparison`.
Agent handoffs, audits, and coordination files are private and outside Git.

## Scientific contracts

Square matrices become undirected nonzero-support graphs; rectangular matrices
become bipartite row/column graphs. Duplicate coordinates are summed before
numerical zeros and diagonal loops are removed. Matrix coefficients are not
interpreted as edge lengths: every retained edge has length one. Preserve original
archives and metadata, including CC-BY-4.0 attribution and matrix-specific headers.
Dimensions and full numerical nonzero counts must match catalog metadata.

Only gallery members are eligible. The frozen initial inventory contains 49
matrices, with four eligible members: HB/illc1033, HB/lock1074, HB/lock_700,
Meszaros/nemscem. All four are included; the intended six-to-eight-example target
cannot be met. No claims of comprehensive structural coverage are made.

Components are embedded and evaluated independently. Fewer than five vertices
receive a named classical small-component placement, not attributed to the requested
method. Raw component coordinates are saved; a separate x-packed coordinate file
is for display only. Neither cross-component distances nor arbitrary packing
contribute to scores.

| Method | Concrete computation |
|---|---|
| Metric MDS | `grip::metric.mds`, random initialization, SMACOF raw-stress optimization, 300 iterations, tolerance 1e-8 |
| MDS + edge-KK | `grip::edge.kk` from the matching saved MDS coordinates; uniform stiffness, unit targets, identity scale, 100 iterations |
| Weighted GRIP | `grip::weighted.grip.nd`, dimension 3, unit edge lengths, no length normalization, 160 rounds and 256 final rounds |
| Isomap on original graph | Classical scaling of exact original-graph shortest-path distances; no reconstructed kNN graph |
| UMAP | Precomputed original graph distances, 3 components, 15 neighbors (capped), min_dist 0.1, random initialization, 300 epochs |
| LLE | Standard LLE, dense eigensolver, 3 components, 15 neighbors (capped), regularization .001; shared landmark-distance features |

Stochastic methods use seeds 17, 29, 43. Dense LLE and classical scaling run once.
Fixed iteration budgets do not imply convergence; backend diagnostics and warnings
are preserved. LLE uses distances to up to 64 deterministic farthest-point landmarks,
divided by component diameter, with lexical vertex-ID tie breaking. This is a
feature representation, not an identical input to the distance-based methods.

## Metric definitions

For all unordered within-component pairs, let d be the original graph distance,
r the Euclidean chord distance, and p the embedded length of one frozen original
shortest route. Routes are SciPy Dijkstra predecessors on sorted CSR adjacency;
predecessor hashes/version are saved. Unit-edge distances agree exactly across
backends; no claim is made that GRIP uses the same tied routes internally.

- Euclidean error: `sqrt(sum((s*r-d)^2)/sum(d^2))`, with `s=sum(r*d)/sum(r^2)`.
- Relative stress: `mean(((s*r-d)/d)^2)`, with `s=sum(r/d)/sum((r/d)^2)`.
- Fixed-path error: `sqrt(sum((p-d)^2)/sum(d^2))`, without scale fitting.
- Edge error: `sqrt(mean((embedded_edge_length-1)^2))`.
- Spearman distance-rank correlation: average ranks for ties; unavailable for
  constant distances; kept component-specific on disconnected graphs.
- Jaccard error: graph hop-radius balls (radii 1,2) versus equal-cardinality
  embedded neighborhoods; lower is better. Embedded ties use lexical vertex IDs.
- Trustworthiness and continuity: standard rank penalties at k=5,10,20,50 where
  `0 < k < n/2`; false-neighbor and lost-neighbor penalties respectively, with
  lexical-ID tie breaking in both spaces. Higher is better.

Disconnected aggregation pools squared-error numerator/denominator sums, relative
error contributions by pair count, and neighborhood scores by eligible vertex
count. Collapsed configurations have unavailable scale-fitted scores, not an
artificial perfect fit. Invalid coordinates/order are rejected. Empty or undefined
metrics are null with a reason. Optional cluster-distance scores are explicitly
deferred; no frozen community partition is available in Phase 1.

The fixed-path measure is not shortest-path recomputation on drawn edge lengths.
It can be zero on folded graphs. Always interpret it alongside chord and local
neighborhood scores. Identity-scale and scale-fitted columns are not interchangeable.
No package metric is silently renamed as a project metric: these scores are evaluated
independently from coordinates, original distances, and frozen routes.

## Execution and artifacts

Each job is a process group, with 600 seconds and 2 GiB aggregate parent/child RSS
limits, one heavy job at a time, one requested numerical thread. RSS is sampled
every 0.1 second: brief peaks may be missed and enforcement may overshoot. The
budget covers preparation, embedding, scoring, imports, and R subprocesses, not
just optimizer time. Timeout/cancellation kills only that process group.
Before launch, reject jobs whose known prepared dense arrays alone exceed the
budget (8-byte distances, 4-byte predecessors and 8-byte landmark features for
the largest component). This necessary allocation check is not a peak-memory
forecast: optimizer/scoring copies and package loading still require supervision.

Run identities cover graph, code, environment, method, parameters, seed and parent
MDS manifest. Cache reads verify every recorded artifact hash. A failed or corrupt
historical directory is preserved; a retry uses a different directory. Successful
result and manifest publication is atomic. Never treat incomplete worker output as
a completed run without a successful supervisor manifest.

`pilot_results.json` and `scores.csv` list all planned attempts, including failures
and unavailable paired refinements. `graph.json` records conversion and structure.
Each run has coordinates, explicit vertex order, component input hashes, scores,
backend metadata, warnings, process log, and a checksummed manifest. `FINDINGS.md`
summarizes the saved results without claiming universal superiority.
`termination_diagnostics.json` distinguishes execution from convergence, and
`landmark_sensitivity.json` records the 16/32/64-landmark LLE comparison.
`tie_sensitivity.json` reverses only lexical tie priority for completed seed-17
layouts, while checking canonical scores against saved results. It does not
rerun optimizers or replace canonical scores. `replicate_summary.json` records
counts, means and observed min/max by graph/method/metric. Three-seed ranges are
descriptive, not confidence intervals; deterministic runs have no variability estimate.
Canonical tie diagnostics are published only after complete coverage succeeds.
Interruption leaves an earlier completed diagnostic intact (or no diagnostic);
the report rejects missing, duplicate, unexpected or stale rows before writing
any deliverable. Terminal progress messages are not published result artifacts.

Tests include analytic/folded paths, independently accumulated fixed paths and
chord sums, rigid/scale transformations, independent scikit-learn rank checks,
stable-ID ties, disconnected/isolate handling, coordinate validation, bounded
transfer/archive safety, cache corruption, process timeout, and six genuine backend
3D smoke tests. The octahedral fixture checks nonplanar 3D output, while legitimately
planar small components are not required to have rank three.

## Sources

- https://sparse.tamu.edu/about (collection, gallery, license)
- https://scikit-learn.org/stable/modules/manifold.html (classical/manifold distinctions)
- https://umap-learn.readthedocs.io/en/latest/parameters.html (UMAP parameters)
- https://scikit-learn.org/stable/modules/generated/sklearn.manifold.LocallyLinearEmbedding.html
- Local design: `docs/suitesparse_embedding_project_design_2026-09-24.md`.
