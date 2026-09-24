# SuiteSparse 3D embedding comparison: project design and implementation phases

Date: 2026-09-24

Status: proposed implementation specification; no benchmark or app changes made

Project name: SuiteSparse 3D Embedding Comparison

## 1. Purpose and agreed scope

Build a gflowui project for exploring different 3D embeddings of a modest-sized,
structurally varied subset of the graphs pictured in the SuiteSparse About-page
sample gallery. Couple the graph viewer to transparent, reproducible comparisons
of distance preservation, neighborhood preservation, runtime, and stability.
The purpose is to understand trade-offs, not assign a universal winning method.

The first implementation phase uses graphs with **strictly fewer than 3,000
vertices**. It validates graph conversion, evaluation measures, and a small set
of 3D adapters before expanding either the method roster or graph sizes.
**LGS is excluded from Phase 1.** A separate agent may develop its 3D extension
in parallel; neither the pilot nor the basic app should depend on that work.

This document specifies future work. It does not claim that all requested
methods currently support 3D, accept graph distances, or are installed. The
companion agent prompt and launch records are private working material stored
outside this repository under `~/.codex/private/gflowui/suitesparse-embeddings/`.
No parallel agent was launched while preparing these documents.

## 2. Graph collection and download policy

### 2.1 Admission limits

- Phase 1: `n < 3000`, at most 100,000 unique undirected non-loop edges.
- Later expansion: at most 10,000 vertices and 100,000 such edges.
- No downloads of million-vertex graphs, and no download-then-subsample workaround.
- Read collection metadata before downloading matrix archives. Record source
  rows, columns, nonzero count, symmetry, archive size where available, and URL.
- For a rectangular matrix interpreted as a bipartite graph, `n = rows + columns`.
- Use a conservative nonzero-count bound to screen edge counts before download;
  explicitly account for whether metadata counts full or triangular storage.
  Unknown or ambiguous dimensions/counts require metadata resolution first.
- Enforce the edge cap again after conversion. Metadata discrepancies cause a
  recorded exclusion; never silently truncate a graph to meet a limit.
- Proposed transfer safeguards: 100 MiB per archive and 1 GiB extracted per
  matrix; reject unsafe archive paths and stop at either cap. These are safety
  defaults, not measured requirements, and changes need an explicit configuration.

One dense double-precision 10,000-by-10,000 matrix is about 800 MB; at 3,000 it is
about 72 MB. These are single-array sizes, not total process-memory estimates.
Admission does not promise that every method can run on every admitted graph.

### 2.2 Selecting the pilot

Inventory the gallery and save a dated, source-linked catalog. Select a target
of 6–8 eligible examples spanning as many of these structures as the gallery
actually provides: mesh/near-geometric, branching, community structure,
heterogeneous degree/hubs, irregular sparse structure, and bipartite structure.
Use domain metadata and measured degree/component/clustering summaries to justify
the selection. Do not label a matrix "community structured" from its picture alone.

The exact graph list is an output of Phase 1, not established by this design.
If the gallery has too few eligible examples or lacks a structural category,
report the gap. Do not silently substitute other collection entries. Small
synthetic graphs are permitted as tests, clearly separated from the gallery cohort.

### 2.3 Matrix-to-graph interpretation

- Square matrices: initial baseline is the undirected union of numerical
  nonzero support of `A` and its transpose, excluding the diagonal. Preserve
  original matrix directionality and symmetry in metadata.
- Rectangular matrices: row and column vertices form separate bipartite sets;
  each numerical nonzero induces an edge. Prefix IDs to prevent collisions.
- Resolve duplicate matrix coordinates according to the matrix format before
  testing numerical nonzero support. Record canceled entries, loops, and isolates.
- Use unit edge lengths initially. Signed, complex, stiffness, or flow coefficients
  are not automatically distances. Preserve source values separately; any later
  weight-to-length transformation creates a separately identified graph variant.
- Retain disconnected components and isolated vertices. Do not add MST repair
  edges. Embed components separately and pack them only for display. Cross-component
  placement is arbitrary and is never used for distance-quality scores.
- Keep stable original vertex IDs and record every transformation and checksum.

## 3. Methods and input representations

The eventual roster contains 13 method families, with LGS parameter settings
counted as runs of one family rather than new methods.

| Method | Pilot role / later work | Required adapter distinction |
| --- | --- | --- |
| Metric MDS | Phase 1 baseline | Minimize Euclidean raw stress against graph shortest-path distances; not classical scaling |
| Metric MDS + edge-KK | Phase 1 baseline | Start from the corresponding saved MDS run, then refine edges; preserve both stages |
| Weighted GRIP | Phase 1 baseline | Pin the actual GRIP function/version and edge-length interpretation; unit targets in the first cohort |
| Isomap on the original graph | Phase 1 baseline | Original-graph shortest paths followed by classical MDS; do not rebuild a neighborhood graph |
| UMAP | Phase 1 nonlinear adapter | Validate 3D and the pinned backend's precomputed-distance route |
| LLE | Phase 1 feature adapter | Use the shared landmark-distance feature representation; record neighbor count and regularization |
| PaCMAP | Phase 3 | Validate genuine 3D output and the chosen input route |
| LocalMAP | Phase 3 | Internal neighbor changes do not change the original graph used for display/evaluation |
| TriMAP | Phase 3 | Verify backend dimensions, distance/feature route, and objective conventions |
| PHATE | Phase 3 | Record graph-derived input and diffusion settings; do not confuse diffusion distances with original graph targets |
| LargeVis | Phase 3 | Confirm public backend's actual 3D support, installation, and input requirements |
| NCVis | Phase 3 | Confirm public backend's actual 3D support, installation, and input requirements |
| LGS | Parallel development; Phase 4 integration | Independently validated 3D extension, provenance, and locality sweep |

Phase 1 must validate the four baselines plus at least one nonlinear adapter and
one feature-based adapter. If UMAP or LLE cannot meet the contract, record the
blocker; a reduced demonstration is not full Phase 1 completion. Additional
methods must not delay validation of the foundational contracts.

For methods requiring features, propose distances to up to 64 landmarks per
component, selected by deterministic farthest-point sampling with stable-ID tie
breaking. Scale all feature columns together by one recorded component distance
scale; do not standardize columns independently by default. Use the same frozen
features across those adapters. Components too small for a method use an explicitly
identified small-component placement, not fabricated method output. Test a modest
landmark-count sensitivity study before treating this representation as adequate.

The graph-distance and feature-input tracks are related but not identical
experiments. Show their input type in tables. A method/input variant is part of
the run identity. No unannounced adjacency-row embeddings or fallback methods.
Unsupported 3D methods remain visibly unavailable until validated; padding a
2D result with zero coordinates is not a 3D implementation.

## 4. Evaluation contract

### 4.1 Shared targets

For a connected component, let `P` be its evaluated unordered vertex pairs,
`d_ij` the original shortest-path distance, `r_ij` the 3D Euclidean distance,
and `p_ij` the sum of embedded edge lengths along a fixed original shortest path.
Use identical pairs and fixed routes for all methods on that graph.

In the pilot, use all within-component pairs for basic distance scores where
resource limits permit. Store deterministic predecessor structures and stream
path summaries; do not materialize all paths as an O(n^3) list. Record the
tie-breaking convention for equal shortest paths. Near-tie route conventions
in GRIP must be matched explicitly, not assumed identical to another backend.

Expanded runs may use a frozen uniform pair sample with uncertainty estimates.
Distance-stratified diagnostics must be labeled separately or weighted back to
their stated population. Never pool exact and sampled results without labels.
Use exact neighborhood ranks in the pilot and a separately validated approximation
only later. Exclude cross-component pairs, reporting their number.

### 4.2 Scalar metrics and definitions

Primary comparison formulas below are proposed project definitions. Store a
formula/version identifier; map package outputs only after formula-level tests.

| Measure | Definition / interpretation | Better |
| --- | --- | --- |
| Euclidean distance error | `sqrt(sum((s*r-d)^2) / sum(d^2))`, with `s = sum(r*d)/sum(r^2)` fitted on P | Lower |
| Relative-distance stress | `mean(((s*r-d)/d)^2)`, fitting s for this inverse-square-weighted objective | Lower |
| Fixed-path geodesic error | `sqrt(sum((p-d)^2)/sum(d^2))`, identity scale as primary; separately named scale-fitted variant allowed | Lower |
| Edge-length error | Root squared edge residual sum divided by root squared target-length sum, identity scale; optional separately named fitted variant | Lower |
| Neighborhood error | Mean `1 - Jaccard(original neighborhood, embedded neighborhood)`; define neighborhood size/radius explicitly | Lower |
| Trustworthiness / continuity | Rank penalties for false embedded neighbors / lost original neighbors; freeze formula, valid neighborhood range, and ties | Higher |
| Distance-rank correlation | Spearman correlation between d and r; average ranks for ties | Higher |
| Cluster-distance preservation | Optional LGS-paper diagnostic using a frozen original-graph partition and its precise published definition | Lower |

For relative stress, `s = sum(r/d)/sum((r/d)^2)`. The LGS paper and its
implementation must be checked for sum-versus-mean and scale conventions before
any output is described as reproducing their score. Preserve distinct identifiers
for raw MDS objective, target-normalized error above, Kruskal Stress-1, and package
scores. Their denominators and scale fits need not agree.

Return a reasoned unavailable status for empty pair sets, invalid denominators,
constant ranks, or unsupported neighborhood sizes. A completely collapsed layout
must not receive a favorable score through a scale-fitting shortcut. Record zero
distances, invalid coordinates, and excluded observations; do not silently replace
nonfinite coordinates with zero. Do not introduce distance floors without naming
the resulting modified metric.

Component aggregation: primary distance errors pool their numerator/denominator
sums; relative stress pools pair contributions; neighborhood measures weight by
eligible vertices. Preserve per-component results. Never correlate arbitrarily
packed cross-component coordinates. Declare whether scale fitting is per component
(default for independently embedded components) or joint; these are different scores.

For unweighted graph neighborhoods, use complete hop-radius balls for the
Jaccard diagnostic and equal-cardinality embedded neighborhoods. For k-neighbor
rank measures, use stable-ID tie breaking, disclose its effect, and include a
tie-sensitivity test. Neighborhood sizes should form a curve (e.g. 5, 10, 20, 50
where valid), not an unexplained single number.

Cluster scores are secondary: freeze a deterministic graph-only community
partition, resolution, and seed. Do not recluster each embedding. Single-cluster
or degenerate configurations yield unavailable scores. Include partition sensitivity
before drawing substantive conclusions from this diagnostic.

### 4.3 Interpretation safeguards and additional diagnostics

Fixed-path error does not measure visual unfolding: two unit edges can fold
onto each other without changing their path lengths. Always pair it with chord
distance and neighborhood diagnostics. Recomputing shortest paths using embedded
edge lengths defines a different, optionally reported diagnostic.

Add short/mid/long-distance residual summaries, Shepard plots, edge-error
distributions, nonedge separation, and stochastic-repeat variability. Later
options include Sammon stress, explicitly named Kruskal stress, and co-ranking
curves. Screen-space edge crossings depend on the camera and are not a primary
3D score. Record runtime and peak memory separately from geometric quality.

### 4.4 Required validation examples

- Straight paths and exact small geometric examples: analytical distance/edge values.
- Folded path: fixed-path preservation with degraded Euclidean preservation.
- Cycles and graphs with tied shortest routes: deterministic path and rank policy.
- Translation, rotation, reflection: invariant distance scores; scale-fitted scores
  also invariant to uniform positive rescaling, identity-scale scores generally not.
- Known neighborhood intrusions/omissions; independent trustworthiness/continuity checks.
- Disconnected graph, isolates, two-vertex components, repeated coordinates,
  all-coincident coordinates, and nonfinite input.
- Compare optimized evaluators with a small independent brute-force evaluator.
- Check sampling against exact scores on small examples; record tolerance and seed.

Use declared numerical tolerances appropriate to the formula and precision.
No universal pass/fail threshold for the quality of a layout is prescribed.

## 5. Data, computation, and reproducibility

Separate canonical source data, derived graph assets, embedding runs, and metric
results. A proposed external project-data root is
`/Users/pgajer/current_projects/suitesparse_embedding_comparison`; it has not been
created. Downloaded archives, caches, and large outputs must not enter gflowui's
package source tree or Git history.

Versioned manifests should include:

- Graph: SuiteSparse identifier/URL, retrieval date, license/citation information,
  raw SHA-256, conversion recipe/version, stable vertex map, canonical edge and
  length hashes, component membership, original and derived counts.
- Prepared input: graph hash, shortest-path implementation/tie policy, pair set,
  landmark IDs/features, partition if used, and all relevant hashes.
- Embedding: graph/input hash, method/backend/version/source commit, dimension,
  parameters, seed, initialization hash, vertex order, coordinates checksum,
  convergence status, elapsed time, peak memory, environment, and logs.
- Evaluation: embedding hash, metric version, pair/path/neighborhood hashes,
  scale convention and fitted values, exact/sample status, counts, exclusions,
  score, uncertainty where appropriate, and failure reasons.

Use atomic writes and validate checksums on cache reads. Invalidate when graph,
preparation, algorithm, parameters, seed, or metric schema changes. Interrupted
or failed runs cannot be mistaken for completed assets. Display jobs as pending,
running, completed, failed, unsupported, or resource-limited; never invent scores.

Proposed initial per-job limits are 10 minutes and 2 GiB memory, with one heavy
worker at a time until machine resources are measured. Preflight dense allocations;
use an enforceable process-memory limit where supported, otherwise supervised
RSS monitoring with a documented possible overshoot. Freeze budgets before a
comparison and report stopped runs; a larger-budget rerun is a new run.

Start with three recorded seeds for stochastic methods and deterministic outputs
only once. Do not compare a best-of-many run with another method's single run
without stating the difference. Record thread counts and numerical environment.
Run computation outside Shiny's render path; cancellation must stop only the
requested worker and retain completed outputs.

## 6. gflowui interface and integration boundaries

### 6.1 Graph and embedding selection

Graph selector changes the graph, available layouts, Overview, and score cohort
together. Embedding selector exposes method, settings, replicate, input type,
availability, and convergence. Expose unavailable requested methods with a reason,
not a misleading selectable layout. Preserve stable vertex selections and colors
when changing layout for the same graph; clear incompatible selections on graph change.

Overview / Graph Data reports all available source metadata, conversion choices,
counts, components, degree summaries, references, and provenance. Distinguish
unknown metadata from zero. Original matrix thumbnails are optional attribution-
preserving assets, not ground-truth coordinates.

### 6.2 Embedding Quality in the General Inspector

- Sortable table: method, settings, replicate, input type, quality scores, runtime,
  memory, and status. Explicitly highlight the currently displayed run.
- Metric-versus-method plots: gray dashed horizontal line at the active run's
  score, labeled with method/settings. Hide it with an explanation if unavailable.
- Trade-off scatterplots: active-run marker with dashed coordinate guides.
- Neighborhood curves: active run drawn as a clearly identified dashed curve.
- LGS locality sweeps appear only after LGS acceptance.
- Expandable distance diagnostics and metric-definition text.
- Clicking a row or run point loads that run. Summary points representing several
  seeds require selecting a specific replicate rather than silently choosing one.
- Export full result tables, definitions, manifests, and reproducible figure
  specifications; figures need legends that fit and explicit better/worse directions.

Quality panels must retain scroll position, open/closed sections, and plot controls
through unrelated events. Camera rotation/zoom and divider movement must not
invalidate inputs, recompute scores, or rebuild the Inspector. Regression tests
should count evaluation calls and verify camera and accordion persistence.

### 6.3 Existing code and proposed ownership

Current read-only inspection identified `docs/gflowui_project_asset_contract.md`,
`R/project_registry_api.R`, and `R/quadform_benchmark_helpers.R` as starting points.
The quadform adapter already separates graph and layout assets, but is not a
drop-in general benchmark contract. Its permissive coordinate parsing must not
be reused as validation for scientific benchmark assets.

Implement a dedicated versioned embedding-comparison adapter and tests; generalize
shared helpers only with existing-project regression coverage. Avoid fake kNN
parameters for arbitrary graphs. Keep numerical method implementations outside
the UI layer; reuse public GRIP APIs after confirming exact semantics. New Python
adapters use an isolated environment and explicit serialization boundary rather
than changing the user's global Python/R libraries. Any GRIP code change requires
its own isolated branch, validation, and factual handoff.

## 7. Implementation phases and acceptance evidence

Each phase ends with committed source/tests, a private factual handoff, exact validation
commands/results, and limitations. Review before merging an isolated worker branch.
Commit/push at phase boundaries where a configured remote permits it; never force
push. Following every app update, start/verify a source-loaded instance and share
its actual URL. Do not claim a port is live without checking it.

Agent prompts, handoffs, audit reports, and coordination notes belong in the
private project folder, not Git or package contents. Keep public project design,
algorithm documentation, tests, and reproducible scientific assets in their
appropriate source locations. Do not add repository symlinks to private records.

### Phase 1 — Small, structurally varied scientific pilot (no LGS)

1. Inventory gallery metadata; freeze the justified under-3,000-vertex cohort and
   exclusions. Implement bounded downloads and tested graph conversion.
2. Define manifests, prepared input, a strict coordinate validator, and metric
   formulas. Implement independent small-graph metric tests before interpreting scores.
3. Validate the four baseline adapters, UMAP's distance route, and LLE's feature
   route in 3D. Preserve MDS and its edge-KK refinement as paired runs.
4. Run the frozen cohort within recorded resource limits and seeds. Produce machine-
   readable results and a brief findings/limitations report; record failures openly.

Exit evidence: reproducible graph/input hashes, structural coverage justification,
passing analytic/invariance/degeneracy tests, six validated adapter contracts,
finite correctly ordered coordinates for successful runs, and a truthful run matrix.
A small-component planar result is valid geometry, not evidence of a forced 2D backend.
No LGS code or LGS availability claim is required for this gate.

### Phase 2 — Usable project and comparison interface

Register the pilot project; implement selectors, Overview, quality table, reference
lines, trade-off/diagnostic plots, and exports. Load existing benchmark assets rather
than rerunning optimization during interaction.

Exit evidence: UI tests for graph/layout synchronization, unavailable metrics,
reference-line updates, failed jobs, camera and panel persistence; existing project
tests still pass. Provide a verified source-loaded app link and user walkthrough.

### Phase 3 — Broaden methods while keeping the small cohort fixed

Add PaCMAP, LocalMAP, TriMAP, PHATE, LargeVis, and NCVis one adapter at a time.
For each, pin a public implementation, inspect dimension/input support and licensing,
validate 3D numerics, and rerun the same metric protocol. Add richer neighborhood
and cluster diagnostics only after independent formula tests.

Exit evidence: method capability matrix and results for each accepted adapter;
unsupported implementations remain documented gaps. Do not mark all 12 non-LGS
methods implemented while any is only a 2D placeholder. A blocked adapter does not
erase successful results or prevent independent adapters from progressing.

### Phase 4 — Integrate independently reviewed 3D LGS

This phase depends on the parallel LGS work and Phase 1 contracts, not on completion
of every Phase 3 adapter. Import its accepted implementation through the same adapter
boundary. Test `k = 16, 32, 64, 128, 256`, clipped/deduplicated per component, plus
the all-neighbors endpoint where feasible; show `k/(component size - 1)`.

Exit evidence: upstream provenance/license, 2D reference comparison, 3D mathematical
tests and nonplanar example, resource measurements, adapter compatibility, and
locality-quality plots. Resolve paper/code discrepancies explicitly before acceptance.

### Phase 5 — Controlled expansion and publication-ready exports

Expand to additional gallery graphs up to 10,000 vertices/100,000 edges only after
pilot runtime/memory evidence. Introduce shared sampled evaluation where necessary,
validated against exact small cases, and method-specific admission estimates.

Exit evidence: metadata-only rejection of oversized graphs, truthful coverage and
resource-failure summaries, exact-versus-sampled agreement study, export bundle
with full provenance, and no performance regressions in ordinary 3D interaction.
Expansion does not require downloading every eligible graph simultaneously.

## 8. Parallel LGS work and integration contract

The main project owns graph conversion, frozen evaluation, UI, and method adapters.
The LGS worker owns only its experimental method implementation, tests, environment,
documentation, and example outputs in an isolated worktree. No shared package
installation, cache writes, or live-app restart. Worktree/branch must be provisioned
before launching that worker; the companion prompt gives exact proposed locations.

Exchange portable, versioned files: JSON request/response manifests, a vertex CSV,
an edge CSV with positive lengths, and a coordinate CSV keyed by vertex ID. Common
required run fields are method, dimension, parameters, seed, input checksum,
implementation commit, status, timings, coordinate checksum, and diagnostics.
An in-process API is optional; portable import remains the integration baseline.

The first LGS implementation targets simple connected unweighted graphs. Reject
unsupported weighted input explicitly. Main-project component handling wraps the
method. Weighted connectivity variants or fast approximations are later methods
unless their equivalence is established; do not silently broaden the algorithm.

## 9. Sources, present evidence, and limitations

Sources consulted for this design:

- [SuiteSparse sample gallery](https://sparse.tamu.edu/about).
- [LGS paper, version 2](https://arxiv.org/html/2308.16403v2).
- [Public LGS repository](https://github.com/JacobLMiller/L2G), including its README
  and `modules/cython_l2g.pyx`; the inspected optimizer is coordinate-specific to 2D.
- [Manifold-learning method definitions](https://scikit-learn.org/stable/modules/manifold.html).
- [Rank-based dimensionality-reduction evaluation](https://proceedings.mlr.press/v4/lee08a.html).
- Local GRIP manuscript:
  `/Users/pgajer/current_projects/manuscripts/grip_manuscripts/grip-software-paper-arxiv/build/grip-arxiv.pdf`.
- Local GRIP score documentation in
  `/Users/pgajer/current_projects/grip/R/gmds_layout_interface.R`, including
  `score.gmds()` scale and target conventions.

Repository observed while writing: `/Users/pgajer/current_projects/gflowui`, branch
`codex/ivue-audit-fixes`, commit `be09af1478947474de883a20b972d30702515055`.
These paths/commit describe inspected state, not a prescribed implementation branch.

Limitations and unverified claims: no gallery download or exact pilot selection has
been performed for this specification; no runtime, memory, backend compatibility,
or comparative-quality claim has been experimentally established. Transfer/job
limits, landmark count, and cohort size are proposed starting settings. The LGS
upstream commit is not yet pinned. Package-score equivalence and public 3D support
for the full roster remain implementation gates. No app code was modified, no app
was launched, and no new source-loaded app link is implied by this document.
