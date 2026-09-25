# Controlled expansion

The original four-graph cohort, coordinates and exact results stay unchanged.
`admit_graphs.py` screens the saved 49-entry gallery against <=10,000 vertices and
100,000 full numerical nonzeros, selects only the two smallest additional eligible
graphs, then applies the same bounded download and numerical-support conversion.
Archive and extraction ceilings remain 100 MiB/1 GiB. No MST repair or subsampling.

`run_expansion.py` admits methods independently using the largest completed
same-method graph and worst replicate: 1.1 times quadratic peak-RSS scaling and
1.25 times component-summed cubic elapsed-time scaling. These are conservative
projections, not measured outcomes. Later graphs may use completed earlier
expansion results as references. Every exclusion remains a terminal row; no
time/memory ceiling is increased. LGS's accepted 2,000-vertex limit still applies.
Existing validated adapters and settings are unchanged. One worker at a time.

## Evaluation

Version `suitesparse-uniform-pairs-v1`: up to 20,000 uniform unordered pairs without
replacement per connected component, deterministic component-ID-derived seed,
identical across methods/replicates. Pair indices and little-endian int64 hashes
are saved. All pairs are used when fewer exist. Original exact graph distances,
predecessors and landmark features remain the backend inputs.

Both Euclidean and relative scales are fitted on the sample separately per
component. Multiply each sampled sum by N/m before pooling component sums, where
N is its full pair population and m its sample size. This estimates the same
full-pair objectives but is not an unbiased estimator of a nonlinear profiled
ratio. Fixed-path lengths follow the retained source-to-target predecessor route.
Edge errors/quantiles and streamed lexical-ID neighborhood ranks are exact.
Cross-component distances are excluded. Spearman correlation is sampled and
remains component-specific when disconnected. Distance-band summaries are
explicitly unavailable, not silently redefined by sampled quantiles.

Approximate 95% conditional sampling intervals use 200 joint-pair bootstrap draws,
refit both scales, and correct component-sum deviations by
sqrt((1-m/N)*m/(m-1)) before graph aggregation. Negative corrected sums are bounded
at zero. Correlation deviations use the same correction and are bounded to[-1,1].
The 2.5/97.5 percentiles are approximate, not guaranteed 95% coverage. Full-population
components are fixed in the bootstrap. Streams differ across disjoint component
IDs; they are shared across methods for comparability. This uncertainty is
conditional on fixed coordinates, not optimizer-seed variability or future graphs.

`test_expansion.py` compares full samples with the original evaluator, tests ties,
degeneracies and population-weighted aggregation. `validate_sampling.py` compares
20 independent samples with six retained exact pilot layouts and reports every
estimate/interval; it does not overwrite original results or select favorable seeds.

All commands use the existing isolated Phase 03 Python environment. Paths/contracts
are explicit CLI arguments. Generated graphs, jobs, reports and publication figures
belong in the external project-data directory, not this source tree.

## Authorized higher-memory MDS reruns

`run_mds_budget.py DATA_ROOT` retries metric MDS and its matching edge-KK refinements
on nemscem, sstmodel and circuit_2, for seeds17,29,43. The revised allowance is
30 GiB for summed worker/descendant RSS, with **no elapsed-time limit**. Jobs run
serially, with a nonblocking process lock preventing two copies of this runner.
The memory watchdog remains active; polling can overshoot the ceiling. Existing
optimizer iteration limits and convergence tolerances are unchanged.

Original results and their 2-GiB/600-second limits remain immutable. New requests
record the earlier index/manifest identities; `mds_30gib_results.json` is separate.
Nemscem retains exact evaluation; the two expanded graphs retain the shared-pair
evaluator. Refinements require the matching new MDS seed, never another run.
Historical time projections do not exclude the explicitly authorized retries.
Known input allocations must still fit the memory budget. This entry point does
not change the frozen pilot/expansion policies or rerun unrelated methods.

`--graphs` can select still-unrecorded cases. An already recorded graph is rejected
rather than overwriting history. An interrupted directory is also preserved.
