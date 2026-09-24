# Controlled expansion

The original four-graph cohort, coordinates and exact results stay unchanged.
`admit_graphs.py` screens the saved49-entry gallery against <=10000vertices and
100000full numerical nonzeros, selects only the two smallest additional eligible
graphs, then applies the same bounded download and numerical-support conversion.
Archive and extraction ceilings remain100MiB/1GiB. No MST repair or subsampling.

`run_expansion.py` admits methods independently using the largest completed
same-method graph and worst replicate:1.1 times quadratic peak-RSS scaling and
1.25 times component-summed cubic elapsed-time scaling. These are conservative
projections, not measured outcomes. Later graphs may use completed earlier
expansion results as references. Every exclusion remains a terminal row; no
time/memory ceiling is increased. LGS's accepted2000-vertex limit still applies.
Existing validated adapters and settings are unchanged. One worker at a time.

## Evaluation

Version `suitesparse-uniform-pairs-v1`: up to20000uniform unordered pairs without
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

Approximate95% conditional sampling intervals use200joint-pair bootstrap draws,
refit both scales, and correct component-sum deviations by
sqrt((1-m/N)*m/(m-1)) before graph aggregation. Negative corrected sums are bounded
at zero. Correlation deviations use the same correction and are bounded to[-1,1].
The2.5/97.5percentiles are approximate, not guaranteed95% coverage. Full-population
components are fixed in the bootstrap. Streams differ across disjoint component
IDs; they are shared across methods for comparability. This uncertainty is
conditional on fixed coordinates, not optimizer-seed variability or future graphs.

`test_expansion.py` compares full samples with the original evaluator, tests ties,
degeneracies and population-weighted aggregation. `validate_sampling.py` compares
20independent samples with six retained exact pilot layouts and reports every
estimate/interval; it does not overwrite original results or select favorable seeds.

All commands use the existing isolated Phase03Python environment. Paths/contracts
are explicit CLI arguments. Generated graphs, jobs, reports and publication figures
belong in the external project-data directory, not this source tree.
