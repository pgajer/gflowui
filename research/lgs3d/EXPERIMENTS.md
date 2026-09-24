# Bounded paper-form locality experiment

The question is how the selected locality count changes basic distance and
neighborhood quality under a fixed, finite optimization budget. These are
standalone synthetic checks of `lgs-paper-union-v1`, not a comparison with other
methods or evidence from the SuiteSparse graph collection.

We used a 48-vertex path, a 7-by-7 grid, and two 24-vertex complete graphs joined
by one edge. Each graph/locality setting used all three seeds (17, 314, 2026),
60 epochs, 3D canonical Gaussian starts, walk depth 10, decay 0.1 and repulsion
0.2. Starts match across locality choices within each graph and seed. Requested
localities 16,32,64,128,256 were clipped/deduplicated to 16,32,n-1. The last setting
contains only raw squared-distance attraction. No seed was discarded or selected
for its quality. Exact settings, source identity, hashes and every run are in
`results/phase04_summary.json`; regenerate with `scripts/locality_sweep.py`.

## Results

Every one of the 27 corrected runs produced finite coordinates within the
resource budget; every optimizer used its full 60 epochs. Completion therefore
does not establish convergence. Values below are means over the three seeds;
brackets show the minimum and maximum Euclidean errors. Lower values are better.
Euclidean error fits one global scale against all graph distances; relative
stress fits its own inverse-distance-weighted scale. Neighborhood errors compare
complete hop balls with equal-size embedded nearest sets (CONTRACT.md).

| Graph | Locality k / fraction | Euclidean error [range] | Relative stress | Hop-1 error | Hop-2 error |
| --- | ---: | ---: | ---: | ---: | ---: |
| Path (48) | 16 / 0.340 | 0.01250 [0.01176, 0.01363] | 0.00022 | 0.00000 | 0.00000 |
| Path (48) | 32 / 0.681 | 0.00167 [0.00166, 0.00170] | 0.00001 | 0.00000 | 0.00000 |
| Path (48) | 47 / 1.000 | 0.00037 [0.00036, 0.00038] | 0.00000 | 0.00000 | 0.00000 |
| Grid (49) | 16 / 0.333 | 0.11351 [0.11343, 0.11357] | 0.02281 | 0.00544 | 0.04802 |
| Grid (49) | 32 / 0.667 | 0.11892 [0.11880, 0.11902] | 0.02363 | 0.00272 | 0.07208 |
| Grid (49) | 48 / 1.000 | 0.10799 [0.10700, 0.10915] | 0.02324 | 0.00952 | 0.10823 |
| Joined cliques (48) | 16 / 0.340 | 0.30268 [0.30243, 0.30302] | 0.41633 | 0.00278 | 0.06389 |
| Joined cliques (48) | 32 / 0.681 | 0.14346 [0.14155, 0.14651] | 0.05879 | 0.00056 | 0.00000 |
| Joined cliques (48) | 47 / 1.000 | 0.12375 [0.12331, 0.12403] | 0.06696 | 0.00000 | 0.00000 |

Increasing locality improved the path's distance errors, but quality was not
monotone across all metrics or graphs. In the grid, hop-2 neighborhood error
increased even though the all-neighbors endpoint had the lowest mean Euclidean
error. For joined cliques at k=16, the attractive constraint graph split into two
components: all three runs report that the positive-repulsion objective is
unbounded below. Their finite-budget metrics remain reported, with that warning.
Across all 27 runs, the full objective increased in 496 epochs; safeguarded pair
steps do not imply full-objective descent.

Numerical-job elapsed times were 1.90–3.59 seconds, and measured
combined supervisor/worker memory was 66.2–67.1 MiB. No completed run
exceeded its 600-second/2,048-MiB cap. Supervision is sampled, not an allocation
ceiling; the response records poll gaps and any observed overshoot.

## Scaling check and limitations

The conservative factor-two cubic runtime projection admitted a 128-vertex path
(k=16, seed 17, same 60 epochs). It completed in 13.41 seconds using 69.4 MiB,
but quality was poor: fitted Euclidean error 0.5572, relative stress 0.5669,
and identity edge error 32.52. Its raw objective increased from about 333,000
to 7,098,000. This establishes execution under the budget, not useful layout
quality or convergence at that size. No parameter tuning or diagnostic rerun
replaces this result.

The 2,000-vertex run was not attempted: the declared projection was about
519,490 seconds, well above 600 seconds (estimated memory alone was below the
cap). This is a conservative extrapolation rather than a measured runtime.
No large graphs, approximate kernels, alternate embeddings or pilot comparison
were run. Fixed-path, community and rank metrics are outside this basic subset.

The initial sweep at commit 0069c6d stopped during preparation in all 27 cases,
because BLAS status flags indicated nonfinite arithmetic despite finite product
entries. Its separate `results/phase04_initial_failed_sweep.json` is retained.
The corrected sweep at commit 0930a20 checks actual products and retains genuine
overflow rejection; METHOD.md documents the change and regression. Later
adapter metadata records also identify OS release and BLAS build configuration;
that addition changes cache identity, not numerical results. The submitted
source is tested again after this metadata addition.
