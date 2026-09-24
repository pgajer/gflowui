# Additional 3D embedding adapters

This experiment keeps the Phase 1 gallery cohort and evaluation contract fixed.
It adds six public backends through the shared, deterministic 64-landmark
distance features. The input is not an adjacency-row embedding. Neighborhoods
inferred internally by a backend never replace the original graph used for
display or scoring. Components with fewer than five vertices retain the
explicitly identified classical placement from the pilot.

## Backends and interpretation

| Method | Pinned implementation | Input/optimizer choices | License |
| --- | --- | --- | --- |
| PaCMAP | pacmap 0.9.1 | 3D, seeded Annoy, random initialization, 100/100/250 iterations | Apache-2.0 |
| LocalMAP | pacmap 0.9.1 | Same route; local-distance threshold 10 | Apache-2.0 |
| TriMAP | trimap 1.1.5 | 3D; exact feature kNN via public `knn_tuple`; NumPy and serial/parallel Numba RNGs seeded; 400 iterations | Apache-2.0 |
| PHATE | phate 2.0.0 | 3D; automatic diffusion time; no additional PCA/landmarks; metric SGD-MDS | GPL-2.0 |
| NCVis | d5c8b96e3b3bb131e12cf2d46daa8635ae1ce339 | 3D; one thread; 50 epochs and 20 initialization epochs | MIT |
| LargeVis | feb8121e8eb9652477f7f564903d189ee663796f | 3D; feature input; one thread; 20 million sampled edges | Apache-2.0 |

Sources: [PaCMAP/LocalMAP](https://github.com/YingfanWang/PaCMAP),
[TriMAP](https://github.com/eamid/trimap),
[PHATE](https://github.com/KrishnaswamyLab/PHATE),
[NCVis](https://github.com/stat-ml/ncvis),
[LargeVis](https://github.com/lferry007/LargeVis).

TriMAP 1.1.5 pins its established Numba implementation; 1.2.0 also imports a
different Torch implementation. Exact feature neighbors avoid an uncontrolled
Annoy seed and are an explicitly named input variant, not a claim to reproduce
the default approximate-neighbor pipeline. PaCMAP/LocalMAP apply one additional
global scalar normalization and feature centering internally, not per-column
variance normalization. Their upstream warning that non-2D results have not
been thoroughly tested is preserved. Our tests establish finite, rank-three,
repeatable output on a graph-derived 125-vertex cube; they do not establish
general embedding quality. PHATE's selected diffusion time is recorded; its
internal potential distances are not evaluation targets. Actual MDS stopping
iteration is not exposed, and convergence warnings must not be suppressed.

## Private environments and native builds

Create a separate Python 3.12 virtual environment and install `requirements.txt`.
Do not modify the accepted pilot environment or global R/Python libraries.
`build_native.py` requires a clean official source checkout at the pinned commit
and a previously nonexistent destination. It copies source before applying
explicit compatibility changes, records resulting file hashes, and installs
NCVis only into the Python environment running the command.

NCVis needs Cython, NumPy, pybind11, setuptools and wheel. Its macOS build recipe
uses Homebrew LLVM libomp instead of upstream Intel iomp5 and declares the
noise-plan memoryview as `size_t` to match the C++ API under current Cython.
The noise-plan array remains NumPy uintp; no optimizer expression is changed.
The first unmodified wrapper build failed with a uintp-pointer/size_t-pointer
type mismatch. This is a disclosed compatibility patch, not an upstream wheel.

LargeVis needs a private GNU GSL installation (tested with 2.8). Build GSL from
its official tarball using `configure --prefix=PRIVATE_PREFIX`, `make`, and
`make install`; never use a system prefix or sudo. The recipe applies the
upstream-documented macOS `lseek64` to `lseek` replacement, corrects one printf
integer format, and permits the `GFLOWUI_LARGEVIS_SEED` environment override.
Without that override, the original seed 314159265 remains unchanged. The
Python adapter sets it to the requested nonnegative seed. Original spatial
update expressions are unchanged. Set `GFLOWUI_LARGEVIS_BINARY` to the resulting
executable. The backend's default small-data budget is actually one billion
edge samples; this experiment explicitly uses 20 million and makes no
convergence claim. Preserve compiler/linker warnings with the build log.

## Validation and execution

Run `test_adapters.py` and the Phase 1 tests. `validate_adapters.py OUTPUT`
launches independent subprocesses for seeds 17, 17 and 29 on a nonplanar cube,
checks input nonmutation and coordinate rank, compares same-seed coordinates
to absolute tolerance 1e-10, and requires different-seed output to differ.
One supervised job runs at a time, with the same 600-second/2-GiB limits as the
cohort. Failure preserves the complete trial and does not pass the contract.

After committing source, `run_cohort.py DATA_ROOT --contracts CONTRACT_JSON
--native-manifests NCVIS_BUILD_JSON LARGEVIS_BUILD_JSON` runs seeds 17/29/43 for
every validated method on every admitted graph. Jobs reuse the Phase 1 exact
component preparation, coordinate validation and metrics. Cache identity
includes source, installed versions, native build files, binary hash, graph,
contract, settings and seed. Failed runs are retained, not overwritten. The
output `phase03_results.json` is separate from all frozen pilot indexes.

Use the existing viewer exporter with all four result indexes to expose new
layouts. A completed adapter contract does not imply every cohort job succeeded;
retain resource failures, warnings and unsupported statuses in the comparison.
