# Experimental LGS paper-form 3D adapter

This standalone research adapter implements `lgs-paper-union-v1`, the explicitly
selected paper-form variant with a genuine 3D optimizer. It retains a separate
2D upstream reference reproduction. It has not been integrated into gflowui.
[METHOD.md](METHOD.md) defines the mathematics and material paper/code differences;
[CONTRACT.md](CONTRACT.md) defines the portable JSON/CSV interface;
[EXPERIMENTS.md](EXPERIMENTS.md) reports the bounded locality experiment, including
its limitations and poor-quality scaling result.

## Reproduce locally

Use CPython 3.12 and a working C compiler. The recorded environment is Python
3.12.10 on macOS arm64; NumPy and Cython match upstream's versions. The numerical runtime uses three locked dependencies; the test environment also
installs a separately locked JSON Schema validator and its dependencies;
graph-tool and the full visualization stack are not installed. Small graph
interfaces and AST-extracted original function bodies replace those imports
for tests, as described in `lgs_reference/upstream.py`. This does not verify
upstream graph loading, graph-tool adjacency representation, community detection,
or the command-line drawing workflow.

From the repository root:

```sh
python3.12 -m venv research/lgs3d/.venv
PIP_CACHE_DIR="$PWD/research/lgs3d/.cache/pip" research/lgs3d/.venv/bin/python -m pip install -r research/lgs3d/requirements-test.lock
research/lgs3d/.venv/bin/python research/lgs3d/scripts/build_oracle.py
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 PYTHONPATH=research/lgs3d research/lgs3d/.venv/bin/python -m unittest discover -s research/lgs3d/tests -v
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 research/lgs3d/.venv/bin/python research/lgs3d/scripts/reproduce.py --output research/lgs3d/outputs/phase02
```

The last command runs the tests and writes `summary.json`, `tests.txt`, and
`environment.json` atomically. Each run has six graph fixtures (2–6 vertices),
60 epochs, walk depth 5, locality 1 and n-1 (deduplicated), repulsion 0.6 and
C seeds 17, 314, 2026; initial coordinates are frozen in the fixture file.
It retains raw final coordinates, neighborhood flags, pair schedules, objectives
and numerical differences in the summary. The separate test suite also checks
walk depths 1,2,5,10 and schedule budgets 1,2,5,30,60,200.

`results/phase02_summary.json` is a compact generated summary from a recorded
source commit; regenerate with the command above. Exact timestamps, timing,
RSS, compiler, and C-random schedules may differ by environment. Within the
recorded runtime, repeated compiled runs with explicit initial coordinates and
C seed are bitwise equal. Cross-platform bitwise reproducibility is not claimed.
The Python translation is compared with numerical tolerances, not byte identity.

## What the evidence establishes

The tests check normalized walk counts against independent walk enumeration,
reference neighbor selection across all valid k on each tiny fixture, pair
accounting, the compiled schedule and updates, and 2D finite-difference gradients
at steps 10^-4, 10^-5 and 10^-6 (absolute tolerance 3e-7, relative tolerance 3e-5).
A separate scalar evaluator checks the inferred optimizer potential and upstream
diagnostic cost. The original public Cython entry point is also compared with
its internally exposed SGD using identical random initial coordinates.

Documented counterexamples test claims that are **false of upstream**: paper/code
objective equivalence, power-decay preservation, unbiased shuffle, robust explicit
array initialization, safe collisions, permutation-invariant tied neighborhoods,
and perfect radius-2 neighborhood scores on a straight path. Passing those tests
means the discrepancy is reproducible, not that the behavior has been fixed.

The native 2D oracle is test-only and assumes valid tiny matrices. External
requests use `run.py`, which validates inputs and supervises the separate
paper-form worker with time and memory limits. The reference-reproduction script
itself remains a tiny serial diagnostic, not an external-input service.
No quality comparison among embedding methods is claimed.

## Source and artifact boundaries

* `vendor/L2G/`: unmodified selected upstream files; hashes in `upstream.json`;
  attribution and retained license in [NOTICE.md](NOTICE.md).
* `METHOD.md`: canonical specification and discrepancy record.
* `lgs_reference/`, `scripts/`, `tests/`, `fixtures/`: canonical reproduction code.
* `.venv/`, `.cache/`, `outputs/`: ignored local environments and generated files.
* `results/`: small generated scientific summaries only.

The upstream files preserve original whitespace and therefore produce warnings
under `git diff --check`; their bytes are kept unchanged for comparison. Build
warnings from deprecated NumPy C APIs and the local linker are recorded in the
build log; the extension loads and its native calls are exercised by tests.

The phase03 implementation preserves the selected paper-form pair objective with
genuine D-dimensional updates. The portable adapter, schemas, resource guards, cache and locality sweep are
separate from the unchanged native 2D oracle.
Package-build exclusion for this research directory is not changed here and
must be resolved by the integration owner before any merge.

## Paper-form numerical validation (phase 03)

The user selected the paper-form model after the joint phases 01–02 audit. This
variant uses decayed walk counts, union-symmetrized top-k neighborhoods, raw
squared distance attraction and logarithmic repulsion. Stable string IDs resolve
equal computed scores. The exact convention, schedule, pair backtracking,
collision and displacement guards, and termination semantics are in METHOD.md.
The unchanged upstream code remains an explicitly different comparison baseline.

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 research/lgs3d/.venv/bin/python research/lgs3d/scripts/validate_paper.py --output research/lgs3d/outputs/phase03
```

This runs the complete test suite and 36 paper-form runs: three tiny graphs,
locality 1 and n-1, dimensions 2 and 3, and seeds 17/314/2026. Starts are frozen
in `fixtures/paper_3d.json`. The run budget is 200 epochs; the implementation's
default remains 60. Tests cover finite differences at three step sizes, independent
objective agreement, stable-ID permutation behavior (including genuinely untied
scores), rigid transformations, analytical pair steps, raw-stress reduction,
collisions and numerical failures. The scalar evaluator in `lgs_paper/reference.py`
is separate from the vectorized kernel and optimizer in `lgs_paper/core.py`.
Only Problems produced by `prepare` are supported; constructing arbitrary
Problem records or calling low-level pair updates with malformed arguments is
not an external-input interface.

For the complete four-vertex graph at all-neighbors locality, the genuine 3D
start reaches a rank-three tetrahedron with squared distance residual near
machine precision. The 2D comparison cannot realize four mutually unit-distance
points. This checks dimensional behavior, not superiority on a graph cohort.
Paths and planar examples are not required to produce coordinate rank three.
All reported coordinates are raw; centering is only used to diagnose rank.

Pair descent need not give full-objective descent: objective-increasing epochs
are counted in every run. Movement stopping is a numerical criterion, not proof
of a global optimum. The optimizer reports floating-point stagnation separately.
A disconnected attractive-pair graph makes the positive-alpha logarithmic
objective unbounded below; such runs carry a warning instead of adding repair
constraints. The all-vertex collision guard costs O(n) per pair, so a full epoch
has O(n^3 D) worst-case work. The CLI resource enforcement and its limits are documented separately in CONTRACT.md.
`results/phase03_summary.json` records the committed-code numerical demonstration.

## Portable adapter and locality experiment (phase 04)

```sh
research/lgs3d/.venv/bin/python research/lgs3d/run.py research/lgs3d/fixtures/adapter_k4/request.json
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 research/lgs3d/.venv/bin/python research/lgs3d/scripts/locality_sweep.py --output research/lgs3d/outputs/locality-sweep
```

The first command returns finite raw 3D coordinates for a four-vertex complete
graph, preserving the deliberately unsorted input IDs. Repeat it to validate a
cache hit. See CONTRACT.md for hashes, all fields, failure statuses, limits,
atomic publication and interruption semantics. The full unittest command above
also exercises this CLI in isolated task-local temporary directories.

The serial sweep uses a 48-vertex path, a 7-by-7 grid, and two 24-vertex cliques
joined by one bridge; seeds 17, 314 and 2026; 60 epochs; walk depth 10, decay 0.1
and repulsion 0.2. Requested k values 16,32,64,128,256 are clipped/deduplicated,
including n-1. Starts match across k for each graph/seed. Every run is reported,
including failures; there is no best-seed selection. Basic distance, identity
edge and radius-1/radius-2 neighborhood errors use independently checked formulas.
The script uses measured runtime with a factor-two cubic projection to admit or
skip optional 128- and 2,000-vertex scaling runs under the same limits. This
preflight is conservative evidence, not a performance guarantee.

Measured results and limitations are in [EXPERIMENTS.md](EXPERIMENTS.md), including
the preserved initial failure and the poor-quality 128-vertex result.

## Final reproducible demonstration (phase 05)

After installing the test lock, run from the repository root:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 research/lgs3d/.venv/bin/python research/lgs3d/scripts/demonstrate.py --output research/lgs3d/outputs/demonstration
```

The runner rebuilds the native reference oracle, runs the complete test suite,
and exercises the JSON/CSV CLI on the four-vertex complete graph at all-neighbors
locality in both 2D and 3D for seeds 17/314/2026, with 200 epochs available. Frozen
starts come from `fixtures/paper_3d.json`; the 2D starts use its first two columns.
It preserves declared unsorted ID order, validates schemas and checksums, checks
cache reuse, and repeats one 3D calculation in a separate output directory.
Use a fresh output directory to measure a new run; subsequent invocations can
reuse valid caches, and the summary explicitly records this.

The independent six-pair residual, centered coordinate rank and tetrahedron
volume check show that the 3D result is nonplanar and realizes the unit targets.
The 2D residual remains positive. This is an analytical dimensional check; it
does not erase the poor 128-vertex result or establish quality on other graphs.
The runner writes requests, raw coordinates/manifests, build/test logs and a
compact `summary.json`. The recorded public `results/phase05_summary.json`
identifies its source commit. App integration and package-build exclusion remain
separate work for the main project owner.
