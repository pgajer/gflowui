# Experimental LGS source interpretation and 2D reproduction

This directory contains the phases 01–02 source interpretation and 2D reference
reproduction, plus the phase03 paper-form numerical implementation. The explicitly
selected variant is `lgs-paper-union-v1`; it has a genuine 3D optimizer and retains
the separate 2D upstream baseline. It has not been integrated into gflowui. There
is no portable request adapter or performance benchmark yet. See [METHOD.md](METHOD.md) for material paper/code differences
and the explicitly named selected paper variant.

## Reproduce locally

Use CPython 3.12 and a working C compiler. The recorded environment is Python
3.12.10 on macOS arm64; NumPy and Cython match upstream's versions. Only the
three dependencies needed for the numerical reproduction are installed;
graph-tool and the full visualization stack are not installed. Small graph
interfaces and AST-extracted original function bodies replace those imports
for tests, as described in `lgs_reference/upstream.py`. This does not verify
upstream graph loading, graph-tool adjacency representation, community detection,
or the command-line drawing workflow.

From the repository root:

```sh
python3.12 -m venv research/lgs3d/.venv
PIP_CACHE_DIR="$PWD/research/lgs3d/.cache/pip" research/lgs3d/.venv/bin/python -m pip install -r research/lgs3d/requirements.lock
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

No invalid-input or resource-safe adapter is provided. Test-only native entry
points assume valid tiny matrices and can crash on malformed inputs. Do not
use them for external requests. Runs here are tiny, serial and normally take
seconds; the reproduction report records process peak memory and elapsed time.
No 2-GiB RSS supervisor is implemented at this phase. The phase-04 adapter must
enforce the proposed 600-second/2-GiB job limits and test interruptions/timeouts
before any scaling run. No large graph downloads or scaling runs are needed
for this reproduction. No quality comparison among embedding methods is claimed.

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
genuine D-dimensional updates. The eventual adapter contract, schemas, resource guards,
cache, locality sweep and scientific demonstration belong to later phases.
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
has O(n^3 D) worst-case work. No scaling or resource-enforcement claim is made.
`results/phase03_summary.json` records the committed-code numerical demonstration.
