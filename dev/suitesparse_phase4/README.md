# Accepted experimental LGS integration

This bridge executes the independently accepted `lgs-paper-union-v1` at commit
`1a89dfd350591f25ef2c68b1037df316dd84416c`. It does not change the numerical
algorithm or claim equivalence to the original 2D L2G optimizer. Upstream is
Jacob Miller's L2G at `9af0e3be0d30c40bf46cc3b6f952187268c3e22d`, BSD-3-Clause;
the dependency preserves its NOTICE, license, mathematical discrepancies and
reference reproduction. The main project's unit-edge graph and exact metric
contract remain the targets. CLI raw coordinates are neither scaled nor padded.

## Private dependency setup

Create a detached worktree at the accepted commit in a new private directory.
Do not use or modify the separate LGS implementer's checkout. In this dependency
worktree create `research/lgs3d/.venv` with Python3.12 and install its
`requirements-test.lock`. The dependency's tests expect that exact relative
environment location. Run its `scripts/demonstrate.py --output PRIVATE_OUTPUT`
with OPENBLAS_NUM_THREADS=1 and OMP_NUM_THREADS=1. This rebuilds the native 2D
test oracle, runs all56tests, and repeats the 2D/3D tetrahedron/cache checks.
Generated dependency build products stay ignored and private; no LGS source
is added to the R package build or merged into this branch.

The main runner uses the Phase3 Python environment for graph preparation and
scoring, and the separate locked dependency Python for LGS. Run from the main
gflowui checkout after committing source, with one job at a time:

    python dev/suitesparse_phase4/run_lgs.py DATA_ROOT/lgs_validation --mode validation --dependency PRIVATE_DEPENDENCY --python PRIVATE_DEPENDENCY/research/lgs3d/.venv/bin/python
    python dev/suitesparse_phase4/run_lgs.py DATA_ROOT --mode gallery --calibration DATA_ROOT/lgs_validation/phase04_validation_results.json --dependency PRIVATE_DEPENDENCY --python PRIVATE_DEPENDENCY/research/lgs3d/.venv/bin/python

The validation graph collection is synthetic and never inserted into the
SuiteSparse gallery selector. Paths, grid, joined cliques, tetrahedron, and a
two-component graph with an isolate exercise the same portable bridge and
main-project score/assembly pipeline. Seeds17,29,43 are kept, with one128-vertex
path calibration. Localities16,32,64,128,256 and n-1 are clipped and deduplicated
by the entire component vector. Isolates are explicitly placed, not attributed
to LGS. Keep actual k and k/(component size-1), not just the requested cap.

## Resource and scientific limits

Gallery admission uses the maximum observed connected calibration (n>=32)
seconds/n^3, doubled,
and summed over nontrivial components. This is a conservative extrapolation,
not a measured gallery runtime or guarantee. The accepted kernel has an
O(n^3 D) worst-case collision guard, <=2000vertices/component and
600seconds/2048MiB limits. Exclusions remain rows with reasons, not successful
layouts. No smaller epoch budget or substitute optimizer is used to force
gallery completion. Failed attempts are retained in distinct directories.

The paper-form objective can be unbounded below when the attractive constraints
are disconnected and repulsion is positive. Safeguarded pair steps do not imply
full-objective descent. Poor128-vertex quality and objective growth are retained.
Locality changes the objective, so compare external graph metrics, not raw
objective values across k. Finite coordinates do not establish convergence.

The bridge checks process exit/status, accepted source/runtime, graph/input
hashes, parameters, coordinate checksum/path containment and exact vertex order.
Both the main graph hash and distinct csv-graph-v1 identity are recorded.
The dependency's own response and complete objective history remain in results.
No shared cache writes or library installation occurs during app viewing.
