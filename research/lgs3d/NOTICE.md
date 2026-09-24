# Attribution and retained source

The unchanged files under `vendor/L2G/` are from Jacob Miller's L2G repository,
https://github.com/JacobLMiller/L2G, commit
`9af0e3be0d30c40bf46cc3b6f952187268c3e22d`, retrieved 2026-09-24.
The full BSD 3-Clause license and copyright notice are in `vendor/L2G/LICENSE`.
This is a selected source snapshot, not the complete upstream repository.

`lgs_reference/reference2d.py` is an attributed translation of the upstream
neighborhood, schedule and optimizer behavior. `scripts/build_oracle.py` appends
test wrappers to a copy of the original Cython source, leaving its algorithm
unchanged. The independent evaluator is a separate scalar derivation of the
formulas in METHOD.md. This work is not a clean-room implementation.

Paper: Jacob Miller, Vahan Huroyan, Stephen Kobourov, *Balancing between the Local
and Global Structures (LGS) in Graph Embedding*, arXiv:2308.16403v2,
https://arxiv.org/html/2308.16403v2 .
