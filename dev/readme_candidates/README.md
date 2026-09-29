# README graph candidates

Prepare small real graphs for visual selection in the existing SuiteSparse 3D
Embedding Comparison project. This cohort supplements the original frozen
gallery; it is not restricted to matrices pictured on the SuiteSparse about page.

The runner first saves the collection metadata CSV and all square matrices with
400–500 rows. It then admits six examples selected for application diversity:
494_bus (power), bcsstk06 and dwt_492 (structural), lshp_406 (thermal), west0479
(chemical process), and oscil_dcop_01 (circuit). Known duplicate and repeated
sequence members are avoided. Connectivity is checked after conversion; no
vertices are silently dropped and no repair edges are added.

Conversion uses the existing audited numerical-support importer: duplicate
entries summed, numerical zeros and diagonal loops removed, off-diagonal support
symmetrized, and every retained edge assigned length one. Original archives,
metadata, attribution and checksums are retained outside the package source.

Run `run.py` with the existing SuiteSparse Python environment. It uses local grip
source, full metric MDS with native SGD, random initialization, uniform pair
weights, 1,000 iterations and seeds 11, 29, 43. No edge-KK follows. Saved records
include wall time for fitting/preparation/scoring, fit-only time, termination
metadata, warnings, coordinates and exact within-graph evaluation. The reused
scorer reports scale-fitted chord error, relative stress, edge error, fixed-route
error, distance ranks and neighborhood measures. These differ from the optimizer
objective. Memory usage is not measured and is recorded as unavailable.

Outputs live in `/Users/pgajer/current_projects/suitesparse_embedding_comparison`:
`readme_candidates/`, `readme_candidate_results.json`, `readme_candidate_cohort.json`
and `readme_combined_cohort.json`. Existing results are not overwritten. Export
with all previous result indexes plus the new index, using the new combined
cohort. Preserve the old viewer manifest before activation.

The README's interim 494_bus fit uses the public default initialization and seed
11 requested by the user; candidate runs explicitly use random initialization.
They must not be described as identical fits. 494_bus is already bundled in
`grip::zheng.graphs`, including source metadata and unit-length conversion rules.
The user will choose the final showcase after inspecting the candidates.

Final admission: four connected graphs and 12 fits; dwt_492 and oscil_dcop_01 were disconnected and excluded. `report.py` writes the admission and results summaries, and `activate.py` stages and merges candidate assets into the existing viewer while preserving previous run IDs. `launch.R` opens gflowui on port 3888 by default.
