# Pair-state graph integration

The optional `metadata$state_graphs` catalogue supplies a frozen reference,
canonical state IDs, sample membership, candidate edges, witnessed faces, fixed
coordinates and fitted layouts. No project name or dataset path appears in the
app implementation. `R/state_graphs.R` defines construction and unit-distance
fitting; `R/mod_state_graphs.R` manages controls, persistence and linked selection.

## Reproduction

From the gflowui checkout, run `Rscript dev/phase2g/prepare-copy.R` with the
arguments shown in that script, followed by `Rscript dev/phase2g/validate.R`.
The preparation validates all 20 cached graphs before saving. Validation also
compares new support thresholds with `linf::linf.udcst.graph()` and checks exact
witness intersections. `devtools::test(filter="state-graphs")` exercises edge,
isolate, tie, membership, empty-selection and persistence behavior.

Run the app against the separate copied registry for browser tests. Deployment
uses `Rscript dev/phase2g/deploy.R COPY_REGISTRY NEW_BACKUP_DIRECTORY`: it backs
up current live mutable assets and adds only the catalogue and its initial
settings, checks for concurrent manifest changes, and verifies preservation of
all existing fields. Never copy tested user settings back over live settings.

## Scope

Samples, state vertices and edge witnesses have distinct IDs. State coverage
uses complete frequency ties in the frozen reference. Unit edge lengths define
shortest paths; visual support widths do not change distances. New fits run in
a background R process, separately within components, using grip full SGD,
uniform pair weights, 500 passes and three starts. Between-component placement
is arbitrary. The pass limit does not establish convergence.

Linked views use Plotly; Samples view retains the chosen renderer. A state
sample filter intersects other filters and remains visibly clearable in Samples
view. Selections, cameras, graph choices and named variants persist. Opening a
local region saves exact reference IDs; generating new sample embeddings is a
separate workflow. Explicit reference reconstruction re-tabulates the saved pure assignments on
currently visible sample IDs through linf::linf.udcst.graph(). It preserves the
original classification and dictionary; filtering never silently changes graph evidence.
