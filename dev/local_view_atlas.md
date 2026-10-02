# Local-view atlas

The atlas separates frozen dataset membership from the graph and embedding used
to display it. Enable it through `metadata$local_views`, with `enabled = TRUE`,
a `vertex_namespace`, and the existing `metadata$vertex_hover$abundances_file`
containing the complete sparse abundance table. No project names or local paths
are encoded in application logic.

An optional `import_manifest` points to a registered source collection whose graph
sets declare `anchor`, `neighborhood`, unique `vertex_ids`, and saved graph/layout
assets. `import_label` controls the button caption. Import validates every graph's
membership and declares shared `asset_paths` in the destination manifest so project
Trash retains files used by both projects. The source project remains intact.

The managed state file is `projects/<project-id>/local_views/atlas.rds` under the
configured gflowui data directory. Each region records a name, ordered dataset
IDs, membership fingerprint, definition, creation date, distance scope and saved
views. Repeated imports merge matching regions and preserve locally computed views, names,
retirement and revision history; they do not duplicate files. Saving uses a short
transaction lock and writes state atomically. Registered project manifests keep
the full-dataset graph inventory; local navigation transforms only the session's
view of that manifest.

Local views appears after Basins. Anchor neighborhoods use one-source abundance
Euclidean, Hellinger or square-root Jensen–Shannon distances against the entire
dataset, regardless of display filtering. Neighborhood size includes the anchor,
and stable IDs break ties. dCST regions use the selected classification in the
current graph, frozen as a union or separate groups. Parent previews highlight
membership in Plotly and can filter other vertices; they do not recompute geometry.
Imported fits use their original assets. Endpoints remain dataset-scoped, while
arms stay specific to the selected graph.

## Local calculations

Saved regions support centered PCA and 3D SGD metric-MDS. MDS targets use ambient
Euclidean, normalized Hellinger or square-root Jensen–Shannon distance, complete
unrooted Fermat paths, or ordinary symmetric-kNN paths with component-MST repair.
All intermediates and landmarks belong to the region. Original components and
repair edges are saved. PCA has only coordinate settings; any displayed tree is
a visualization scaffold, not a fitted target. Ordinary kNN ignores Fermat power.

Full distances are the automatic choice up to 1,200 samples; larger regions use
landmark-to-all targets. Both use inverse-squared target weights. Landmark count,
SGD iterations, power, neighbors, seed and workspace budget are configurable.
The budget is a conservative admission estimate, not a hard operating-system
memory limit. Zero nonself MDS targets are rejected with an explicit error.
Jensen–Shannon Fermat currently builds an explicit complete graph and can require
considerably more memory. Display edges are capped at 20,000; stored distances and
full graph assets are unaffected.

Jobs run one at a time in supervised background R processes, with stage progress
and cancellation. They continue when navigating to another project. Closing the
server interrupts running work; interrupted jobs require resubmission. Frozen
input, settings and software hashes identify the cache; damaged assets trigger
a new attempt. Failed/cancelled partial output is never registered as a view.
Fixed-budget SGD results include warnings and target reconstruction error; these
are fitting diagnostics, not held-out accuracy or proof of convergence.

## Anchor charts and coverage

For a composition anchor a, q = a / ||a|| and z(x) = x / (qᵀx) − q. All feature
columns and the full anchor vector are frozen. A pure-feature anchor recovers
nonreference ratios. General coordinates may be signed, so only Euclidean
distance is allowed. Preview the denominator coverage at a configurable threshold.
The default stops if any sample is uncovered; explicit exclusion fits retained
samples while preserving original region membership and exporting every coverage
decision. At least four covered samples are required.

## Revisions and replay

Preview one new membership and choose a saved region to revise. The new revision
has a new ID and no inherited fits; earlier membership and views remain available.
Rename and reversible retirement do not remove assets. Include retired regions to
restore them. Refreshing imports preserves revisions and computed fits.

Each newly computed view offers a ZIP reproducibility bundle: input data, stable
IDs, coordinates/coverage specification, distance targets and landmarks, graph,
repair metadata, layout, diagnostics, software fingerprints and exact atlas source.
SHA-256 checksums are verified before export/replay. The included reproduce.R works
after relocation without access to the original abundance file. It sources the
included calculation functions; R and calculation dependencies must match recorded
versions and binaries. The gflowui installed helper version need not match.
Bundles contain the region's original abundance data and should be handled as data.

Still pending: fitting local regions using full-dataset path distances; overlap
summaries, linked highlighting and alignment/comparison on shared samples. The
existing 10D routes can be imported, but new jobs currently fit directly in 3D.

Validation: `tests/testthat/test-local-view-atlas.R`, with related endpoint-set,
dCST-focus, graph-neighbor-control and project-Trash tests. The research design
and registration script maintain the concrete comb-V3V4-tx deployment.
