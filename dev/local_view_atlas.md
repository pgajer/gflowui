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
views. Repeated imports replace matching region definitions; they do not duplicate
files. Saving a region writes state atomically. Registered project manifests keep
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

Future computation jobs default to recomputing distances within the region.
Launching new fits, charts, overlap comparisons and region revisions/retirement
are not implemented in this first stage.

Validation: `tests/testthat/test-local-view-atlas.R`, with related endpoint-set,
dCST-focus, graph-neighbor-control and project-Trash tests. The research design
and registration script maintain the concrete comb-V3V4-tx deployment.
