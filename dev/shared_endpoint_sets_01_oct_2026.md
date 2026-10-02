# Dataset-wide endpoint tables

Named endpoint sets are available across every graph and embedding of the same
dataset. Changing metric, power or neighbor count retains the selected set.
The **Endpoint set** dropdown offers the dataset's separate named tables and
identifies their source graph. **New**, **Rename** and **Duplicate** manage
alternatives; source graph and embedding are provenance rather than boundaries.

Stable vertex IDs map endpoints into each graph, including reordered or subset
assets. Missing samples remain saved. The visible/total count accounts for the
current graph, component and display filter. Detector scores continue to describe
the source embedding and are not recalculated when switching views.

The generic manifest field `endpoint_vertex_namespace` identifies the dataset,
with the project ID as the default. Unrelated namespaces remain separate. The
former `endpoint_scope_id` and selected `k` no longer restrict sharing. Assets
without unique stable IDs retain the legacy local editor. No project names or
research paths are embedded in the application logic.

Version-one stores are backed up and upgraded without merging sets or changing
their IDs, labels, endpoint rows or provenance. Original working files and
snapshots are imported separately and once, using each original graph's IDs.
The saved active choice now belongs to the dataset. Revision checks reject stale
edits; the local store is not a multi-process transactional database.

Focused tests cover graph changes, reordered IDs, subset preservation, independent
named alternatives, namespace separation, migration and shared edits. Existing
endpoint-layout, detector, abundance-label and provider checks are also used.
This is targeted feature validation, not a full CRAN check.

The comb registration declares one dataset namespace for all routes. Its
`register_endpoint_sharing.R` updates manifest metadata without recomputing
scientific distances, graphs or embeddings.

[User and manifest documentation — Markdown](/Users/pgajer/current_projects/gflowui-fermat-palettes/README.md#shared-endpoint-sets) · [HTML](/Users/pgajer/current_projects/gflowui-fermat-palettes/README.html#shared-endpoint-sets).
