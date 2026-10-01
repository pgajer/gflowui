# Endpoint tables across embeddings

Endpoint sets now follow stable sample IDs across embeddings of the same declared
graph or distance construction. They are independent of coordinate order. The
comb-V3V4-tx registration declares groups by base metric, distance construction,
power and neighbor count; embedding route is excluded.

In **Endpoints**, choose a named set, or create, rename or duplicate one. Edits
are shared across its embedding routes. **Show a set from another graph…** opens
a read-only comparison table and overlay; **Copy to this graph** creates an
independent editable copy. Endpoint labels retain their source meaning, including
labels derived from ratio coordinates. Missing samples and hidden components do
not delete endpoints; the visible/total count explains the current display.

The implementation uses generic manifest fields `endpoint_scope_id` and
`endpoint_vertex_namespace`. Selected `k` also separates scopes. Without an
explicit scope, the graph-set ID remains the boundary. Without valid stable
vertex IDs, the old local editor remains available and sharing is disabled.
No project names or local research paths occur in the application logic.

Existing working files and snapshots are imported independently and only once,
using each original graph's IDs. Their files are preserved. A saved older table
with only row numbers depends on the original graph asset retaining its vertex
order; there is no reliable way to recover an earlier order after replacement.

Source embedding and graph, detector options, geometry fingerprint and newly
saved detector scores are retained. Historical records retain only the metadata
originally saved; changing views never recomputes or relabels source scores.
The project-managed `endpoint_sets/sets.rds` stores named sets and the active
selection per graph. Revision checks reject stale edits from another session.
This is a local desktop store, not a multi-process transactional database.

Focused checks cover stable-ID reordering, missing-sample preservation, independent
metric/k scopes, migration of multiple working files and snapshots, shared label
edits, New/Rename/Duplicate, cross-graph read-only overlays and copies, original
file checksums, and restored set selection. Existing endpoint-layout, detector,
abundance-label and manifest-provider checks also passed. This is targeted
feature validation, not a full CRAN check.

The comb registration source declares sharing for future layouts. A lightweight
`register_endpoint_sharing.R` updates only these fields in the existing manifest,
retaining a backup and avoiding experimental recomputation. Scientific distances,
graphs and embeddings are unchanged.

[User and manifest documentation — Markdown](/Users/pgajer/current_projects/gflowui-fermat-palettes/README.md#shared-endpoint-sets) · [HTML](/Users/pgajer/current_projects/gflowui-fermat-palettes/README.html#shared-endpoint-sets).
