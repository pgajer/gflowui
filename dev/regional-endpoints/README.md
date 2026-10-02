# Endpoint sets for local regions — 02_oct_2026

A saved region now owns endpoint sets independently of the whole dataset. These
sets follow stable sample IDs across every graph and embedding of that region.
This prevents an endpoint added while exploring a local fit from silently becoming
a dataset-wide annotation.

On the first visit, the region receives the intersection of the selected parent
set with its saved membership. The name includes the region label and membership
revision, without the current metric or embedding. Display filters and connected
component selections do not determine inheritance. A new membership revision
inherits from the selected set of its preceding revision. This is a one-time copy;
later edits do not synchronize in either direction.

The endpoint selector lists sets for the active scope. **New**, **Rename**, and
**Duplicate** manage alternatives; each scope remembers its selected set. Regional
edits save immediately. **Save Snapshot** adds a regional copy to the same selector,
without writing a legacy graph snapshot or making a global alternative. **Import endpoints from…** explicitly adds missing members
from another set in the same dataset, preserving existing labels and leaving the
source unchanged. An unsaved membership preview must be saved before its endpoints
can be edited. The original global alternatives and legacy files remain available.

Label size, label offset, marker size, and marker color are stored with the set.
Inherited and newly added labels use the same style. Plotly labels now use scene
annotations with a constant pixel font size, avoiding the depth-dependent size of
3D text traces. Coordinates still follow the selected embedding. Many overlapping
labels can still overlap; this change does not introduce automatic label placement.
Arms retain their existing graph-specific path semantics.

## Saved-data correction

For the user's comb-V3V4-tx project, the newly added two-phylotype endpoint was
copied into the Li / 4000 samples, revision 1 set and removed from the active global
set. The global count changed from nine to eight; the regional set contains the
inherited Li endpoint and the newly added endpoint. All other global sets were
verified unchanged. The existing store was backed up before the correction.

The local backup and migration audit sit beside `endpoint_sets/sets.rds` in the
project's application-data folder, named `sets.rds.before_regional_endpoints_02_oct_2026`
and `regional_migration_02_oct_2026.rds`. These contain user data and are not part of
the source repository.

## Verification

Automated checks cover membership intersection, stable-ID remapping, partial-view
edits, parent isolation, new region revisions, multiple sets and remembered
selections, explicit import, persisted styles, blocked unsaved previews, and common
annotation font sizes. The related application, atlas, endpoint, and scene tests
pass. Nineteen pre-existing integration checks require unavailable fixtures or
optional functions and were skipped. The JavaScript checks were run directly with
the bundled Node executable because R did not find Node on its PATH; all three
passed. This was targeted validation, not a full CRAN check.

The actual project was checked in the browser with the abundance-Euclidean and
Hellinger complete-Fermat power-2 regional layouts. Both retain the same two
endpoints and regional set name; both rendered labels have the same 16.8-pixel
font size at the saved 1.4x setting.

```r
testthat::test_local(
  filter = "regional-endpoints|endpoint-sets|local-view-atlas|atlas-|view-performance|app-constructs",
  reporter = "summary"
)
```
