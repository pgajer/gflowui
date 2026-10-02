# Source datasets and linked within-dCST views

Implemented 02_oct_2026. These controls compare source-dataset composition and
continuous variation within a two-phylotype dCST in the existing gflowui project.
They do not recompute graphs, distances, embeddings, or dCST assignments.

## Using the viewer

Open comb-V3V4-tx at [the running viewer](http://127.0.0.1:3874/).
The **Source datasets** and **Within-dCST 2D** panels follow Graphs.

In Source datasets, rows are sorted by the number of distinct vertices in the
current graph, largest first. Local fitted views therefore have local counts.
The counts precede display filters, so selecting a dataset does not remove the
other dataset choices. Check several rows to show their union. This intersects
existing component, dCST and region-preview filters. No checked rows means all
datasets. Choose **Source dataset** under **Color by** in Graphs to use the
editable dataset palette. Otherwise filtering preserves the existing coloring.
Colors autosave in the project manifest and apply across global and local views.

The project has 25,042 distinct compositions from 25,095 source records in 44
source datasets. Each record retains its original dataset membership. A shared
composition is included when any contributing dataset is selected. Under dataset
coloring, shared compositions use the gray **Multiple source datasets** category;
unknown identities use **Unknown source**. Dataset counts overlap for shared
vertices and should not be summed as a count of distinct graph vertices.

**Dataset × dCST** opens a horizontally scrollable cross-table. Choose dCST
level 1 or 2, source records or distinct vertices per dataset, and counts or row/
column percentages. Denominators use all members of the current graph before
display filters. Clicking a cell adds a dataset–dCST intersection filter; use
**Clear cross-table cell filter** to remove it. Existing filters still apply.
The CSV download names its level, count unit and normalization.

In **Within-dCST 2D**, enable **Show linked 2D view** and choose a pair. Its plot
appears below the 3D graph, with both plots shortened to fit together. The pair
is ordered as A then B in the displayed dCST label. Using original relative
abundances, the coordinates are

\[
t=\frac{x_B}{x_A+x_B},\qquad r=1-x_A-x_B.
\]

Here t is the fraction of B within the pair, and r is the combined abundance of
all other phylotypes. They remain unchanged across graph, metric, homogeneous
coordinate and embedding choices. These are compositional summaries, not
metric-dependent arclength and distance from a fitted curve. Different residual
communities can share the same coordinates.

The 2D plot shows members of the selected dCST that pass the current display
filters, colored by source dataset. Its axes initially fit the plotted range;
Plotly zoom and reset remain available. Click in either Plotly display, or use
box/lasso selection in 2D. Orange rings identify the same composition IDs in
both displays. Selection replaces the previous selection unless **Add to
selection / toggle clicked points** is checked. A 3D click selects that point's
pair when an explicit pair is available. **Show only linked selection in 3D**
hides other points; clearing the selection restores the otherwise filtered
view. Selection is session-local, follows IDs across views, and resets on a
project change. It does not modify endpoint sets or save a new region.

The coordinate CSV contains IDs, t, r, original A/B abundances, phylotype names,
dCST label, source category and linked-selection status. Missing abundances and
zero pair mass are omitted from the plotted/visible-coordinate export.

There are 65 explicit two-phylotype groups and 14 other merged level-2 groups
in the whole project. Single-phylotype, longer and unresolved labels are excluded
from this initial view rather than assigned an invented pair. The pure reference
vertex need not belong to the chosen two-phylotype dCST and is not automatically
added to that group's plot.

## Configuration and reproducibility

Reusable app code contains no project-name or local-path branches. Projects opt
in through `metadata$source_datasets$file`, pointing to an RDS list with:

- `records`: a data frame with `vertex_id`, unique `record_id`, and `dataset`.
- `pairs`: an optional data frame with unique `group`, `a`, and `b`; group values
  match `dcst_level2`, and features match the original abundance asset exactly.
- Optional `provenance` describing the inputs and pair definition.

Original abundances use the existing `metadata$vertex_hover$abundances_file`
asset. Dataset colors live in `metadata$source_datasets$palette`. The two-phylotype
panel requires original abundances and level-2 dCST metadata. Plotly supplies
linked 2D/3D click interaction.

[Registration script](/Users/pgajer/current_projects/gflowui-fermat-palettes/dev/source-datasets/register-comb.R)
builds the small annotation asset from the existing record-to-composition mapping,
registers it as a project artifact, and retains a pre-change manifest backup.
Run it from the gflowui checkout. It matches explicit label pairs to abundance
feature names and records excluded labels and the record-map SHA-256 hash.
Generated annotations and the manifest remain local; no microbiome table is
committed to the package repository.

## Verification

Targeted tests cover multi-dataset vertices, record versus vertex counts,
percentage denominators, ID ordering, zero/missing pair mass, coordinate
reconstruction, shared point selections, filtering, and color persistence.
Existing dCST focus and atlas-color suites pass. The scene-update and linked-event JavaScript
regressions pass (including repeated redraws without duplicate click handlers). Full-data server checks verified global HMP filtering (3,613
vertices), local Li 4000 HMP–Li/Gardnerella intersection (68 vertices), and
selection persistence across global/local navigation. Browser checks verify
rendered tables, 2D point selection and its 3D highlight.

The broader uniform-layout test's compiled-component comparison fails because
it calls `dgraphs::graph.connected.components()` with a raw adjacency list,
whereas the installed version requires a dgraph object. Its other nine
expectations pass. This dependency/API mismatch is outside the changed feature;
a full package/CRAN check has not been claimed.
