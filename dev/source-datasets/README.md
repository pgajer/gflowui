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
level 1, 2 or 3 (when available), source records or distinct vertices per dataset, and counts or row/
column percentages. Denominators use all members of the current graph before
display filters. Clicking a cell adds a dataset–dCST intersection filter; use
**Clear cross-table cell filter** to remove it. Existing filters still apply.
The CSV download names its level, count unit and normalization.

In **Within-dCST 2D**, enable **Show linked 2D view**. The 3D graph appears on the left and the linked 2D plot on the right
when the available viewer width exceeds 900 pixels. Narrower viewing areas stack
the plots, with 3D above 2D. Both plots resize when the sidebar changes width. The dCST checkboxes in Graphs now control both plots: several
checked groups show their union in both, and no checks shows all groups.
There is no separate single-pair selector. Level-1 filtering is also respected;
each plotted point still uses the explicit pair of its level-2 dCST.
The default 2D colors use the same dCST level and saved palette as Graphs.
Level 3 is available for all 25,042 comb-V3V4-tx compositions (137 groups)
and inherited by local views. Level-3 filtering and colors apply to both plots;
the 2D coordinates continue to describe each point’s level-2 phylotype pair.
Source-dataset coloring remains an alternative. Legends are descriptive;
use the shared checkbox table to filter both views.

The pair is ordered as A then B in the dCST label. The **2D coordinates** menu
provides two descriptions, always calculated from original relative abundances:

\[
t=\frac{x_B}{x_A+x_B},\qquad r=1-x_A-x_B
\]

in relative-abundance mode, and

\[
u=\frac{x_B}{x_A},\qquad
\rho_A=\sqrt{\sum_{j\ne A,B}\left(\frac{x_j}{x_A}\right)^2}
\]

in homogeneous-coordinate mode. Here t is the fraction of B within the pair;
r is the total abundance outside the pair; u is position on the B axis in the
A-based ratio chart; and rho is Euclidean distance to that axis. The pure pair
has rho zero and u = t/(1-t). These homogeneous distances use Euclidean geometry
in the ratio chart; neither description depends on a graph or embedding.

Homogeneous coordinates require positive abundance of A. No pseudocount is
introduced. Undefined points are counted in the panel and omitted from the 2D
plot; their 3D membership is unchanged. The chart remains valid when A is not
dominant, but then it lies outside the A-dominant face; the panel reports this
case and the axes allow ratios above one. Missing/unsupported pair definitions
are also counted. Different pairs can be displayed together: each point's hover
identifies its dCST and its A and B phylotypes. This is a comparison of local
pair charts, not a single shared taxon coordinate system across all dCSTs.

Click in either Plotly display, or use box/lasso selection in 2D. Orange rings
identify the same composition IDs. Selection replaces the previous selection
unless **Add to selection / toggle clicked points** is checked. A 3D click no
longer changes which dCSTs are included. Point selections survive changes of
coordinate mode and embedding. **Show only linked selection in 3D** hides other
points; the 2D plot follows the resulting visibility mask. Clearing the point
selection restores the otherwise filtered view. Selection is session-local and
resets on a project change. It does not modify endpoint sets or save a new region.

The coordinate CSV includes IDs, both coordinate descriptions, original A/B
abundances, phylotype names, dCST, A-dominance status, plotted coordinates, the
chosen coordinate mode, source category and point-selection status. It contains
the points currently plottable in the selected mode. Axis ranges initially fit
the visible groups; Plotly zoom and reset remain available.

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
selection persistence across global/local navigation. The homogeneous extension
also verifies exact binary-axis coordinates, residual Euclidean norms, zero A,
ratios above one, pair-specific coordinates and palette matching. In Li 4000,
selecting Li–Gardnerella and Li–crispatus yields the same 1,378 vertices in both
plots and both coordinate systems; clearing the dCST filter restores all 4,000.
Browser checks verify
rendered tables, 2D point selection and its 3D highlight.

The broader uniform-layout test's compiled-component comparison fails because
it calls `dgraphs::graph.connected.components()` with a raw adjacency list,
whereas the installed version requires a dgraph object. Its other nine
expectations pass. This dependency/API mismatch is outside the changed feature;
a full package/CRAN check has not been claimed.
