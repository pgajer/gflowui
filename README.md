# gflowui

`gflowui` is a companion R package for `gflow` that provides an interactive
Shiny interface for graph-based conditional expectation workflows.

## Scope

The package is structured to support this end-to-end workflow:

1. Load and validate matrix-like biological data.
2. Build and select candidate graphs over a k range.
3. Compute conditional expectations for outcomes/features over selected graphs.
4. Visualize results in 3D with endpoint overlays.

## Development status

This repository currently contains an MVP scaffold:

- app shell and module wiring
- `gflow` adapter service layer stubs
- test skeleton
- Codex handoff prompt for follow-on implementation

## Run the app during development

```r
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
}
gflowui::run_gflowui()
```

## 3D renderers

`gflowui` supports three 3D renderer modes:

1. `RGL (live)` (default): on-the-fly WebGL rendering from in-memory graph layout
   data, with interactive sphere/point parameter updates.
2. `HTML`: prebuilt HTML artifact rendering (iframe).
3. `Plotly`: reactive Plotly-based 3D rendering.

The app now defaults to `RGL (live)` and falls back to `HTML`/`Plotly` when
`rgl` is unavailable.

## Vertex abundance hover labels

Projects with original relative-abundance profiles can opt into richer Plotly
hover labels through `metadata$vertex_hover$abundances_file`. Labels show the
stable vertex ID, its graph-local number, and ranked phylotype abundances as
percentages. **Phylotypes on hover** defaults to four and accepts any count from
one through the number of features. Only nonzero phylotypes are listed.

The shared RDS asset contains `sample_ids`, `taxon_names`, and parallel
`indices` / `abundances` lists, one sorted nonzero profile per sample ID. Values
must be positive proportions summing to one; indices address `taxon_names`.
Every graph must supply stable `vertex_ids`. Matching uses IDs, not display row
positions, so filtered views and subject overlays retain the correct profiles.
Coordinate transformations do not change these original-abundance labels.
Missing IDs are explicitly reported instead of borrowing another vertex's data.

## Next implementation targets

- Replace adapter stubs with real `gflow` calls.
- Add async job execution for expensive graph/smoothing steps.
- Add export of standalone HTML artifacts for consortium sharing.

Endpoint inspection can reuse that same asset by setting
`metadata$endpoint_label_provider$mode = "vertex_abundances"`, with
`metric_coordinates` mapping graph-set base metrics to `abundance`, `ratio`, or
`sqrt_ratio`, and `reference_taxa` mapping anchor names to exact phylotype names.
Profiles match stable vertex IDs. Ratio labels identify numerator and reference;
the pure-reference composition is labeled as the chart origin. Endpoint profile
values use the selected coordinates, while hover values remain original abundances.

## Embedding endpoint candidates

In **Endpoints → Detect endpoints from embedding**, the default rule uses ten
nearest neighbors, a maximum pairwise direction angle of 90 degrees, and a
nearest-neighbor spacing cutoff at its estimated mode. This optional tool uses
`FNN`'s k-d tree and all vertices in the current displayed 3D coordinate system,
including hidden vertices. It does not alter the embedding or graph.

Controls provide neighbor count, angle threshold, optional exclusion of one
angular outlier, nearest- or k-th-neighbor support, mode/percentile/manual/no
spacing cutoff, a cutoff multiplier, and mode smoothing. Optional merging
keeps the narrowest-angle candidate first, breaking ties by spacing and vertex
number, and suppresses nearby candidates within the specified multiple of the
smaller local k-th-neighbor radius. Suppressed candidates remain in the table
and can be selected explicitly. Coincident positions share one representative.

Orange Plotly diamonds preview selected candidates. The paginated table shows
phylotype-derived labels, spacing, raw and adjusted angles, and merging status.
The histogram shows the spacing distribution and cutoff. Changes to coordinates,
graph identity, or detection settings deactivate results until detection is rerun.
**Add selected to Working Endpoints** preserves existing labels and records the
method, settings, and embedding fingerprint with newly added endpoints. The CSV
export includes scores for every vertex, settings, and the selection. Detection
results and options are session-local; imported working endpoints and downloaded
scores persist. These are geometric candidates for review, not biological claims.

Regression checks cover straight and branching arms, a closed circle, isolated
close pairs, coincident points, transformations, merging, stale previews, and
import into the working set. `tests/testthat/test-embedding-endpoints.R` contains
these checks; `FNN` is needed to run the detector and its numerical tests.

## Delete a project

Open **Settings → Delete Project**, review the file list, then choose **Move to
Trash**. The button is inside Settings, away from the workspace's Settings and
Save Project buttons. Successful deletion closes the project and removes it
from the project selector.

The saved manifest, project-owned registered assets under the project root, and
per-project gflowui state/cache files move together to one macOS Trash bundle.
The research project root and unrelated files remain in place. Files referenced
by other registered projects, and external referenced assets outside the project
root, are retained and listed in the confirmation. Directory references include
their contents; symbolic-link directories are not traversed. Unregistered results
and dependencies known only to external analysis scripts are not inferred.

The bundle contains `restore-map.csv` with original file locations and
`recovery.rds` with the manifest and registry entry. To recover, retrieve the
bundle from Trash, restore its mapped files to their original locations, and
restore the saved registry entry (or register the recovered project again).
Unsaved session changes are not part of the saved project bundle.

This currently uses the macOS Foundation Trash API through Swift. There is no
permanent-delete fallback. Assets must be movable to the staging bundle on the
registry's filesystem; a failed move or registry update restores files already
moved and leaves the project registered. A changed project requires a new review.

## Provenance, reproduction and asset inventories

Each project has a **Provenance & assets** panel. Creators can use **Settings →
Edit provenance / attach documents** to describe data selection, methods,
reproduction commands, software revisions, random seeds and limitations, attach
documents, or append an asset-inventory CSV (`path`, `role`, `description`,
optional `sha256`). These commands are documentation; the viewer never runs them.

For scripted creation, pass `provenance = list(...)` to `register_project()`.
For an existing project, call `set_project_provenance(project_id, provenance)`.
Omitting provenance when re-registering preserves the current record. Replacing
it retains the previous record under the project's managed provenance history.

Local documents become SHA-256-verified snapshots. Use self-contained HTML or
attach its required companion files. URLs remain remote references. Large source
and result assets remain references, with optional supplied SHA-256 hashes. The
panel downloads attached documents, the provenance JSON and an inventory of
registered and creator-supplied assets, including missing-file status. Source
assets listed only as provenance are retained when deleting a project; attached
snapshots and provenance history move with its managed state to Trash.

```r
register_project(
  project_root = "/path/to/viewer-assets", project_id = "example",
  scan_results = FALSE,
  provenance = list(
    summary = "A reproducible distance and embedding comparison",
    data = "Describe sample selection and feature processing here.",
    methods = "Describe distances, graphs, fitting and evaluation here.",
    reproduction = "Rscript /path/to/analysis.R",
    software = "Record package versions and source revisions.",
    seeds = "Distance-query landmarks: 42; embedding fit: 73",
    documents = list(list(path = "/path/to/methods.html", label = "Methods")),
    assets = list(list(path = "/path/to/input.rds", role = "Input",
                       description = "Frozen input matrix"))
  )
)
```

### Conditional graph selectors

A grouped selector field can specify `show_when = list(construction = "Symmetric kNN + MST")`.
The field is displayed only when the selected graph matches these literal metadata values.
Hidden selectors retain their internal selection state. For collections containing a single
precomputed graph, set `neighbor_parameter = FALSE` on each graph set to hide the generic
`k` and `Optimal k` controls. A separate grouped field can still expose the scientific
neighbor count as `Neighbors (k)`.

## Graph controls and panel order

The Graphs panel begins with always-visible graph metadata, followed by graph
selection and the connected-component selector. Graph Layout contains renderer,
vertex shape and vertex size; the default size is 0.6x. Explicit saved size
presets remain supported. A separate, non-collapsible **Vertex annotations &
filtering** subsection holds phylotype hover counts, coloring and dCST filters.
Grouped selectors do not repeat their choices in a second summary label.
The workflow panel order is **Graphs → Endpoints → Arms → Subjects**.

**Set Reference** saves the selected graph as the project reference and default
graph set. **Update / Expand Graphs** builds graph sets from data loaded in the
Data panel or registers an existing graph RDS; it does not run an external
Fermat experiment. **Add Data** opens that CSV-loading panel; it does not append
rows to existing graph assets. **Run Monitor** shows the latest in-app job
message, not the progress of external experiment workers. These controls also
provide explanatory hover text.

## Arrange projects

Click **gflowui** at the top left to open **Projects**, from either the opening
screen or an active project. The title has a subtle hover cue and a visible
keyboard-focus outline. Drag project handles or use the up/down buttons, then
choose **Save order**. **Sort A–Z** offers a starting order; **Cancel** leaves the
saved order unchanged. **Open** selects a project without saving ordering edits.
Save work or exit the current project before switching if it has unsaved changes.

The opening Projects dropdown follows this order across sessions. Preferences
are stored by stable project ID in `project-order.rds` alongside the registry,
separately from project manifests and assets. Renaming and result refreshes do
not change positions; new projects are appended. Saving reconciles projects
added or removed while the dialog was open without overwriting the registry.
