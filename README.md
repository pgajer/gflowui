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
