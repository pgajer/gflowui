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
