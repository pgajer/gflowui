# gflowui

`gflowui` is a companion R package for `gflow` that provides an interactive
Shiny interface for graph construction, conditional-expectation and
occupation-density fields, endpoint/arm workflows, canonical basin analysis,
and linked 3D exploration.

## Scope

The package is structured to support this end-to-end workflow:

1. Load and validate matrix-like biological data.
2. Build and select candidate graphs over a k range.
3. Compute conditional expectations for outcomes/features over selected graphs.
4. Evaluate occupation-density fields on a selected graph.
5. Reconstruct canonical maximum and minimum basins with
   `gflow::create.basin.complex()`.
6. Explore a plateau-aware superlevel merge tree, basin characteristics,
   linked plots, and graph overlays.
7. Visualize results in 3D with subject, endpoint, and arm overlays.

## Development status

The package currently includes:

- a project registry and manifest-backed scientific asset contract;
- Plotly, RGL, and prebuilt-HTML graph renderers;
- graph, subject, occupation-density, conditional-expectation, endpoint, and
  arm controls;
- canonical both-direction basin reconstruction using fixed CLOSEST flow;
- an adaptive, crossing-free maximum-basin merge-tree display;
- a linked Basin Plot Workspace and Basin Inspector;
- settings-only Basin Analysis recipes and complete ZIP export; and
- source-loaded and package-installed regression coverage.

## Run the app during development

```r
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
}
gflowui::run_gflowui()
```

The app prints its source-loaded URL, normally on `127.0.0.1`. Keep the R
process running while using that URL.

## Basin Analysis

After opening a project and showing an occupation-density or conditional-
expectation field:

1. Open `Basins`.
2. Confirm the intended `Estimate source`.
3. Select `Compute & Open Basin Analysis`.
4. Use the General Inspector's three linked panels:
   - the maximum-basin superlevel merge tree;
   - the Basin Plot Workspace; and
   - the Basin Inspector table.

All linked interactions use canonical maximum-basin IDs. Selecting a tree
branch, scatter point, Inspector row, or assigned graph vertex changes
transient display state only. `Pin`/`Unpin` is the selection-driven action
that recomputes the display proposal and its required ancestor closure.

The graph keeps its current color source until `Show basin colors` is selected
explicitly. Display recipes save validated settings, not basin results,
canonical IDs, component choice, pins, selection, or layout. Complete ZIP
export always contains the unfiltered basin-characteristics table.

See [docs/BASIN_ANALYSIS.md](docs/BASIN_ANALYSIS.md) for scientific semantics,
controls, lifecycle behavior, persistence, accessibility, performance
expectations, and troubleshooting. Maintainer notes are in
[docs/BASIN_ANALYSIS_DEVELOPER.md](docs/BASIN_ANALYSIS_DEVELOPER.md).

## 3D renderers

`gflowui` supports three 3D renderer modes:

1. `RGL (live)` (default): on-the-fly WebGL rendering from in-memory graph layout
   data, with interactive sphere/point parameter updates.
2. `HTML`: prebuilt HTML artifact rendering (iframe).
3. `Plotly`: reactive Plotly-based 3D rendering.

The app now defaults to `RGL (live)` and falls back to `HTML`/`Plotly` when
`rgl` is unavailable.

## Development

Build and check the source package with the reviewed `gflow` dependency first
on `R_LIBS`:

```sh
R CMD build .
R CMD check --no-manual gflowui_0.0.0.9000.tar.gz
```

Generated tarballs, `.Rcheck` directories, screenshots, browser downloads,
project registries, scientific caches, and exported basin bundles do not
belong in the source tree.
