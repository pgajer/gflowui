# gflowui Operator Quickstart

This guide is for collaborators who want to use the app (not develop it).

## 1) Start the app

In R:

```r
gflowui::run_gflowui()
```

From a development checkout:

```r
pkgload::load_all(".", export_all = FALSE)
gflowui::run_gflowui()
```

## 2) Open a project

- In the `Projects` panel, choose a project from the dropdown.
- Example projects already registered may include:
  - `Symptoms`
  - `AGP`

After selection, the app switches into project workspace mode.

## 3) Use `Graph(s) Structure` controls

Top section:

1. `Data Type`
  - Select graph family (for example `ASV`, `ASV-top20`, `ASV-top30`, `ASV-top50`).
  - Right side shows matrix dimensions as `(samples x features)`.
  - Some projects may show several project-specific dropdowns instead of one
    flat `Data Type` list. In that case, those selectors resolve the active
    graph family and the UI still shows the resulting matrix dimensions.
2. `k`
   - Choose the graph `k` value.
   - Click `Set As Reference Graph` to make current data type + k the project default.
3. `Optimal k`
   - Select criterion (for example `median norm-GCV`).
   - Click `Show` to open the criterion artifact externally.
4. `Update / Expand Graphs...`
   - Use when adding/updating graph assets.

## 4) Use `Graph Layout` controls

- `Renderer`: `RGL (live)`, `HTML`, or `Plotly`
  - `RGL (live)` is the default and renders WebGL on the fly from current data.
    Use this when adjusting sphere/point size and color options interactively.
  - `HTML` shows pre-generated HTML artifacts from project outputs.
  - `Plotly` remains available as an interactive fallback.
- `Vertex Layout`: `Sphere` or `Point`
- `Vertex size`: size multiplier
- `Color by`: select variable/feature/outcome used for coloring

The right panel updates the visualization based on these controls.

## 5) Analyze basins

After showing an occupation-density or conditional-expectation field:

1. Open `Basins` and verify `Estimate source`.
2. Click `Compute & Open Basin Analysis`.
3. Use the linked merge tree, Plot Workspace, and Basin Inspector in the
   General Inspector.
4. Check a row's `Show` box for transient linked selection.
5. Use `Pin` only when that maximum basin must be protected in a recomputed
   display proposal.
6. Click `Show basin colors` to replace the graph's current color source
   explicitly. Opening or selecting basins alone preserves the current graph
   colors.
7. Use `Save current recipe` for settings-only persistence, or
   `Save full basin bundle` for the complete unfiltered analysis.

See [BASIN_ANALYSIS.md](BASIN_ANALYSIS.md) for the scientific semantics,
current/retained attempt behavior, recipes, accessibility, performance
expectations, and export validation.

## 6) Save and exit

- `Save Project`: writes current project state.
- `Exit Project`: leaves workspace.
  - If there are unsaved changes, choose whether to save first.

## 7) Run Monitor

- Appears in the left panel when tasks run.
- Shows project status, renderer mode, and latest job note.

## Troubleshooting

1. App starts but no project appears:
   - Ask project maintainer to register projects in `gflowui`.
2. Renderer is blank:
   - If `RGL (live)` is selected, install the `rgl` package.
   - Otherwise switch renderer between `RGL (live)`, `HTML`, and `Plotly`.
3. `Show` under `Optimal k` does nothing:
   - The selected criterion artifact may not exist for that data type.
4. `Set As Reference Graph` fails:
   - Ensure both `Data Type` and `k` are selected and valid.
5. A selected basin is not visible in the filtered static tree:
   - It is a hidden transient selection. Open the complete tree or Pin it to
     request proposal reconstruction and ancestor closure.
6. Basin colors do not replace density colors:
   - This is intentional. Click `Show basin colors` to change the graph color
     source explicitly.
