# gflowui Operator Quickstart

This guide is for collaborators who want to use the app (not develop it).

## 1) Start the app

In R:

```r
pkgload::load_all("/Users/pgajer/current_projects/gflowui", export_all = FALSE)
gflowui::run_gflowui()
```

If `pkgload` is not installed:

```r
source("/Users/pgajer/current_projects/gflowui/app.R")
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

## 5) Save and exit

- `Save Project`: writes current project state.
- `Exit Project`: leaves workspace.
  - If there are unsaved changes, choose whether to save first.

## 6) Run Monitor

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
