# gflow -> gflowui Project Asset Contract

This document defines a repeatable handoff from a `gflow` analysis pipeline to a `gflowui` project registered with `register_project()`.

## 1) Core idea

`gflowui` consumes a project manifest with three asset groups:

1. `graph_sets`: graph families and their layout/optimal-k metadata.
2. `condexp_sets`: conditional expectation outputs (fit files or long tables).
3. `endpoint_runs`: endpoint detection outputs.

For robust startup and UI population, each graph set should include explicit metadata:

- `data_type_id`, `data_type_label`
- `n_samples`, `n_features`
- `optimal_k_artifacts` (named criterion -> file)
- `layout_assets` with `grip_layouts`
- `color_assets` for metadata-driven color options (for example `CST`, `subCST`)

## 2) Graph-set contract

Each graph set should provide at minimum:

- `id`, `label`
- `graph_file`: `.rds` from `gflow::build.iknn.graphs.and.selectk()`
- `k_values`
- `n_samples`, `n_features`

Recommended additions:

- `optimal_k_artifacts`: named files for each k-selection criterion
- `layout_assets$grip_layouts`: one `grip.layout()` result per k
- `layout_assets$grip_layout_params`: the exact `grip.layout()` arguments
- `color_assets`: metadata file/object and columns for categorical coloring

Example `color_assets`:

```r
list(
  metadata_file = "/path/to/data/S_asv.rda",
  metadata_object = "mt.asv",
  vector_columns = c("CST", "subCST"),
  preferred_order = c("CST", "subCST"),
  labels = c(CST = "CST", subCST = "subCST")
)
```

## 3) Conditional expectation contract

Two supported formats:

1. `type = "fit_files"`: per-family directories of `fit.rdgraph.regression()` output, keyed by k.
2. `type = "long_table"`: a single long table `.rds` with `outcome`, `k`, `y_fitted` (and optional `y_observed`).

Include:

- `id`, `label`, `k_values`
- `outcomes` (explicit outcome names)
- for `fit_files`: `family_runs` entries with `family`, `fit_files`, and summary path
- for `long_table`: `long_table_file`, optional `gcv_summary_file`

## 4) Endpoint run contract

Each endpoint run should include:

- `id`, `label`, `run_dir`
- one or more of: `summary_csv`, `labels_csv`, `per_k_bundles`, `bundle_file`
- `k_values`

## 5) Registering the project

Use `profile = "custom"` when supplying explicit assets:

```r
gflowui::register_project(
  project_root = "/path/to/project",
  project_id = "example_project",
  project_name = "Example Project",
  profile = "custom",
  scan_results = FALSE,
  graph_sets = graph_sets,
  condexp_sets = condexp_sets,
  endpoint_runs = endpoint_runs,
  defaults = list(
    graph_set_id = "all",
    condexp_set_id = "vag_odor_binary",
    endpoint_run_id = "evenness_k05"
  ),
  overwrite = TRUE
)
```

## 6) Validation checklist

After registration:

1. `list_projects(include_manifests = TRUE)` shows non-empty `graph_sets`.
2. `Graphs -> Color by` includes `CST`/`subCST` (when available), conditional expectations, and falls back to `Vertex Degree`.
3. Renderer switch (`HTML`, `RGL`, `Plotly`) keeps graph structure synchronized because all use the same `grip.layout()` coordinates.
