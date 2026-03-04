# gflowui Graph Data-Type Options Spec

## Scope
This document defines how `gflowui` should represent and expose graph assets by data type in the `Graph(s) Structure` panel.

## Panel Architecture
Single `Graph(s) Structure` panel with two sections:

1. `Graph(s) Structure` (top)
2. `Graph Layout` (bottom)

Separated by a horizontal rule.

## Top Section: Graph(s) Structure

### Row 1
`Data Type:` `[dropdown]` `(n_samples x n_features)`

- Dropdown selects a graph family built from a specific data matrix/feature space.
- Right-side text shows dimensions of the matrix used for graph construction.

### Row 2
`k:` `[dropdown]` `Set As Reference Graph`

- Dropdown lists available `k` values for the selected data type graph family.
- Button sets selected `(data_type, k)` as reference in project defaults.
- Reference values control initial graph display when project opens.

### Row 3
`Optimal k:` `[criterion dropdown]` `Show`

- Criterion dropdown examples:
  - `median norm-GCV`
  - `edit distance`
  - `response GCV`
  - `feature-family GCV`
- `Show` opens the criterion-vs-`k` PDF externally (macOS `open` / system default preview app).

### Row 4
`Update / Expand Graphs`

- Starts graph generation/extension workflow for the selected data type.

## Bottom Section: Graph Layout

### Row 1
`Renderer:` `[HTML | Plotly]`

- Default: `HTML` when available, else `Plotly`.
- Plotly remains available for interactive tasks (for example manual endpoint selection).

### Row 2
`Vertex Layout:` `[Sphere | Point]`

- Sphere for publication-like rendering.
- Point for dense/high-vertex performance.

### Row 3
`Vertex size:` `[size scale dropdown]`

- Discrete scale options (for example `0.5x`, `0.75x`, `1x`, `1.25x`, `1.5x`, `2x`).
- Mirrors behavior from existing HTML graph drivers.

### Row 4
`Color by:` `[dropdown]`

- Includes categorical and numeric project assets for selected data type and sample set.
- Examples: `CST`, `subCST`, `sptb.hat`, `vag_odor.hat`, endpoint labels, other conditional expectations.

## Data-Type Definition Model

Each data type entry should provide:

- `type_id`: stable machine id (for example `asv`, `asv_top20`, `mb_clr`)
- `label`: UI label (for example `ASV-top20`)
- `description`: short provenance summary
- `n_samples`: integer
- `n_features`: integer
- `sample_ids_key`: how row alignment is defined
- `matrix_source`: path or object id for source matrix
- `transform`: transformation tag (`raw`, `clr`, `log1p`, etc.)
- `feature_space`: taxonomy (`ASV`, `Phylotype`, `MB`, `MG`, `MT`, `IM`)
- `graph_set_id`: linked graph asset set id
- `k_values`: available k values
- `reference_k`: current reference k for this type (optional; global default still stored in project defaults)
- `optimal_k_methods`: available methods and their result artifacts

## Initial Supported Data Types (Symptoms)

- `ASV`: full ASV relative abundance table
- `ASV-top20`: top 20 highest-variance ASV features
- `ASV-top30`: top 30 highest-variance ASV features
- `ASV-top50`: top 50 highest-variance ASV features

## Planned/Future Data Types

- `Phylotype`
- `MB` and `MB-CLR`
- `MG` variants such as `MG-VOG`, `MG-VOG-cltrs`
- `MT`
- `IM`

## Naming Convention

Use `{feature_space}` or `{feature_space}-{qualifier}`:

- `ASV`
- `ASV-top20`
- `MB-CLR`
- `MG-VOG`

Keep `type_id` lowercase with underscores for internal use:

- `asv`
- `asv_top20`
- `mb_clr`
- `mg_vog`

## Alignment Contract

All matrices/vectors should use labeled sample ids.

- `X`, `y`, `Y`, `Z_i` are aligned by sample id.
- Partial overlap is allowed.
- UI should surface overlap counts where relevant for graph/condexp operations.

## UI Behavior Notes

- Hide standalone `Project Assets` panel.
- Hide standalone `Reference Graph View` control block.
- Keep right panel dedicated to visualization.
- Keep `Project` status chip in header.
- Keep `Run Monitor` in left panel, shown when jobs run.

## Implementation Notes

Suggested manifest extensions per graph set:

- `data_type_id`
- `data_type_label`
- `n_samples`
- `n_features`
- `optimal_k_artifacts` keyed by method
- `layout_assets` (HTML paths indexed by renderer/layout/size/color presets)

This schema supports your proposed UI without duplicating controls across panels.
