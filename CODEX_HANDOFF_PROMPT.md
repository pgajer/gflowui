# gflowui Codex Handoff Prompt

Use this prompt in a new Codex session with project root:
`/Users/pgajer/current_projects/gflowui`

## Mission
You are continuing development of `gflowui`, a standalone R package + Shiny app that makes `gflow` graph workflows accessible to VMRC researchers (especially non-programmers).

The app must support:
- project-based workflow,
- graph family selection by data type,
- reference graph selection (`set + k`),
- endpoint and conditional expectation assets,
- fast 3D visualization (HTML-first, Plotly fallback),
- simple, minimal UX with controls in left panel and visualization in right panel.

## User-Driven Product Direction (current)
The UI should follow this architecture:

- Left panel:
  - `Projects` entry when no project is open.
  - After project selection, hide project picker and show workflow controls.
  - Core control panel is `Graph(s) Structure`, split into two parts:
    - top: graph structure controls,
    - bottom: `Graph Layout` controls.
- Right panel:
  - visualization-only (no workflow controls).

Within `Graph(s) Structure` top section:
- `Data Type:` dropdown + `(n_samples x n_features)` summary.
- `k:` dropdown + `Set As Reference Graph` button.
- `Optimal k:` dropdown + `Show` button to open artifact externally.
- `Update / Expand Graphs...` button.

Within `Graph Layout` section:
- `Renderer:` HTML or Plotly.
- `Vertex Layout:` Sphere or Point.
- `Vertex size:` size scale.
- `Color by:` available variables/conditional expectations.

## What is implemented now

### Core app and launcher
- `run_gflowui()` works with older/newer Shiny by passing app object positionally.
- Files:
  - `R/app_main.R`
  - `R/app_ui.R`
  - `R/app_server.R`

### gflow backend wiring
- Adapter layer is live (not stub-only) and calls real `gflow` functions:
  - `build.iknn.graphs.and.selectk`
  - `fit.rdgraph.regression`
  - `refit.rdgraph.regression`
  - `geodesic.core.endpoints`
- File:
  - `R/services_gflow_adapter.R`

### Project registry and manifest APIs
- Public functions:
  - `register_project()`
  - `list_projects()`
  - `unregister_project()`
  - `discover_project_artifacts()`
- Discovery profiles for:
  - `symptoms_restart`
  - `agp_restart`
  - `custom`
- Registry/manifests stored in:
  - `tools::R_user_dir("gflowui", which = "data")/projects`
- File:
  - `R/project_registry_api.R`

### Project-centered app workflow
- Start state: select existing project or create new.
- Active project state: workflow controls shown; project picker hidden.
- Save/Exit with unsaved-change handling.
- Run monitor panel on left.

### Reference graph + renderer model
- Reference graph defaults persisted in manifest:
  - `defaults$reference_graph_set_id`
  - `defaults$reference_k`
  - `defaults$reference_reason`
- Right panel rendering:
  - HTML iframe if available (preferred when renderer=HTML),
  - Plotly fallback when needed,
  - Plotly-only interactive coloring/size behavior.
- Renderer state is computed reactively from left-panel controls.

### Graph controls consolidation
- `Project Assets` block removed from left panel.
- `Reference Graph View` control block removed from right panel.
- Controls moved into `Graph(s) Structure` panel + `Graph Layout` subsection.
- `Set As Reference Graph` implemented inline.
- `Optimal k -> Show` opens external artifact via `open` on macOS.

### Startup stability fixes
- Added scalar guards for Shiny init (`NULL`/length-zero inputs).
- Prevents `if (...)` errors from `integer(0)`/`character(0)` during startup.
- Key helper functions in `R/app_server.R`:
  - `%||%`
  - `scalar_int()`
  - `scalar_chr()`

### Styling
- Minimal visual language, left-control/right-view split, chips in top bar.
- File:
  - `inst/app/www/styles.css`

### Spec document
- Data-type graph options and UI spec:
  - `docs/graph_data_type_options_spec.md`

## Important files map
- `R/app_server.R`: primary orchestration; currently large and monolithic.
- `R/app_ui.R`: app shell/layout.
- `R/services_gflow_adapter.R`: backend compute wrapper layer.
- `R/project_registry_api.R`: registry + manifest + discovery.
- `R/mod_*.R`: module UIs/servers for Data/Graph/CondExp/Visualize (legacy and fallback flows).
- `inst/app/www/styles.css`: all app CSS.
- `tests/testthat/test-app-constructs.R`
- `tests/testthat/test-project-registry.R`

## Current verification commands

Run during development:

```r
pkgload::load_all("/Users/pgajer/current_projects/gflowui", export_all = FALSE)
gflowui::run_gflowui()
```

Test suite:

```r
pkgload::load_all("/Users/pgajer/current_projects/gflowui", export_all = FALSE)
testthat::test_dir("/Users/pgajer/current_projects/gflowui/tests/testthat", reporter = "summary")
```

## Recommended next tasks (ordered)
1. Break `R/app_server.R` into focused helpers/modules:
   - project session state,
   - graph structure controls,
   - renderer/view logic,
   - manifest persistence helpers.
2. Promote explicit manifest metadata for each graph set:
   - `data_type_id`, `data_type_label`, `n_samples`, `n_features`,
   - `optimal_k_artifacts`,
   - layout assets (renderer/vertex-size/color presets).
3. Replace current heuristic data-type and feature-count inference with manifest-grounded values.
4. Improve `Optimal k -> Show` artifact selection:
   - explicit artifact registration by criterion,
   - deterministic mapping per data type and k.
5. Connect HTML vertex size/layout selections to concrete pre-rendered asset variants (not only heuristic matching).
6. Add endpoint overlay options sourced from project endpoint assets (not just current viz-state object).
7. Add light integration tests for:
   - graph structure panel initialization,
   - renderer switching,
   - reference graph persistence.

## Project bootstrapping note for VMRC datasets
To (re)register external projects:

```r
pkgload::load_all("/Users/pgajer/current_projects/gflowui", export_all = FALSE)

gflowui::register_project(
  project_root = "/Users/pgajer/current_projects/symptoms",
  project_id = "symptoms",
  project_name = "Symptoms",
  profile = "symptoms_restart",
  overwrite = TRUE
)

gflowui::register_project(
  project_root = "/Users/pgajer/current_projects/AGP",
  project_id = "agp",
  project_name = "AGP",
  profile = "agp_restart",
  overwrite = TRUE
)
```

## Working tree note
Repository may be intentionally dirty. Do not discard unrelated local changes unless explicitly requested.
