# Manual Endpoint Editing & Selection — Design Document

## 1. Context & Motivation

gflow identifies graph endpoints via two algorithmic methods:

1. **Evenness method** — computes `evenness.hat` via `gflow::fit.rdgraph.regression()` over a specific graph, then identifies local minima and basin characteristics via `gflow::compute.gfc()`. Used in both AGP and symptoms projects.
2. **Geodesic core method** — uses `gflow::geodesic.core.endpoints()` to identify endpoints based on eccentricity and distance-to-core metrics. Used in the AGP project.

Both methods produce pre-computed endpoint datasets that gflowui loads from CSV/RDS files and displays in the **Endpoints** accordion panel. The current UI presents a METHOD/K table with checkboxes controlling which datasets are overlaid on the 3D graph view.

**What's missing** is a third approach: **manual endpoint selection and editing**. Users need to visually inspect the graph, click on vertices to designate them as endpoints, edit labels, and combine manual picks with algorithmic results.

---

## 2. Current Architecture (Summary)

### Data Flow

```
manifest.endpoint_runs
    │
    ▼
resolve_endpoint_run()          Select run by ID or preferred k
    │
    ▼
endpoint_rows_for_run()         Create rows per method/k combination
    │
    ▼
endpoint_panel_state()          Aggregate rows, add selected flags
    │
    ▼
build_endpoint_method_k_table() Render HTML checkboxes (METHOD | K columns)
    │  [user checks boxes]
    ▼
endpoint_overlay_selection()    Track checked row keys (reactiveVal)
    │
    ▼
endpoint_overlay_active()       Aggregate vertices & labels from selected rows
    │
    ▼
read_endpoint_labels_from_row() Load labels from CSV/RDS per row
    │
    ▼
Plotly or RGL rendering         Display endpoints with markers and labels
```

### Key Reactive Values (app_server.R, lines ~906-914)

- `endpoint_overlay_selection()` — character vector of selected dataset row keys
- `endpoint_autoselect_done()` — prevents re-auto-selection after initial load
- `rgl_gen()` — generation counter forcing RGL widget refresh on parameter change

### Current UI (lines ~3000-3082)

The Endpoints accordion panel contains:
- METHOD/K checkbox table (`build_endpoint_method_k_table()`)
- "Update / Recompute Endpoints..." button (non-functional placeholder)
- Endpoint Layout controls (label size, offset, marker size, marker color)

### Plotly Rendering (lines ~2018-2253)

- Vertex traces carry `text = sprintf("vertex=%d<br>%s=%s", ...)` for hover
- Endpoint markers rendered as separate `scatter3d` trace
- Endpoint labels rendered as `scatter3d` text trace with offset positions
- **No event handling** — no `event_data()`, `plotly_click`, or `plotly_selected` calls exist

### RGL Rendering (lines ~2256-2491)

- Uses `rglwidget` (WebGL mode) with generation-stamped output IDs
- Endpoint markers rendered via `rgl::spheres3d()` or `rgl::points3d()`
- Labels via `rgl::texts3d()`
- **No vertex selection capability** — `rglwidget` WebGL mode does not support interactive vertex picking (`select3d`, `identify3d` are not available in the browser-side widget)

### Label Generation Patterns

Labels are loaded from pre-computed files with columns named `label`, `endpoint.label`, `endpoint_label`, `name`, `end.label`, or `end_label`. The fallback when no label is found is `sprintf("v%d", vertex_id)`.

---

## 3. Proposed Design

### 3.1 Panel Restructuring

Replace the current Endpoints accordion panel content with three sections:

#### Section A: Endpoint Datasets (replaces METHOD/K table)

A single-column checkbox table. Each row represents one pre-computed endpoint dataset.

```
┌──────────────────────────────┐
│ Endpoint Datasets            │
├──┬───────────────────────────┤
│ ☑│ evenness_k5               │
├──┼───────────────────────────┤
│ ☐│ geodesic_core             │
└──┴───────────────────────────┘
```

- Replaces `build_endpoint_method_k_table()` with `build_endpoint_dataset_table()`
- Each row maps to one existing endpoint_row (method + k collapsed into a single label like `"evenness_k5"` or `"geodesic_core"`)
- Checkbox behavior is identical to current: controls whether that dataset's endpoints are included in `endpoint_overlay_active()`
- The existing reactive infrastructure (`endpoint_overlay_selection()`, observer at lines 1406-1430) is reused with minimal change

#### Section B: Active Endpoints (NEW — collapsible)

An expandable panel listing all individual endpoints from checked datasets plus manually added endpoints.

```
┌──────────────────────────────────────────────┐
│ ▼ Active Endpoints (12)                      │
├──┬──────────┬────────────────────┬───────────┤
│ ☑│ v42      │ [Bacteroidetes   ] │ evenness  │
│ ☑│ v157     │ [Firmicutes-A    ] │ evenness  │
│ ☑│ v83      │ [Proteobacteria  ] │ evenness  │
│ ☐│ v201     │ [Actinobacteria  ] │ evenness  │  ← unchecked = hidden
│ ☑│ v519     │ [manual_ep_1     ] │ manual  ✕ │  ← removable
│ ☑│ v734     │ [v734            ] │ manual  ✕ │
└──┴──────────┴────────────────────┴───────────┘
```

Columns:
1. **Visibility checkbox** — checked = rendered on graph; unchecked = hidden. Initially all checked.
2. **Vertex ID** — read-only display (e.g., "v42")
3. **Editable label** — `shiny::textInput()` style widget. Shows the label from the dataset or `"v{id}"` default for manual endpoints. User can edit any label inline.
4. **Source badge + action** — shows origin ("evenness", "geodesic", "manual"). Manual endpoints get a remove (✕) button; dataset endpoints do not (they're controlled by the dataset checkbox above).

Design notes:
- Dataset endpoints appear as **read-only rows** (can hide via checkbox, cannot delete — controlled by dataset toggle in Section A)
- Manual endpoints appear as **editable/removable rows**
- The list is **additive**: final rendered endpoints = (visible dataset endpoints) ∪ (visible manual endpoints)
- The list updates reactively when dataset checkboxes change

#### Section C: Manual Selection Controls (NEW)

```
┌──────────────────────────────────────┐
│ [◉ Selection Mode: ON ]             │
│ Click a vertex in Plotly view to     │
│ add it as an endpoint.               │
│ Use lasso/box select for dense       │
│ regions.                             │
└──────────────────────────────────────┘
```

- Toggle button activates/deactivates vertex selection mode
- Button changes appearance when active (e.g., green highlight) to provide clear visual feedback
- Help text explains the interaction
- Requires Plotly mode (not RGL) — if RGL is the active renderer, show a note: "Switch to Plotly view to enable manual selection"

#### Section D: Endpoint Layout (unchanged)

The existing label size, offset, marker size, marker color controls remain as-is.

---

### 3.2 Plotly Click Event Wiring

#### Adding customdata for vertex identification

In `output$reference_plot` (lines ~2087-2253), add `customdata` to all vertex scatter traces so click events return the vertex ID:

For categorical traces (line ~2103):
```r
plotly::add_trace(
  ...,
  customdata = plot_data$vertex[sel],   # NEW
  ...
)
```

For continuous traces (line ~2141):
```r
plotly::add_trace(
  ...,
  customdata = plot_data$vertex,         # NEW
  ...
)
```

Add `source` attribute to the `plot_ly()` call (line ~2087):
```r
p <- plotly::plot_ly(source = "reference_plot")
```

#### Single-click handler

```r
shiny::observeEvent(
  plotly::event_data("plotly_click", source = "reference_plot"),
  {
    if (!isTRUE(manual_endpoint_selection_mode())) return()

    click <- plotly::event_data("plotly_click", source = "reference_plot")
    if (is.null(click)) return()

    vertex_id <- suppressWarnings(as.integer(click$customdata[[1]]))
    if (!is.finite(vertex_id) || vertex_id < 1L) return()

    add_manual_endpoint(vertex_id)
  },
  ignoreInit = TRUE
)
```

**Behavior**: When selection mode is ON, clicking any vertex immediately adds it as a manual endpoint with default label `"v{id}"`. A brief toast notification confirms the addition. No confirmation dialog (per design decision).

#### Lasso/box select handler

```r
shiny::observeEvent(
  plotly::event_data("plotly_selected", source = "reference_plot"),
  {
    if (!isTRUE(manual_endpoint_selection_mode())) return()

    sel <- plotly::event_data("plotly_selected", source = "reference_plot")
    if (is.null(sel) || nrow(sel) < 1L) return()

    vertex_ids <- suppressWarnings(as.integer(sel$customdata))
    vertex_ids <- unique(vertex_ids[is.finite(vertex_ids) & vertex_ids > 0L])
    if (length(vertex_ids) < 1L) return()

    if (length(vertex_ids) == 1L) {
      add_manual_endpoint(vertex_ids[[1]])
    } else {
      # Store candidates and show picker modal
      lasso_candidates(vertex_ids)
      shiny::showModal(build_lasso_picker_modal(vertex_ids))
    }
  },
  ignoreInit = TRUE
)
```

**Behavior**: Lasso/box select captures multiple vertices. If exactly one vertex is selected, it's added immediately. If multiple, a modal dialog appears listing the selected vertices (vertex ID + coordinates + any existing data labels) and the user clicks one to designate as endpoint. The modal includes an "Add All" option for power users who want to add all selected vertices at once.

---

### 3.3 New Reactive Values

Add after existing endpoint reactive values (line ~914):

```r
# Manual endpoint storage
# List with: vertices (integer), labels (named character), visible (named logical)
manual_endpoints <- shiny::reactiveVal(list(
  vertices = integer(0),
  labels = structure(character(0), names = character(0)),
  visible = structure(logical(0), names = character(0))
))

# Whether selection mode is active
manual_endpoint_selection_mode <- shiny::reactiveVal(FALSE)

# Lasso selection candidates (temporary, for modal picker)
lasso_candidates <- shiny::reactiveVal(integer(0))

# Individual endpoint visibility overrides for dataset endpoints
# Named logical: names are vertex IDs as strings, values are TRUE/FALSE
dataset_endpoint_visibility <- shiny::reactiveVal(
  structure(logical(0), names = character(0))
)
```

---

### 3.4 Helper Functions

#### `add_manual_endpoint(vertex_id, label = NULL)`

```r
add_manual_endpoint <- function(vertex_id, label = NULL) {
  vertex_id <- suppressWarnings(as.integer(vertex_id))
  if (!is.finite(vertex_id) || vertex_id < 1L) return(invisible(NULL))

  current <- manual_endpoints()
  if (vertex_id %in% current$vertices) {
    shiny::showNotification(
      sprintf("Vertex v%d is already a manual endpoint.", vertex_id),
      type = "warning", duration = 2
    )
    return(invisible(NULL))
  }

  # Also check dataset endpoints for duplicate warning (non-blocking)
  ep_active <- endpoint_overlay_active_datasets_only()
  if (vertex_id %in% ep_active$vertices) {
    shiny::showNotification(
      sprintf("Note: v%d already exists in a dataset. Adding as manual override.", vertex_id),
      type = "message", duration = 3
    )
  }

  lbl <- if (!is.null(label) && nzchar(label)) label else sprintf("v%d", vertex_id)
  nm <- as.character(vertex_id)

  new_vertices <- c(current$vertices, vertex_id)
  new_labels <- c(current$labels, structure(lbl, names = nm))
  new_visible <- c(current$visible, structure(TRUE, names = nm))

  manual_endpoints(list(
    vertices = new_vertices,
    labels = new_labels,
    visible = new_visible
  ))

  rgl_gen(rgl_gen() + 1L)

  shiny::showNotification(
    sprintf("Added endpoint v%d", vertex_id),
    type = "message", duration = 2
  )
}
```

#### `remove_manual_endpoint(vertex_id)`

```r
remove_manual_endpoint <- function(vertex_id) {
  vertex_id <- suppressWarnings(as.integer(vertex_id))
  current <- manual_endpoints()
  keep <- current$vertices != vertex_id
  nm <- as.character(vertex_id)

  manual_endpoints(list(
    vertices = current$vertices[keep],
    labels = current$labels[names(current$labels) != nm],
    visible = current$visible[names(current$visible) != nm]
  ))

  rgl_gen(rgl_gen() + 1L)
}
```

#### `update_manual_endpoint_label(vertex_id, new_label)`

```r
update_manual_endpoint_label <- function(vertex_id, new_label) {
  nm <- as.character(vertex_id)
  current <- manual_endpoints()
  if (nm %in% names(current$labels)) {
    current$labels[[nm]] <- new_label
    manual_endpoints(current)
    rgl_gen(rgl_gen() + 1L)
  }
}
```

---

### 3.5 Modified `endpoint_overlay_active()`

The existing reactive (lines 1432-1472) is extended to merge manual endpoints:

```r
endpoint_overlay_active <- shiny::reactive({
  # --- Existing dataset endpoint aggregation (unchanged) ---
  st <- endpoint_panel_state()
  rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
  # ... [existing code lines 1435-1467] ...

  # --- NEW: Apply per-endpoint visibility overrides for dataset endpoints ---
  vis_overrides <- dataset_endpoint_visibility()
  if (length(vis_overrides) > 0L && length(vertices_all) > 0L) {
    hidden <- names(vis_overrides)[!vis_overrides]
    hidden_int <- suppressWarnings(as.integer(hidden))
    hidden_int <- hidden_int[is.finite(hidden_int)]
    vertices_all <- setdiff(vertices_all, hidden_int)
    # Remove hidden vertices from label_lookup too
    for (h in as.character(hidden_int)) {
      label_lookup <- label_lookup[names(label_lookup) != h]
    }
  }

  # --- NEW: Merge manual endpoints ---
  manual <- manual_endpoints()
  if (is.list(manual) && length(manual$vertices) > 0L) {
    vis <- manual$visible %||% structure(rep(TRUE, length(manual$vertices)),
                                          names = as.character(manual$vertices))
    visible_manual <- manual$vertices[vis[as.character(manual$vertices)]]
    visible_manual <- visible_manual[is.finite(visible_manual) & visible_manual > 0L]

    vertices_all <- c(vertices_all, visible_manual)

    manual_labs <- manual$labels %||% character(0)
    for (jj in seq_along(visible_manual)) {
      nm <- as.character(visible_manual[[jj]])
      lbl <- manual_labs[[nm]] %||% sprintf("v%d", visible_manual[[jj]])
      label_lookup[[nm]] <- lbl
    }
  }

  # --- Existing dedup and return ---
  vertices_all <- sort(unique(suppressWarnings(as.integer(vertices_all))))
  vertices_all <- vertices_all[is.finite(vertices_all) & vertices_all > 0L]
  list(vertices = vertices_all, labels = label_lookup)
})
```

An additional helper reactive `endpoint_overlay_active_datasets_only()` provides dataset-only endpoints (without manual) for duplicate detection in `add_manual_endpoint()`.

---

### 3.6 Lasso Picker Modal

```r
build_lasso_picker_modal <- function(vertex_ids) {
  rows <- lapply(vertex_ids, function(vid) {
    shiny::tags$tr(
      shiny::tags$td(sprintf("v%d", vid)),
      shiny::tags$td(
        shiny::actionButton(
          sprintf("lasso_pick_%d", vid),
          "Add",
          class = "btn btn-sm btn-outline-primary"
        )
      )
    )
  })

  shiny::modalDialog(
    title = sprintf("Select endpoint from %d vertices", length(vertex_ids)),
    size = "s",
    shiny::div(
      class = "table-responsive",
      style = "max-height: 300px; overflow-y: auto;",
      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Vertex"),
            shiny::tags$th("")
          )
        ),
        shiny::tags$tbody(rows)
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton("lasso_add_all", "Add All", class = "btn btn-outline-secondary"),
      shiny::modalButton("Cancel")
    )
  )
}
```

Observers for each `lasso_pick_{vid}` button and for `lasso_add_all`:

```r
# Individual pick from lasso
shiny::observe({
  candidates <- lasso_candidates()
  lapply(candidates, function(vid) {
    shiny::observeEvent(input[[sprintf("lasso_pick_%d", vid)]], {
      add_manual_endpoint(vid)
      shiny::removeModal()
      lasso_candidates(integer(0))
    }, once = TRUE, ignoreInit = TRUE)
  })
})

# Add all from lasso
shiny::observeEvent(input$lasso_add_all, {
  candidates <- lasso_candidates()
  for (vid in candidates) {
    add_manual_endpoint(vid)
  }
  shiny::removeModal()
  lasso_candidates(integer(0))
}, ignoreInit = TRUE)
```

---

### 3.7 UI Builder Functions

#### `build_endpoint_dataset_table()` (replaces `build_endpoint_method_k_table()`)

```r
build_endpoint_dataset_table <- function(rows_df) {
  if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
    return(shiny::p(class = "gf-hint", "No endpoint datasets found."))
  }

  head_row <- shiny::tags$tr(
    shiny::tags$th(""),
    shiny::tags$th("Endpoint Dataset")
  )
  body_rows <- lapply(seq_len(nrow(rows_df)), function(ii) {
    rr <- rows_df[ii, , drop = FALSE]
    in_id <- as.character(rr$input_id[[1]] %||% "")
    checked <- isTRUE(rr$selected[[1]])
    dataset_label <- sprintf("%s_k%s", rr$method[[1]], rr$k_display[[1]])
    shiny::tags$tr(
      shiny::tags$td(
        shiny::tags$input(
          type = "checkbox", id = in_id,
          checked = if (checked) "checked" else NULL
        )
      ),
      shiny::tags$td(dataset_label)
    )
  })

  shiny::div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table table-sm gf-asset-table",
      shiny::tags$thead(head_row),
      shiny::tags$tbody(body_rows)
    )
  )
}
```

#### `build_active_endpoints_list()` (NEW)

```r
build_active_endpoints_list <- function(
  dataset_vertices,   # integer vector from dataset sources
  dataset_labels,     # named character from datasets
  dataset_sources,    # named character: vertex_id -> source name
  manual_ep           # manual_endpoints() list
) {
  # Combine dataset + manual endpoints
  all_vertices <- integer(0)
  all_labels <- character(0)
  all_sources <- character(0)
  all_is_manual <- logical(0)
  all_visible <- logical(0)

  # Dataset endpoints
  for (vid in dataset_vertices) {
    nm <- as.character(vid)
    all_vertices <- c(all_vertices, vid)
    all_labels <- c(all_labels, dataset_labels[[nm]] %||% sprintf("v%d", vid))
    all_sources <- c(all_sources, dataset_sources[[nm]] %||% "dataset")
    all_is_manual <- c(all_is_manual, FALSE)
    all_visible <- c(all_visible, TRUE)  # default; overridden by visibility state
  }

  # Manual endpoints
  if (is.list(manual_ep) && length(manual_ep$vertices) > 0L) {
    for (jj in seq_along(manual_ep$vertices)) {
      vid <- manual_ep$vertices[[jj]]
      nm <- as.character(vid)
      all_vertices <- c(all_vertices, vid)
      all_labels <- c(all_labels, manual_ep$labels[[nm]] %||% sprintf("v%d", vid))
      all_sources <- c(all_sources, "manual")
      all_is_manual <- c(all_is_manual, TRUE)
      all_visible <- c(all_visible, isTRUE(manual_ep$visible[[nm]]))
    }
  }

  if (length(all_vertices) < 1L) {
    return(shiny::p(class = "gf-hint", "No endpoints active."))
  }

  head_row <- shiny::tags$tr(
    shiny::tags$th(""),
    shiny::tags$th("Vertex"),
    shiny::tags$th("Label"),
    shiny::tags$th("Source"),
    shiny::tags$th("")
  )

  body_rows <- lapply(seq_along(all_vertices), function(ii) {
    vid <- all_vertices[[ii]]
    nm <- as.character(vid)
    shiny::tags$tr(
      class = if (all_is_manual[[ii]]) "gf-ep-row-manual" else "gf-ep-row-dataset",
      shiny::tags$td(
        shiny::tags$input(
          type = "checkbox",
          class = "gf-ep-visible-check",
          `data-vertex` = nm,
          checked = if (all_visible[[ii]]) "checked" else NULL
        )
      ),
      shiny::tags$td(class = "gf-ep-vertex-id", sprintf("v%d", vid)),
      shiny::tags$td(
        shiny::tags$input(
          type = "text",
          class = "form-control form-control-sm gf-ep-label-input",
          `data-vertex` = nm,
          value = all_labels[[ii]],
          style = "width: 110px; padding: 2px 4px; font-size: 0.85em;"
        )
      ),
      shiny::tags$td(
        shiny::tags$span(
          class = paste0("badge gf-ep-source-badge gf-ep-source-",
                         if (all_is_manual[[ii]]) "manual" else "dataset"),
          all_sources[[ii]]
        )
      ),
      shiny::tags$td(
        if (all_is_manual[[ii]]) {
          shiny::tags$button(
            class = "btn btn-sm btn-outline-danger gf-ep-remove-btn",
            `data-vertex` = nm,
            "\u00d7"  # × symbol
          )
        }
      )
    )
  })

  shiny::div(
    class = "table-responsive gf-ep-list-container",
    style = "max-height: 280px; overflow-y: auto;",
    shiny::tags$table(
      class = "table table-sm gf-ep-list-table",
      shiny::tags$thead(head_row),
      shiny::tags$tbody(body_rows)
    )
  )
}
```

---

### 3.8 JavaScript Bridge for Inline Edits

Since the endpoint list uses raw HTML inputs (not Shiny inputs), we need a small JS snippet to relay changes back to the Shiny server. Add to `inst/app/www/` or inline via `shiny::tags$script()`:

```javascript
// Relay endpoint label edits to Shiny
$(document).on('change', '.gf-ep-label-input', function() {
  var vertex = $(this).data('vertex');
  var label = $(this).val();
  Shiny.setInputValue('ep_label_edit', {vertex: vertex, label: label}, {priority: 'event'});
});

// Relay endpoint visibility checkbox changes
$(document).on('change', '.gf-ep-visible-check', function() {
  var vertex = $(this).data('vertex');
  var visible = this.checked;
  Shiny.setInputValue('ep_visibility_toggle', {vertex: vertex, visible: visible}, {priority: 'event'});
});

// Relay manual endpoint remove button clicks
$(document).on('click', '.gf-ep-remove-btn', function() {
  var vertex = $(this).data('vertex');
  Shiny.setInputValue('ep_manual_remove', {vertex: parseInt(vertex)}, {priority: 'event'});
});
```

Corresponding server observers:

```r
shiny::observeEvent(input$ep_label_edit, {
  msg <- input$ep_label_edit
  vid <- suppressWarnings(as.integer(msg$vertex))
  lbl <- as.character(msg$label %||% "")
  if (is.finite(vid) && nzchar(lbl)) {
    update_manual_endpoint_label(vid, lbl)
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$ep_visibility_toggle, {
  msg <- input$ep_visibility_toggle
  vid <- suppressWarnings(as.integer(msg$vertex))
  vis <- isTRUE(msg$visible)
  if (is.finite(vid)) {
    # Check if it's a manual endpoint
    manual <- manual_endpoints()
    nm <- as.character(vid)
    if (vid %in% manual$vertices) {
      manual$visible[[nm]] <- vis
      manual_endpoints(manual)
    } else {
      # Dataset endpoint visibility override
      overrides <- dataset_endpoint_visibility()
      overrides[[nm]] <- vis
      dataset_endpoint_visibility(overrides)
    }
    rgl_gen(rgl_gen() + 1L)
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$ep_manual_remove, {
  msg <- input$ep_manual_remove
  vid <- suppressWarnings(as.integer(msg$vertex))
  if (is.finite(vid)) {
    remove_manual_endpoint(vid)
  }
}, ignoreInit = TRUE)
```

---

### 3.9 Session Lifecycle

#### Reset on project switch

Add to the existing `observeEvent(rv$project.id, ...)` cleanup block:

```r
manual_endpoints(list(
  vertices = integer(0),
  labels = structure(character(0), names = character(0)),
  visible = structure(logical(0), names = character(0))
))
manual_endpoint_selection_mode(FALSE)
lasso_candidates(integer(0))
dataset_endpoint_visibility(structure(logical(0), names = character(0)))
```

#### Renderer mode awareness

When the active renderer is RGL (not Plotly), the selection mode toggle should be disabled with a tooltip:

```r
if (!isTRUE(renderer_is_plotly())) {
  shiny::tags$div(
    class = "gf-hint",
    "Manual vertex selection requires Plotly view. ",
    "Switch renderer in Graph Layout controls."
  )
} else {
  # Show the toggle button
}
```

---

## 4. Revised Endpoint Panel Layout (Complete)

```
┌─────────────────────────────────────────────┐
│ ▼ Endpoints                                 │
│                                             │
│  Endpoint Datasets                          │
│  ┌──┬────────────────────────────────┐      │
│  │ ☑│ evenness_k5                    │      │
│  │ ☐│ geodesic_core                  │      │
│  └──┴────────────────────────────────┘      │
│                                             │
│  ▶ Active Endpoints (8)                     │
│  [expandable — see Section 3.7]             │
│                                             │
│  ┌──────────────────────────────────┐       │
│  │ [◉ Selection Mode: ON ]         │       │
│  │  Click vertex in Plotly to add.  │       │
│  │  Lasso/box for dense regions.    │       │
│  └──────────────────────────────────┘       │
│                                             │
│  ─────────────────────────────────          │
│  Endpoint Layout                            │
│  Label size:    [1x         ▼]              │
│  Label offset:  [1x         ▼]              │
│  Marker size:   [1x         ▼]              │
│  Marker color:  [Red        ▼]              │
└─────────────────────────────────────────────┘
```

---

## 5. Implementation Phases

### Phase 1: Reactive Infrastructure
1. Add new `reactiveVal` declarations (`manual_endpoints`, `manual_endpoint_selection_mode`, `lasso_candidates`, `dataset_endpoint_visibility`)
2. Add helper functions (`add_manual_endpoint`, `remove_manual_endpoint`, `update_manual_endpoint_label`)
3. Extend `endpoint_overlay_active()` to merge manual endpoints and apply visibility overrides
4. Add cleanup to project-switch observer
5. Add `endpoint_overlay_active_datasets_only()` helper reactive

### Phase 2: UI Components
6. Replace `build_endpoint_method_k_table()` with `build_endpoint_dataset_table()`
7. Create `build_active_endpoints_list()` function
8. Add selection mode toggle button and help text
9. Add JavaScript bridge for inline edits/visibility/remove
10. Add CSS classes for endpoint list styling

### Phase 3: Plotly Event Wiring
11. Add `source = "reference_plot"` and `customdata = vertex_ids` to plotly traces
12. Add click event observer with selection-mode guard
13. Add lasso/box select event observer
14. Add lasso picker modal and its observers

### Phase 4: Polish & Edge Cases
15. Renderer mode detection (disable selection mode when RGL is active)
16. Duplicate vertex detection (manual vs. dataset)
17. Toast notifications for user feedback
18. Scrollable endpoint list for large datasets
19. CSS for visual feedback (selection mode active state, source badges)

---

## 6. Edge Cases & Considerations

| Scenario | Handling |
|---|---|
| Click vertex already in dataset | Allow as manual override; show info notification |
| Click vertex already manual | Show warning; do not duplicate |
| Remove manual endpoint that duplicates dataset | Dataset endpoint remains visible |
| Edit label of dataset endpoint | Currently not supported in this design (dataset labels are read-only from files). Could be added as a future enhancement. |
| Switch from Plotly to RGL while selection mode ON | Auto-deactivate selection mode |
| Hundreds of endpoints in Active list | Scrollable container (max-height: 280px), static HTML rendering (no dynamic Shiny inputs) |
| Project switch | All manual state reset to empty |
| Label set to empty string | Fallback to `"v{id}"` |

---

## 7. Future Enhancements (Not in Scope)

- **Persist manual endpoints** to an RDS/CSV file alongside project assets so they survive session restarts
- **Drag-and-drop reordering** of endpoints in the Active list
- **Batch import** of manual endpoints from a CSV file
- **Edit dataset endpoint labels** (currently read-only; would require writing back to project files)
- **Undo/redo** stack for manual endpoint operations
- **Keyboard shortcuts** for selection mode toggle (e.g., `Shift+E`)
