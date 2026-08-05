phase5_panel_bundle <- function(
    suffix = "base",
    vertex.mass = c(0.52, 0.03, 0.21, 0.02, 0.14, 0.08)) {
  adjacency <- list(
    2L,
    c(1L, 3L, 4L),
    2L,
    c(2L, 5L, 6L),
    4L,
    4L
  )
  edge.lengths <- lapply(
    adjacency,
    function(neighbors) rep(1, length(neighbors))
  )
  field <- c(5, 1, 4, 0, 3, 2)
  vertex.ids <- paste0("v", seq_along(field))
  graph.identity <- gflowui:::gflowui_basin_graph_identity(
    adj_list = adjacency,
    edge_length_list = edge.lengths,
    vertex_id = vertex.ids,
    graph_id = paste0("phase5-graph-", suffix),
    graph_k = 2L
  )
  build.identity <- gflow::get.gflow.build.identity()
  construction.record <- list(
    schema = "gflowui_basin_construction_identity/2",
    project.id = paste0("phase5-project-", suffix),
    graph.set.id = "phase5-set",
    graph = graph.identity,
    source.key = "occupation_density_active",
    source.fingerprint = paste0("source-", suffix),
    field.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(field),
    mass.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(vertex.mass),
    mass.provenance = list(kind = "test"),
    alignment.validation = list(status = "validated"),
    construction = list(
      method = "trajectory_flow",
      direction = "both"
    ),
    gflow.build.id = build.identity$build.id,
    gflow.runtime.id = build.identity$runtime$id
  )
  construction <- list(
    record = construction.record,
    fingerprint = gflowui:::gflowui_basin_sha256(
      construction.record
    )
  )
  basin <- gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "trajectory_flow",
    direction = "both",
    vertex.mass = vertex.mass,
    method.params = list(
      edge.length.quantile.thld = 1,
      store.trajectories = FALSE
    ),
    vertex.id = vertex.ids
  )
  prominence <- gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "superlevel_merge_tree",
    direction = "both",
    vertex.mass = vertex.mass,
    vertex.id = vertex.ids
  )
  result <- list(
    basin = basin,
    prominence_complex = prominence,
    build_identity = build.identity
  )
  request <- list(
    source = list(
      key = "occupation_density_active",
      label = paste("Phase 5", suffix),
      values = field,
      graph = list(
        adj_list = adjacency,
        weight_list = edge.lengths
      )
    ),
    vertex_id = vertex.ids,
    alignment = list(source.id = paste0("subject-", suffix)),
    source_fingerprint = paste0("source-", suffix),
    construction_identity = construction
  )
  gflowui:::gflowui_basin_bundle_from_overlay(result, request)
}

phase5_panel_state <- function(bundle,
                               controls = NULL,
                               selected.ids = character()) {
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  if (is.list(controls)) {
    state$controls[names(controls)] <- controls
  }
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("recompute")
  )
  result <- gflowui:::gflowui_basin_execute_pending(state$pending.work)
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("result", result = result)
  )
  if (length(selected.ids)) {
    state <- gflowui:::gflowui_basin_reduce_state(
      state,
      gflowui:::gflowui_basin_state_event(
        "selection_change",
        ids = selected.ids
      )
    )
  }
  state
}

phase5_records_runtime <- function(records, suffix = "records") {
  canonical <- data.frame(
    basin.id = records$id,
    type = "max",
    extremum.vertex = seq_len(nrow(records)),
    birth.level = records$peak,
    death.level = records$peak - records$prominence,
    persistence = records$prominence,
    parent.basin.id = records$parent,
    component = 1L,
    peak.value = records$peak,
    trajectory.basin.id = records$id,
    trajectory.flow.mass = records$mass,
    trajectory.flow.support = records$support,
    stringsAsFactors = FALSE
  )
  canonical <- canonical[
    order(canonical$basin.id, method = "radix"),
    ,
    drop = FALSE
  ]
  data <- list(
    identity = {
      fields <- gflowui:::.gflowui_basin_required_identities()
      stats::setNames(
        lapply(fields, function(field) paste(field, suffix, sep = "-")),
        fields
      )
    },
    direction = "max",
    graph = list(),
    vertex.ids = character(),
    source.values = numeric(),
    trajectory.table = data.frame(),
    canonical.tree = list(test.fixture = suffix),
    canonical = canonical,
    validation = list(
      source = "valid",
      mapping = "valid",
      trajectory_flow_mass = "valid",
      trajectory_flow_support = "valid",
      source_peak = "valid",
      canonical_prominence = "valid"
    ),
    component.selection = list(
      id = 1L,
      rule = "greatest_positive_mass",
      fallback.reason = NULL,
      totals = c(`1` = sum(canonical$trajectory.flow.mass))
    ),
    component.ids = 1L
  )
  bundle <- new.env(parent = emptyenv())
  class(bundle) <- c(
    "runtime.scientific.bundle",
    "gflowui_basin_scientific_bundle",
    "environment"
  )
  bundle$bundle.id <- gflowui:::.gflowui_basin_bundle_id()
  bundle$data <- gflowui:::.gflowui_basin_copy(data)
  lockEnvironment(bundle, bindings = TRUE)
  parent <- stats::setNames(
    canonical$parent.basin.id,
    canonical$basin.id
  )
  accessor <- function(
      x,
      direction = "max",
      component = NULL,
      basin.ids = NULL,
      close.ancestors = FALSE,
      ...) {
    requested <- if (is.null(basin.ids)) {
      canonical$basin.id
    } else {
      as.character(basin.ids)
    }
    closure <- requested
    repeat {
      ancestors <- unname(parent[closure])
      expanded <- unique(c(
        closure,
        ancestors[!is.na(ancestors)]
      ))
      if (length(expanded) == length(closure)) break
      closure <- expanded
    }
    added <- setdiff(closure, requested)
    if (!close.ancestors && length(added)) {
      stop("selection is not ancestor-closed")
    }
    selected <- if (close.ancestors) closure else requested
    branches <- canonical[
      match(sort(selected, method = "radix"), canonical$basin.id),
      ,
      drop = FALSE
    ]
    list(
      requested.basin.ids = requested,
      closure.added.ids = sort(added, method = "radix"),
      basin.ids = branches$basin.id,
      branches = branches,
      validation.status = "ok"
    )
  }
  list(bundle = bundle, accessor = accessor)
}

phase5_runtime_state <- function(runtime, controls) {
  state <- gflowui:::gflowui_basin_new_runtime_state(runtime$bundle)
  state$controls[names(controls)] <- controls
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("recompute")
  )
  result <- gflowui:::gflowui_basin_execute_pending(
    state$pending.work,
    layout.accessor = runtime$accessor
  )
  gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("result", result = result)
  )
}

phase6_inspector_result <- function(state) {
  canonical <- gflowui:::gflowui_basin_bundle_snapshot(
    state$bundle
  )$canonical
  maxima <- data.frame(
    key = paste("max", canonical$trajectory.basin.id, sep = "|"),
    type = "max",
    basin.id = canonical$trajectory.basin.id,
    extremum.vertex = canonical$extremum.vertex,
    rank = rev(seq_len(nrow(canonical))),
    primary.support.size = canonical$trajectory.flow.support,
    primary.support.mass = canonical$trajectory.flow.mass,
    extremum.value = canonical$peak.value,
    prominence = canonical$persistence,
    raw.support.size = canonical$trajectory.flow.support,
    retained.support.size = canonical$trajectory.flow.support,
    retained.support.mass = canonical$trajectory.flow.mass,
    retention.status = "retained",
    color = "#2563EB",
    selected = FALSE,
    display.label = paste0("rank-dependent-", seq_len(nrow(canonical))),
    stringsAsFactors = FALSE
  )
  minimum <- data.frame(
    key = "min|min_fixture",
    type = "min",
    basin.id = "min_fixture",
    extremum.vertex = max(canonical$extremum.vertex) + 1L,
    rank = 1L,
    primary.support.size = 2,
    primary.support.mass = 0.01,
    extremum.value = -1,
    prominence = 0.25,
    raw.support.size = 2,
    retained.support.size = 2,
    retained.support.mass = 0.01,
    retention.status = "retained",
    color = "#06B6D4",
    selected = FALSE,
    display.label = "rank-dependent-minimum",
    stringsAsFactors = FALSE
  )
  all.table <- rbind(maxima, minimum)
  gflowui:::gflowui_basin_prepare_analysis_result(list(
    all_table = all.table,
    table = all.table[seq_len(min(2L, nrow(all.table))), , drop = FALSE]
  ))
}

phase5_single_branch_bundle <- function() {
  adjacency <- list(integer())
  source <- stats::setNames(1, "v1")
  tree <- gflow::get.basin.merge.tree(
    gflow::create.basin.complex(
      adjacency,
      list(numeric()),
      source,
      method = "superlevel_merge_tree",
      direction = "max",
      vertex.mass = 1,
      vertex.id = "v1"
    )
  )
  identity <- {
    fields <- gflowui:::.gflowui_basin_required_identities()
    stats::setNames(
      lapply(fields, function(field) paste(field, "single", sep = "-")),
      fields
    )
  }
  gflowui:::gflowui_basin_new_scientific_bundle(
    graph = adjacency,
    vertex.ids = "v1",
    source.values = source,
    identity = identity,
    trajectory.table = data.frame(
      trajectory.basin.id = "trajectory_single",
      direction = "max",
      component = 1L,
      extremum.vertex = 1L,
      primary.support.mass = 1,
      primary.support.size = 1,
      stringsAsFactors = FALSE
    ),
    canonical.tree = tree
  )
}

test_that("merge-tree panel model derives complete disclosed state", {
  bundle <- phase5_panel_bundle("model")
  state <- phase5_panel_state(bundle)
  model <- gflowui:::gflowui_basin_merge_tree_model(state)

  expect_true(model$available)
  expect_true(model$renderable)
  expect_identical(model$display.source, "current")
  expect_identical(model$attempt.outcome, "proposal_created")
  expect_identical(
    sort(model$layout$basin.ids),
    sort(model$proposal$final.ids)
  )
  expect_identical(
    model$counts$final,
    length(model$proposal$final.ids)
  )
  expect_identical(
    model$status$mass.owner,
    "trajectory-flow primary.support.mass"
  )
  expect_true(model$mass$available)
  expect_true(is.finite(model$mass$core.coverage))
  expect_true(is.finite(model$mass$final.coverage))
})

test_that("static tree uses exact layout and trajectory-flow annotations", {
  bundle <- phase5_panel_bundle("plot")
  state <- phase5_panel_state(bundle)
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  inputs <- gflowui:::gflowui_basin_tree_plot_inputs(model)
  canonical <- gflowui:::gflowui_basin_bundle_snapshot(bundle)$canonical
  index <- match(
    inputs$tree$basin.table$basin.id,
    canonical$basin.id
  )

  expect_identical(
    inputs$tree$basin.table$trajectory.flow.mass,
    canonical$trajectory.flow.mass[index]
  )
  expect_identical(
    inputs$tree$basin.table$trajectory.flow.support,
    canonical$trajectory.flow.support[index]
  )
  expect_identical(inputs$layout$leaf.order, model$layout$leaf.order)
  expect_setequal(names(inputs$colors), model$proposal$final.ids)

  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 1200, height = 1000, res = 120)
  plotted <- gflowui:::gflowui_basin_plot_merge_tree(model)
  grDevices::dev.off()
  expect_gt(file.info(image)$size, 1000)
  expect_identical(plotted$layout$leaf.order, model$layout$leaf.order)
  expect_identical(
    sort(plotted$layout$basin.ids),
    sort(model$proposal$final.ids)
  )
})

test_that("tree terminology identifies continuation semantics", {
  bundle <- phase5_panel_bundle("density-value-terminology")
  state <- phase5_panel_state(bundle)
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  complete.layout <- gflowui:::gflowui_basin_complete_layout(
    bundle,
    component = state$context$component
  )
  captured <- new.env(parent = emptyenv())
  captured$titles <- character()
  plotter <- function(..., main.tree, basin.ids = NULL) {
    captured$titles <- c(captured$titles, main.tree)
    list(layout = if (is.null(basin.ids)) complete.layout else model$layout)
  }

  gflowui:::gflowui_basin_draw_merge_tree(
    model$panel,
    plotter = plotter
  )
  gflowui:::gflowui_basin_draw_merge_tree(
    model$panel,
    complete = TRUE,
    plotter = plotter
  )

  expect_identical(
    captured$titles,
    c(
      "Filtered crossing-free field-value elder-rule merge tree",
      "Complete crossing-free field-value elder-rule merge tree"
    )
  )
  ui <- htmltools::renderTags(
    gflowui:::gflowui_basin_merge_tree_panel_ui(model$panel)
  )$html
  expect_match(
    ui,
    "The tree is built from graph superlevel sets",
    fixed = TRUE
  )
  expect_match(ui, "Each local maximum starts a branch", fixed = TRUE)
  expect_match(
    ui,
    "Exact birth-value ties use the extremum-vertex index",
    fixed = TRUE
  )
  expect_match(
    ui,
    "The continuation selector changes only which basin identity",
    fixed = TRUE
  )
  expect_match(
    ui,
    "Continuation lifetime reports the corresponding quantity",
    fixed = TRUE
  )
  expect_match(ui, "How the tree and controls work", fixed = TRUE)
  expect_match(ui, "Core branch budget", fixed = TRUE)
  expect_match(
    ui,
    "soft upper limit on the number of positive-mass branches",
    fixed = TRUE
  )
  expect_match(
    ui,
    "exact mass ties kept together",
    fixed = TRUE
  )
  expect_match(
    ui,
    "does not discard scientifically required branches",
    fixed = TRUE
  )
  expect_match(
    ui,
    "Sentinels protect scientifically notable branches",
    fixed = TRUE
  )
  expect_match(
    ui,
    "labels the union of the top Important-label count branches",
    fixed = TRUE
  )
  expect_match(
    ui,
    "Use all branches (Filter: None)",
    fixed = TRUE
  )
  expect_match(
    ui,
    "The horizontal navigator starts at a finite level above every maximum",
    fixed = TRUE
  )
  expect_match(
    ui,
    "Merge plateaus are the one or more graph vertices",
    fixed = TRUE
  )
  expect_match(
    ui,
    "A display recipe lets you reuse the same filtering",
    fixed = TRUE
  )
  expect_match(ui, "Initially selected for display", fixed = TRUE)
  expect_match(ui, "Final branches displayed", fixed = TRUE)
  expect_match(ui, "Static rendering", fixed = TRUE)
  expect_false(grepl("Positive-mass coverage", ui, fixed = TRUE))
  expect_false(grepl("Sentinel-only", ui, fixed = TRUE))
  expect_false(grepl("Mass ownership", ui, fixed = TRUE))
  expect_false(grepl("Display source", ui, fixed = TRUE))
  expect_false(grepl('id="basin_tree_component"', ui, fixed = TRUE))

  multi.component.summary <- htmltools::renderTags(
    gflowui:::.gflowui_basin_panel_summary_ui(
      total.maximum.count = 9L,
      component.count = 2L,
      component.id = 2L,
      component.maximum.count = 4L,
      core.count = 3L,
      final.count = 4L,
      core.outcome = "coverage",
      render.outcome = "renderable"
    )
  )$html
  expect_match(
    multi.component.summary,
    "Graph component",
    fixed = TRUE
  )
  expect_match(
    multi.component.summary,
    "2 of 2 (4 maximum basins)",
    fixed = TRUE
  )
  expect_identical(
    gflowui:::.gflowui_basin_complete_viewer_title(),
    "Complete Interactive Density-Value Elder-Rule Basin Merge Tree"
  )
})

test_that("continuation selector drives layout metadata and adaptive titles", {
  bundle <- phase5_panel_bundle(
    "continuation-policy",
    vertex.mass = c(0.05, 0.03, 0.04, 0.02, 0.80, 0.06)
  )
  state <- phase5_panel_state(bundle)
  canonical.model <- gflowui:::gflowui_basin_merge_tree_model(state)
  mass.model <- gflowui:::gflowui_basin_merge_tree_model(
    state,
    continuation.rule = "mass"
  )
  support.model <- gflowui:::gflowui_basin_merge_tree_model(
    state,
    continuation.rule = "support"
  )

  expect_identical(mass.model$continuation$rule, "mass")
  expect_identical(
    canonical.model$layout$component.root.basin.id,
    "basin_max_v00000001"
  )
  expect_identical(
    mass.model$layout$component.root.basin.id,
    "basin_max_v00000005"
  )
  expect_identical(
    mass.model$layout$continuation$measure,
    "Trajectory-flow basin mass"
  )
  expect_true(all(is.finite(
    mass.model$layout$branches$continuation.lifetime
  )))
  expect_identical(
    gflowui:::gflowui_basin_continuation_tree_title(
      mass.model$continuation
    ),
    "Filtered crossing-free trajectory-flow mass-priority continuation tree"
  )
  expect_identical(
    gflowui:::gflowui_basin_continuation_tree_title(
      support.model$continuation,
      complete = TRUE
    ),
    "Complete crossing-free trajectory-flow support-priority continuation tree"
  )
  expect_match(
    gflowui:::gflowui_basin_continuation_description("mass"),
    "Exact mass ties use the canonical field-value elder rule",
    fixed = TRUE
  )
  interactive <- gflowui:::gflowui_basin_interactive_tree_data(
    state,
    level.index = length(
      gflowui:::gflowui_basin_interactive_levels(state)
    ) - 1L,
    continuation.rule = "mass"
  )
  expect_identical(
    interactive$cut$components$basin.id,
    "basin_max_v00000005"
  )
})

test_that("Phase 6 views preserve complete maxima and stable identities", {
  records <- data.frame(
    id = c("r", "a", "b", "c", "d", "e"),
    parent = c(NA, "r", "r", "r", "r", "r"),
    mass = c(0.5, 0.3, 0.19, 0.009, 0.0009, 0.0001),
    support = c(20, 12, 8, 1, 2, 3),
    peak = c(10, 8, 7, 100, 6, 5),
    prominence = c(10, 4, 3, 2, 1, 0.5),
    stringsAsFactors = FALSE
  )
  runtime <- phase5_records_runtime(records, "phase6-inspector")
  controls <- gflowui:::gflowui_basin_default_controls(nrow(records))
  controls$filter.mode <- "top_k"
  controls$top.k <- 3L
  controls$peak.sentinel.enabled <- FALSE
  controls$prominence.sentinel.enabled <- FALSE
  controls$support.sentinel.enabled <- FALSE
  controls$sentinel.top.n <- 0L
  state <- phase5_runtime_state(runtime, controls)
  result <- phase6_inspector_result(state)

  expect_equal(nrow(result$table), 7L)
  expect_equal(nrow(result$all_table), 7L)
  expect_setequal(
    result$all_table$display.label[result$all_table$type == "max"],
    paste0("M", seq_len(6L))
  )
  initial <- gflowui:::gflowui_basin_inspector_rows(
    result,
    state,
    scope = "initial_display",
    sort.by = "mass"
  )
  all.maxima <- gflowui:::gflowui_basin_inspector_rows(
    result,
    state,
    scope = "all_maxima",
    sort.by = "peak"
  )
  expect_equal(nrow(initial), 3L)
  expect_equal(nrow(all.maxima), 6L)
  expect_false("max|c" %in% initial$key)
  expect_true("max|c" %in% all.maxima$key)
  expect_identical(
    all.maxima$proposal.visibility[all.maxima$key == "max|c"],
    "hidden"
  )
  expect_true(all(c(
    "proposal.membership.class",
    "proposal.inclusion.reasons",
    "proposal.core",
    "proposal.sentinel",
    "proposal.ancestor.only",
    "proposal.pinned",
    "proposal.selected",
    "proposal.visibility"
  ) %in% names(all.maxima)))

  mass.sorted <- gflowui:::gflowui_basin_inspector_rows(
    result,
    state,
    scope = "all_maxima",
    sort.by = "mass"
  )
  peak.sorted <- gflowui:::gflowui_basin_inspector_rows(
    result,
    state,
    scope = "all_maxima",
    sort.by = "peak"
  )
  mass.labels <- stats::setNames(mass.sorted$canonical.label, mass.sorted$key)
  peak.labels <- stats::setNames(peak.sorted$canonical.label, peak.sorted$key)
  expect_identical(mass.labels[sort(names(mass.labels))],
                   peak.labels[sort(names(peak.labels))])
  expect_false(identical(mass.sorted$key, peak.sorted$key))

  defaults <- gflowui:::gflowui_basin_default_plot_specs("phase6")
  expect_true(all(vapply(defaults, function(spec) {
    identical(spec$scope, "component_maxima") &&
      identical(spec$type, "max") &&
      identical(spec$point_color, "proposal")
  }, logical(1))))
  plot.data <- gflowui:::gflowui_basin_plot_data(
    result,
    scope = defaults[[1L]]$scope,
    type = defaults[[1L]]$type,
    analysis_state = state
  )
  expect_equal(nrow(plot.data), 6L)
  expect_true("max|c" %in% plot.data$key)
  expect_identical(plot.data$membership[plot.data$key == "max|c"], "hidden")
  expect_equal(plot.data$extremum_value[plot.data$key == "max|c"], 100)

  selected.id <- gflowui:::gflowui_basin_selected_canonical_ids(
    result,
    state,
    "max|c"
  )
  expect_identical(selected.id, "c")
  selected.state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("selection_change", ids = "c")
  )
  selected.row <- gflowui:::gflowui_basin_inspector_rows(
    result,
    selected.state,
    scope = "selected"
  )
  expect_identical(selected.row$key, "max|c")
  expect_true(selected.row$proposal.selected)
  expect_true(selected.row$proposal.hidden)

  pin.pending <- gflowui:::gflowui_basin_reduce_state(
    selected.state,
    gflowui:::gflowui_basin_state_event("pin", id = "c")
  )
  pin.result <- gflowui:::gflowui_basin_execute_pending(
    pin.pending$pending.work,
    layout.accessor = runtime$accessor
  )
  pinned.state <- gflowui:::gflowui_basin_reduce_state(
    pin.pending,
    gflowui:::gflowui_basin_state_event("result", result = pin.result)
  )
  pinned.row <- gflowui:::gflowui_basin_inspector_rows(
    result,
    pinned.state,
    scope = "pinned"
  )
  pinned.core <- gflowui:::gflowui_basin_inspector_rows(
    result,
    pinned.state,
    scope = "core"
  )
  expect_identical(pinned.row$key, "max|c")
  expect_false("max|c" %in% pinned.core$key)
  expect_true(pinned.row$proposal.pinned)
  expect_true(pinned.row$proposal.visible)
  expect_match(pinned.row$proposal.inclusion.reasons, "pinned", fixed = TRUE)
  pinned.model <- gflowui:::gflowui_basin_merge_tree_model(
    pinned.state,
    layout.accessor = runtime$accessor
  )
  selection.ui <- htmltools::renderTags(
    gflowui:::.gflowui_basin_panel_selection_ui(pinned.model$panel)
  )$html
  expect_match(selection.ui, 'id="basin_tree_unpin_selected"', fixed = TRUE)
  expect_false(grepl(
    'id="basin_tree_pin_selected"',
    selection.ui,
    fixed = TRUE
  ))

  exported <- gflowui:::gflowui_basin_export_characteristics(result)
  expect_equal(nrow(exported), 7L)
  expect_identical(
    exported$extremum_basin,
    result$all_table$display.label
  )
  expect_identical(exported$rank, result$all_table$label.rank)
})

test_that("merge-tree labels use the global readable basin map", {
  records <- data.frame(
    id = c("root", "a", "b"),
    parent = c(NA, "root", "root"),
    mass = c(0.2, 0.7, 0.1),
    support = c(8, 3, 5),
    peak = c(10, 8, 7),
    prominence = c(10, 4, 3),
    stringsAsFactors = FALSE
  )
  runtime <- phase5_records_runtime(records, "readable-label-map")
  controls <- gflowui:::gflowui_basin_default_controls(nrow(records))
  controls$filter.mode <- "none"
  state <- phase5_runtime_state(runtime, controls)
  result <- phase6_inspector_result(state)
  label.map <- gflowui:::gflowui_basin_canonical_label_map(result, state)
  model <- gflowui:::gflowui_basin_merge_tree_model(
    state,
    layout.accessor = runtime$accessor,
    display.labels = label.map
  )
  expect_identical(
    unname(model$labels$text[names(label.map)]),
    unname(label.map)
  )
  expect_true(all(grepl("^M[0-9]+$", unname(label.map))))

  real.bundle <- phase5_panel_bundle("readable-complete-labels")
  real.state <- phase5_panel_state(real.bundle)
  real.result <- phase6_inspector_result(real.state)
  real.map <- gflowui:::gflowui_basin_canonical_label_map(
    real.result,
    real.state
  )
  complete <- gflowui:::gflowui_basin_complete_interactive_data(
    real.state,
    label.text = real.map
  )
  expect_identical(
    unname(stats::setNames(
      complete$points$display.label,
      complete$points$basin.id
    )[names(real.map)]),
    unname(real.map)
  )
})

test_that("sparse and absent label modes satisfy the canonical plot API", {
  bundle <- phase5_panel_bundle("labels")
  state <- phase5_panel_state(bundle)
  state$presentation$label.mode <- "none"
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  inputs <- gflowui:::gflowui_basin_tree_plot_inputs(model)

  expect_true(all(nzchar(inputs$labels)))
  expect_identical(anyDuplicated(inputs$labels), 0L)

  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 1200, height = 1000, res = 120)
  expect_silent(gflowui:::gflowui_basin_plot_merge_tree(model))
  grDevices::dev.off()
  expect_gt(file.info(image)$size, 1000)
})

test_that("diagnostic rendering excludes exact zero mass", {
  bundle <- phase5_panel_bundle("diagnostic")
  state <- phase5_panel_state(bundle)
  state$presentation$diagnostics.visible <- TRUE
  model <- gflowui:::gflowui_basin_merge_tree_model(state)

  expect_true(model$diagnostics$available)
  expect_true(all(is.finite(model$diagnostics$log10.mass)))
  expect_false(any(model$diagnostics$ranked$log10.mass == -Inf))
  expect_identical(
    nrow(model$diagnostics$ranked) + model$diagnostics$zero.count,
    model$component.maximum.count
  )

  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 900, height = 1200, res = 120)
  plotted <- gflowui:::gflowui_basin_plot_diagnostics(model)
  grDevices::dev.off()
  expect_gt(file.info(image)$size, 1000)
  expect_identical(plotted, model$diagnostics)
})

test_that("overflow explanations are cause-specific and exact", {
  counts <- list(core = 51L, sentinel.only = 12L, final = 87L)
  expect_match(
    gflowui:::.gflowui_basin_panel_overflow_text(
      "core_overflow",
      counts,
      50L
    ),
    "core contains 51.*budget of 50"
  )
  expect_match(
    gflowui:::.gflowui_basin_panel_overflow_text(
      "sentinel_overflow",
      counts,
      60L
    ),
    "core plus mandatory sentinels contains 63.*budget of 60"
  )
  expect_match(
    gflowui:::.gflowui_basin_panel_overflow_text(
      "closure_overflow",
      counts,
      80L
    ),
    "ancestor closure expands.*87.*budget of 80"
  )
  expect_null(gflowui:::.gflowui_basin_panel_overflow_text(
    "renderable",
    counts,
    100L
  ))
})

test_that("complete interactive data preserves canonical coordinates", {
  bundle <- phase5_panel_bundle("complete")
  state <- phase5_panel_state(bundle)
  data <- gflowui:::gflowui_basin_complete_interactive_data(state)
  layout <- gflowui:::gflowui_basin_complete_layout(
    bundle,
    component = state$context$component
  )

  expect_identical(data$layout$leaf.order, layout$leaf.order)
  expect_identical(data$points$basin.id, layout$branches$basin.id)
  expect_identical(
    data$points$x,
    layout$coordinates$branches$x
  )
  expect_identical(
    data$points$birth.level,
    layout$coordinates$branches$birth.level
  )
  expect_identical(
    data$points$death.level,
    layout$coordinates$branches$death.level
  )
  expect_identical(nrow(data$vertical), 3L * nrow(data$points))
  expect_identical(
    nrow(data$horizontal),
    3L * nrow(layout$coordinates$events)
  )
})

test_that("interactive tree and canonical graph cut share exact levels", {
  bundle <- phase5_panel_bundle("interactive-cut")
  state <- phase5_panel_state(bundle)
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  ids <- snapshot$canonical$basin.id
  labels <- stats::setNames(
    paste0("M", seq_along(ids)),
    ids
  )
  colors <- stats::setNames(
    grDevices::hcl.colors(length(ids), "Dynamic"),
    ids
  )
  levels <- gflowui:::gflowui_basin_interactive_levels(state)

  expect_gt(levels[[1L]], max(snapshot$source.values))
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  expect_setequal(
    levels[-1L],
    unique(c(
      structure$layout$coordinates$branches$birth.level,
      structure$layout$events$merge.level,
      min(structure$component.values)
    ))
  )

  initial <- gflowui:::gflowui_basin_interactive_tree_data(
    state,
    label.text = labels,
    basin.colors = colors
  )
  expect_true(initial$above.maximum)
  expect_identical(initial$n.active.vertices, 0L)
  expect_identical(initial$n.active.components, 0L)
  expect_identical(
    initial$scope.ids,
    sort(state$current.proposal$final.ids, method = "radix")
  )

  merge.index <- match(1, levels) - 1L
  current <- gflowui:::gflowui_basin_interactive_tree_data(
    state,
    level.index = merge.index,
    merge.scope = "current",
    label.text = labels,
    basin.colors = colors
  )
  expect_identical(current$height, 1)
  expect_true(2L %in% current$membership$vertex)
  expect_identical(nrow(current$merge.plateaus), 1L)
  expect_identical(current$merge.plateaus$vertices[[1L]], 2L)
  expect_match(current$merge.plateaus$label[[1L]], "s\\(M")

  complete <- gflowui:::gflowui_basin_interactive_tree_data(
    state,
    scope = "complete",
    level.index = merge.index,
    component.colors = "single",
    merge.scope = "reached",
    label.text = labels,
    basin.colors = colors
  )
  expect_identical(
    complete$scope.ids,
    sort(ids, method = "radix")
  )
  expect_identical(
    unique(unname(complete$component.colors)),
    "#2563EB"
  )
  expect_true(all(complete$maxima$peak.value >= complete$height))
})

test_that("topology events are exact, grouped, and deterministically ordered", {
  bundle <- phase5_panel_bundle("topology-events")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  events <- gflowui:::gflowui_basin_interactive_events(structure)

  expect_true(all(c(
    "event.index", "event.number", "event.count", "height",
    "above.maximum", "component.floor", "birth.count",
    "birth.basin.ids", "merge.count", "merge.plateau.ids",
    "event.kind", "event.summary", "aria.value.text"
  ) %in% names(events)))
  expect_identical(events$event.index, seq_len(nrow(events)) - 1L)
  expect_identical(events$event.number, seq_len(nrow(events)))
  expect_true(all(diff(events$height) < 0))
  expect_true(events$above.maximum[[1L]])
  expect_true(events$component.floor[[nrow(events)]])
  expect_gt(events$height[[1L]], max(structure$component.values))
  expect_identical(
    events$height[[nrow(events)]],
    min(structure$component.values)
  )
  expect_true(any(lengths(events$merge.plateau.ids) > 0L))
  expect_true(all(nzchar(events$summary)))
  expect_true(all(nzchar(events$aria.label)))
  expect_identical(
    events$height.text,
    trimws(formatC(events$height, format = "g", digits = 3))
  )
  expect_identical(events$birth.count, lengths(events$birth.basin.ids))
  expect_identical(events$merge.count, lengths(events$merge.plateau.ids))

  coincident <- which(events$height == 0)
  expect_length(coincident, 1L)
  expect_setequal(
    events$birth.basin.ids[[coincident]],
    structure$layout$coordinates$branches$basin.id[
      structure$layout$coordinates$branches$birth.level == 0
    ]
  )
  expect_setequal(
    events$merge.plateau.ids[[coincident]],
    structure$layout$events$merge.plateau.id[
      structure$layout$events$merge.level == 0
    ]
  )
  expect_identical(events$event.kind[[coincident]], "merge_and_floor")

  combined.structure <- structure
  combined.structure$layout$coordinates$branches$birth.level[[4L]] <- 1
  combined.events <- gflowui:::gflowui_basin_interactive_events(
    combined.structure
  )
  combined.index <- which(combined.events$height == 1)
  expect_identical(
    combined.events$event.kind[[combined.index]],
    "birth_and_merge"
  )
  expect_gt(combined.events$birth.count[[combined.index]], 0L)
  expect_gt(combined.events$merge.count[[combined.index]], 0L)
})

test_that("event remapping preserves exact height and otherwise snaps downward", {
  bundle <- phase5_panel_bundle("event-remap")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  events <- gflowui:::gflowui_basin_interactive_events(structure)

  exact.index <- min(2L, nrow(events) - 1L)
  exact.height <- events$height[[exact.index + 1L]]
  expect_identical(
    gflowui:::gflowui_basin_remap_event_index(events, exact.height),
    exact.index
  )
  between <- mean(events$height[2:3])
  expect_identical(
    gflowui:::gflowui_basin_remap_event_index(events, between),
    2L
  )
  expect_identical(
    gflowui:::gflowui_basin_remap_event_index(events, NA_real_),
    0L
  )
  expect_identical(
    gflowui:::gflowui_basin_remap_event_index(
      events,
      events$height[[nrow(events)]] - 1
    ),
    nrow(events) - 1L
  )
})

test_that("event cuts activate the full component at the exact floor", {
  bundle <- phase5_panel_bundle("event-floor")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  events <- gflowui:::gflowui_basin_interactive_events(structure)
  floor.cut <- gflowui:::gflowui_basin_interactive_cut(
    structure,
    events,
    event.index = nrow(events) - 1L,
    merge.scope = "current"
  )

  expect_identical(floor.cut$height, min(structure$component.values))
  expect_identical(
    floor.cut$n.active.vertices,
    length(structure$component.values)
  )
  expect_true(all(floor.cut$membership$vertex %in%
    seq_along(structure$data$source.values)))
})

test_that("event cuts reuse a prepared static layout", {
  bundle <- phase5_panel_bundle("event-static-layout")
  state <- phase5_panel_state(bundle)
  calls <- new.env(parent = emptyenv())
  calls$layout <- 0L
  calls$cut <- 0L
  layout.accessor <- function(...) {
    calls$layout <- calls$layout + 1L
    gflow::get.basin.merge.tree.layout(...)
  }
  cut.accessor <- function(...) {
    calls$cut <- calls$cut + 1L
    gflow::cut.basin.merge.tree(...)
  }
  structure <- gflowui:::gflowui_basin_interactive_structure(
    state,
    layout.accessor = layout.accessor
  )
  events <- gflowui:::gflowui_basin_interactive_events(structure)
  gflowui:::gflowui_basin_interactive_cut(
    structure,
    events,
    event.index = 0L,
    cut.accessor = cut.accessor
  )
  gflowui:::gflowui_basin_interactive_cut(
    structure,
    events,
    event.index = min(1L, nrow(events) - 1L),
    cut.accessor = cut.accessor
  )

  expect_identical(calls$layout, 1L)
  expect_identical(calls$cut, 2L)
})

test_that("event tables omit regular levels and support negative floors", {
  bundle <- phase5_panel_bundle("event-negative")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  with.regular <- structure
  with.regular$component.values <- c(structure$component.values, 2.5)
  regular.events <- gflowui:::gflowui_basin_interactive_events(with.regular)
  expect_false(2.5 %in% regular.events$height)

  shifted <- structure
  shifted$component.values <- structure$component.values - 10
  shifted$layout$coordinates$branches$birth.level <-
    shifted$layout$coordinates$branches$birth.level - 10
  shifted$layout$coordinates$branches$death.level <-
    shifted$layout$coordinates$branches$death.level - 10
  shifted$layout$coordinates$events$merge.level <-
    shifted$layout$coordinates$events$merge.level - 10
  shifted$layout$events$merge.level <- shifted$layout$events$merge.level - 10
  negative.events <- gflowui:::gflowui_basin_interactive_events(shifted)
  expect_identical(negative.events$height[[nrow(negative.events)]], -10)
  expect_true(negative.events$component.floor[[nrow(negative.events)]])
  expect_true(all(diff(negative.events$height) < 0))
})

test_that("every navigator event delegates to the canonical exact cut", {
  bundle <- phase5_panel_bundle("event-equivalence")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  events <- gflowui:::gflowui_basin_interactive_events(structure)

  for (index in events$event.index) {
    navigator <- gflowui:::gflowui_basin_interactive_cut(
      structure,
      events,
      event.index = index
    )
    direct <- gflow::cut.basin.merge.tree(
      structure$data$canonical.tree,
      height = events$height[[index + 1L]],
      direction = "max",
      component = structure$component
    )
    expect_identical(navigator$cut$components, direct$components)
    expect_identical(navigator$cut$membership, direct$membership)
    expect_identical(navigator$components, direct$components)
    expect_identical(navigator$membership, direct$membership)
  }
})

test_that("proposal and complete domains follow their resolved layouts", {
  bundle <- phase5_panel_bundle("event-scope")
  state <- phase5_panel_state(
    bundle,
    controls = list(
      filter.mode = "top_k",
      top.k = 1L,
      sentinel.top.n = 0L,
      peak.sentinel.enabled = FALSE,
      prominence.sentinel.enabled = FALSE,
      support.sentinel.enabled = FALSE
    )
  )
  proposal <- gflowui:::gflowui_basin_interactive_structure(
    state,
    scope = "proposal"
  )
  complete <- gflowui:::gflowui_basin_interactive_structure(
    state,
    scope = "complete"
  )
  proposal.events <- gflowui:::gflowui_basin_interactive_events(proposal)
  complete.events <- gflowui:::gflowui_basin_interactive_events(complete)

  expect_setequal(
    unique(unlist(proposal.events$birth.basin.ids)),
    proposal$layout$coordinates$branches$basin.id
  )
  expect_setequal(
    unique(unlist(complete.events$birth.basin.ids)),
    complete$layout$coordinates$branches$basin.id
  )
  expect_lte(nrow(proposal.events), nrow(complete.events))
  expect_lte(length(proposal$scope.ids), length(complete$scope.ids))
})

test_that("malformed event domains fail closed", {
  bundle <- phase5_panel_bundle("event-validation")
  state <- phase5_panel_state(bundle)
  structure <- gflowui:::gflowui_basin_interactive_structure(state)
  events <- gflowui:::gflowui_basin_interactive_events(structure)
  malformed <- events
  malformed$height[[2L]] <- malformed$height[[1L]]

  expect_error(
    gflowui:::gflowui_basin_validate_interactive_events(malformed),
    "malformed"
  )
  expect_error(
    gflowui:::gflowui_basin_interactive_cut(structure, malformed),
    "malformed"
  )
})

test_that("hidden selection remains presentation-only until pin", {
  bundle <- phase5_panel_bundle("selection")
  controls <- list(
    filter.mode = "top_k",
    top.k = 1L,
    sentinel.top.n = 0L,
    peak.sentinel.enabled = FALSE,
    prominence.sentinel.enabled = FALSE,
    support.sentinel.enabled = FALSE
  )
  state <- phase5_panel_state(bundle, controls)
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  hidden <- setdiff(
    model$proposal$component$ids,
    model$proposal$final.ids
  )
  skip_if(!length(hidden), "Synthetic tree has no hidden branch")
  selected.state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "selection_change",
      ids = hidden[[1L]]
    )
  )
  selected.model <- gflowui:::gflowui_basin_merge_tree_model(
    selected.state
  )
  expect_identical(
    selected.model$proposal$attempt.id,
    model$proposal$attempt.id
  )
  expect_identical(selected.model$selected.hidden, hidden[[1L]])
  expect_false(hidden[[1L]] %in% selected.model$proposal$final.ids)

  pinned <- gflowui:::gflowui_basin_reduce_state(
    selected.state,
    gflowui:::gflowui_basin_state_event("pin", id = hidden[[1L]])
  )
  expect_identical(pinned$active.attempt$outcome, "pending")
  expect_gt(
    pinned$active.attempt$attempt.id,
    selected.model$proposal$attempt.id
  )
  result <- gflowui:::gflowui_basin_execute_pending(pinned$pending.work)
  pinned <- gflowui:::gflowui_basin_reduce_state(
    pinned,
    gflowui:::gflowui_basin_state_event("result", result = result)
  )
  expect_true(hidden[[1L]] %in% pinned$current.proposal$final.ids)
  expect_true(hidden[[1L]] %in% pinned$current.proposal$pinned.ids)
})

test_that("all constructor overflow outcomes remain count-exact", {
  runtime <- phase5_records_runtime(data.frame(
    id = c("a", "b", "c"),
    parent = c("b", "c", NA_character_),
    mass = c(0.6, 0.3, 0.1),
    support = c(3, 2, 1),
    peak = c(9, 6, 3),
    prominence = c(5, 3, 1),
    stringsAsFactors = FALSE
  ))
  common <- list(
    sentinel.top.n = 0L,
    peak.sentinel.enabled = FALSE,
    prominence.sentinel.enabled = FALSE,
    support.sentinel.enabled = FALSE
  )
  core <- phase5_runtime_state(runtime, c(
    common,
    list(
      filter.mode = "none",
      final.render.budget = 2L
    )
  ))
  core.model <- gflowui:::gflowui_basin_merge_tree_model(
    core,
    layout.accessor = runtime$accessor
  )
  expect_identical(
    core$current.proposal$render.outcome,
    "core_overflow"
  )
  expect_false(core.model$renderable)
  expect_match(core.model$overflow.text, "core contains 3")
  expect_error(
    gflowui:::gflowui_basin_plot_merge_tree(core.model),
    class = "gflowui_basin_panel_overflow_error"
  )

  sentinel <- phase5_runtime_state(runtime, c(
    common,
    list(
      filter.mode = "top_k",
      top.k = 1L,
      final.render.budget = 1L
    )
  ))
  sentinel.model <- gflowui:::gflowui_basin_merge_tree_model(
    sentinel,
    layout.accessor = runtime$accessor
  )
  expect_identical(
    sentinel$current.proposal$render.outcome,
    "sentinel_overflow"
  )
  expect_identical(sentinel.model$counts$core, 1L)
  expect_identical(sentinel.model$counts$sentinel.only, 1L)
  expect_match(
    sentinel.model$overflow.text,
    "core plus mandatory sentinels contains 2"
  )

  closure <- phase5_runtime_state(runtime, c(
    common,
    list(
      filter.mode = "top_k",
      top.k = 1L,
      final.render.budget = 2L
    )
  ))
  closure.model <- gflowui:::gflowui_basin_merge_tree_model(
    closure,
    layout.accessor = runtime$accessor
  )
  expect_identical(
    closure$current.proposal$render.outcome,
    "closure_overflow"
  )
  expect_identical(closure.model$counts$core, 1L)
  expect_identical(closure.model$counts$sentinel.only, 1L)
  expect_identical(closure.model$counts$ancestor.only, 1L)
  expect_identical(closure.model$counts$final, 3L)
  expect_match(closure.model$overflow.text, "expands.*3")
})

test_that("empty and one-branch panel states are renderable without blanks", {
  empty <- gflowui:::gflowui_basin_merge_tree_panel_model(NULL)
  expect_false(empty$ready)
  expect_identical(empty$outcome, "not_started")
  expect_null(empty$layout)

  state <- phase5_panel_state(phase5_single_branch_bundle())
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  expect_true(model$renderable)
  expect_identical(model$component.maximum.count, 1L)
  expect_identical(model$counts$final, 1L)
  expect_identical(nrow(model$layout$events), 0L)

  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 1100, height = 1000, res = 120)
  plotted <- gflowui:::gflowui_basin_plot_merge_tree(model)
  grDevices::dev.off()
  expect_gt(file.info(image)$size, 1000)
  expect_identical(plotted$branch.count, 1L)
})

test_that("long and all-label rendering is explicit and nonblank", {
  bundle <- phase5_panel_bundle("long-labels")
  state <- phase5_panel_state(bundle)
  before.attempt <- state$active.attempt$attempt.id
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "label.mode",
      value = "all"
    )
  )
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  expect_identical(state$active.attempt$attempt.id, before.attempt)
  expect_setequal(model$labels$ids, model$proposal$final.ids)
  expect_match(model$labels$warning, "crowded")
  expect_gte(
    gflowui:::gflowui_basin_panel_plot_width(
      model$counts$final,
      "all"
    ),
    920L
  )
  ids <- model$proposal$component$ids
  long.labels <- stats::setNames(
    paste0(
      ids,
      "_canonical_maximum_basin_with_a_deliberately_long_label"
    ),
    ids
  )
  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 1800, height = 1100, res = 120)
  plotted <- gflowui:::gflowui_basin_draw_merge_tree(
    model$panel,
    label.text = long.labels
  )
  grDevices::dev.off()
  expect_gt(file.info(image)$size, 1000)
  expect_identical(
    sort(plotted$layout$basin.ids),
    sort(model$proposal$final.ids)
  )
})

test_that("presentation events do not duplicate pending proposal work", {
  bundle <- phase5_panel_bundle("pending-presentation")
  state <- phase5_panel_state(bundle)
  pending <- gflowui:::gflowui_basin_start_panel_event(
    state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "coverage.target",
      value = 0.95
    ),
    session.id = "phase5-session",
    construction.fingerprint = "phase5-construction"
  )
  expect_s3_class(pending$job, "gflowui_basin_async_job")
  pending.key <- pending$job$job.id
  pending.work <- pending$state$pending.work
  attempt.id <- pending$state$active.attempt$attempt.id

  diagnostic <- gflowui:::gflowui_basin_start_panel_event(
    pending$state,
    gflowui:::gflowui_basin_state_event(
      "diagnostic_visibility",
      visible = FALSE
    ),
    session.id = "phase5-session",
    construction.fingerprint = "phase5-construction"
  )
  expect_null(diagnostic$job)
  expect_identical(diagnostic$disposition, "presentation_updated")
  expect_identical(diagnostic$state$active.attempt$attempt.id, attempt.id)
  expect_identical(diagnostic$state$pending.work, pending.work)

  canonical <- gflowui:::gflowui_basin_bundle_snapshot(
    diagnostic$state$bundle
  )$canonical
  selected.id <- canonical$basin.id[
    canonical$component == diagnostic$state$context$component
  ][[1L]]
  selected <- gflowui:::gflowui_basin_start_panel_event(
    diagnostic$state,
    gflowui:::gflowui_basin_state_event(
      "selection_change",
      ids = selected.id
    ),
    session.id = "phase5-session",
    construction.fingerprint = "phase5-construction"
  )
  expect_null(selected$job)
  expect_identical(selected$state$active.attempt$attempt.id, attempt.id)
  expect_identical(selected$state$pending.work, pending.work)
  expect_identical(pending$job$job.id, pending.key)

  replacement <- gflowui:::gflowui_basin_start_panel_event(
    selected$state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "final.render.budget",
      value = 70L
    ),
    session.id = "phase5-session",
    construction.fingerprint = "phase5-construction"
  )
  expect_s3_class(replacement$job, "gflowui_basin_async_job")
  expect_gt(
    replacement$state$active.attempt$attempt.id,
    attempt.id
  )
  expect_false(identical(replacement$job$job.id, pending.key))
})

test_that("retained display separates active controls from displayed status", {
  bundle <- phase5_panel_bundle("retained")
  state <- phase5_panel_state(bundle)
  current.ids <- state$current.proposal$final.ids
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "coverage.target",
      value = 0
    )
  )
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  expect_identical(state$active.attempt$outcome, "blocked")
  expect_identical(model$display.source, "retained_last_valid")
  expect_true(model$retained)
  expect_identical(model$proposal$final.ids, current.ids)
  expect_identical(model$proposal$accepted.parameters$coverage.target, 0.99)
  expect_identical(model$controls$coverage.target, 0)
})

test_that("tree clicks resolve canonical branches without proposal mutation", {
  bundle <- phase5_panel_bundle("tree-click")
  state <- phase5_panel_state(bundle)
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  branches <- model$layout$coordinates$branches
  target <- branches[1L, , drop = FALSE]
  attempt.before <- state$active.attempt$attempt.id
  id <- gflowui:::gflowui_basin_tree_nearest_id(
    model,
    click.x = as.numeric(target$x),
    click.y = mean(c(
      as.numeric(target$birth.level),
      as.numeric(target$death.level)
    ))
  )
  expect_identical(id, as.character(target$basin.id))
  selected <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "selection_change",
      ids = id
    )
  )
  expect_identical(selected$selected.ids, id)
  expect_identical(
    selected$active.attempt$attempt.id,
    attempt.before
  )
  expect_identical(
    selected$current.proposal$final.ids,
    state$current.proposal$final.ids
  )
  expect_length(gflowui:::gflowui_basin_tree_nearest_id(
    model,
    click.x = max(branches$x) + 100,
    click.y = max(branches$birth.level) + 100
  ), 0L)
})

test_that("linked panel status reports one current or retained proposal", {
  bundle <- phase5_panel_bundle("linked-status")
  state <- phase5_panel_state(bundle)
  current <- gflowui:::gflowui_basin_linked_display_status(state)
  expect_identical(current$display.source, "current")
  expect_identical(
    current$active.attempt.id,
    current$displayed.attempt.id
  )
  expect_match(current$text, "Current proposal attempt", fixed = TRUE)

  retained <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "final.render.budget",
      value = 0
    )
  )
  expect_identical(retained$display.source, "retained_last_valid")
  status <- gflowui:::gflowui_basin_linked_display_status(retained)
  expect_identical(status$display.source, "retained_last_valid")
  expect_gt(status$active.attempt.id, status$displayed.attempt.id)
  expect_match(status$text, "Retained proposal attempt", fixed = TRUE)
  expect_match(status$text, "active attempt", fixed = TRUE)
})

test_that("blocked states without retained proposals keep recovery controls", {
  bundle <- phase5_panel_bundle("blocked-recovery")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  state$controls$coverage.target <- 0
  state <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event("recompute")
  )
  expect_identical(state$active.attempt$outcome, "blocked")
  expect_identical(state$display.source, "none")
  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  expect_false(model$available)
  ui <- htmltools::renderTags(
    gflowui:::gflowui_basin_merge_tree_panel_ui(model$panel)
  )$html
  expect_match(ui, 'id="basin_tree_coverage"', fixed = TRUE)
  expect_match(ui, 'id="basin_tree_show_all"', fixed = TRUE)
  expect_false(grepl(
    'id="basin_tree_open_complete"',
    ui,
    fixed = TRUE
  ))
  complete <- gflowui:::gflowui_basin_complete_interactive_data(state)
  expect_identical(
    nrow(complete$points),
    sum(
      gflowui:::gflowui_basin_bundle_snapshot(bundle)$canonical$component ==
        state$context$component
    )
  )
})

test_that("Subject 15 panel reconciles rank-17 core and complete count", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  records <- data.frame(
    id = fixture$canonical_branch_id,
    parent = fixture$parent_canonical_branch_id,
    mass = fixture$primary_support_mass,
    support = fixture$primary_support_size,
    peak = fixture$peak_value,
    prominence = fixture$canonical_prominence,
    stringsAsFactors = FALSE
  )
  runtime <- phase5_records_runtime(records, "subject15")
  controls <- gflowui:::gflowui_basin_default_controls(nrow(records))
  state <- phase5_runtime_state(runtime, controls)
  model <- gflowui:::gflowui_basin_merge_tree_model(
    state,
    layout.accessor = runtime$accessor
  )
  ranked <- fixture[
    order(
      -fixture$primary_support_mass,
      fixture$canonical_branch_id,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  expect_identical(model$direction.maximum.count, 352L)
  expect_identical(model$component.maximum.count, 352L)
  expect_identical(model$proposal$core$outcome, "strong_gap")
  expect_identical(length(model$proposal$core$ids), 17L)
  expect_setequal(
    model$proposal$core$ids,
    ranked$canonical_branch_id[seq_len(17L)]
  )
  expect_equal(
    model$proposal$core$gap.decades,
    12.9397631299771,
    tolerance = 1e-12
  )
  expect_equal(
    model$mass$core.coverage,
    0.99999999999991729,
    tolerance = 1e-14
  )
  expect_identical(model$counts$final, 17L)
})

test_that("Phase 7 canonical adapters link plots, tree, table, and graph", {
  bundle <- phase5_panel_bundle("phase7-linking")
  state <- phase5_panel_state(bundle)
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  canonical <- snapshot$canonical[
    snapshot$canonical$component == state$context$component,
    ,
    drop = FALSE
  ]
  table <- data.frame(
    key = paste("max", canonical$trajectory.basin.id, sep = "|"),
    type = "max",
    basin.id = canonical$trajectory.basin.id,
    extremum.vertex = canonical$extremum.vertex,
    stringsAsFactors = FALSE
  )
  assignment <- data.frame(
    vertex = canonical$extremum.vertex,
    direction = "max",
    assignment.status = "assigned",
    basin.id = canonical$trajectory.basin.id,
    stringsAsFactors = FALSE
  )
  result <- list(
    all_table = table,
    basin = list(assignment = assignment)
  )
  selected.id <- canonical$basin.id[[1L]]
  selected.key <- table$key[[1L]]

  expect_identical(
    gflowui:::gflowui_basin_canonical_ids_to_keys(
      result,
      state,
      selected.id
    ),
    selected.key
  )
  expect_identical(
    gflowui:::gflowui_basin_selected_canonical_ids(
      result,
      state,
      selected.key
    ),
    selected.id
  )
  expect_identical(
    gflowui:::gflowui_basin_vertex_canonical_id(
      result,
      state,
      canonical$extremum.vertex[[1L]]
    ),
    selected.id
  )
  expect_length(
    gflowui:::gflowui_basin_vertex_canonical_id(
      result,
      state,
      canonical$extremum.vertex[[1L]] + 0.5
    ),
    0L
  )

  model <- gflowui:::gflowui_basin_merge_tree_model(state)
  branches <- model$layout$coordinates$branches
  branch <- branches[
    as.character(branches$basin.id) == selected.id,
    ,
    drop = FALSE
  ]
  expect_identical(
    gflowui:::gflowui_basin_tree_nearest_id(
      model,
      click.x = branch$x[[1L]],
      click.y = mean(c(
        branch$birth.level[[1L]],
        branch$death.level[[1L]]
      ))
    ),
    selected.id
  )

  plot.data <- data.frame(
    key = c("max|z", "max|a"),
    x = c(1, 1),
    y = c(2, 2),
    stringsAsFactors = FALSE
  )
  spec <- list(kind = "scatter", features = c("x", "y"))
  expect_identical(
    gflowui:::gflowui_basin_plot_nearest_key(
      plot.data,
      spec,
      click.x = 1,
      click.y = 2
    ),
    "max|a"
  )
})

test_that("Phase 7 status separates retained display from active attempt", {
  state <- phase5_panel_state(phase5_panel_bundle("phase7-status"))
  current <- gflowui:::gflowui_basin_linked_display_status(state)
  expect_true(current$available)
  expect_identical(current$display.source, "current")
  expect_identical(
    current$active.attempt.id,
    current$displayed.attempt.id
  )
  expect_match(current$text, "Current proposal attempt")

  retained <- gflowui:::gflowui_basin_reduce_state(
    state,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "coverage.target",
      value = 0
    )
  )
  status <- gflowui:::gflowui_basin_linked_display_status(retained)
  expect_true(status$available)
  expect_identical(status$display.source, "retained_last_valid")
  expect_identical(status$active.outcome, "blocked")
  expect_gt(status$active.attempt.id, status$displayed.attempt.id)
  expect_match(status$text, "Retained proposal attempt")
  expect_match(status$text, "active attempt")
})

test_that("Phase 7 recipe transport preserves strict field types", {
  numeric.recipe <- list(
    recipe.version = 1,
    final.render.budget = 50,
    sentinel.top.n = 2
  )
  normalized <- gflowui:::.gflowui_basin_recipe_from_transport(
    numeric.recipe
  )
  expect_type(normalized$recipe.version, "integer")
  expect_type(normalized$final.render.budget, "integer")
  expect_type(normalized$sentinel.top.n, "integer")

  adversarial <- numeric.recipe
  adversarial$recipe.version <- "1"
  adversarial$final.render.budget <- "50"
  unchanged <- gflowui:::.gflowui_basin_recipe_from_transport(
    adversarial
  )
  expect_identical(unchanged$recipe.version, "1")
  expect_identical(unchanged$final.render.budget, "50")
})
