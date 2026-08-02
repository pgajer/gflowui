phase5_panel_bundle <- function(suffix = "base") {
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
  vertex.mass <- c(0.52, 0.03, 0.21, 0.02, 0.14, 0.08)
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

test_that("tree terminology identifies density-value survival semantics", {
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
      "Filtered crossing-free density-value elder-rule merge tree",
      "Complete crossing-free density-value elder-rule merge tree"
    )
  )
  ui <- htmltools::renderTags(
    gflowui:::gflowui_basin_merge_tree_panel_ui(model$panel)
  )$html
  expect_match(ui, "greater birth density survives each merge", fixed = TRUE)
  expect_match(
    ui,
    "mass and support are annotations and filtering quantities",
    fixed = TRUE
  )
  expect_match(ui, "do not change tree parentage", fixed = TRUE)
  expect_identical(
    gflowui:::.gflowui_basin_complete_viewer_title(),
    "Complete Interactive Density-Value Elder-Rule Basin Merge Tree"
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
  expect_match(ui, 'id="basin_tree_open_complete"', fixed = TRUE)
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
