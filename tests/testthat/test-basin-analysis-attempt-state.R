phase3_identity <- function(suffix = "base") {
  fields <- c(
    "project",
    "subject",
    "graph",
    "topology",
    "vertex_map",
    "field",
    "source",
    "estimate",
    "trajectory_construction",
    "canonical_tree_construction"
  )
  setNames(
    lapply(fields, function(field) paste(field, suffix, sep = "-")),
    fields
  )
}

phase3_graph_case <- function(disconnected = FALSE,
                              support.outlier = FALSE) {
  if (disconnected) {
    adjacency <- list(
      2L,
      c(1L, 3L),
      c(2L, 4L),
      3L,
      6L,
      c(5L, 7L),
      c(6L, 8L),
      7L
    )
    field <- c(5, 0, 4, 1, 6, 0, 3, 1)
  } else {
    adjacency <- list(
      2L,
      c(1L, 3L, 4L),
      2L,
      c(2L, 5L, 6L),
      4L,
      4L
    )
    field <- c(5, 1, 4, 0, 3, 2)
  }
  edge.lengths <- lapply(
    adjacency,
    function(neighbors) rep(1, length(neighbors))
  )
  vertex.ids <- as.character(seq_along(field))
  source.values <- setNames(field, vertex.ids)
  vertex.mass <- rep(1 / length(field), length(field))
  flow <- gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "trajectory_flow",
    direction = "max",
    vertex.mass = vertex.mass,
    method.params = list(
      edge.length.quantile.thld = 1,
      store.trajectories = FALSE
    )
  )
  tree <- gflow::get.basin.merge.tree(gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "superlevel_merge_tree",
    direction = "max",
    vertex.mass = vertex.mass
  ))
  trajectory <- flow$basin.table[
    flow$basin.table$type == "max",
    ,
    drop = FALSE
  ]
  trajectory <- data.frame(
    trajectory.basin.id = trajectory$basin.id,
    direction = trajectory$type,
    component = tree$graph.input$validation$component[
      trajectory$extremum.vertex
    ],
    extremum.vertex = trajectory$extremum.vertex,
    primary.support.mass = trajectory$primary.support.mass,
    primary.support.size = trajectory$primary.support.size,
    stringsAsFactors = FALSE
  )
  if (support.outlier && nrow(trajectory) > 1L) {
    trajectory$primary.support.size[[nrow(trajectory)]] <- 100
  }
  list(
    adjacency = adjacency,
    vertex.ids = vertex.ids,
    source.values = source.values,
    trajectory = trajectory,
    tree = tree
  )
}

phase3_bundle <- function(case, suffix = "base") {
  gflowui:::gflowui_basin_new_scientific_bundle(
    graph = case$adjacency,
    vertex.ids = case$vertex.ids,
    source.values = case$source.values,
    identity = phase3_identity(suffix),
    trajectory.table = case$trajectory,
    canonical.tree = case$tree
  )
}

phase3_event <- function(type, ...) {
  gflowui:::gflowui_basin_state_event(type, ...)
}

phase3_reduce <- function(state, type, ...) {
  gflowui:::gflowui_basin_reduce_state(
    state,
    phase3_event(type, ...)
  )
}

phase3_install_pending <- function(
    state,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  result <- gflowui:::gflowui_basin_execute_pending(
    state$pending.work,
    layout.accessor = layout.accessor
  )
  list(
    state = phase3_reduce(state, "result", result = result),
    result = result
  )
}

phase3_current_state <- function(bundle) {
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  state <- phase3_reduce(state, "recompute")
  phase3_install_pending(state)$state
}

test_that("proposal-slot transitions and three-key installation are atomic [38-43]", {
  bundle <- phase3_bundle(phase3_graph_case(), "transitions")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)

  expect_identical(state$next.attempt.id, 1L)
  state <- phase3_reduce(state, "recompute")
  expect_identical(state$active.attempt$attempt.id, 1L)
  expect_identical(state$active.attempt$outcome, "pending")
  expect_identical(state$display.source, "none")
  expect_null(state$current.proposal)
  expect_null(state$retained.last.valid.proposal)

  first <- phase3_install_pending(state)
  state <- first$state
  expect_identical(state$active.attempt$outcome, "proposal_created")
  expect_identical(state$display.source, "current")
  expect_s3_class(state$current.proposal, "basin_display_proposal")
  expect_null(state$retained.last.valid.proposal)
  installed.ids <- state$current.proposal$final.ids
  first$result$proposal$final.ids <- character()
  expect_identical(state$current.proposal$final.ids, installed.ids)
  state$caches <- list(layout = "current-layout")

  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0.95
  )
  expect_identical(state$active.attempt$attempt.id, 2L)
  expect_identical(state$active.attempt$outcome, "pending")
  expect_null(state$current.proposal)
  expect_s3_class(
    state$retained.last.valid.proposal,
    "basin_display_proposal"
  )
  expect_identical(state$display.source, "retained_last_valid")
  expect_identical(state$caches, list(layout = "current-layout"))
  second.result <- gflowui:::gflowui_basin_execute_pending(
    state$pending.work
  )

  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0
  )
  expect_identical(state$active.attempt$attempt.id, 3L)
  expect_identical(state$active.attempt$outcome, "blocked")
  expect_identical(state$active.attempt$reason, "settings_invalid")
  expect_identical(state$display.source, "retained_last_valid")
  expect_null(state$pending.work)
  before.stale <- state
  state <- phase3_reduce(state, "result", result = second.result)
  expect_identical(state, before.stale)

  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0.9
  )
  expect_identical(state$active.attempt$attempt.id, 4L)
  pending <- state
  good <- gflowui:::gflowui_basin_execute_pending(
    pending$pending.work
  )
  mismatches <- list(
    bundle = function(result) {
      result$bundle.id <- paste0(result$bundle.id, "-stale")
      result
    },
    generation = function(result) {
      result$context.generation <-
        result$context.generation + 1L
      result
    },
    attempt = function(result) {
      result$attempt.id <- result$attempt.id + 1L
      result
    }
  )
  for (name in names(mismatches)) {
    stale <- mismatches[[name]](
      unserialize(serialize(good, NULL))
    )
    expect_identical(
      phase3_reduce(pending, "result", result = stale),
      pending,
      info = name
    )
  }
  state <- phase3_reduce(pending, "result", result = good)
  expect_identical(state$display.source, "current")
  expect_null(state$retained.last.valid.proposal)
  expect_identical(state$caches, list())
  after.install <- state
  state <- phase3_reduce(state, "result", result = good)
  expect_identical(state, after.install)

  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0.85
  )
  failed <- gflowui:::gflowui_basin_execute_pending(
    state$pending.work,
    layout.accessor = function(...) stop("forced layout failure")
  )
  state <- phase3_reduce(state, "result", result = failed)
  expect_identical(state$active.attempt$outcome, "construction_failed")
  expect_identical(state$active.attempt$reason, "layout_invalid")
  expect_null(state$current.proposal)
  expect_identical(state$display.source, "retained_last_valid")

  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0.8
  )
  state <- phase3_install_pending(state)$state
  expect_identical(state$active.attempt$attempt.id, 6L)
  expect_identical(state$display.source, "current")
  expect_null(state$retained.last.valid.proposal)
})

test_that("failed construction without retained state displays none", {
  bundle <- phase3_bundle(phase3_graph_case(), "failed-empty")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  state <- phase3_reduce(state, "recompute")
  failed <- gflowui:::gflowui_basin_execute_pending(
    state$pending.work,
    layout.accessor = function(...) stop("forced")
  )
  state <- phase3_reduce(state, "result", result = failed)

  expect_identical(state$active.attempt$outcome, "construction_failed")
  expect_identical(state$display.source, "none")
  expect_null(
    gflowui:::gflowui_basin_displayed_proposal(state)
  )
})

test_that("bundle and context changes clear state and reject prior work [40,41]", {
  first.bundle <- phase3_bundle(
    phase3_graph_case(disconnected = TRUE),
    "context-first"
  )
  state <- phase3_current_state(first.bundle)
  component.ids <- state$current.proposal$component$ids
  state <- phase3_reduce(
    state,
    "pin",
    id = component.ids[[1L]]
  )
  state <- phase3_install_pending(state)$state
  expect_identical(state$pinned.ids, component.ids[[1L]])
  state <- phase3_reduce(
    state,
    "selection_change",
    ids = component.ids[[1L]]
  )
  state$caches <- list(layout = "cached")
  state <- phase3_reduce(
    state,
    "control_change",
    name = "coverage.target",
    value = 0.9
  )
  old.work <- state$pending.work
  old.result <- gflowui:::gflowui_basin_execute_pending(old.work)
  old.attempt <- state$active.attempt$attempt.id

  replacement <- phase3_bundle(
    phase3_graph_case(disconnected = TRUE),
    "context-replacement"
  )
  state <- phase3_reduce(
    state,
    "bundle_change",
    bundle = replacement
  )
  expect_identical(
    state$active.attempt$attempt.id,
    old.attempt + 1L
  )
  expect_identical(state$context.generation, 2L)
  expect_identical(state$bundle.id, replacement$bundle.id)
  expect_null(state$current.proposal)
  expect_null(state$retained.last.valid.proposal)
  expect_identical(state$pinned.ids, character())
  expect_identical(state$selected.ids, character())
  expect_identical(state$caches, list())
  expect_identical(state$display.source, "none")
  expect_identical(state$active.attempt$outcome, "pending")
  expect_false(identical(state$pending.work, old.work))

  after.change <- state
  state <- phase3_reduce(state, "result", result = old.result)
  expect_identical(state, after.change)
  state <- phase3_install_pending(state)$state
  expect_identical(state$display.source, "current")

  available <- gflowui:::gflowui_basin_bundle_snapshot(
    replacement
  )$component.ids
  other <- setdiff(available, state$context$component)[[1L]]
  state$caches <- list(plot = "cached")
  state <- phase3_reduce(
    state,
    "component_change",
    component = other
  )
  expect_identical(state$context.generation, 3L)
  expect_identical(state$context$component, as.integer(other))
  expect_identical(state$context$selection.rule, "explicit")
  expect_null(state$current.proposal)
  expect_null(state$retained.last.valid.proposal)
  expect_identical(state$caches, list())
  expect_identical(state$active.attempt$outcome, "pending")
})

test_that("scientifically invalid replacements clear retained display", {
  valid.case <- phase3_graph_case()
  mutations <- list(
    source_invalid = function(case) {
      case$source.values[[1L]] <- Inf
      case
    },
    mapping_invalid = function(case) {
      case$trajectory$extremum.vertex <-
        as.character(case$trajectory$extremum.vertex)
      case
    },
    support_invalid = function(case) {
      case$trajectory$primary.support.size[[1L]] <- 0.5
      case
    },
    prominence_invalid = function(case) {
      row <- which(case$tree$basin.table$type == "max")[[1L]]
      case$tree$basin.table$persistence[[row]] <- -1
      case
    },
    mass_invalid = function(case) {
      case$trajectory$primary.support.mass[[1L]] <- NA_real_
      case
    }
  )
  reasons <- c(
    source_invalid = "source_invalid",
    mapping_invalid = "mapping_invalid",
    support_invalid = "support_invalid",
    prominence_invalid = "prominence_invalid",
    mass_invalid = "mass_invalid"
  )

  for (name in names(mutations)) {
    state <- phase3_current_state(
      phase3_bundle(valid.case, paste0("valid-", name))
    )
    invalid.case <- mutations[[name]](phase3_graph_case())
    invalid.bundle <- phase3_bundle(
      invalid.case,
      paste0("invalid-", name)
    )
    state <- phase3_reduce(
      state,
      "bundle_change",
      bundle = invalid.bundle
    )
    expect_identical(state$context.generation, 2L, info = name)
    expect_identical(
      state$active.attempt$outcome,
      "blocked",
      info = name
    )
    expect_identical(
      state$active.attempt$reason,
      reasons[[name]],
      info = name
    )
    expect_null(state$current.proposal, info = name)
    expect_null(state$retained.last.valid.proposal, info = name)
    expect_null(state$pending.work, info = name)
    expect_identical(state$display.source, "none", info = name)
  }
})

test_that("proposal and presentation events allocate on distinct boundaries [38,50-54]", {
  bundle <- phase3_bundle(phase3_graph_case(), "event-types")
  state <- phase3_current_state(bundle)
  original <- state$current.proposal
  next.id <- state$next.attempt.id

  state <- phase3_reduce(
    state,
    "control_change",
    name = "top.k",
    value = NA_real_
  )
  expect_identical(state$next.attempt.id, next.id)
  expect_true(is.na(state$controls$top.k))
  expect_identical(state$current.proposal, original)

  state <- phase3_reduce(
    state,
    "control_change",
    name = "label.mode",
    value = "displayed"
  )
  expect_identical(state$next.attempt.id, next.id)
  expect_identical(state$presentation$label.mode, "displayed")
  state <- phase3_reduce(
    state,
    "control_change",
    name = "important.label.n",
    value = NA_real_
  )
  expect_identical(state$next.attempt.id, next.id)
  expect_identical(state$presentation$important.label.n, 6L)

  hidden <- setdiff(
    original$component$ids,
    original$final.ids
  )
  selected <- if (length(hidden)) hidden[[1L]] else original$final.ids[[1L]]
  state <- phase3_reduce(
    state,
    "selection_change",
    ids = selected
  )
  expect_identical(state$next.attempt.id, next.id)
  expect_identical(state$selected.ids, selected)
  state <- phase3_reduce(
    state,
    "diagnostic_visibility",
    visible = FALSE
  )
  expect_identical(state$next.attempt.id, next.id)
  expect_false(state$presentation$diagnostics.visible)
  before.viewer <- state
  state <- phase3_reduce(state, "open_viewer")
  expect_identical(state, before.viewer)

  proposal.fields <- c(
    "filter.mode",
    "final.render.budget",
    "sentinel.top.n",
    "peak.sentinel.enabled",
    "prominence.sentinel.enabled",
    "support.sentinel.enabled",
    "coverage.target",
    "strong.gap.decades",
    "core.branch.budget"
  )
  values <- list(
    cumulative_mass = "cumulative_mass",
    final.render.budget = 81L,
    sentinel.top.n = 1L,
    peak.sentinel.enabled = FALSE,
    prominence.sentinel.enabled = FALSE,
    support.sentinel.enabled = FALSE,
    coverage.target = 0.8,
    strong.gap.decades = 2,
    core.branch.budget = 40L
  )
  names(values) <- proposal.fields
  for (field in proposal.fields) {
    before <- state$next.attempt.id
    state <- phase3_reduce(
      state,
      "control_change",
      name = field,
      value = values[[field]]
    )
    expected.allocate <- field != "strong.gap.decades"
    expect_identical(
      state$next.attempt.id,
      before + if (expected.allocate) 1L else 0L,
      info = field
    )
  }
  before <- state$next.attempt.id
  state <- phase3_reduce(
    state,
    "control_change",
    name = "filter.mode",
    value = "none"
  )
  expect_identical(state$next.attempt.id, before + 1L)
  expect_identical(state$active.attempt$outcome, "pending")
  before <- state$next.attempt.id
  state <- phase3_reduce(
    state,
    "control_change",
    name = "filter.mode",
    value = "top_k"
  )
  expect_identical(state$next.attempt.id, before + 1L)
  expect_identical(state$active.attempt$outcome, "blocked")
  expect_identical(state$active.attempt$reason, "settings_invalid")
})

test_that("pin and unpin recompute membership without slot aliasing [43,52]", {
  bundle <- phase3_bundle(phase3_graph_case(), "pins")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  controls <- list(
    filter.mode = "top_k",
    top.k = 1L,
    sentinel.top.n = 0L,
    peak.sentinel.enabled = FALSE,
    prominence.sentinel.enabled = FALSE,
    support.sentinel.enabled = FALSE
  )
  for (field in names(controls)) {
    state$controls[[field]] <- controls[[field]]
  }
  state <- phase3_reduce(state, "recompute")
  state <- phase3_install_pending(state)$state
  baseline <- state$current.proposal$final.ids
  baseline.counts <- gflowui:::gflowui_basin_derive_counts(
    state$current.proposal,
    bundle
  )
  hidden <- setdiff(
    state$current.proposal$component$ids,
    baseline
  )[[1L]]

  before <- state$next.attempt.id
  state <- phase3_reduce(state, "pin", id = hidden)
  expect_identical(state$next.attempt.id, before + 1L)
  expect_null(state$current.proposal)
  expect_false(is.null(state$retained.last.valid.proposal))
  state <- phase3_install_pending(state)$state
  expect_true(hidden %in% state$current.proposal$final.ids)
  expect_true(hidden %in% state$current.proposal$pinned.ids)
  expect_gt(
    gflowui:::gflowui_basin_derive_counts(
      state$current.proposal,
      bundle
    )$final,
    baseline.counts$final
  )
  expect_null(state$retained.last.valid.proposal)

  before <- state$next.attempt.id
  state <- phase3_reduce(state, "unpin", id = hidden)
  expect_identical(state$next.attempt.id, before + 1L)
  state <- phase3_install_pending(state)$state
  expect_false(hidden %in% state$current.proposal$pinned.ids)
  expect_identical(state$current.proposal$final.ids, baseline)
  expect_identical(
    gflowui:::gflowui_basin_derive_counts(
      state$current.proposal,
      bundle
    )$final,
    baseline.counts$final
  )
  expect_false(
    !is.null(state$current.proposal) &&
      !is.null(state$retained.last.valid.proposal)
  )
})

test_that("sentinel toggles conditionally reconstruct the mandatory union [53]", {
  case <- phase3_graph_case(support.outlier = TRUE)
  bundle <- phase3_bundle(case, "sentinel-union")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  state$controls$filter.mode <- "top_k"
  state$controls$top.k <- 1L
  state$controls$sentinel.top.n <- 1L
  state$controls$peak.sentinel.enabled <- FALSE
  state$controls$prominence.sentinel.enabled <- FALSE
  state$controls$support.sentinel.enabled <- FALSE
  state <- phase3_reduce(state, "recompute")
  state <- phase3_install_pending(state)$state
  data <- gflowui:::gflowui_basin_bundle_snapshot(bundle)$canonical
  support.id <- data$basin.id[
    which.max(data$trajectory.flow.support)
  ]
  expect_false(support.id %in% state$current.proposal$final.ids)

  before <- state$next.attempt.id
  state <- phase3_reduce(
    state,
    "control_change",
    name = "support.sentinel.enabled",
    value = TRUE
  )
  expect_identical(state$next.attempt.id, before + 1L)
  state <- phase3_install_pending(state)$state
  expect_true(support.id %in% state$current.proposal$sentinels$ids)
  expect_true(
    "support" %in%
      state$current.proposal$sentinels$reasons[[support.id]]
  )
  expect_true(support.id %in% state$current.proposal$final.ids)
})

test_that("recipes revalidate and recompute without restoring proposal state [18-20,44]", {
  bundle <- phase3_bundle(
    phase3_graph_case(disconnected = TRUE),
    "recipe-state"
  )
  state <- phase3_current_state(bundle)
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  automatic.component <- snapshot$component.selection$id
  explicit.component <- setdiff(
    snapshot$component.ids,
    automatic.component
  )[[1L]]
  state <- phase3_reduce(
    state,
    "component_change",
    component = explicit.component
  )
  state <- phase3_install_pending(state)$state
  expect_identical(state$context$selection.rule, "explicit")
  state <- phase3_reduce(
    state,
    "diagnostic_visibility",
    visible = FALSE
  )
  component.size <- sum(
    snapshot$canonical$component == automatic.component
  )
  controls <- gflowui:::gflowui_basin_default_controls(component.size)
  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  controls$important.label.n <- 2L
  controls$label.mode <- "selected"
  recipe <- gflowui:::gflowui_basin_recipe(
    controls,
    component.size
  )

  expect_false(any(c(
    "proposal",
    "bundle.id",
    "component",
    "pinned.ids",
    "selected.ids",
    "final.ids"
  ) %in% names(recipe)))
  before <- state$next.attempt.id
  state <- phase3_reduce(
    state,
    "recipe_restore",
    recipe = recipe
  )
  expect_identical(state$next.attempt.id, before + 1L)
  expect_identical(state$active.attempt$outcome, "pending")
  expect_null(state$current.proposal)
  expect_identical(state$context$component, automatic.component)
  expect_false(state$presentation$diagnostics.visible)
  expect_identical(state$controls$filter.mode, "top_k")
  expect_identical(state$presentation$label.mode, "selected")
  first <- phase3_install_pending(state)$state
  first.proposal <- first$current.proposal

  second <- phase3_reduce(
    first,
    "recipe_restore",
    recipe = recipe
  )
  second <- phase3_install_pending(second)$state
  comparable <- function(proposal) {
    proposal[c(
      "accepted.parameters",
      "component",
      "pinned.ids",
      "mass.status",
      "core",
      "sentinels",
      "ancestor.only.ids",
      "final.ids",
      "render.outcome"
    )]
  }
  expect_identical(
    comparable(second$current.proposal),
    comparable(first.proposal)
  )
  expect_false(identical(
    second$current.proposal$attempt.id,
    first.proposal$attempt.id
  ))

  invalid <- recipe
  invalid$recipe.version <- 99L
  before <- second$next.attempt.id
  second <- phase3_reduce(
    second,
    "recipe_restore",
    recipe = invalid
  )
  expect_identical(second$next.attempt.id, before + 1L)
  expect_identical(second$active.attempt$outcome, "blocked")
  expect_identical(second$active.attempt$reason, "recipe_invalid")
  expect_identical(second$display.source, "retained_last_valid")
})
