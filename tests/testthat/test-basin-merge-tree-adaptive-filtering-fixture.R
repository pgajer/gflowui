phase2_identity <- function(suffix = "base") {
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

phase2_graph_case <- function(disconnected = FALSE) {
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
  rows <- flow$basin.table$type == "max"
  trajectory <- flow$basin.table[rows, , drop = FALSE]
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
  list(
    adjacency = adjacency,
    edge.lengths = edge.lengths,
    vertex.ids = vertex.ids,
    source.values = source.values,
    trajectory = trajectory,
    tree = tree
  )
}

phase2_bundle <- function(case, suffix = "base") {
  gflowui:::gflowui_basin_new_scientific_bundle(
    graph = case$adjacency,
    vertex.ids = case$vertex.ids,
    source.values = case$source.values,
    identity = phase2_identity(suffix),
    trajectory.table = case$trajectory,
    canonical.tree = case$tree
  )
}

phase2_attempt <- function(
    bundle,
    controls = NULL,
    component = NULL,
    pinned.ids = character(),
    attempt.id = 1L,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  context <- gflowui:::gflowui_basin_context(
    bundle,
    component = component
  )
  component.size <- sum(
    snapshot$canonical$component == context$component
  )
  if (is.null(controls)) {
    controls <- gflowui:::gflowui_basin_default_controls(component.size)
  }
  gflowui:::gflowui_basin_construct_proposal(
    context,
    bundle,
    controls,
    pinned.ids = pinned.ids,
    attempt.id = attempt.id,
    layout.accessor = layout.accessor
  )
}

phase2_fixture <- function() {
  utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
}

phase2_fixture_runtime <- function(fixture = phase2_fixture()) {
  canonical <- data.frame(
    basin.id = fixture$canonical_branch_id,
    type = fixture$direction,
    extremum.vertex = fixture$extremum_vertex,
    birth.level = fixture$peak_value,
    death.level = fixture$peak_value - fixture$canonical_prominence,
    persistence = fixture$canonical_prominence,
    parent.basin.id = fixture$parent_canonical_branch_id,
    component = fixture$component,
    peak.value = fixture$peak_value,
    trajectory.basin.id = fixture$trajectory_basin_id,
    trajectory.flow.mass = fixture$primary_support_mass,
    trajectory.flow.support = fixture$primary_support_size,
    stringsAsFactors = FALSE
  )
  canonical <- canonical[
    order(canonical$component, canonical$basin.id, method = "radix"),
    ,
    drop = FALSE
  ]
  data <- list(
    identity = phase2_identity("subject15"),
    direction = "max",
    graph = list(),
    vertex.ids = character(),
    source.values = numeric(),
    trajectory.table = data.frame(),
    canonical.tree = list(test.fixture = "subject15"),
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
      totals = c(`1` = gflowui:::.gflowui_basin_fixed_sum(
        canonical$trajectory.flow.mass[
          order(canonical$basin.id, method = "radix")
        ]
      ))
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

  parent <- setNames(
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
    component.ids <- canonical$basin.id[
      canonical$component == component
    ]
    requested <- if (is.null(basin.ids)) {
      component.ids
    } else {
      as.character(basin.ids)
    }
    if (length(setdiff(requested, component.ids))) {
      stop("unknown or mixed-component basin ids")
    }
    closure <- requested
    repeat {
      ancestors <- unname(parent[closure])
      expanded <- unique(c(closure, ancestors[!is.na(ancestors)]))
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
  list(bundle = bundle, accessor = accessor, canonical = canonical)
}

phase2_records_runtime <- function(records) {
  fixture <- data.frame(
    direction = "max",
    component = 1L,
    trajectory_basin_id = records$id,
    canonical_branch_id = records$id,
    extremum_vertex = seq_len(nrow(records)),
    parent_canonical_branch_id = records$parent,
    is_component_survivor = is.na(records$parent),
    primary_support_mass = records$mass,
    primary_support_size = records$support,
    peak_value = records$peak,
    canonical_prominence = records$prominence,
    stringsAsFactors = FALSE
  )
  phase2_fixture_runtime(fixture)
}

test_that("bundle validates source alignment and complete canonical mapping [1,2]", {
  case <- phase2_graph_case()
  bundle <- phase2_bundle(case)
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)

  expect_identical(snapshot$validation$source, "valid")
  expect_identical(snapshot$validation$mapping, "valid")
  expect_identical(
    snapshot$canonical$trajectory.basin.id,
    snapshot$canonical$basin.id
  )
  expect_identical(
    snapshot$canonical$peak.value,
    unname(case$source.values[snapshot$canonical$extremum.vertex])
  )

  bad.source <- case
  bad.source$source.values[[1L]] <- NA_real_
  invalid <- phase2_bundle(bad.source, "bad-source")
  invalid.snapshot <- gflowui:::gflowui_basin_bundle_snapshot(invalid)
  expect_identical(invalid.snapshot$validation$source, "source_invalid")
  expect_identical(invalid.snapshot$validation$source_peak, "peak_invalid")

  mutations <- list(
    incomplete = function(table) table[-1L, , drop = FALSE],
    duplicate = function(table) {
      table$extremum.vertex[[2L]] <- table$extremum.vertex[[1L]]
      table
    },
    mixed_direction = function(table) {
      table$direction[[1L]] <- "min"
      table
    },
    mixed_component = function(table) {
      table$component[[1L]] <- table$component[[1L]] + 1L
      table
    },
    missing_id = function(table) {
      table$trajectory.basin.id[[1L]] <- NA_character_
      table
    }
  )
  for (mutate in mutations) {
    changed <- case
    changed$trajectory <- mutate(changed$trajectory)
    changed.bundle <- phase2_bundle(changed, "bad-mapping")
    expect_identical(
      gflowui:::gflowui_basin_bundle_snapshot(
        changed.bundle
      )$validation$mapping,
      "mapping_invalid"
    )
  }
})

test_that("malformed mapping keys return mapping_invalid without coercion", {
  case <- phase2_graph_case()
  mutations <- list(
    character_extremum = function(table) {
      table$extremum.vertex <- as.character(table$extremum.vertex)
      table
    },
    factor_extremum = function(table) {
      table$extremum.vertex <- factor(table$extremum.vertex)
      table
    },
    list_extremum = function(table) {
      table$extremum.vertex <- as.list(table$extremum.vertex)
      table
    },
    fractional_extremum = function(table) {
      table$extremum.vertex[[1L]] <- 1.5
      table
    },
    infinite_extremum = function(table) {
      table$extremum.vertex[[1L]] <- Inf
      table
    },
    out_of_range_extremum = function(table) {
      table$extremum.vertex[[1L]] <- 0
      table
    },
    character_component = function(table) {
      table$component <- as.character(table$component)
      table
    },
    factor_component = function(table) {
      table$component <- factor(table$component)
      table
    },
    list_component = function(table) {
      table$component <- as.list(table$component)
      table
    },
    matrix_component = function(table) {
      table$component <- matrix(table$component, ncol = 1L)
      table
    },
    fractional_component = function(table) {
      table$component[[1L]] <- 1.5
      table
    },
    nonfinite_component = function(table) {
      table$component[[1L]] <- NaN
      table
    },
    factor_direction = function(table) {
      table$direction <- factor(table$direction)
      table
    },
    list_direction = function(table) {
      table$direction <- as.list(table$direction)
      table
    },
    numeric_trajectory_id = function(table) {
      table$trajectory.basin.id <- seq_len(nrow(table))
      table
    },
    factor_trajectory_id = function(table) {
      table$trajectory.basin.id <- factor(
        table$trajectory.basin.id
      )
      table
    }
  )

  for (name in names(mutations)) {
    changed <- case
    changed$trajectory <- mutations[[name]](changed$trajectory)
    bundle <- expect_silent(
      phase2_bundle(changed, paste0("malformed-", name))
    )
    snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
    expect_identical(
      snapshot$validation$mapping,
      "mapping_invalid",
      info = name
    )
    expect_identical(
      phase2_attempt(bundle)$reason,
      "mapping_invalid",
      info = name
    )
  }
})

test_that("whole-direction rankings gate component selection [3,4]", {
  case <- phase2_graph_case(disconnected = TRUE)
  valid <- phase2_bundle(case)
  expect_true(all(unlist(
    gflowui:::gflowui_basin_bundle_snapshot(valid)$validation
  ) == "valid"))

  all.zero <- case
  all.zero$trajectory$primary.support.mass <- 0
  zero.bundle <- phase2_bundle(all.zero, "zero")
  zero.snapshot <- gflowui:::gflowui_basin_bundle_snapshot(zero.bundle)
  expect_identical(
    zero.snapshot$validation$trajectory_flow_mass,
    "mass_unavailable"
  )
  expect_identical(
    zero.snapshot$component.selection$fallback.reason,
    "smallest_component_mass_unavailable"
  )

  invalid.columns <- list(
    primary.support.mass = c(NA_real_, "mass_invalid"),
    primary.support.size = c(0.5, "support_invalid")
  )
  for (field in names(invalid.columns)) {
    changed <- case
    changed$trajectory[[field]][[1L]] <- invalid.columns[[field]][[1L]]
    changed.bundle <- phase2_bundle(changed, paste0("bad-", field))
    snapshot <- gflowui:::gflowui_basin_bundle_snapshot(changed.bundle)
    status.field <- if (field == "primary.support.mass") {
      "trajectory_flow_mass"
    } else {
      "trajectory_flow_support"
    }
    expect_identical(
      snapshot$validation[[status.field]],
      invalid.columns[[field]][[2L]]
    )
    expect_identical(
      snapshot$component.selection$id,
      min(snapshot$component.ids)
    )
  }

  bad.peak <- case
  bad.peak$source.values[[1L]] <- Inf
  peak.bundle <- phase2_bundle(bad.peak, "bad-peak")
  expect_identical(
    gflowui:::gflowui_basin_bundle_snapshot(
      peak.bundle
    )$component.selection$fallback.reason,
    "peak_invalid"
  )

  bad.prominence <- case
  row <- which(bad.prominence$tree$basin.table$type == "max")[[1L]]
  bad.prominence$tree$basin.table$persistence[[row]] <- -1
  prominence.bundle <- phase2_bundle(
    bad.prominence,
    "bad-prominence"
  )
  expect_identical(
    gflowui:::gflowui_basin_bundle_snapshot(
      prominence.bundle
    )$validation$canonical_prominence,
    "prominence_invalid"
  )
})

test_that("invalid prominence does not mask parent-event corruption", {
  case <- phase2_graph_case()
  branches <- case$tree$basin.table
  row <- which(
    branches$type == "max" &
      !is.na(branches$parent.basin.id)
  )[[1L]]
  case$tree$basin.table$persistence[[row]] <- NA_real_
  case$tree$basin.table$parent.basin.id[[row]] <-
    case$tree$basin.table$basin.id[[row]]

  expect_error(
    phase2_bundle(case, "bad-prominence-and-parent-event"),
    "event survivor disagrees",
    class = "gflowui_basin_bundle_error"
  )
})

test_that("Filter None is the mass-only validation exception [5]", {
  case <- phase2_graph_case()
  case$trajectory$primary.support.mass[[1L]] <- NA_real_
  bundle <- phase2_bundle(case, "mass-invalid")
  controls <- gflowui:::gflowui_basin_default_controls(
    nrow(case$trajectory)
  )

  blocked <- phase2_attempt(bundle, controls)
  expect_identical(blocked$status, "blocked")
  expect_identical(blocked$reason, "mass_invalid")

  controls$filter.mode <- "none"
  complete <- phase2_attempt(bundle, controls)
  expect_identical(complete$status, "proposal_created")
  expect_identical(complete$proposal$core$outcome, "complete")
  mass <- gflowui:::gflowui_basin_derive_mass(
    complete$proposal,
    bundle
  )
  expect_false(mass$available)
  expect_identical(mass$unavailable.reason, "mass_invalid")
  labels <- gflowui:::gflowui_basin_derive_labels(
    complete$proposal,
    bundle
  )
  expect_match(labels$omissions[[1L]], "mass labels unavailable")
})

test_that("bundle identity binds proposals and every derivation [6,9]", {
  case <- phase2_graph_case()
  first <- phase2_bundle(case, "first")
  second <- phase2_bundle(case, "second")
  proposal <- phase2_attempt(first)$proposal

  expect_identical(proposal$bundle.id, first$bundle.id)
  expect_false(identical(first$bundle.id, second$bundle.id))
  expect_error(
    gflowui:::gflowui_basin_derive_counts(proposal, second),
    class = "gflowui_basin_stale_error"
  )
  expect_error(
    gflowui:::gflowui_basin_derive_layout(proposal, second),
    class = "gflowui_basin_stale_error"
  )
  expect_error(
    gflowui:::gflowui_basin_derive_labels(proposal, second),
    class = "gflowui_basin_stale_error"
  )
})

test_that("bundle replacement is replacement-only and clears context state [7,8,10]", {
  case <- phase2_graph_case()
  source.before <- case$source.values
  table.before <- case$trajectory
  tree.before <- case$tree
  bundle <- phase2_bundle(case, "immutable")
  state <- gflowui:::gflowui_basin_new_runtime_state(bundle)
  state$current.proposal <- phase2_attempt(bundle)$proposal
  state$display.source <- "current"
  state$pinned.ids <- state$current.proposal$core$ids[[1L]]
  state$selected.ids <- state$current.proposal$core$ids[[1L]]
  state$caches <- list(layout = "cached")

  case$source.values[[1L]] <- -999
  case$trajectory$primary.support.mass[[1L]] <- 999
  case$tree$basin.table$birth.level[[1L]] <- -999
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  expect_identical(snapshot$source.values, unname(source.before))
  expect_identical(
    snapshot$trajectory.table$primary.support.mass,
    table.before$primary.support.mass
  )
  expect_identical(
    snapshot$canonical.tree$basin.table$birth.level,
    tree.before$basin.table$birth.level
  )
  expect_error(
    bundle$data <- list(),
    "locked"
  )
  snapshot$source.values[[1L]] <- 123
  expect_identical(
    gflowui:::gflowui_basin_bundle_snapshot(bundle)$source.values,
    unname(source.before)
  )

  replacement <- phase2_bundle(
    phase2_graph_case(),
    "replacement-same-names"
  )
  replaced <- gflowui:::gflowui_basin_replace_runtime_bundle(
    state,
    replacement
  )
  expect_false(identical(bundle$bundle.id, replacement$bundle.id))
  expect_identical(replaced$context$context.generation, 2L)
  expect_null(replaced$current.proposal)
  expect_null(replaced$retained.last.valid.proposal)
  expect_identical(replaced$pinned.ids, character())
  expect_identical(replaced$selected.ids, character())
  expect_identical(replaced$caches, list())
  expect_identical(replaced$active.attempt$outcome, "pending")
  expect_identical(
    replaced$pending.work$bundle.id,
    replacement$bundle.id
  )
})

test_that("active settings validate ordinary R domains and ranges [11-13]", {
  controls <- gflowui:::gflowui_basin_default_controls(4L)
  invalid <- list(
    coverage.target = list(NA_real_, Inf, 0, -1, 1.1),
    strong.gap.decades = list(NA_real_, Inf, -1),
    core.branch.budget = list(NA_real_, Inf, -1, 2.5, 2),
    final.render.budget = list(NA_real_, Inf, 0, 1.5),
    sentinel.top.n = list(NA_real_, Inf, -1, 1.5),
    peak.sentinel.enabled = list(NA, 1),
    prominence.sentinel.enabled = list(NA, 1),
    support.sentinel.enabled = list(NA, 1)
  )
  for (field in names(invalid)) {
    for (value in invalid[[field]]) {
      changed <- controls
      changed[[field]] <- value
      expect_false(
        gflowui:::gflowui_basin_validate_controls(changed, 4L)$valid
      )
    }
  }
  for (field in c(
    "core.branch.budget",
    "final.render.budget",
    "sentinel.top.n",
    "important.label.n",
    "top.k"
  )) {
    changed <- controls
    if (field == "top.k") changed$filter.mode <- "top_k"
    changed[[field]] <- .Machine$integer.max + 1
    validated <- gflowui:::gflowui_basin_validate_controls(
      changed,
      .Machine$integer.max
    )
    if (field == "important.label.n") {
      expect_false(validated$presentation$valid)
    } else {
      expect_false(validated$valid)
    }
  }
  controls$filter.mode <- "top_k"
  controls$top.k <- 5L
  expect_false(
    gflowui:::gflowui_basin_validate_controls(controls, 4L)$valid
  )
})

test_that("validation is mode-aware and accepted parameters are minimal [14-17]", {
  controls <- gflowui:::gflowui_basin_default_controls(4L)
  controls$top.k <- NA_real_
  controls$minimum.mass <- -Inf
  auto <- gflowui:::gflowui_basin_validate_controls(controls, 4L)
  expect_true(auto$valid)
  expect_identical(
    names(auto$accepted.parameters),
    c(
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
  )
  expect_false("minimum.core.branches" %in% names(controls))
  expect_false(
    "minimum.core.branches" %in% names(auto$accepted.parameters)
  )
  expect_false(
    "important.label.n" %in% names(auto$accepted.parameters)
  )

  case <- phase2_graph_case()
  bundle <- phase2_bundle(case, "presentation")
  controls$important.label.n <- NA_real_
  controls$label.mode <- "unsupported"
  validation <- gflowui:::gflowui_basin_validate_controls(
    controls,
    nrow(case$trajectory)
  )
  expect_true(validation$valid)
  expect_false(validation$presentation$valid)
  proposal <- phase2_attempt(bundle, controls)
  expect_identical(proposal$status, "proposal_created")
  expect_error(
    gflowui:::gflowui_basin_derive_labels(
      proposal$proposal,
      bundle,
      important.label.n = NA_real_
    ),
    class = "gflowui_basin_presentation_error"
  )
})

test_that("recipes persist generic settings and recompute selection [18-20]", {
  case <- phase2_graph_case(disconnected = TRUE)
  bundle <- phase2_bundle(case, "recipe")
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)
  selected.size <- sum(
    snapshot$canonical$component ==
      snapshot$component.selection$id
  )
  controls <- gflowui:::gflowui_basin_default_controls(selected.size)
  recipe <- gflowui:::gflowui_basin_recipe(controls, selected.size)

  expect_identical(recipe$recipe.version, 1L)
  expect_false(any(c(
    "bundle.id", "component", "pinned.ids", "selected.ids",
    "proposal", "coverage", "layout", "minimum.core.branches"
  ) %in% names(recipe)))
  restored <- gflowui:::gflowui_basin_restore_recipe(recipe, bundle)
  expect_identical(
    restored$context$component,
    snapshot$component.selection$id
  )
  expect_identical(restored$attempt$status, "proposal_created")

  invalid <- recipe
  invalid$recipe.version <- 99L
  expect_error(
    gflowui:::gflowui_basin_restore_recipe(invalid, bundle),
    class = "gflowui_basin_recipe_error"
  )
  invalid <- recipe
  invalid$unknown.feature <- TRUE
  expect_error(
    gflowui:::gflowui_basin_restore_recipe(invalid, bundle),
    class = "gflowui_basin_recipe_error"
  )
  invalid <- recipe
  invalid$coverage.target <- 0
  expect_error(
    gflowui:::gflowui_basin_restore_recipe(invalid, bundle),
    class = "gflowui_basin_recipe_error"
  )
})

test_that("mass grouping is row-permutation invariant and never splits ties [21]", {
  ids <- c("d", "a", "c", "b", "z")
  mass <- c(0, 0.4, 0.3, 0.3, 0)
  first <- gflowui:::.gflowui_basin_mass_groups(ids, mass)
  permutation <- c(5L, 2L, 4L, 1L, 3L)
  second <- gflowui:::.gflowui_basin_mass_groups(
    ids[permutation],
    mass[permutation]
  )
  expect_identical(first, second)
  expect_identical(first$groups[[2L]], c("b", "c"))
  expect_identical(first$groups[[3L]], c("d", "z"))

  parameters <- list(filter.mode = "top_k", top.k = 2L)
  core <- gflowui:::.gflowui_basin_manual_core(ids, mass, parameters)
  expect_identical(core$ids, c("a", "b", "c"))
  expect_identical(core$warnings, "tie_overflow")
})

test_that("component totals use fixed canonical order and stable exact ties [22]", {
  canonical <- data.frame(
    basin.id = c("b", "a", "d", "c"),
    component = c(1L, 1L, 2L, 2L),
    trajectory.flow.mass = c(1, 1e16, 2, 1e16),
    stringsAsFactors = FALSE
  )
  ranking <- list(
    trajectory_flow_mass = "valid",
    trajectory_flow_support = "valid",
    source_peak = "valid",
    canonical_prominence = "valid"
  )
  selected <- gflowui:::.gflowui_basin_component_selection(
    canonical,
    "valid",
    ranking
  )
  expect_identical(selected$id, 2L)

  canonical$trajectory.flow.mass <- c(1, 2, 1, 2)
  tied <- gflowui:::.gflowui_basin_component_selection(
    canonical[c(4L, 2L, 3L, 1L), ],
    "valid",
    ranking
  )
  expect_identical(unname(tied$totals), c(3, 3))
  expect_identical(tied$id, 1L)
})

test_that("Auto covers strong, smooth, terminal, equal, and bounded tails [23,24]", {
  controls <- gflowui:::gflowui_basin_default_controls(6L)
  parameters <- gflowui:::gflowui_basin_validate_controls(
    controls,
    6L
  )$accepted.parameters
  strong <- gflowui:::.gflowui_basin_auto_core(
    letters[1:4],
    c(0.5, 0.3, 0.2, 1e-6),
    parameters
  )
  expect_identical(strong$outcome, "strong_gap")
  expect_identical(strong$boundary, 3L)

  smooth <- gflowui:::.gflowui_basin_auto_core(
    letters[1:6],
    c(0.30, 0.25, 0.20, 0.12, 0.08, 0.05),
    parameters
  )
  expect_identical(smooth$outcome, "coverage")

  terminal <- gflowui:::.gflowui_basin_auto_core(
    letters[1:3],
    c(0.5, 0.3, 1e-9),
    parameters
  )
  expect_identical(terminal$outcome, "coverage")

  equal <- gflowui:::.gflowui_basin_auto_core(
    letters[1:4],
    rep(0.25, 4L),
    parameters
  )
  expect_identical(equal$ids, letters[1:4])
  expect_identical(equal$outcome, "coverage")

  one <- gflowui:::.gflowui_basin_auto_core(
    "a",
    1,
    parameters
  )
  expect_identical(one$outcome, "single_positive")
  two <- gflowui:::.gflowui_basin_auto_core(
    c("a", "b"),
    c(0.8, 0.2),
    parameters
  )
  expect_identical(two$outcome, "coverage")

  tied.parameters <- parameters
  tied.parameters$core.branch.budget <- 3L
  tied.parameters$coverage.target <- 0.99
  budget.tie <- gflowui:::.gflowui_basin_auto_core(
    letters[1:5],
    c(0.4, 0.2, 0.2, 0.2, 0.01),
    tied.parameters
  )
  expect_identical(budget.tie$ids, letters[1:4])
  expect_identical(budget.tie$warnings, "tie_overflow")
  expect_identical(budget.tie$outcome, "coverage")
})

test_that("zero masses stay out of logarithms and coverage shares one denominator [25,26]", {
  runtime <- phase2_records_runtime(data.frame(
    id = c("a", "b", "c", "d"),
    parent = c("b", "d", "d", NA),
    mass = c(0.6, 0.4, 0, 0),
    support = c(4, 3, 2, 1),
    peak = c(4, 3, 2, 1),
    prominence = c(1, 1, 1, 4),
    stringsAsFactors = FALSE
  ))
  controls <- gflowui:::gflowui_basin_default_controls(4L)
  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  controls$sentinel.top.n <- 0L
  result <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )
  diagnostics <- gflowui:::gflowui_basin_derive_diagnostics(
    result$proposal,
    runtime$bundle
  )
  mass <- gflowui:::gflowui_basin_derive_mass(
    result$proposal,
    runtime$bundle
  )
  expect_true(all(is.finite(diagnostics$log10.mass)))
  expect_identical(length(diagnostics$log10.mass), 2L)
  expect_identical(diagnostics$zero.count, 2L)
  expect_identical(mass$denominator, 1)
  expect_equal(mass$core.coverage, 0.6)
  expect_equal(mass$final.coverage, 1)
})

test_that("Cumulative Mass obeys exact budget and straddling rules [27,28]", {
  parameters <- gflowui:::gflowui_basin_validate_controls(
    gflowui:::gflowui_basin_default_controls(6L),
    6L
  )$accepted.parameters
  ids <- letters[1:6]

  below <- parameters
  below$coverage.target <- 0.5
  below$core.branch.budget <- 4L
  result <- gflowui:::.gflowui_basin_cumulative_core(
    ids,
    c(0.5, 0.2, 0.1, 0.08, 0.07, 0.05),
    below
  )
  expect_identical(result$outcome, "coverage")
  expect_identical(result$boundary, 1L)

  exact <- below
  exact$coverage.target <- 0.8
  exact$core.branch.budget <- 3L
  result <- gflowui:::.gflowui_basin_cumulative_core(
    ids,
    c(0.4, 0.25, 0.15, 0.1, 0.06, 0.04),
    exact
  )
  expect_identical(result$boundary, 3L)
  expect_identical(result$outcome, "coverage")

  straddling <- exact
  straddling$coverage.target <- 0.95
  straddling$core.branch.budget <- 3L
  result <- gflowui:::.gflowui_basin_cumulative_core(
    ids,
    c(0.4, 0.2, 0.2, 0.2, 0.01, 0.01),
    straddling
  )
  expect_identical(result$boundary, 4L)
  expect_identical(result$warnings, "tie_overflow")
  expect_identical(result$outcome, "coverage")

  capped <- exact
  capped$coverage.target <- 0.99
  capped$core.branch.budget <- 3L
  result <- gflowui:::.gflowui_basin_cumulative_core(
    ids,
    c(0.4, 0.2, 0.15, 0.1, 0.08, 0.07),
    capped
  )
  expect_identical(result$boundary, 3L)
  expect_identical(result$warnings, character())
  expect_identical(result$outcome, "coverage_capped")
})

test_that("manual filters use raw mass and preserve complete zero ties [29-31]", {
  ids <- c("a", "b", "c", "d")
  mass <- c(0.6, 0.4, 0, 0)

  minimum <- gflowui:::.gflowui_basin_manual_core(
    ids,
    mass,
    list(filter.mode = "minimum_mass", minimum.mass = 0.4)
  )
  expect_identical(minimum$ids, c("a", "b"))
  empty <- gflowui:::.gflowui_basin_manual_core(
    ids,
    mass,
    list(filter.mode = "minimum_mass", minimum.mass = 1)
  )
  expect_identical(empty$outcome, "threshold_empty")
  zero <- gflowui:::.gflowui_basin_manual_core(
    ids,
    mass,
    list(filter.mode = "minimum_mass", minimum.mass = 0)
  )
  expect_identical(zero$ids, ids)
  top <- gflowui:::.gflowui_basin_manual_core(
    ids,
    mass,
    list(filter.mode = "top_k", top.k = 3L)
  )
  expect_identical(top$ids, ids)
  expect_identical(top$warnings, "tie_overflow")
  complete <- gflowui:::.gflowui_basin_manual_core(
    ids,
    mass,
    list(filter.mode = "none")
  )
  expect_identical(complete$outcome, "complete")
  expect_identical(complete$ids, ids)
})

test_that("constructor preserves component, closure, and sentinel contracts [32-35]", {
  runtime <- phase2_records_runtime(data.frame(
    id = c("a", "b", "c", "d", "e"),
    parent = c("b", "e", "e", "e", NA),
    mass = c(0.6, 0.2, 0.1, 0.1, 0),
    support = c(1, 5, 4, 3, 2),
    peak = c(1, 2, 5, 4, 3),
    prominence = c(1, 2, 3, 5, 4),
    stringsAsFactors = FALSE
  ))
  controls <- gflowui:::gflowui_basin_default_controls(5L)
  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  controls$sentinel.top.n <- 1L
  result <- phase2_attempt(
    runtime$bundle,
    controls,
    pinned.ids = "d",
    layout.accessor = runtime$accessor
  )
  proposal <- result$proposal
  expect_identical(result$status, "proposal_created")
  expect_true(all(proposal$final.ids %in% proposal$component$ids))
  expect_setequal(
    proposal$final.ids,
    c(
      proposal$core$ids,
      proposal$sentinels$ids,
      proposal$ancestor.only.ids
    )
  )
  expect_true(all(c(
    "pinned", "component_survivor", "peak", "prominence", "support"
  ) %in% unlist(proposal$sentinels$reasons, use.names = FALSE)))
  counts <- gflowui:::gflowui_basin_derive_counts(
    proposal,
    runtime$bundle
  )
  expect_identical(
    sum(counts$primary.reason.counts),
    counts$sentinel.only
  )

  controls$peak.sentinel.enabled <- FALSE
  controls$prominence.sentinel.enabled <- FALSE
  controls$support.sentinel.enabled <- FALSE
  disabled <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )$proposal
  reasons <- unlist(disabled$sentinels$reasons, use.names = FALSE)
  expect_false(any(c("peak", "prominence", "support") %in% reasons))
})

test_that("render outcomes are cause-specific and never trim mandatory IDs [36,48,49]", {
  records <- data.frame(
    id = c("a", "b", "c"),
    parent = c("b", "c", NA),
    mass = c(0.6, 0.3, 0.1),
    support = c(1, 3, 2),
    peak = c(1, 3, 2),
    prominence = c(1, 3, 2),
    stringsAsFactors = FALSE
  )
  runtime <- phase2_records_runtime(records)

  controls <- gflowui:::gflowui_basin_default_controls(3L)
  controls$filter.mode <- "none"
  controls$sentinel.top.n <- 0L
  controls$peak.sentinel.enabled <- FALSE
  controls$prominence.sentinel.enabled <- FALSE
  controls$support.sentinel.enabled <- FALSE
  controls$final.render.budget <- 2L
  core <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )$proposal
  expect_identical(core$render.outcome, "core_overflow")
  expect_identical(length(core$core$ids), 3L)

  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  controls$final.render.budget <- 1L
  controls$peak.sentinel.enabled <- TRUE
  controls$sentinel.top.n <- 1L
  sentinel <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )$proposal
  expect_identical(sentinel$render.outcome, "sentinel_overflow")
  expect_true(length(sentinel$sentinels$ids) > 1L)

  controls$peak.sentinel.enabled <- FALSE
  controls$final.render.budget <- 2L
  closure <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )$proposal
  expect_identical(closure$render.outcome, "closure_overflow")
  expect_identical(closure$core$ids, "a")
  expect_identical(closure$ancestor.only.ids, "b")
  expect_identical(closure$final.ids, c("a", "b", "c"))
})

test_that("counts, labels, diagnostics, and status are bundle-derived [37]", {
  case <- phase2_graph_case()
  bundle <- phase2_bundle(case, "derived")
  proposal <- phase2_attempt(bundle)$proposal

  counts <- gflowui:::gflowui_basin_derive_counts(proposal, bundle)
  mass <- gflowui:::gflowui_basin_derive_mass(proposal, bundle)
  labels <- gflowui:::gflowui_basin_derive_labels(
    proposal,
    bundle,
    important.label.n = 2L
  )
  diagnostics <- gflowui:::gflowui_basin_derive_diagnostics(
    proposal,
    bundle
  )
  status <- gflowui:::gflowui_basin_derive_status(proposal, bundle)

  expect_identical(counts$core, length(proposal$core$ids))
  expect_identical(counts$final, length(proposal$final.ids))
  expect_equal(
    mass$core.coverage,
    gflowui:::.gflowui_basin_selected_mass(
      gflowui:::.gflowui_basin_proposal_component(proposal, bundle),
      proposal$core$ids
    ) / mass$denominator
  )
  expect_true(all(labels$ids %in% proposal$final.ids))
  expect_true(all(is.finite(diagnostics$log10.mass)))
  expect_match(status$text, proposal$core$outcome, fixed = TRUE)
  expect_identical(
    status$mass.owner,
    "trajectory-flow primary.support.mass"
  )
  expect_false(any(c(
    "counts", "coverage", "label.ids", "coordinates",
    "status.text", "fingerprint", "creation.time"
  ) %in% names(proposal)))
})

test_that("same bundle, context, and controls construct deterministically [44]", {
  case <- phase2_graph_case()
  bundle <- phase2_bundle(case, "deterministic")
  first <- phase2_attempt(bundle, attempt.id = 7L)
  second <- phase2_attempt(bundle, attempt.id = 7L)
  expect_identical(first, second)
})

test_that("filters preserve complete-tree identity and Phase 1 layout facts [45-47]", {
  case <- phase2_graph_case()
  bundle <- phase2_bundle(case, "topology")
  before <- gflowui:::gflowui_basin_bundle_snapshot(
    bundle
  )$canonical.tree
  controls <- gflowui:::gflowui_basin_default_controls(
    nrow(case$trajectory)
  )
  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  filtered <- phase2_attempt(bundle, controls)$proposal
  layout <- gflowui:::gflowui_basin_derive_layout(filtered, bundle)
  after <- gflowui:::gflowui_basin_bundle_snapshot(
    bundle
  )$canonical.tree

  expect_identical(before, after)
  expect_identical(layout$validation.status, "ok")
  matched <- match(
    layout$branches$basin.id,
    before$basin.table$basin.id
  )
  expect_identical(
    layout$branches$birth.level,
    before$basin.table$birth.level[matched]
  )
  expect_identical(
    layout$branches$parent.basin.id,
    before$basin.table$parent.basin.id[matched]
  )
  expect_error(
    gflow::get.basin.merge.tree.layout(
      before,
      direction = "max",
      basin.ids = "unknown",
      close.ancestors = TRUE
    ),
    class = "gflow_basin_input_error"
  )
  nonroot <- before$basin.table$basin.id[
    !is.na(before$basin.table$parent.basin.id)
  ][[1L]]
  expect_error(
    gflow::get.basin.merge.tree.layout(
      before,
      direction = "max",
      basin.ids = nonroot,
      close.ancestors = FALSE
    ),
    class = "gflow_basin_input_error"
  )
})

test_that("complete filtering, viewer layout, selection, and pin stay distinct [50-54]", {
  runtime <- phase2_records_runtime(data.frame(
    id = c("a", "b", "c"),
    parent = c("b", "c", NA),
    mass = c(0.6, 0.3, 0.1),
    support = c(1, 2, 3),
    peak = c(1, 3, 2),
    prominence = c(1, 2, 3),
    stringsAsFactors = FALSE
  ))
  controls <- gflowui:::gflowui_basin_default_controls(3L)
  controls$filter.mode <- "top_k"
  controls$top.k <- 1L
  controls$sentinel.top.n <- 0L
  controls$peak.sentinel.enabled <- FALSE
  controls$prominence.sentinel.enabled <- FALSE
  controls$support.sentinel.enabled <- FALSE
  filtered <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )$proposal
  before <- filtered

  labels <- gflowui:::gflowui_basin_derive_labels(
    filtered,
    runtime$bundle,
    label.mode = "selected",
    selected.ids = "b"
  )
  expect_identical(labels$selected.hidden, character())
  hidden <- setdiff(filtered$component$ids, filtered$final.ids)
  if (length(hidden)) {
    hidden.labels <- gflowui:::gflowui_basin_derive_labels(
      filtered,
      runtime$bundle,
      label.mode = "selected",
      selected.ids = hidden[[1L]]
    )
    expect_identical(hidden.labels$selected.hidden, hidden[[1L]])
    expect_identical(hidden.labels$ids, character())
  }
  gflowui:::gflowui_basin_derive_diagnostics(filtered, runtime$bundle)
  expect_identical(filtered, before)

  pinned <- phase2_attempt(
    runtime$bundle,
    controls,
    pinned.ids = "b",
    attempt.id = 2L,
    layout.accessor = runtime$accessor
  )$proposal
  expect_identical(pinned$attempt.id, 2L)
  expect_true("b" %in% pinned$final.ids)
  expect_true("pinned" %in% pinned$sentinels$reasons$b)

  toggled.controls <- controls
  toggled.controls$peak.sentinel.enabled <- TRUE
  toggled.controls$sentinel.top.n <- 1L
  toggled <- phase2_attempt(
    runtime$bundle,
    toggled.controls,
    attempt.id = 3L,
    layout.accessor = runtime$accessor
  )$proposal
  expect_false(identical(toggled$sentinels$ids, filtered$sentinels$ids))

  controls$filter.mode <- "none"
  complete <- phase2_attempt(
    runtime$bundle,
    controls,
    attempt.id = 4L,
    layout.accessor = runtime$accessor
  )$proposal
  viewer <- runtime$accessor(
    list(),
    direction = "max",
    component = 1L
  )
  expect_identical(complete$core$outcome, "complete")
  expect_identical(complete$final.ids, viewer$branches$basin.id)
})

test_that("Subject 15 fixture mapping and canonical parentage are complete [56]", {
  fixture <- phase2_fixture()
  provenance <- utils::read.csv(
    test_path(
      "fixtures",
      "basin_merge_tree_subject15_maxima_provenance.csv"
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(fixture), 352L)
  expect_true(all(fixture$direction == "max"))
  expect_true(all(fixture$component == 1L))
  expect_identical(anyDuplicated(fixture$trajectory_basin_id), 0L)
  expect_identical(anyDuplicated(fixture$canonical_branch_id), 0L)
  expect_identical(
    fixture$trajectory_basin_id,
    fixture$canonical_branch_id
  )
  expect_identical(sum(fixture$is_component_survivor), 1L)
  expect_true(all(
    fixture$parent_canonical_branch_id[
      !fixture$is_component_survivor
    ] %in% fixture$canonical_branch_id
  ))
  expect_identical(
    provenance$upstream_repository_commit,
    "4615555547f3f406e79436c308d28fd78985b64e"
  )
})

test_that("Subject 15 reproduces the rank-17 Revision 9 proposal [57]", {
  fixture <- phase2_fixture()
  runtime <- phase2_fixture_runtime(fixture)
  controls <- gflowui:::gflowui_basin_default_controls(352L)
  result <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )
  proposal <- result$proposal
  mass <- gflowui:::gflowui_basin_derive_mass(
    proposal,
    runtime$bundle
  )
  expected.ids <- sort(c(
    "basin_max_v00001598",
    "basin_max_v00001628",
    "basin_max_v00001635",
    "basin_max_v00001575",
    "basin_max_v00001641",
    "basin_max_v00001578",
    "basin_max_v00001609",
    "basin_max_v00001603",
    "basin_max_v00001622",
    "basin_max_v00001590",
    "basin_max_v00001614",
    "basin_max_v00001621",
    "basin_max_v00001638",
    "basin_max_v00001574",
    "basin_max_v00001618",
    "basin_max_v00001640",
    "basin_max_v00001589"
  ))

  ranked <- sort(fixture$primary_support_mass, decreasing = TRUE)
  expect_equal(
    gflowui:::.gflowui_basin_fixed_sum(ranked),
    1.0000000000000087,
    tolerance = 3e-16
  )
  expect_identical(result$status, "proposal_created")
  expect_identical(proposal$core$outcome, "strong_gap")
  expect_identical(proposal$core$boundary, 17L)
  expect_equal(
    proposal$core$gap.decades,
    12.939763129977104,
    tolerance = 1e-12
  )
  expect_equal(proposal$core$informational.cutoff, 4.13957621441213e-09)
  expect_setequal(proposal$core$ids, expected.ids)
  expect_identical(proposal$ancestor.only.ids, character())
  expect_identical(proposal$final.ids, expected.ids)
  expect_equal(mass$denominator, 1.0000000000000087, tolerance = 3e-16)
  expect_equal(mass$core.coverage, 0.99999999999991729)
  expect_identical(proposal$render.outcome, "renderable")
})

test_that("Subject 15 Filter None exposes all 352 branches [58]", {
  runtime <- phase2_fixture_runtime()
  controls <- gflowui:::gflowui_basin_default_controls(352L)
  controls$filter.mode <- "none"
  result <- phase2_attempt(
    runtime$bundle,
    controls,
    layout.accessor = runtime$accessor
  )
  expect_identical(result$status, "proposal_created")
  expect_identical(result$proposal$core$outcome, "complete")
  expect_identical(length(result$proposal$core$ids), 352L)
  expect_identical(length(result$proposal$final.ids), 352L)
})

test_that("all 58 Revision 9 specification tests have explicit phase ownership", {
  ownership <- list(
    phase_2_policy = c(1:37, 44:49, 56:58),
    phase_3_reducer = c(38:43, 50:54),
    phase_5_rendering = 55L
  )
  assigned <- unlist(ownership, use.names = FALSE)
  expect_identical(sort(assigned), seq_len(58L))
  expect_identical(anyDuplicated(assigned), 0L)
})

# Phase 2 also exercises the pure display-policy distinctions in tests 50-54.
# Their attempt-allocation and reducer semantics remain owned by Phase 3.
