.gflowui_basin_bundle_session <- local({
  state <- new.env(parent = emptyenv())
  state$counter <- 0
  state$token <- digest::digest(
    list(
      time = as.numeric(Sys.time()),
      pid = Sys.getpid(),
      temporary.path = tempfile("gflowui-basin-bundle-session-")
    ),
    algo = "sha256",
    serialize = TRUE
  )
  state
})

.gflowui_basin_copy <- function(value) {
  unserialize(serialize(value, NULL, version = 3L))
}

.gflowui_basin_stop <- function(message,
                                class = "gflowui_basin_policy_error") {
  condition <- structure(
    list(message = message, call = NULL),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

.gflowui_basin_scalar_string <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.gflowui_basin_bundle_id <- function() {
  .gflowui_basin_bundle_session$counter <-
    .gflowui_basin_bundle_session$counter + 1
  sprintf(
    "bundle-%s-%010d",
    substr(.gflowui_basin_bundle_session$token, 1L, 20L),
    .gflowui_basin_bundle_session$counter
  )
}

.gflowui_basin_fixed_sum <- function(values) {
  total <- 0
  for (value in values) {
    total <- total + value
  }
  total
}

.gflowui_basin_required_identities <- function() {
  c(
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
}

.gflowui_basin_validate_identities <- function(identity) {
  required <- .gflowui_basin_required_identities()
  if (!is.list(identity) ||
      !identical(sort(names(identity)), sort(required)) ||
      !all(vapply(identity, .gflowui_basin_scalar_string, logical(1)))) {
    .gflowui_basin_stop(
      paste(
        "Scientific bundle identities must contain exactly the required",
        "nonempty scalar strings."
      ),
      "gflowui_basin_bundle_error"
    )
  }
  identity[required]
}

.gflowui_basin_tree_components <- function(tree, branches) {
  component <- tree$graph.input$validation$component
  vertices <- branches$extremum.vertex
  if (!is.numeric(vertices) ||
      anyNA(vertices) ||
      any(vertices != floor(vertices)) ||
      any(vertices < 1L | vertices > length(component))) {
    .gflowui_basin_stop(
      "Canonical branch extrema do not align with graph vertices.",
      "gflowui_basin_bundle_error"
    )
  }
  as.integer(component[as.integer(vertices)])
}

.gflowui_basin_prominence_validation_tree <- function(tree,
                                                       direction,
                                                       branches) {
  birth <- branches$birth.level
  death <- branches$death.level
  repairable <- is.numeric(branches$persistence) &&
    is.numeric(birth) &&
    is.numeric(death) &&
    length(birth) == nrow(branches) &&
    length(death) == nrow(branches) &&
    !anyNA(birth) &&
    !anyNA(death) &&
    all(is.finite(birth)) &&
    all(is.finite(death))
  if (!repairable) {
    return(tree)
  }

  expected <- birth - death
  if (any(!is.finite(expected)) || any(expected < 0)) {
    return(tree)
  }

  validation.tree <- .gflowui_basin_copy(tree)
  rows <- which(validation.tree$basin.table$type == direction)
  invalid.rows <- !is.finite(branches$persistence) |
    branches$persistence < 0
  persistence <- validation.tree$basin.table$persistence
  persistence[rows[invalid.rows]] <- expected[invalid.rows]
  validation.tree$basin.table$persistence <- persistence
  validation.tree
}

.gflowui_basin_validate_tree <- function(tree,
                                         direction,
                                         branches = NULL,
                                         allow.invalid.prominence = FALSE) {
  if (!inherits(tree, "basin.merge.tree") ||
      !identical(direction, "max") ||
      !direction %in% c(tree$direction, if (tree$direction == "both") {
        c("max", "min")
      } else {
        character()
      })) {
    .gflowui_basin_stop(
      "Version 1 requires a canonical maximum basin merge tree.",
      "gflowui_basin_bundle_error"
    )
  }
  if (is.null(branches)) {
    branches <- tree$basin.table[
      tree$basin.table$type == direction,
      ,
      drop = FALSE
    ]
  }
  if (!nrow(branches)) {
    .gflowui_basin_stop(
      "The canonical maximum merge tree has no branches.",
      "gflowui_basin_bundle_error"
    )
  }
  branches$component <- .gflowui_basin_tree_components(tree, branches)
  validation.tree <- if (allow.invalid.prominence) {
    .gflowui_basin_prominence_validation_tree(
      tree,
      direction,
      branches
    )
  } else {
    tree
  }
  for (component in sort(unique(branches$component))) {
    tryCatch(
      gflow::get.basin.merge.tree.layout(
        validation.tree,
        direction = direction,
        component = component
      ),
      error = function(error) {
        .gflowui_basin_stop(
          sprintf(
            "Canonical merge-tree validation failed: %s",
            conditionMessage(error)
          ),
          "gflowui_basin_bundle_error"
        )
      }
    )
  }
  branches
}

.gflowui_basin_plain_character_vector <- function(value, size) {
  is.character(value) &&
    !is.object(value) &&
    is.null(dim(value)) &&
    length(value) == size &&
    !anyNA(value)
}

.gflowui_basin_whole_number_vector <- function(value,
                                                size,
                                                lower = 1,
                                                upper = .Machine$integer.max) {
  (is.integer(value) || is.double(value)) &&
    !is.object(value) &&
    is.null(dim(value)) &&
    length(value) == size &&
    !anyNA(value) &&
    all(is.finite(value)) &&
    all(value == floor(value)) &&
    all(value >= lower) &&
    all(value <= upper)
}

.gflowui_basin_mapping_table <- function(trajectory.table,
                                         canonical,
                                         direction,
                                         vertex.count) {
  required <- c(
    "trajectory.basin.id",
    "direction",
    "component",
    "extremum.vertex",
    "primary.support.mass",
    "primary.support.size"
  )
  invalid <- !is.data.frame(trajectory.table) ||
    !is.data.frame(canonical) ||
    !all(required %in% names(trajectory.table)) ||
    !all(c(
      "basin.id",
      "type",
      "component",
      "extremum.vertex"
    ) %in% names(canonical)) ||
    nrow(trajectory.table) != nrow(canonical) ||
    !.gflowui_basin_scalar_string(direction) ||
    !.gflowui_basin_whole_number_vector(vertex.count, 1L)
  if (invalid) {
    return(list(status = "mapping_invalid", table = NULL))
  }

  trajectory <- trajectory.table[, required, drop = FALSE]
  size <- nrow(canonical)
  invalid <- !.gflowui_basin_plain_character_vector(
    trajectory$trajectory.basin.id,
    size
  ) ||
    !.gflowui_basin_plain_character_vector(
      trajectory$direction,
      size
    ) ||
    !.gflowui_basin_whole_number_vector(
      trajectory$component,
      size
    ) ||
    !.gflowui_basin_whole_number_vector(
      trajectory$extremum.vertex,
      size,
      upper = vertex.count
    ) ||
    !.gflowui_basin_plain_character_vector(
      canonical$basin.id,
      size
    ) ||
    !.gflowui_basin_plain_character_vector(canonical$type, size) ||
    !.gflowui_basin_whole_number_vector(canonical$component, size) ||
    !.gflowui_basin_whole_number_vector(
      canonical$extremum.vertex,
      size,
      upper = vertex.count
    )
  if (invalid) {
    return(list(status = "mapping_invalid", table = NULL))
  }

  invalid <- any(!nzchar(canonical$basin.id)) ||
    anyDuplicated(canonical$basin.id) ||
    any(!nzchar(trajectory$trajectory.basin.id)) ||
    anyDuplicated(trajectory$trajectory.basin.id) ||
    any(trajectory$direction != direction) ||
    any(canonical$type != direction) ||
    anyDuplicated(canonical$extremum.vertex) ||
    anyDuplicated(trajectory$extremum.vertex) ||
    !setequal(
      canonical$extremum.vertex,
      trajectory$extremum.vertex
    )
  if (invalid) {
    return(list(status = "mapping_invalid", table = NULL))
  }

  match.index <- match(
    canonical$extremum.vertex,
    trajectory$extremum.vertex
  )
  mapped <- trajectory[match.index, , drop = FALSE]
  row.names(mapped) <- NULL
  invalid <- anyNA(match.index) ||
    any(mapped$component != canonical$component) ||
    any(mapped$extremum.vertex != canonical$extremum.vertex)
  if (invalid) {
    return(list(status = "mapping_invalid", table = NULL))
  }
  mapped$canonical.basin.id <- canonical$basin.id
  mapped <- mapped[
    ,
    c(
      "trajectory.basin.id",
      "canonical.basin.id",
      "direction",
      "component",
      "extremum.vertex",
      "primary.support.mass",
      "primary.support.size"
    ),
    drop = FALSE
  ]
  list(status = "valid", table = mapped)
}

.gflowui_basin_source_status <- function(graph,
                                         vertex.ids,
                                         source.values,
                                         tree) {
  tree.vertex.ids <- as.character(tree$graph.input$vertex.id)
  aligned <- is.list(graph) &&
    identical(graph, tree$graph.input$adj.list) &&
    is.character(vertex.ids) &&
    length(vertex.ids) == tree$n.vertices &&
    !anyNA(vertex.ids) &&
    !anyDuplicated(vertex.ids) &&
    identical(vertex.ids, tree.vertex.ids) &&
    is.numeric(source.values) &&
    length(source.values) == tree$n.vertices &&
    identical(names(source.values), vertex.ids) &&
    all(is.finite(source.values)) &&
    identical(
      as.numeric(source.values),
      as.numeric(tree$field$construction.values)
    )
  if (aligned) "valid" else "source_invalid"
}

.gflowui_basin_ranking_status <- function(canonical,
                                          mapping,
                                          source.values,
                                          source.status) {
  if (mapping$status != "valid") {
    return(list(
      trajectory_flow_mass = "mass_invalid",
      trajectory_flow_support = "support_invalid",
      source_peak = "peak_invalid",
      canonical_prominence = "prominence_invalid"
    ))
  }
  mass <- mapping$table$primary.support.mass
  support <- mapping$table$primary.support.size
  prominence <- canonical$persistence
  peak <- if (source.status == "valid") {
    unname(source.values[canonical$extremum.vertex])
  } else {
    rep(NA_real_, nrow(canonical))
  }
  mass.status <- if (!is.numeric(mass) ||
      length(mass) != nrow(canonical) ||
      anyNA(mass) ||
      any(!is.finite(mass)) ||
      any(mass < 0)) {
    "mass_invalid"
  } else if (!any(mass > 0)) {
    "mass_unavailable"
  } else {
    "valid"
  }
  support.status <- if (!is.numeric(support) ||
      length(support) != nrow(canonical) ||
      anyNA(support) ||
      any(!is.finite(support)) ||
      any(support < 0) ||
      any(support != floor(support))) {
    "support_invalid"
  } else {
    "valid"
  }
  peak.status <- if (!is.numeric(peak) ||
      length(peak) != nrow(canonical) ||
      anyNA(peak) ||
      any(!is.finite(peak))) {
    "peak_invalid"
  } else {
    "valid"
  }
  prominence.status <- if (!is.numeric(prominence) ||
      length(prominence) != nrow(canonical) ||
      anyNA(prominence) ||
      any(!is.finite(prominence)) ||
      any(prominence < 0)) {
    "prominence_invalid"
  } else {
    "valid"
  }
  list(
    trajectory_flow_mass = mass.status,
    trajectory_flow_support = support.status,
    source_peak = peak.status,
    canonical_prominence = prominence.status
  )
}

.gflowui_basin_component_selection <- function(canonical,
                                                mapping.status,
                                                ranking) {
  components <- sort(unique(as.integer(canonical$component)))
  smallest <- components[[1L]]
  if (mapping.status != "valid") {
    return(list(
      id = smallest,
      rule = "smallest_component",
      fallback.reason = "mapping_invalid",
      totals = stats::setNames(
        rep(NA_real_, length(components)),
        components
      )
    ))
  }
  nonmass <- unlist(ranking[c(
    "trajectory_flow_support",
    "source_peak",
    "canonical_prominence"
  )], use.names = FALSE)
  invalid.nonmass <- nonmass[nonmass != "valid"]
  if (length(invalid.nonmass)) {
    return(list(
      id = smallest,
      rule = "smallest_component",
      fallback.reason = invalid.nonmass[[1L]],
      totals = stats::setNames(
        rep(NA_real_, length(components)),
        components
      )
    ))
  }
  if (ranking$trajectory_flow_mass == "mass_invalid") {
    return(list(
      id = smallest,
      rule = "smallest_component",
      fallback.reason = "smallest_component_mass_invalid",
      totals = stats::setNames(
        rep(NA_real_, length(components)),
        components
      )
    ))
  }
  mass <- canonical$trajectory.flow.mass
  ids <- canonical$basin.id
  totals <- vapply(components, function(component) {
    rows <- which(canonical$component == component & mass > 0)
    rows <- rows[order(ids[rows], method = "radix")]
    .gflowui_basin_fixed_sum(mass[rows])
  }, numeric(1))
  names(totals) <- as.character(components)
  if (!any(totals > 0)) {
    return(list(
      id = smallest,
      rule = "smallest_component",
      fallback.reason = "smallest_component_mass_unavailable",
      totals = totals
    ))
  }
  greatest <- max(totals)
  selected <- components[totals == greatest][[1L]]
  list(
    id = selected,
    rule = "greatest_positive_mass",
    fallback.reason = NULL,
    totals = totals
  )
}

#' Build an immutable in-session basin-analysis scientific bundle
#'
#' This private constructor snapshots and validates every scientific input
#' needed by the Revision 9 display policy. The returned environment has
#' locked bindings and never retains caller-owned mutable references.
#'
#' @keywords internal
#' @noRd
gflowui_basin_new_scientific_bundle <- function(
    graph,
    vertex.ids,
    source.values,
    identity,
    trajectory.table,
    canonical.tree,
    direction = "max") {
  identity <- .gflowui_basin_validate_identities(identity)
  tree <- .gflowui_basin_copy(canonical.tree)
  raw.branches <- if (inherits(tree, "basin.merge.tree") &&
      is.data.frame(tree$basin.table) &&
      all(c(
        "basin.id",
        "type",
        "extremum.vertex",
        "birth.level",
        "death.level",
        "persistence",
        "parent.basin.id"
      ) %in% names(tree$basin.table))) {
    tree$basin.table[
      tree$basin.table$type == direction,
      ,
      drop = FALSE
    ]
  } else {
    NULL
  }
  prominence.invalid <- !is.data.frame(raw.branches) ||
    !nrow(raw.branches) ||
    !is.numeric(raw.branches$persistence) ||
    anyNA(raw.branches$persistence) ||
    any(!is.finite(raw.branches$persistence)) ||
    any(raw.branches$persistence < 0)
  canonical <- if (prominence.invalid && is.data.frame(raw.branches) &&
      nrow(raw.branches)) {
    .gflowui_basin_validate_tree(
      tree,
      direction,
      branches = raw.branches,
      allow.invalid.prominence = TRUE
    )
  } else {
    .gflowui_basin_validate_tree(tree, direction)
  }
  source.status <- .gflowui_basin_source_status(
    graph,
    vertex.ids,
    source.values,
    tree
  )
  mapping <- .gflowui_basin_mapping_table(
    trajectory.table,
    canonical,
    direction,
    vertex.count = tree$n.vertices
  )
  ranking <- .gflowui_basin_ranking_status(
    canonical,
    mapping,
    source.values,
    source.status
  )
  if (prominence.invalid) {
    ranking$canonical_prominence <- "prominence_invalid"
  }

  canonical$peak.value <- if (source.status == "valid") {
    unname(source.values[canonical$extremum.vertex])
  } else {
    rep(NA_real_, nrow(canonical))
  }
  if (mapping$status == "valid") {
    canonical$trajectory.basin.id <-
      mapping$table$trajectory.basin.id
    canonical$trajectory.flow.mass <-
      as.numeric(mapping$table$primary.support.mass)
    canonical$trajectory.flow.support <-
      as.numeric(mapping$table$primary.support.size)
  } else {
    canonical$trajectory.basin.id <- NA_character_
    canonical$trajectory.flow.mass <- NA_real_
    canonical$trajectory.flow.support <- NA_real_
  }
  canonical <- canonical[
    order(
      canonical$component,
      canonical$basin.id,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  row.names(canonical) <- NULL
  selection <- .gflowui_basin_component_selection(
    canonical,
    mapping$status,
    ranking
  )
  validation <- c(
    list(
      source = source.status,
      mapping = mapping$status
    ),
    ranking
  )
  data <- list(
    identity = identity,
    direction = direction,
    graph = .gflowui_basin_copy(graph),
    vertex.ids = as.character(vertex.ids),
    source.values = as.numeric(source.values),
    trajectory.table = .gflowui_basin_copy(trajectory.table),
    canonical.tree = tree,
    canonical = canonical,
    validation = validation,
    component.selection = selection,
    component.ids = sort(unique(canonical$component))
  )
  bundle <- new.env(parent = emptyenv())
  class(bundle) <- c(
    "runtime.scientific.bundle",
    "gflowui_basin_scientific_bundle",
    "environment"
  )
  bundle$bundle.id <- .gflowui_basin_bundle_id()
  bundle$data <- .gflowui_basin_copy(data)
  lockEnvironment(bundle, bindings = TRUE)
  bundle
}

.gflowui_basin_assert_bundle <- function(bundle) {
  if (!inherits(bundle, "gflowui_basin_scientific_bundle") ||
      !is.environment(bundle) ||
      !environmentIsLocked(bundle) ||
      !bindingIsLocked("bundle.id", bundle) ||
      !bindingIsLocked("data", bundle) ||
      !.gflowui_basin_scalar_string(bundle$bundle.id)) {
    .gflowui_basin_stop(
      "A locked runtime scientific bundle is required.",
      "gflowui_basin_bundle_error"
    )
  }
  invisible(TRUE)
}

gflowui_basin_bundle_snapshot <- function(bundle) {
  .gflowui_basin_assert_bundle(bundle)
  .gflowui_basin_copy(bundle$data)
}

gflowui_basin_context <- function(bundle,
                                  context.generation = 1L,
                                  component = NULL) {
  .gflowui_basin_assert_bundle(bundle)
  data <- gflowui_basin_bundle_snapshot(bundle)
  if (!is.numeric(context.generation) ||
      length(context.generation) != 1L ||
      is.na(context.generation) ||
      !is.finite(context.generation) ||
      context.generation != floor(context.generation) ||
      context.generation < 1 ||
      context.generation > .Machine$integer.max) {
    .gflowui_basin_stop(
      "'context.generation' must be a positive supported whole number.",
      "gflowui_basin_context_error"
    )
  }
  explicit <- !is.null(component)
  if (explicit) {
    if (!is.numeric(component) ||
        length(component) != 1L ||
        is.na(component) ||
        component != floor(component) ||
        !component %in% data$component.ids) {
      .gflowui_basin_stop(
        "The explicitly selected component is unavailable.",
        "gflowui_basin_context_error"
      )
    }
    selected <- as.integer(component)
    selection.rule <- "explicit"
    fallback.reason <- NULL
  } else {
    selected <- as.integer(data$component.selection$id)
    selection.rule <- data$component.selection$rule
    fallback.reason <- data$component.selection$fallback.reason
  }
  key.input <- c(
    data$identity,
    list(
      bundle.id = bundle$bundle.id,
      direction = data$direction,
      component = selected
    )
  )
  list(
    context.key = digest::digest(
      key.input,
      algo = "sha256",
      serialize = TRUE
    ),
    bundle.id = bundle$bundle.id,
    context.generation = as.integer(context.generation),
    direction = data$direction,
    component = selected,
    selection.rule = selection.rule,
    fallback.reason = fallback.reason,
    identity = data$identity
  )
}

gflowui_basin_new_runtime_state <- function(bundle,
                                            context.generation = 1L) {
  context <- gflowui_basin_context(
    bundle,
    context.generation = context.generation
  )
  list(
    bundle = bundle,
    context = context,
    current.proposal = NULL,
    retained.last.valid.proposal = NULL,
    pinned.ids = character(),
    selected.ids = character(),
    caches = list(),
    pending.work = NULL
  )
}

gflowui_basin_replace_runtime_bundle <- function(state, bundle) {
  if (!is.list(state) ||
      is.null(state$context) ||
      !is.numeric(state$context$context.generation)) {
    .gflowui_basin_stop(
      "A valid basin-analysis runtime state is required.",
      "gflowui_basin_context_error"
    )
  }
  next.generation <- state$context$context.generation + 1
  if (next.generation > .Machine$integer.max) {
    .gflowui_basin_stop(
      "The context generation exceeded the supported R integer range.",
      "gflowui_basin_context_error"
    )
  }
  gflowui_basin_new_runtime_state(
    bundle,
    context.generation = next.generation
  )
}
