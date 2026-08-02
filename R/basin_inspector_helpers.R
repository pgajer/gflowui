.gflowui_basin_inspector_empty <- function() {
  data.frame(
    key = character(),
    type = character(),
    basin.id = character(),
    canonical.basin.id = character(),
    canonical.label = character(),
    canonical.label.rank = integer(),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_stable_labels <- function(table) {
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(.gflowui_basin_inspector_empty())
  }
  required <- c("type", "basin.id", "extremum.vertex")
  if (!all(required %in% names(table))) {
    stop(
      "Stable basin labels require type, basin ID, and extremum vertex.",
      call. = FALSE
    )
  }
  type <- as.character(table$type)
  basin.id <- as.character(table$basin.id)
  extremum.vertex <- suppressWarnings(as.integer(table$extremum.vertex))
  if (any(!type %in% c("max", "min")) ||
      anyNA(basin.id) || any(!nzchar(basin.id)) ||
      anyNA(extremum.vertex) || anyDuplicated(paste(type, basin.id))) {
    stop("The basin table cannot receive stable canonical labels.", call. = FALSE)
  }
  label.rank <- rep.int(NA_integer_, nrow(table))
  label <- rep.int(NA_character_, nrow(table))
  for (direction in c("max", "min")) {
    rows <- which(type == direction)
    ordered <- rows[order(
      extremum.vertex[rows],
      basin.id[rows],
      method = "radix"
    )]
    label.rank[ordered] <- seq_along(ordered)
    label[ordered] <- paste0(
      if (identical(direction, "max")) "M" else "m",
      seq_along(ordered)
    )
  }
  table$canonical.label.rank <- label.rank
  table$canonical.label <- label
  table$display.label <- label
  table
}

gflowui_basin_prepare_analysis_result <- function(result) {
  if (!is.list(result) || !is.data.frame(result$all_table)) {
    return(result)
  }
  all.table <- gflowui_basin_stable_labels(result$all_table)
  result$all_table <- all.table
  ## Basin Analysis row scope is now derived from the display proposal. Keep
  ## the complete table as the selection/color authority; the Inspector view
  ## is a non-mutating projection of it.
  result$table <- all.table
  result
}

.gflowui_basin_inspector_pair <- function(result, state) {
  if (!is.list(result) || !is.data.frame(result$all_table) ||
      !is.list(state)) {
    return(NULL)
  }
  proposal <- tryCatch(
    gflowui_basin_displayed_proposal(state),
    error = function(error) NULL
  )
  if (is.null(proposal)) {
    return(NULL)
  }
  snapshot <- tryCatch(
    gflowui_basin_bundle_snapshot(state$bundle),
    error = function(error) NULL
  )
  if (!is.list(snapshot) || !is.data.frame(snapshot$canonical)) {
    return(NULL)
  }
  list(proposal = proposal, canonical = snapshot$canonical)
}

.gflowui_basin_join_canonical_ids <- function(table, canonical) {
  output <- as.character(table$basin.id)
  maxima <- which(as.character(table$type) == "max")
  if (!length(maxima)) {
    return(output)
  }
  trajectory.id <- as.character(canonical$trajectory.basin.id)
  canonical.id <- as.character(canonical$basin.id)
  matched <- match(as.character(table$basin.id[maxima]), trajectory.id)
  fallback <- is.na(matched)
  if (any(fallback)) {
    matched[fallback] <- match(
      as.character(table$basin.id[maxima[fallback]]),
      canonical.id
    )
  }
  resolved <- !is.na(matched)
  output[maxima[resolved]] <- canonical.id[matched[resolved]]
  output
}

.gflowui_basin_reason_text <- function(proposal, id) {
  reasons <- character()
  if (id %in% proposal$core$ids) {
    reasons <- c(reasons, paste0("core:", proposal$core$outcome))
  }
  sentinel <- proposal$sentinels$reasons[[id]]
  if (length(sentinel)) {
    reasons <- c(reasons, as.character(sentinel))
  }
  if (id %in% proposal$ancestor.only.ids) {
    reasons <- c(reasons, "ancestor_closure")
  }
  paste(unique(reasons), collapse = "; ")
}

gflowui_basin_proposal_context_table <- function(
    result,
    state = NULL,
    selected_keys = character()) {
  if (!is.list(result) || !is.data.frame(result$all_table)) {
    return(.gflowui_basin_inspector_empty())
  }
  table <- gflowui_basin_stable_labels(result$all_table)
  table$canonical.basin.id <- as.character(table$basin.id)
  table$proposal.component <- FALSE
  table$proposal.initial.display <- FALSE
  table$proposal.core <- FALSE
  table$proposal.sentinel <- FALSE
  table$proposal.ancestor.only <- FALSE
  table$proposal.membership.class <- ifelse(
    as.character(table$type) == "max",
    "unavailable",
    "not_applicable"
  )
  table$proposal.inclusion.reasons <- ""
  table$proposal.pinned <- FALSE
  table$proposal.selected <- as.character(table$key) %in%
    as.character(selected_keys)
  table$proposal.visible <- NA
  table$proposal.hidden <- NA
  table$proposal.visibility <- ifelse(
    as.character(table$type) == "max",
    "unavailable",
    "not_applicable"
  )

  pair <- .gflowui_basin_inspector_pair(result, state)
  if (is.null(pair)) {
    return(table)
  }
  proposal <- pair$proposal
  table$canonical.basin.id <- .gflowui_basin_join_canonical_ids(
    table,
    pair$canonical
  )
  maxima <- as.character(table$type) == "max"
  component <- maxima & table$canonical.basin.id %in%
    proposal$component$ids
  ids <- table$canonical.basin.id
  table$proposal.component <- component
  table$proposal.membership.class[maxima & !component] <- "other_component"
  table$proposal.membership.class[component] <- "hidden"
  table$proposal.membership.class[component & ids %in%
    proposal$final.ids] <- "displayed"
  table$proposal.membership.class[component & ids %in%
    proposal$ancestor.only.ids] <- "ancestor_only"
  sentinel.only <- setdiff(proposal$sentinels$ids, proposal$core$ids)
  table$proposal.membership.class[component & ids %in%
    sentinel.only] <- "sentinel_only"
  table$proposal.membership.class[component & ids %in%
    proposal$core$ids] <- "core"
  table$proposal.membership.class[component & ids %in%
    proposal$pinned.ids] <- "pinned"
  table$proposal.initial.display <- component & ids %in%
    proposal$final.ids
  table$proposal.core <- component & ids %in% proposal$core$ids
  table$proposal.sentinel <- component & ids %in% proposal$sentinels$ids
  table$proposal.ancestor.only <- component & ids %in%
    proposal$ancestor.only.ids
  table$proposal.pinned <- component & ids %in% proposal$pinned.ids
  table$proposal.selected <- table$proposal.selected |
    (component & ids %in% state$selected.ids)
  table$proposal.visible[component] <- ids[component] %in% proposal$final.ids
  table$proposal.hidden[component] <- !table$proposal.visible[component]
  table$proposal.visibility[maxima & !component] <- "other_component"
  table$proposal.visibility[component & table$proposal.visible] <- "visible"
  table$proposal.visibility[component & table$proposal.hidden] <- "hidden"
  component.rows <- which(component)
  table$proposal.inclusion.reasons[component.rows] <- vapply(
    ids[component.rows],
    function(id) .gflowui_basin_reason_text(proposal, id),
    character(1)
  )
  table
}

gflowui_basin_inspector_scope_choices <- function() {
  c(
    "Initial display" = "initial_display",
    "Core" = "core",
    "Sentinels" = "sentinels",
    "Pinned" = "pinned",
    "Selected" = "selected",
    "All maximum basins" = "all_maxima",
    "All minimum basins" = "all_minima",
    "All basins" = "all"
  )
}

gflowui_basin_inspector_sort_choices <- function() {
  c(
    "Mass" = "mass",
    "Support" = "support",
    "Peak value" = "peak",
    "Prominence" = "prominence",
    "Canonical label" = "canonical_label"
  )
}

gflowui_basin_inspector_rows <- function(
    result,
    state = NULL,
    scope = "initial_display",
    sort.by = "mass",
    selected_keys = character()) {
  table <- gflowui_basin_proposal_context_table(
    result,
    state = state,
    selected_keys = selected_keys
  )
  if (!nrow(table)) {
    return(table)
  }
  scope <- as.character(scope %||% "initial_display")
  keep <- switch(
    scope,
    initial_display = table$proposal.initial.display,
    core = table$proposal.core,
    sentinels = table$proposal.sentinel,
    pinned = table$proposal.pinned,
    selected = table$proposal.selected,
    all_maxima = as.character(table$type) == "max",
    all_minima = as.character(table$type) == "min",
    all = rep(TRUE, nrow(table)),
    table$proposal.initial.display
  )
  keep[is.na(keep)] <- FALSE
  table <- table[keep, , drop = FALSE]
  if (!nrow(table)) {
    return(table)
  }
  sort.by <- as.character(sort.by %||% "mass")
  value <- switch(
    sort.by,
    support = suppressWarnings(as.numeric(table$primary.support.size)),
    peak = suppressWarnings(as.numeric(table$extremum.value)),
    prominence = suppressWarnings(as.numeric(table$prominence)),
    canonical_label = suppressWarnings(as.numeric(table$canonical.label.rank)),
    suppressWarnings(as.numeric(table$primary.support.mass))
  )
  direction.order <- match(as.character(table$type), c("max", "min"))
  decreasing <- !identical(sort.by, "canonical_label")
  order.value <- if (decreasing) -value else value
  if (identical(sort.by, "peak")) {
    minima <- as.character(table$type) == "min"
    order.value[minima] <- value[minima]
  }
  ordered <- order(
    direction.order,
    !is.finite(value),
    order.value,
    table$canonical.label.rank,
    method = "radix"
  )
  table <- table[ordered, , drop = FALSE]
  table$view.sort.rank <- seq_len(nrow(table))
  row.names(table) <- NULL
  table
}

gflowui_basin_selected_canonical_ids <- function(
    result,
    state,
    selected_keys) {
  table <- gflowui_basin_proposal_context_table(
    result,
    state = state,
    selected_keys = selected_keys
  )
  if (!nrow(table)) {
    return(character())
  }
  rows <- as.character(table$type) == "max" &
    as.character(table$key) %in% as.character(selected_keys) &
    table$proposal.component
  sort(unique(as.character(table$canonical.basin.id[rows])), method = "radix")
}

gflowui_basin_canonical_ids_to_keys <- function(
    result,
    state,
    canonical_ids) {
  if (!is.character(canonical_ids) ||
      anyNA(canonical_ids) ||
      anyDuplicated(canonical_ids)) {
    return(character())
  }
  table <- gflowui_basin_proposal_context_table(
    result,
    state = state,
    selected_keys = character()
  )
  if (!nrow(table)) {
    return(character())
  }
  rows <- as.character(table$type) == "max" &
    table$proposal.component &
    as.character(table$canonical.basin.id) %in% canonical_ids
  resolved <- table[rows, c("key", "canonical.basin.id"), drop = FALSE]
  if (!nrow(resolved)) {
    return(character())
  }
  ordered <- match(canonical_ids, as.character(resolved$canonical.basin.id))
  ordered <- ordered[!is.na(ordered)]
  unique(as.character(resolved$key[ordered]))
}

gflowui_basin_vertex_canonical_id <- function(
    result,
    state,
    vertex) {
  vertex.value <- suppressWarnings(as.numeric(vertex))
  if (!is.list(result) ||
      !is.data.frame(result$all_table) ||
      !is.list(result$basin) ||
      !is.data.frame(result$basin$assignment) ||
      !is.list(state) ||
      length(vertex.value) != 1L ||
      !is.finite(vertex.value) ||
      vertex.value != floor(vertex.value) ||
      vertex.value < 1) {
    return(character())
  }
  assignment <- result$basin$assignment
  required <- c(
    "vertex", "direction", "assignment.status", "basin.id"
  )
  if (!all(required %in% names(assignment))) {
    return(character())
  }
  rows <- suppressWarnings(as.numeric(assignment$vertex)) == vertex.value &
    as.character(assignment$direction) == "max" &
    as.character(assignment$assignment.status) == "assigned"
  basin.ids <- unique(as.character(assignment$basin.id[rows]))
  basin.ids <- basin.ids[!is.na(basin.ids) & nzchar(basin.ids)]
  if (length(basin.ids) != 1L) {
    return(character())
  }
  table <- gflowui_basin_proposal_context_table(
    result,
    state = state,
    selected_keys = character()
  )
  matched <- as.character(table$type) == "max" &
    as.character(table$basin.id) == basin.ids[[1L]] &
    table$proposal.component
  ids <- unique(as.character(table$canonical.basin.id[matched]))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 1L) ids else character()
}

gflowui_basin_linked_display_status <- function(state) {
  if (!is.list(state)) {
    return(list(
      available = FALSE,
      display.source = "none",
      active.outcome = "not_started",
      active.attempt.id = NA_integer_,
      displayed.attempt.id = NA_integer_,
      text = "No Basin Analysis proposal is available."
    ))
  }
  .gflowui_basin_assert_runtime_state(state)
  active <- state$active.attempt
  proposal <- gflowui_basin_displayed_proposal(state)
  active.outcome <- as.character(active$outcome %||% "not_started")
  active.id <- suppressWarnings(as.integer(
    active$attempt.id %||% NA_integer_
  ))
  displayed.id <- suppressWarnings(as.integer(
    proposal$attempt.id %||% NA_integer_
  ))
  detail <- paste(as.character(active$messages %||% character()), collapse = " ")
  text <- if (identical(state$display.source, "current")) {
    sprintf(
      paste(
        "Current proposal attempt %d is displayed across the tree, plots,",
        "Inspector, and graph."
      ),
      displayed.id
    )
  } else if (identical(state$display.source, "retained_last_valid")) {
    sprintf(
      paste(
        "Retained proposal attempt %d is displayed across the tree, plots,",
        "Inspector, and graph while active attempt %d is %s%s."
      ),
      displayed.id,
      active.id,
      gsub("_", " ", active.outcome, fixed = TRUE),
      if (nzchar(detail)) paste0(": ", detail) else ""
    )
  } else {
    sprintf(
      "No proposal is displayed; active attempt %d is %s%s.",
      active.id,
      gsub("_", " ", active.outcome, fixed = TRUE),
      if (nzchar(detail)) paste0(": ", detail) else ""
    )
  }
  list(
    available = !is.null(proposal),
    display.source = state$display.source,
    active.outcome = active.outcome,
    active.attempt.id = active.id,
    displayed.attempt.id = displayed.id,
    text = text
  )
}
