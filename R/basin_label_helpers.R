gflowui_basin_label_basis_choices <- function() {
  c(
    "Field extremum value" = "extremum.value",
    "Trajectory-flow basin mass" = "primary.support.mass",
    "Trajectory-flow basin support" = "primary.support.size"
  )
}

gflowui_basin_normalize_label_basis <- function(
    value,
    default = "primary.support.mass") {
  choices <- unname(gflowui_basin_label_basis_choices())
  value <- as.character(value %||% default)
  if (length(value) != 1L ||
      is.na(value) ||
      !(value %in% choices)) {
    default
  } else {
    value
  }
}

gflowui_basin_label_basis_name <- function(value) {
  choices <- gflowui_basin_label_basis_choices()
  value <- gflowui_basin_normalize_label_basis(value)
  name <- names(choices)[match(value, unname(choices))]
  if (length(name) == 1L && !is.na(name) && nzchar(name)) {
    name
  } else {
    value
  }
}

gflowui_basin_label_basis_description <- function(value) {
  value <- gflowui_basin_normalize_label_basis(value)
  switch(
    value,
    extremum.value = paste(
      "Maximum basins are numbered from the highest field maximum;",
      "minimum basins are numbered from the lowest field minimum."
    ),
    primary.support.size = paste(
      "Maximum and minimum basins are numbered separately from the",
      "largest uniquely assigned trajectory-flow support."
    ),
    paste(
      "Maximum and minimum basins are numbered separately from the",
      "largest uniquely assigned trajectory-flow mass."
    )
  )
}

gflowui_basin_label_basis_availability <- function(table) {
  choices <- unname(gflowui_basin_label_basis_choices())
  available <- stats::setNames(rep(FALSE, length(choices)), choices)
  if (!is.data.frame(table) || nrow(table) < 1L ||
      !all(c("type", "extremum.vertex") %in% names(table))) {
    return(available)
  }
  fields <- c(
    "extremum.value",
    "primary.support.mass",
    "primary.support.size"
  )
  for (field in fields) {
    if (!(field %in% names(table))) {
      next
    }
    value <- suppressWarnings(as.numeric(table[[field]]))
    available[[field]] <- length(value) == nrow(table) &&
      all(is.finite(value))
  }
  available
}

gflowui_basin_resolve_label_basis <- function(
    table,
    requested = "primary.support.mass") {
  requested <- gflowui_basin_normalize_label_basis(requested)
  available <- gflowui_basin_label_basis_availability(table)
  if (isTRUE(available[[requested]])) {
    return(list(
      requested = requested,
      resolved = requested,
      available = available,
      fallback = FALSE,
      message = ""
    ))
  }
  fallback.order <- c(
    "primary.support.size",
    "extremum.value",
    "primary.support.mass"
  )
  resolved <- fallback.order[
    vapply(fallback.order, function(key) isTRUE(available[[key]]), logical(1))
  ]
  if (!length(resolved)) {
    stop(
      "No complete basin characteristic is available for readable labels.",
      call. = FALSE
    )
  }
  resolved <- resolved[[1L]]
  list(
    requested = requested,
    resolved = resolved,
    available = available,
    fallback = TRUE,
    message = sprintf(
      "%s is unavailable for this basin complex; labels use %s.",
      gflowui_basin_label_basis_name(requested),
      gflowui_basin_label_basis_name(resolved)
    )
  )
}

gflowui_basin_apply_label_basis_table <- function(
    table,
    label_basis = "primary.support.mass") {
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(table)
  }
  required <- c("type", "basin.id", "extremum.vertex")
  if (!all(required %in% names(table))) {
    stop(
      "Basin labels require type, basin ID, and extremum vertex.",
      call. = FALSE
    )
  }
  resolution <- gflowui_basin_resolve_label_basis(table, label_basis)
  basis <- resolution$resolved
  type <- as.character(table$type)
  basin.id <- as.character(table$basin.id)
  extremum.vertex <- suppressWarnings(as.integer(table$extremum.vertex))
  value <- suppressWarnings(as.numeric(table[[basis]]))
  if (any(!type %in% c("max", "min")) ||
      anyNA(basin.id) ||
      any(!nzchar(basin.id)) ||
      anyNA(extremum.vertex) ||
      anyDuplicated(paste(type, basin.id)) ||
      any(!is.finite(value))) {
    stop("The basin table cannot receive ranked display labels.", call. = FALSE)
  }
  rank <- rep.int(NA_integer_, nrow(table))
  label <- rep.int(NA_character_, nrow(table))
  for (direction in c("max", "min")) {
    rows <- which(type == direction)
    order.value <- if (
      identical(basis, "extremum.value") &&
        identical(direction, "min")
    ) {
      value[rows]
    } else {
      -value[rows]
    }
    ordered <- rows[order(
      order.value,
      extremum.vertex[rows],
      basin.id[rows],
      method = "radix"
    )]
    rank[ordered] <- seq_along(ordered)
    label[ordered] <- paste0(
      if (identical(direction, "max")) "M" else "m",
      seq_along(ordered)
    )
  }
  table$label.basis <- rep.int(basis, nrow(table))
  table$label.rank <- rank
  table$display.label <- label
  table
}

gflowui_basin_apply_label_basis <- function(
    result,
    label_basis = "primary.support.mass") {
  if (!is.list(result) || !is.data.frame(result$all_table)) {
    return(result)
  }
  all.table <- gflowui_basin_stable_labels(result$all_table)
  resolution <- gflowui_basin_resolve_label_basis(all.table, label_basis)
  all.table <- gflowui_basin_apply_label_basis_table(
    all.table,
    resolution$resolved
  )
  result$all_table <- all.table
  if (is.data.frame(result$table)) {
    matched <- match(as.character(result$table$key), as.character(all.table$key))
    valid <- !is.na(matched)
    for (field in c(
        "canonical.label",
        "canonical.label.rank",
        "label.basis",
        "label.rank",
        "display.label"
    )) {
      result$table[[field]] <- ifelse(
        valid,
        all.table[[field]][matched],
        NA
      )
    }
  } else {
    result$table <- all.table
  }
  result$label_basis <- resolution$resolved
  result$label_basis_label <- gflowui_basin_label_basis_name(
    resolution$resolved
  )
  result$label_basis_availability <- resolution$available
  result$label_basis_message <- resolution$message
  result
}

gflowui_basin_canonical_label_map <- function(result, state) {
  table <- gflowui_basin_proposal_context_table(result, state = state)
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(structure(character(), names = character()))
  }
  rows <- as.character(table$type) == "max" &
    as.logical(table$proposal.component)
  rows[is.na(rows)] <- FALSE
  ids <- as.character(table$canonical.basin.id[rows])
  labels <- as.character(table$display.label[rows])
  valid <- !is.na(ids) & nzchar(ids) & !is.na(labels) & nzchar(labels)
  stats::setNames(labels[valid], ids[valid])
}
