gflowui_estimate_basin_overlay <- function(
    adj_list,
    edge_length_list,
    field,
    direction = "max",
    top_k = 6L,
    vertex_mass = NULL) {
  if (!is.list(adj_list) || length(adj_list) < 1L) {
    stop("The selected graph has no adjacency data.", call. = FALSE)
  }
  if (!is.list(edge_length_list) ||
      length(edge_length_list) != length(adj_list)) {
    stop("The selected graph has no aligned edge lengths.", call. = FALSE)
  }

  field <- suppressWarnings(as.numeric(field))
  n_vertices <- length(adj_list)
  if (length(field) != n_vertices) {
    stop(
      "The selected estimate is not aligned with the displayed graph.",
      call. = FALSE
    )
  }
  direction <- match.arg(as.character(direction), c("max", "min"))
  top_k <- suppressWarnings(as.integer(top_k))
  if (!is.finite(top_k) || top_k < 1L) {
    top_k <- 6L
  }

  finite_vertices <- which(is.finite(field))
  if (length(finite_vertices) < 2L) {
    stop(
      "The selected estimate has fewer than two finite graph values.",
      call. = FALSE
    )
  }
  old_to_new <- rep.int(NA_integer_, n_vertices)
  old_to_new[finite_vertices] <- seq_along(finite_vertices)
  induced_adj <- vector("list", length(finite_vertices))
  induced_weights <- vector("list", length(finite_vertices))
  for (ii in seq_along(finite_vertices)) {
    old_vertex <- finite_vertices[[ii]]
    neighbors <- suppressWarnings(as.integer(adj_list[[old_vertex]]))
    weights <- suppressWarnings(as.numeric(edge_length_list[[old_vertex]]))
    if (length(neighbors) != length(weights)) {
      stop(
        "The selected graph has misaligned adjacency and edge lengths.",
        call. = FALSE
      )
    }
    keep <- is.finite(neighbors) &
      neighbors >= 1L &
      neighbors <= n_vertices &
      is.finite(old_to_new[neighbors]) &
      is.finite(weights) &
      weights >= 0
    induced_adj[[ii]] <- as.integer(old_to_new[neighbors[keep]])
    induced_weights[[ii]] <- as.numeric(weights[keep])
  }

  mass_use <- NULL
  if (!is.null(vertex_mass)) {
    vertex_mass <- suppressWarnings(as.numeric(vertex_mass))
    if (length(vertex_mass) != n_vertices ||
        any(!is.finite(vertex_mass[finite_vertices])) ||
        any(vertex_mass[finite_vertices] < 0) ||
        sum(vertex_mass[finite_vertices]) <= 0) {
      stop(
        "The selected estimate has invalid vertex mass for basin ranking.",
        call. = FALSE
      )
    }
    mass_use <- vertex_mass[finite_vertices]
  }

  basin <- gflow::create.basin.complex(
    adj.list = induced_adj,
    edge.length.list = induced_weights,
    field = field[finite_vertices],
    method = "trajectory_flow",
    direction = direction,
    vertex.mass = mass_use,
    method.params = list(
      modulation = "CLOSEST",
      plateau.policy = "connected_exact",
      edge.length.quantile.thld = 1,
      long.edge.fallback = "allow_and_flag",
      store.trajectories = TRUE,
      symmetric.seeding = FALSE,
      tie.breaking = FALSE,
      primary.assignment.policy = "backend_primary"
    ),
    simplify.params = list(),
    verbose = FALSE
  )
  if (!is.list(basin) || !identical(as.character(basin$status), "ok")) {
    detail <- as.character(
      basin$diagnostics$message %||%
        basin$diagnostics$error %||%
        "The basin backend did not return a usable result."
    )
    stop(detail, call. = FALSE)
  }

  basin_table <- basin$basin.table
  assignment <- basin$assignment
  required_basin <- c(
    "basin.id", "extremum.vertex", "extremum.value",
    "primary.support.size", "primary.support.mass"
  )
  if (!is.data.frame(basin_table) ||
      !all(required_basin %in% names(basin_table)) ||
      !is.data.frame(assignment) ||
      !all(c("vertex", "basin.id") %in% names(assignment))) {
    stop("The basin backend returned an incomplete result.", call. = FALSE)
  }

  use_mass <- !is.null(mass_use) &&
    any(is.finite(suppressWarnings(as.numeric(
      basin_table$primary.support.mass
    ))))
  rank_value <- if (isTRUE(use_mass)) {
    suppressWarnings(as.numeric(basin_table$primary.support.mass))
  } else {
    suppressWarnings(as.numeric(basin_table$primary.support.size))
  }
  rank_value[!is.finite(rank_value)] <- -Inf
  basin_order <- order(
    -rank_value,
    as.character(basin_table$basin.id),
    method = "radix"
  )
  basin_table <- basin_table[basin_order, , drop = FALSE]
  basin_table$rank <- seq_len(nrow(basin_table))
  top_k <- min(top_k, nrow(basin_table))
  top <- basin_table[seq_len(top_k), , drop = FALSE]

  display_labels <- if (isTRUE(use_mass)) {
    sprintf(
      "Basin %02d (mass %.3f)",
      top$rank,
      suppressWarnings(as.numeric(top$primary.support.mass))
    )
  } else {
    sprintf(
      "Basin %02d (support %d)",
      top$rank,
      suppressWarnings(as.integer(top$primary.support.size))
    )
  }
  labels_by_id <- stats::setNames(
    display_labels,
    as.character(top$basin.id)
  )

  assignment_vertex <- suppressWarnings(as.integer(assignment$vertex))
  assignment_old_vertex <- rep.int(NA_integer_, length(assignment_vertex))
  valid_assignment <- is.finite(assignment_vertex) &
    assignment_vertex >= 1L &
    assignment_vertex <= length(finite_vertices)
  assignment_old_vertex[valid_assignment] <-
    finite_vertices[assignment_vertex[valid_assignment]]
  assignment_labels <- unname(
    labels_by_id[as.character(assignment$basin.id)]
  )
  assignment_labels[is.na(assignment_labels)] <- "Other basins"
  values <- rep.int("Unavailable", n_vertices)
  valid_old_vertex <- is.finite(assignment_old_vertex)
  values[assignment_old_vertex[valid_old_vertex]] <-
    assignment_labels[valid_old_vertex]

  extremum_vertex <- suppressWarnings(as.integer(top$extremum.vertex))
  extremum_old_vertex <- rep.int(NA_integer_, length(extremum_vertex))
  valid_extremum <- is.finite(extremum_vertex) &
    extremum_vertex >= 1L &
    extremum_vertex <= length(finite_vertices)
  extremum_old_vertex[valid_extremum] <-
    finite_vertices[extremum_vertex[valid_extremum]]

  table <- data.frame(
    rank = as.integer(top$rank),
    basin = as.character(top$basin.id),
    mass = suppressWarnings(as.numeric(top$primary.support.mass)),
    support = suppressWarnings(as.integer(top$primary.support.size)),
    extremum.vertex = extremum_old_vertex,
    extremum.value = suppressWarnings(as.numeric(top$extremum.value)),
    stringsAsFactors = FALSE
  )
  list(
    values = values,
    table = table,
    top_k = as.integer(top_k),
    basin_count = as.integer(nrow(basin_table)),
    direction = direction,
    ranking = if (isTRUE(use_mass)) "primary mass" else "primary support size",
    basin = basin
  )
}
