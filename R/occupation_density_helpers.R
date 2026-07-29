gflowui_occupation_density_sets <- function(manifest) {
  sets <- manifest$occupation_density_sets
  if (!is.list(sets)) list() else sets
}

gflowui_occupation_density_set <- function(manifest, set_id = NULL) {
  sets <- gflowui_occupation_density_sets(manifest)
  if (length(sets) < 1L) {
    return(NULL)
  }
  ids <- vapply(sets, function(x) as.character(x$id %||% ""), character(1))
  wanted <- as.character(set_id %||% manifest$defaults$occupation_density_set_id %||% "")
  idx <- match(wanted, ids)
  if (!is.finite(idx)) {
    idx <- 1L
  }
  sets[[idx]]
}

gflowui_occupation_density_path <- function(path, project_root) {
  pp <- as.character(path %||% "")
  if (!nzchar(pp)) {
    return("")
  }
  candidate <- if (grepl("^(/|~)", pp)) path.expand(pp) else file.path(project_root, pp)
  normalizePath(candidate, mustWork = TRUE)
}

gflowui_normalize_density <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out) | out < 0] <- 0
  total <- sum(out)
  if (!is.finite(total) || total <= 0) {
    stop("The occupation-density estimate has no finite positive mass.", call. = FALSE)
  }
  out / total
}

gflowui_numeric_color_encoding <- function(
    values,
    transform = "identity",
    title = "Value",
    density_log_decades = 6) {
  raw <- suppressWarnings(as.numeric(values))
  mapped <- raw
  colorbar <- list(title = as.character(title))
  floor_value <- NA_real_

  if (!identical(as.character(transform), "density_log10")) {
    return(list(
      raw_values = raw,
      mapped_values = mapped,
      colorbar = colorbar,
      floor_value = floor_value
    ))
  }

  positive <- raw[is.finite(raw) & raw > 0]
  if (length(positive) < 1L) {
    return(list(
      raw_values = raw,
      mapped_values = mapped,
      colorbar = colorbar,
      floor_value = floor_value
    ))
  }

  density_log_decades <- suppressWarnings(as.numeric(density_log_decades))
  if (!is.finite(density_log_decades) || density_log_decades <= 0) {
    density_log_decades <- 6
  }
  peak <- max(positive)
  floor_value <- max(peak * 10^(-density_log_decades), .Machine$double.xmin)
  finite <- is.finite(raw)
  mapped[finite] <- log10(pmax(raw[finite], floor_value))

  limits <- c(log10(floor_value), log10(peak))
  ticks <- pretty(limits, n = 5)
  ticks <- ticks[is.finite(ticks) & ticks > limits[[1]] & ticks < limits[[2]]]
  ticks <- sort(unique(c(limits[[1]], ticks, limits[[2]])))
  tick_labels <- formatC(10^ticks, format = "e", digits = 1)
  if (length(tick_labels) > 0L) {
    tick_labels[[1L]] <- paste0("<=", formatC(floor_value, format = "e", digits = 1))
  }

  colorbar <- list(
    title = paste0(as.character(title), "<br>(log10 color)"),
    tickvals = ticks,
    ticktext = tick_labels
  )
  list(
    raw_values = raw,
    mapped_values = mapped,
    colorbar = colorbar,
    floor_value = floor_value,
    color_limits = limits
  )
}

gflowui_density_colors <- function(include_none = FALSE) {
  colors <- c(
    yellow = "#FDE725",
    orange = "#F97316",
    white = "#F8FAFC",
    purple = "#7E22CE",
    blue = "#2563EB",
    green = "#16A34A",
    red = "#C51B1D"
  )
  if (isTRUE(include_none)) {
    colors <- c(none = "", colors)
  }
  colors
}

gflowui_density_midpoint_colors <- function() {
  c(
    none = "",
    gflowui_density_colors()
  )
}

gflowui_density_color <- function(choice, default, include_none = FALSE) {
  choices <- gflowui_density_colors(include_none = include_none)
  key <- tolower(trimws(as.character(choice %||% default)))
  if (length(key) < 1L || !(key[[1L]] %in% names(choices))) {
    key <- default
  } else {
    key <- key[[1L]]
  }
  unname(choices[[key]])
}

gflowui_density_palette <- function(
    low = "yellow",
    midpoint = "none",
    high = "red",
    low_alpha = 1,
    midpoint_alpha = 1,
    high_alpha = 1) {
  normalize_alpha <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    if (!is.finite(value)) {
      return(1)
    }
    max(0, min(1, value))
  }
  with_alpha <- function(color, alpha) {
    if (!nzchar(color)) {
      return("")
    }
    alpha <- normalize_alpha(alpha)
    if (alpha >= 1) {
      return(color)
    }
    grDevices::adjustcolor(color, alpha.f = alpha)
  }
  low_color <- gflowui_density_color(low, default = "yellow")
  middle <- gflowui_density_color(
    midpoint,
    default = "none",
    include_none = TRUE
  )
  high_color <- gflowui_density_color(high, default = "red")
  c(
    with_alpha(low_color, low_alpha),
    if (nzchar(middle)) {
      with_alpha(middle, midpoint_alpha)
    } else {
      character(0)
    },
    with_alpha(high_color, high_alpha)
  )
}

gflowui_plotly_colorscale <- function(colors) {
  colors <- as.character(colors)
  colors <- colors[nzchar(colors)]
  if (length(colors) < 2L) {
    colors <- c("#FDE725", "#C51B1D")
  }
  colors <- vapply(colors, function(color) {
    rgba <- grDevices::col2rgb(color, alpha = TRUE)
    sprintf(
      "rgba(%d,%d,%d,%.4f)",
      rgba[[1L]],
      rgba[[2L]],
      rgba[[3L]],
      rgba[[4L]] / 255
    )
  }, character(1))
  positions <- seq(0, 1, length.out = length(colors))
  lapply(seq_along(colors), function(ii) {
    c(positions[[ii]], colors[[ii]])
  })
}

gflowui_density_local_extrema <- function(values, adj_list) {
  values <- suppressWarnings(as.numeric(values))
  if (!is.list(adj_list) || length(values) != length(adj_list)) {
    stop(
      "Density values and graph adjacency must have the same length.",
      call. = FALSE
    )
  }

  n_vertices <- length(values)
  maxima <- logical(n_vertices)
  minima <- logical(n_vertices)
  for (vertex in seq_len(n_vertices)) {
    if (!is.finite(values[[vertex]])) {
      next
    }
    neighbors <- suppressWarnings(as.integer(adj_list[[vertex]]))
    neighbors <- unique(neighbors[
      is.finite(neighbors) &
        neighbors >= 1L &
        neighbors <= n_vertices &
        neighbors != vertex
    ])
    neighbors <- neighbors[is.finite(values[neighbors])]
    if (length(neighbors) < 1L) {
      next
    }
    maxima[[vertex]] <- all(values[[vertex]] > values[neighbors])
    minima[[vertex]] <- all(values[[vertex]] < values[neighbors])
  }

  ranked <- function(vertices, type) {
    if (length(vertices) < 1L) {
      return(data.frame(
        vertex = integer(0),
        value = numeric(0),
        type = character(0),
        rank = integer(0),
        label = character(0),
        stringsAsFactors = FALSE
      ))
    }
    decreasing <- identical(type, "maximum")
    ord <- if (decreasing) {
      order(-values[vertices], vertices)
    } else {
      order(values[vertices], vertices)
    }
    vertices <- vertices[ord]
    prefix <- if (decreasing) "M" else "m"
    data.frame(
      vertex = vertices,
      value = values[vertices],
      type = type,
      rank = seq_along(vertices),
      label = sprintf("%s_%d", prefix, seq_along(vertices)),
      stringsAsFactors = FALSE
    )
  }

  rbind(
    ranked(which(maxima), "maximum"),
    ranked(which(minima), "minimum")
  )
}

gflowui_occupation_density_method <- function(set, method_id) {
  methods <- set$methods %||% list()
  method_ids <- vapply(methods, function(x) as.character(x$id %||% ""), character(1))
  idx <- match(as.character(method_id), method_ids)
  if (!is.finite(idx)) {
    stop("Unknown occupation-density method.", call. = FALSE)
  }
  methods[[idx]]
}

gflowui_precomputed_density_path <- function(set, project_root, method_id) {
  method <- gflowui_occupation_density_method(set, method_id)
  if (!identical(as.character(method$source %||% ""), "precomputed_path")) {
    stop("The occupation-density method is not a precomputed path.", call. = FALSE)
  }
  path_file <- gflowui_occupation_density_path(method$path_file, project_root)
  path <- readRDS(path_file)
  required <- c(
    "subject.id", "probability.mass", "path.summary", "field.index",
    "raw.basins", "assignments"
  )
  if (!is.list(path) || !all(required %in% names(path))) {
    stop("The precomputed occupation-density path is malformed.", call. = FALSE)
  }
  probability_mass <- as.matrix(path$probability.mass)
  path_summary <- path$path.summary
  if (!is.numeric(probability_mass) ||
      !is.data.frame(path_summary) ||
      !all(c("eta.index", "eta", "mean.brier") %in% names(path_summary)) ||
      nrow(probability_mass) < 1L ||
      ncol(probability_mass) != nrow(path_summary)) {
    stop("The precomputed density matrix and path summary are not aligned.", call. = FALSE)
  }
  eta_index <- suppressWarnings(as.integer(path_summary$eta.index))
  if (anyNA(eta_index) || anyDuplicated(eta_index) ||
      !identical(sort(eta_index), seq_len(ncol(probability_mass)))) {
    stop("The precomputed path must contain consecutive, unique time indices.", call. = FALSE)
  }
  column_mass <- colSums(probability_mass)
  if (any(!is.finite(probability_mass)) || any(probability_mass < 0) ||
      any(!is.finite(column_mass)) ||
      any(abs(column_mass - 1) > 1e-8)) {
    stop("The precomputed probability-mass fields are invalid.", call. = FALSE)
  }
  selected <- path$selected
  required_identity <- c(
    "graph.id", "graph.k", "graph.fingerprint", "vertex.fingerprint"
  )
  if (!is.list(path$settings) || !is.data.frame(selected) ||
      nrow(selected) != 1L ||
      !all(required_identity %in% names(selected))) {
    stop(
      paste(
        "The precomputed path lacks the required graph and ordered-vertex",
        "alignment contract."
      ),
      call. = FALSE
    )
  }
  graph_id <- as.character(path$settings$graph.id %||% "")
  graph_k <- suppressWarnings(as.integer(path$settings$graph.k %||% NA_integer_))
  if (length(graph_id) != 1L || !nzchar(graph_id) ||
      !is.finite(graph_k) || graph_k < 1L ||
      !identical(graph_id, as.character(selected$graph.id[[1L]])) ||
      !identical(graph_k, suppressWarnings(as.integer(selected$graph.k[[1L]])))) {
    stop(
      "The precomputed path has inconsistent graph ID or graph k metadata.",
      call. = FALSE
    )
  }
  contract <- set$basin_source_contract
  source_vertex_id <- enc2utf8(as.character(
    contract$source.vertex.id %||%
      path$spectral.coordinates$point.id %||%
      character()
  ))
  source_vertex_fingerprint <- if (
    length(source_vertex_id) == nrow(probability_mass) &&
      !anyNA(source_vertex_id) && all(nzchar(source_vertex_id)) &&
      !anyDuplicated(source_vertex_id)
  ) {
    gflowui_basin_sha256(list(
      schema = "hmp_graph_heat_vertices_v1",
      vertex.id = source_vertex_id
    ))
  } else {
    ""
  }
  if (!is.list(contract) ||
      length(source_vertex_id) != nrow(probability_mass) ||
      !identical(
        source_vertex_fingerprint,
        as.character(selected$vertex.fingerprint[[1L]])
      ) ||
      !identical(
        as.character(contract$graph.id %||% ""),
        graph_id
      ) ||
      !identical(
        suppressWarnings(as.integer(contract$graph.k %||% NA_integer_)),
        graph_k
      ) ||
      !identical(
        as.character(contract$graph.fingerprint %||% ""),
        as.character(selected$graph.fingerprint[[1L]])
      ) ||
      !identical(
        as.character(contract$vertex.id.fingerprint %||% ""),
        as.character(selected$vertex.fingerprint[[1L]])
      ) ||
      !nzchar(as.character(
        contract$display.vertex.id.fingerprint %||% ""
      ))) {
    stop(
      paste(
        "The occupation-density manifest contract does not match the",
        "precomputed path graph or ordered source vertices."
      ),
      call. = FALSE
    )
  }
  asset_fingerprint <- gflowui_basin_file_sha256(path_file)
  list(
    method = method,
    path = path,
    path_file = path_file,
    probability_mass = probability_mass,
    path_summary = path_summary,
    source_asset_fingerprint = asset_fingerprint,
    alignment_base = list(
      contract.version = as.character(
        path$contract.id %||% path$settings$contract.id %||% ""
      ),
      algorithm = paste(
        "hmp_graph_heat_graph_v1 + hmp_graph_heat_vertices_v1 +",
        "gflowui_basin_field_v1; SHA-256 serialized-R exact comparison"
      ),
      graph.id = graph_id,
      graph.k = graph_k,
      graph.fingerprint = as.character(selected$graph.fingerprint[[1L]]),
      vertex.id.fingerprint = as.character(selected$vertex.fingerprint[[1L]]),
      display.vertex.id.fingerprint = as.character(
        contract$display.vertex.id.fingerprint
      ),
      source.vertex.id = source_vertex_id,
      source.asset.fingerprint = asset_fingerprint
    )
  )
}

gflowui_precomputed_selected_eta_index <- function(path_summary) {
  if ("brier.selected" %in% names(path_summary)) {
    selected <- which(as.logical(path_summary$brier.selected))
    if (length(selected) == 1L) {
      return(as.integer(path_summary$eta.index[[selected]]))
    }
  }
  score <- suppressWarnings(as.numeric(path_summary$mean.brier))
  if (!any(is.finite(score))) {
    stop("The precomputed path has no finite Brier score.", call. = FALSE)
  }
  as.integer(path_summary$eta.index[[which.min(score)]])
}

gflowui_precomputed_basin_overlay <- function(path, eta_index, top_k) {
  eta_index <- as.integer(eta_index)
  top_k <- as.integer(top_k)
  n_vertices <- nrow(path$probability.mass)
  field_rows <- path$field.index[
    suppressWarnings(as.integer(path$field.index$path.parameter.index)) == eta_index,
    ,
    drop = FALSE
  ]
  if (nrow(field_rows) != 1L || !"field.id" %in% names(field_rows)) {
    stop("The selected time does not identify exactly one basin field.", call. = FALSE)
  }
  field_id <- as.character(field_rows$field.id[[1L]])
  basins <- path$raw.basins[
    suppressWarnings(as.integer(path$raw.basins$path.parameter.index)) == eta_index,
    ,
    drop = FALSE
  ]
  assignments <- path$assignments[
    as.character(path$assignments$field.id) == field_id,
    ,
    drop = FALSE
  ]
  required_basin <- c("raw.basin.id", "basin.mass", "support.count", "peak.vertex.id")
  required_assignment <- c("point.id", "raw.basin.id")
  if (nrow(basins) < 1L ||
      !all(required_basin %in% names(basins)) ||
      nrow(assignments) != n_vertices ||
      !all(required_assignment %in% names(assignments))) {
    stop("The selected basin field is incomplete.", call. = FALSE)
  }
  point_id <- suppressWarnings(as.integer(assignments$point.id))
  if (anyNA(point_id) || anyDuplicated(point_id) ||
      !identical(sort(point_id), seq_len(n_vertices))) {
    stop("Basin assignments do not identify every graph vertex exactly once.", call. = FALSE)
  }
  basin_mass <- suppressWarnings(as.numeric(basins$basin.mass))
  if (any(!is.finite(basin_mass)) || any(basin_mass < 0)) {
    stop("The selected basin masses are invalid.", call. = FALSE)
  }
  basin_order <- order(
    -basin_mass,
    as.character(basins$raw.basin.id),
    method = "radix"
  )
  basins <- basins[basin_order, , drop = FALSE]
  basins$rank <- seq_len(nrow(basins))
  top_k <- max(1L, min(top_k, nrow(basins)))
  top <- basins[seq_len(top_k), , drop = FALSE]
  top$display.label <- sprintf(
    "Basin %02d (mass %.3f)",
    top$rank,
    suppressWarnings(as.numeric(top$basin.mass))
  )
  labels_by_id <- stats::setNames(
    as.character(top$display.label),
    as.character(top$raw.basin.id)
  )
  labels <- unname(labels_by_id[as.character(assignments$raw.basin.id)])
  labels[is.na(labels)] <- "Other basins"
  values <- rep(NA_character_, n_vertices)
  values[point_id] <- labels
  table <- data.frame(
    rank = as.integer(top$rank),
    basin = as.character(top$raw.basin.id),
    mass = suppressWarnings(as.numeric(top$basin.mass)),
    cumulative.mass = cumsum(suppressWarnings(as.numeric(top$basin.mass))),
    support = suppressWarnings(as.integer(top$support.count)),
    peak.vertex = suppressWarnings(as.integer(top$peak.vertex.id)),
    stringsAsFactors = FALSE
  )
  list(
    values = values,
    table = table,
    top_k = top_k,
    basin_count = nrow(basins),
    field_id = field_id
  )
}

gflowui_precomputed_graph_heat_density <- function(
    set,
    project_root,
    subject_id,
    eta_index = NULL,
    display_mode = "density",
    top_k = 6L) {
  asset <- gflowui_precomputed_density_path(
    set, project_root, "graph_heat_kernel"
  )
  path <- asset$path
  if (!identical(as.character(subject_id), as.character(path$subject.id))) {
    stop("Unknown subject.", call. = FALSE)
  }
  selected_eta_index <- gflowui_precomputed_selected_eta_index(asset$path_summary)
  eta_index <- suppressWarnings(as.integer(eta_index %||% selected_eta_index))
  row <- match(eta_index, suppressWarnings(as.integer(asset$path_summary$eta.index)))
  if (!is.finite(row)) {
    stop("The heat-time index is outside the available path.", call. = FALSE)
  }
  path_row <- asset$path_summary[row, , drop = FALSE]
  graph_k <- suppressWarnings(as.integer(
    path$settings$graph.k %||% path$selected$graph.k %||% 3L
  ))
  selected <- data.frame(
    subject.id = as.character(subject_id),
    graph.k = graph_k,
    eta.index = eta_index,
    eta = suppressWarnings(as.numeric(path_row$eta[[1L]])),
    mean.brier = suppressWarnings(as.numeric(path_row$mean.brier[[1L]])),
    brier.selected = eta_index == selected_eta_index,
    stringsAsFactors = FALSE
  )
  source_values <- as.numeric(asset$probability_mass[, row])
  values <- gflowui_normalize_density(source_values)
  alignment_contract <- asset$alignment_base
  alignment_contract$source.id <- sprintf(
    "%s#eta-index-%d",
    basename(asset$path_file),
    eta_index
  )
  alignment_contract$source.field.fingerprint <-
    gflowui_basin_field_fingerprint(source_values)
  alignment_contract$field.fingerprint <-
    gflowui_basin_field_fingerprint(values)
  result <- list(
    values = values,
    source_values = source_values,
    selected = selected,
    method = asset$method,
    source_file = asset$path_file,
    source_fingerprint = asset$source_asset_fingerprint,
    alignment_contract = alignment_contract,
    normalization = list(
      method = "clamp-negative-then-unit-mass",
      exact.identity = identical(source_values, values),
      maximum.absolute.difference = max(abs(source_values - values))
    ),
    color_type = "numeric",
    colorbar_title = "Probability mass",
    display_mode = "density",
    selected_eta_index = selected_eta_index,
    path_summary = asset$path_summary
  )
  if (identical(as.character(display_mode), "top_k_basins")) {
    overlay <- gflowui_precomputed_basin_overlay(path, eta_index, top_k)
    result$values <- overlay$values
    result$color_type <- "categorical"
    result$colorbar_title <- sprintf("Top %d basins", overlay$top_k)
    result$display_mode <- "top_k_basins"
    result$basin_table <- overlay$table
    result$basin_count <- overlay$basin_count
    result$top_k <- overlay$top_k
  }
  result
}

gflowui_selected_occupation_density <- function(
    set,
    project_root,
    subject_id,
    method_id,
    selector) {
  method <- gflowui_occupation_density_method(set, method_id)
  if (identical(as.character(method$source %||% ""), "precomputed_path")) {
    return(gflowui_precomputed_graph_heat_density(
      set = set,
      project_root = project_root,
      subject_id = subject_id,
      eta_index = NULL,
      display_mode = "density"
    ))
  }
  fit_path <- gflowui_occupation_density_path(method$selected_fit_file, project_root)
  fit <- readRDS(fit_path)
  subject_idx <- match(as.character(subject_id), as.character(fit$subject.id))
  selector_idx <- match(as.character(selector), dimnames(fit$rho)$selector)
  if (!is.finite(subject_idx) || !is.finite(selector_idx)) {
    stop("The selected subject or selector is absent from the fit asset.", call. = FALSE)
  }
  selected <- fit$selected
  row <- selected[
    as.character(selected$subject.id) == as.character(subject_id) &
      as.character(selected$selector) == as.character(selector),
    ,
    drop = FALSE
  ]
  list(
    values = gflowui_normalize_density(fit$rho[, subject_idx, selector_idx]),
    selected = row,
    method = method
  )
}

gflowui_parameterized_graph_heat_density <- function(
    set,
    project_root,
    subject_id,
    graph_k,
    eta_index) {
  if (!requireNamespace("geosmooth", quietly = TRUE)) {
    stop("The geosmooth package is required for parameterized graph-heat estimates.", call. = FALSE)
  }
  common <- readRDS(gflowui_occupation_density_path(set$common_file, project_root))
  subject_idx <- match(as.character(subject_id), as.character(common$subject.id))
  if (!is.finite(subject_idx)) {
    stop("Unknown subject.", call. = FALSE)
  }
  method <- set$methods[[match(
    "graph_heat_kernel",
    vapply(set$methods, function(x) as.character(x$id %||% ""), character(1))
  )]]
  template <- as.character(method$basis_file_template %||% "")
  basis_path <- sprintf(template, as.integer(graph_k))
  basis_cache <- readRDS(gflowui_occupation_density_path(basis_path, project_root))
  eta_index <- as.integer(eta_index)
  if (!is.finite(eta_index) || eta_index < 1L || eta_index > length(basis_cache$eta.grid)) {
    stop("The heat-time index is outside the available path.", call. = FALSE)
  }
  response <- common$Y[, subject_idx]
  response <- response / max(1, sum(response))
  raw <- geosmooth::apply.metric.graph.lowpass.path(
    basis_cache$basis,
    response,
    basis_cache$eta.grid[[eta_index]],
    unresolved.action = "error"
  )$fitted.values[, 1L]
  list(
    values = gflowui_normalize_density(raw),
    selected = data.frame(
      graph.k = as.integer(graph_k),
      eta.index = eta_index,
      eta = basis_cache$eta.grid[[eta_index]],
      stringsAsFactors = FALSE
    ),
    method = method
  )
}

gflowui_parameterized_chart_density <- function(
    set,
    project_root,
    subject_id,
    support_size,
    chart_dim,
    bandwidth_multiplier) {
  if (!requireNamespace("geosmooth", quietly = TRUE)) {
    stop("The geosmooth package is required for parameterized chart-kernel estimates.", call. = FALSE)
  }
  common <- readRDS(gflowui_occupation_density_path(set$common_file, project_root))
  subject_idx <- match(as.character(subject_id), as.character(common$subject.id))
  if (!is.finite(subject_idx)) {
    stop("Unknown subject.", call. = FALSE)
  }
  method <- set$methods[[match(
    "chart_kernel",
    vapply(set$methods, function(x) as.character(x$id %||% ""), character(1))
  )]]
  pca_path <- sprintf(as.character(method$pca_file_template), as.integer(support_size))
  pca <- readRDS(gflowui_occupation_density_path(pca_path, project_root))
  builder <- utils::getFromNamespace(".state.density.chart.kernel.fixed.candidate", "geosmooth")
  fitter <- utils::getFromNamespace(".state.density.chart.kernel.fixed.fitted.matrix", "geosmooth")
  fixed <- builder(list(
    support.size = as.integer(support_size),
    kernel = "tricube",
    bandwidth.multiplier = as.numeric(bandwidth_multiplier),
    coordinate.method = "local.pca",
    chart.dim = as.integer(chart_dim),
    quadrature.weights = rep(1, common$n.vertices),
    auto.chart.support.metric = "both",
    auto.chart.selection.metric = "operator"
  ), common$n.vertices)
  raw <- fitter(
    X = common$X,
    y.mat = common$Y[, subject_idx, drop = FALSE],
    fixed = fixed,
    local.pca.supports = pca
  )[, 1L]
  list(
    values = gflowui_normalize_density(raw),
    selected = data.frame(
      support.size = as.integer(support_size),
      chart.dim = as.integer(chart_dim),
      bandwidth.multiplier = as.numeric(bandwidth_multiplier),
      stringsAsFactors = FALSE
    ),
    method = method
  )
}

#' Evaluate a registered subject occupation-density estimate
#'
#' Loads a selected density estimate or evaluates one explicitly requested
#' point on a registered graph-heat or chart-kernel candidate path.
#'
#' @param manifest A registered gflowui project manifest.
#' @param set_id Occupation-density set identifier.
#' @param subject_id Subject identifier.
#' @param method_id Registered density method identifier.
#' @param mode Either `"selected"` or `"parameters"`.
#' @param selector Data-only selector used in selected mode.
#' @param parameters Named method parameters used in parameter mode.
#'
#' @return A list containing normalized vertex values, selected parameters,
#'   and the method specification.
#' @export
gflowui_evaluate_occupation_density <- function(
    manifest,
    set_id,
    subject_id,
    method_id,
    mode = c("selected", "parameters"),
    selector = "minimum_brier",
    parameters = list()) {
  mode <- match.arg(mode)
  set <- gflowui_occupation_density_set(manifest, set_id)
  if (is.null(set)) {
    stop("No occupation-density assets are registered.", call. = FALSE)
  }
  root <- normalizePath(manifest$project_root, mustWork = TRUE)
  method <- gflowui_occupation_density_method(set, method_id)
  if (identical(mode, "selected")) {
    return(gflowui_selected_occupation_density(
      set, root, subject_id, method_id, selector
    ))
  }
  if (identical(method_id, "graph_heat_kernel")) {
    if (identical(as.character(method$source %||% ""), "precomputed_path")) {
      return(gflowui_precomputed_graph_heat_density(
        set = set,
        project_root = root,
        subject_id = subject_id,
        eta_index = parameters$eta_index,
        display_mode = parameters$display_mode %||% "density",
        top_k = parameters$top_k %||% 6L
      ))
    }
    return(gflowui_parameterized_graph_heat_density(
      set,
      root,
      subject_id,
      parameters$graph_k,
      parameters$eta_index
    ))
  }
  if (identical(method_id, "chart_kernel")) {
    return(gflowui_parameterized_chart_density(
      set,
      root,
      subject_id,
      parameters$support_size,
      parameters$chart_dim,
      parameters$bandwidth_multiplier
    ))
  }
  stop("Parameterized evaluation is unavailable for this method.", call. = FALSE)
}
