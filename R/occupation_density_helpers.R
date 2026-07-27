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
  list(
    method = method,
    path = path,
    path_file = path_file,
    probability_mass = probability_mass,
    path_summary = path_summary
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
  result <- list(
    values = gflowui_normalize_density(asset$probability_mass[, row]),
    selected = selected,
    method = asset$method,
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
  builder <- getFromNamespace(".state.density.chart.kernel.fixed.candidate", "geosmooth")
  fitter <- getFromNamespace(".state.density.chart.kernel.fixed.fitted.matrix", "geosmooth")
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
