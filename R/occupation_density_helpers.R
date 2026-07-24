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

gflowui_selected_occupation_density <- function(
    set,
    project_root,
    subject_id,
    method_id,
    selector) {
  methods <- set$methods %||% list()
  method_ids <- vapply(methods, function(x) as.character(x$id %||% ""), character(1))
  idx <- match(as.character(method_id), method_ids)
  if (!is.finite(idx)) {
    stop("Unknown occupation-density method.", call. = FALSE)
  }
  method <- methods[[idx]]
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
  if (identical(mode, "selected")) {
    return(gflowui_selected_occupation_density(
      set, root, subject_id, method_id, selector
    ))
  }
  if (identical(method_id, "graph_heat_kernel")) {
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
