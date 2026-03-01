# Adapter layer between UI modules and gflow calls.
#
# This layer is intentionally small so UI logic remains stable while
# underlying computational implementations evolve.

require_gflow <- function() {
  if (!requireNamespace("gflow", quietly = TRUE)) {
    stop("Package 'gflow' is required. Install it before running gflowui.", call. = FALSE)
  }
}

gflow_build_graph_stub <- function(X, kmin, kmax, method = "both") {
  require_gflow()

  n <- nrow(X)
  selected.k <- as.integer(round((kmin + kmax) / 2))

  # TODO: replace with real call to gflow::build.iknn.graphs.and.selectk().
  list(
    selected.k = selected.k,
    connectivity = if (n > selected.k) "likely connected" else "possibly disconnected",
    method = method,
    n.vertices = n
  )
}

gflow_fit_condexp_stub <- function(graph_obj, y) {
  require_gflow()

  # TODO: replace with fit/refit rdgraph pipeline over selected graph.
  mu <- stats::median(y, na.rm = TRUE)
  list(
    fitted.values = rep(mu, length(y)),
    graph = graph_obj
  )
}

gflow_detect_endpoints_stub <- function(graph_obj) {
  require_gflow()

  # TODO: replace with gflow::geodesic.core.endpoints() when graph object
  # contains adjacency and edge-length structures.
  n <- graph_obj$n.vertices %||% 0
  endpoints <- if (n >= 2) c(1L, n) else integer(0)
  list(endpoints = endpoints)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
