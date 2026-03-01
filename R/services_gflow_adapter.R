# Adapter layer between UI modules and gflow calls.
#
# This layer is intentionally small so UI logic remains stable while
# underlying computational implementations evolve.

require_gflow <- function() {
  if (!requireNamespace("gflow", quietly = TRUE)) {
    stop("Package 'gflow' is required. Install it before running gflowui.", call. = FALSE)
  }
}

gflow_build_graph <- function(X, kmin, kmax, method = "edit", labels = NULL, verbose = FALSE, ...) {
  require_gflow()
  f_build <- .get_gflow_function("build.iknn.graphs.and.selectk")

  method <- match.arg(method, choices = c("both", "edit", "mixing", "none"))
  X <- .as_numeric_matrix(X)

  if (nrow(X) < 3L) {
    stop("X must have at least 3 rows.", call. = FALSE)
  }
  if (ncol(X) < 1L) {
    stop("X must have at least 1 feature column.", call. = FALSE)
  }

  kmin <- as.integer(kmin)
  kmax <- as.integer(kmax)
  if (!is.finite(kmin) || !is.finite(kmax) || kmin < 1L || kmax < kmin) {
    stop("Invalid k range: require 1 <= kmin <= kmax.", call. = FALSE)
  }
  if (kmax >= nrow(X)) {
    stop(sprintf("kmax (%d) must be < number of rows in X (%d).", kmax, nrow(X)), call. = FALSE)
  }

  labels_use <- NULL
  if (!is.null(labels)) {
    labels_use <- as.vector(labels)
    if (length(labels_use) != nrow(X)) {
      stop("labels length must match nrow(X).", call. = FALSE)
    }
  }
  if (method %in% c("mixing", "both") && is.null(labels_use)) {
    stop("Method 'mixing'/'both' requires labels.", call. = FALSE)
  }

  build_call <- list(
    X = X,
    kmin = kmin,
    kmax = kmax,
    method = method,
    verbose = isTRUE(verbose)
  )
  if (!is.null(labels_use)) {
    build_call$labels <- labels_use
  }
  extra <- list(...)
  extra$X <- NULL
  extra$kmin <- NULL
  extra$kmax <- NULL
  extra$method <- NULL
  extra$labels <- NULL
  extra$verbose <- NULL
  build_call <- c(build_call, extra)

  res <- do.call(f_build, build_call)

  selected <- .select_k_from_build_result(res, method = method)
  selected_idx <- selected$index
  selected_k <- selected$k

  g_list <- res$X.graphs$geom_pruned_graphs
  if (!is.list(g_list) || length(g_list) < selected_idx) {
    stop("Could not retrieve selected graph from gflow result.", call. = FALSE)
  }
  selected_graph <- g_list[[selected_idx]]
  adj_list <- selected_graph$adj_list %||% selected_graph$adjacency.list
  weight_list <- selected_graph$weight_list %||% selected_graph$edge.length.list

  conn_row <- res$connectivity[res$connectivity$k == selected_k, , drop = FALSE]
  if (nrow(conn_row) == 0L) {
    conn_txt <- "connectivity unavailable"
  } else {
    conn_txt <- sprintf(
      "%d component(s), LCC %.1f%%",
      as.integer(conn_row$n.components[1]),
      100 * as.numeric(conn_row$lcc.frac[1])
    )
  }

  list(
    build.result = res,
    selected.k = selected_k,
    selected.k.source = selected$source,
    selected.k.index = selected_idx,
    selected.graph = selected_graph,
    adj.list = adj_list,
    weight.list = weight_list,
    connectivity = conn_txt,
    method = method,
    n.vertices = nrow(X)
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

.as_numeric_matrix <- function(X) {
  if (!is.matrix(X)) {
    X <- tryCatch(as.matrix(X), error = function(e) NULL)
  }
  if (is.null(X)) {
    stop("X must be matrix-like.", call. = FALSE)
  }
  if (is.character(X)) {
    stop("X contains non-numeric columns; provide numeric feature columns only.", call. = FALSE)
  }
  storage.mode(X) <- "double"
  if (any(!is.finite(X))) {
    stop("X contains NA/Inf values after numeric conversion.", call. = FALSE)
  }
  X
}

.select_k_from_build_result <- function(res, method = "edit") {
  k_values <- as.integer(res$k.values)

  pick_if_valid <- function(k, label) {
    if (length(k) == 1L && is.finite(k) && k %in% k_values) {
      return(list(k = as.integer(k), source = label))
    }
    NULL
  }

  candidates <- switch(
    method,
    edit = list(
      pick_if_valid(res$k.opt.edit, "k.opt.edit"),
      pick_if_valid(res$k.cc.edit, "k.cc.edit")
    ),
    mixing = list(
      pick_if_valid(res$k.opt.mixing, "k.opt.mixing"),
      pick_if_valid(res$k.cc.mixing, "k.cc.mixing")
    ),
    both = list(
      pick_if_valid(res$k.opt.edit, "k.opt.edit"),
      pick_if_valid(res$k.opt.mixing, "k.opt.mixing"),
      pick_if_valid(res$k.cc.edit, "k.cc.edit"),
      pick_if_valid(res$k.cc.mixing, "k.cc.mixing")
    ),
    none = list()
  )

  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) > 0L) {
    out <- candidates[[1L]]
    out$index <- match(out$k, k_values)
    return(out)
  }

  conn <- res$connectivity
  if (is.data.frame(conn) && "n.components" %in% names(conn)) {
    idx_conn <- which(conn$n.components == 1L)
    if (length(idx_conn) > 0L) {
      k_conn <- as.integer(conn$k[min(idx_conn)])
      return(list(
        k = k_conn,
        source = "first.connected.k",
        index = match(k_conn, k_values)
      ))
    }
    if ("lcc.size" %in% names(conn)) {
      idx_lcc <- which(conn$lcc.size == max(conn$lcc.size, na.rm = TRUE))
      if (length(idx_lcc) > 0L) {
        k_lcc <- as.integer(conn$k[min(idx_lcc)])
        return(list(
          k = k_lcc,
          source = "max.lcc.k",
          index = match(k_lcc, k_values)
        ))
      }
    }
  }

  list(k = k_values[[1L]], source = "first.k", index = 1L)
}

.get_gflow_function <- function(name) {
  ns <- asNamespace("gflow")
  if (exists(name, envir = ns, inherits = FALSE)) {
    return(get(name, envir = ns, inherits = FALSE))
  }
  stop(
    sprintf(
      "gflow function '%s' not found. Install/update gflow to a version that provides it.",
      name
    ),
    call. = FALSE
  )
}
