`%||%` <- function(x, y) {
  if (
    is.null(x) ||
    length(x) < 1L ||
    (is.character(x) && length(x) > 0L && !nzchar(x[[1]]))
  ) {
    y
  } else {
    x
  }
}

.gflowui_now <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

empty_working_arm_rows <- function() {
  data.frame(
    arm_id = character(0),
    family_id = character(0),
    label = character(0),
    family_label = character(0),
    endpoint_a = integer(0),
    endpoint_b = integer(0),
    endpoint_a_key = character(0),
    endpoint_b_key = character(0),
    endpoint_a_label = character(0),
    endpoint_b_label = character(0),
    endpoint_a_virtual = logical(0),
    endpoint_b_virtual = logical(0),
    path_method = character(0),
    thickening_method = character(0),
    path_vertices_json = character(0),
    arm_vertices_json = character(0),
    arm_coords_json = character(0),
    parameter_summary = character(0),
    params_json = character(0),
    source_k = integer(0),
    accepted = logical(0),
    visible = logical(0),
    source_type = character(0),
    source_dataset_id = character(0),
    manually_added = logical(0),
    updated_at = character(0),
    stringsAsFactors = FALSE
  )
}

empty_working_arm_state <- function(ctx = NULL) {
  list(
    version = "1",
    project_id = as.character(ctx$project_id %||% ""),
    graph_set_id = as.character(ctx$graph_set_id %||% ""),
    k = suppressWarnings(as.integer(ctx$k %||% NA_integer_)),
    base_dataset_id = NA_character_,
    base_dataset_label = NA_character_,
    base_source_k = suppressWarnings(as.integer(NA_integer_)),
    is_modified = FALSE,
    last_snapshot_id = NA_character_,
    last_snapshot_label = NA_character_,
    last_session_id = NA_character_,
    rows = empty_working_arm_rows(),
    updated_at = .gflowui_now()
  )
}

sanitize_arm_json <- function(x) {
  txt <- as.character(x %||% "")
  txt <- txt[!is.na(txt)]
  if (length(txt) < 1L || !nzchar(txt[[1]])) {
    return("[]")
  }
  txt[[1]]
}

encode_arm_integer_json <- function(x) {
  vv <- suppressWarnings(as.integer(x %||% integer(0)))
  vv <- vv[is.finite(vv)]
  jsonlite::toJSON(as.integer(vv), auto_unbox = FALSE, null = "null")
}

decode_arm_integer_json <- function(x) {
  txt <- sanitize_arm_json(x)
  out <- tryCatch(jsonlite::fromJSON(txt), error = function(e) integer(0))
  vv <- suppressWarnings(as.integer(out %||% integer(0)))
  vv[is.finite(vv)]
}

encode_arm_numeric_json <- function(x) {
  vv <- suppressWarnings(as.numeric(x %||% numeric(0)))
  keep <- is.finite(vv)
  if (!is.null(names(vv))) {
    vv <- vv[keep]
    names(vv) <- names(x %||% numeric(0))[keep]
  } else {
    vv <- vv[keep]
  }
  jsonlite::toJSON(vv, auto_unbox = FALSE, null = "null")
}

decode_arm_numeric_json <- function(x) {
  txt <- sanitize_arm_json(x)
  out <- tryCatch(jsonlite::fromJSON(txt), error = function(e) numeric(0))
  vv <- suppressWarnings(as.numeric(out %||% numeric(0)))
  vv[is.finite(vv)]
}

sanitize_working_arm_state <- function(x, ctx = NULL) {
  out <- if (is.list(x)) x else empty_working_arm_state(ctx = ctx)
  rows <- if (is.data.frame(out$rows)) out$rows else empty_working_arm_rows()
  template <- empty_working_arm_rows()
  missing_cols <- setdiff(names(template), names(rows))
  if (length(missing_cols) > 0L) {
    for (cc in missing_cols) {
      rows[[cc]] <- template[[cc]]
    }
  }
  rows <- rows[, names(template), drop = FALSE]
  rows$arm_id <- as.character(rows$arm_id)
  rows$family_id <- as.character(rows$family_id)
  rows$label <- as.character(rows$label)
  rows$family_label <- as.character(rows$family_label)
  rows$endpoint_a <- suppressWarnings(as.integer(rows$endpoint_a))
  rows$endpoint_b <- suppressWarnings(as.integer(rows$endpoint_b))
  rows$endpoint_a_key <- as.character(rows$endpoint_a_key)
  rows$endpoint_b_key <- as.character(rows$endpoint_b_key)
  rows$endpoint_a_label <- as.character(rows$endpoint_a_label)
  rows$endpoint_b_label <- as.character(rows$endpoint_b_label)
  rows$endpoint_a_virtual <- as.logical(rows$endpoint_a_virtual)
  rows$endpoint_b_virtual <- as.logical(rows$endpoint_b_virtual)
  rows$path_method <- as.character(rows$path_method)
  rows$thickening_method <- as.character(rows$thickening_method)
  rows$path_vertices_json <- vapply(rows$path_vertices_json, sanitize_arm_json, character(1))
  rows$arm_vertices_json <- vapply(rows$arm_vertices_json, sanitize_arm_json, character(1))
  rows$arm_coords_json <- vapply(rows$arm_coords_json, sanitize_arm_json, character(1))
  rows$parameter_summary <- as.character(rows$parameter_summary)
  rows$params_json <- vapply(rows$params_json, sanitize_arm_json, character(1))
  rows$source_k <- suppressWarnings(as.integer(rows$source_k))
  rows$accepted <- as.logical(rows$accepted)
  rows$visible <- as.logical(rows$visible)
  rows$source_type <- as.character(rows$source_type)
  rows$source_dataset_id <- as.character(rows$source_dataset_id)
  rows$manually_added <- as.logical(rows$manually_added)
  rows$updated_at <- as.character(rows$updated_at)
  keep <- nzchar(rows$arm_id)
  keep[is.na(keep)] <- FALSE
  rows <- rows[keep, , drop = FALSE]
  if (nrow(rows) > 0L) {
    rows <- rows[!duplicated(rows$arm_id), , drop = FALSE]
    missing_family <- !nzchar(rows$family_label)
    rows$family_label[missing_family] <- paste(
      ifelse(nzchar(rows$endpoint_a_label[missing_family]), rows$endpoint_a_label[missing_family], rows$endpoint_a_key[missing_family]),
      ifelse(nzchar(rows$endpoint_b_label[missing_family]), rows$endpoint_b_label[missing_family], rows$endpoint_b_key[missing_family]),
      sep = " - "
    )
    missing_label <- !nzchar(rows$label)
    rows$label[missing_label] <- rows$family_label[missing_label]
    missing_family_id <- !nzchar(rows$family_id)
    rows$family_id[missing_family_id] <- gsub("[^a-z0-9]+", "_", tolower(rows$family_label[missing_family_id]))
    rows$accepted[is.na(rows$accepted)] <- FALSE
    rows$visible[is.na(rows$visible)] <- FALSE
    rows$endpoint_a_virtual[is.na(rows$endpoint_a_virtual)] <- FALSE
    rows$endpoint_b_virtual[is.na(rows$endpoint_b_virtual)] <- FALSE
    rows$manually_added[is.na(rows$manually_added)] <- FALSE
  }
  out$project_id <- as.character(out$project_id %||% ctx$project_id %||% "")
  out$graph_set_id <- as.character(out$graph_set_id %||% ctx$graph_set_id %||% "")
  out$k <- suppressWarnings(as.integer(out$k %||% ctx$k %||% NA_integer_))
  out$base_dataset_id <- as.character(out$base_dataset_id %||% NA_character_)
  out$base_dataset_label <- as.character(out$base_dataset_label %||% NA_character_)
  out$base_source_k <- suppressWarnings(as.integer(out$base_source_k %||% NA_integer_))
  out$is_modified <- isTRUE(out$is_modified)
  out$last_snapshot_id <- as.character(out$last_snapshot_id %||% NA_character_)
  out$last_snapshot_label <- as.character(out$last_snapshot_label %||% NA_character_)
  out$last_session_id <- as.character(out$last_session_id %||% NA_character_)
  out$rows <- rows
  out$updated_at <- as.character(out$updated_at %||% .gflowui_now())
  out
}

working_arm_is_modified <- function(state) {
  isTRUE(state$is_modified)
}

working_arm_mark_clean <- function(state, base_dataset_id = NULL, base_dataset_label = NULL, base_source_k = NULL, session_id = "") {
  out <- sanitize_working_arm_state(state, ctx = NULL)
  if (!is.null(base_dataset_id)) {
    out$base_dataset_id <- as.character(base_dataset_id %||% NA_character_)
  }
  if (!is.null(base_dataset_label)) {
    out$base_dataset_label <- as.character(base_dataset_label %||% NA_character_)
  }
  if (!is.null(base_source_k)) {
    out$base_source_k <- suppressWarnings(as.integer(base_source_k %||% NA_integer_))
  }
  out$is_modified <- FALSE
  out$last_session_id <- as.character(session_id %||% "")
  out$updated_at <- .gflowui_now()
  sanitize_working_arm_state(out, ctx = NULL)
}

working_arm_mark_modified <- function(state, session_id = "") {
  out <- sanitize_working_arm_state(state, ctx = NULL)
  out$is_modified <- TRUE
  out$last_session_id <- as.character(session_id %||% "")
  out$updated_at <- .gflowui_now()
  sanitize_working_arm_state(out, ctx = NULL)
}

working_arm_is_recovered <- function(state, session_id = "") {
  st <- sanitize_working_arm_state(state, ctx = NULL)
  working_arm_is_modified(st) &&
    nzchar(as.character(st$last_session_id %||% "")) &&
    !identical(as.character(st$last_session_id %||% ""), as.character(session_id %||% ""))
}

empty_arm_dataset_meta <- function(ctx = NULL) {
  list(
    version = "1",
    project_id = as.character(ctx$project_id %||% ""),
    graph_set_id = as.character(ctx$graph_set_id %||% ""),
    default_dataset_id = NA_character_,
    updated_at = .gflowui_now()
  )
}

sanitize_arm_dataset_meta <- function(x, ctx = NULL) {
  out <- if (is.list(x)) x else empty_arm_dataset_meta(ctx = ctx)
  out$project_id <- as.character(out$project_id %||% ctx$project_id %||% "")
  out$graph_set_id <- as.character(out$graph_set_id %||% ctx$graph_set_id %||% "")
  out$default_dataset_id <- as.character(out$default_dataset_id %||% NA_character_)
  out$updated_at <- as.character(out$updated_at %||% .gflowui_now())
  out
}

sanitize_arm_token_id <- function(x, fallback = "arm") {
  id <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(x %||% "")))
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) {
    id <- fallback
  }
  id
}

arm_family_label <- function(endpoint_a_label, endpoint_b_label) {
  a <- as.character(endpoint_a_label %||% "A")
  b <- as.character(endpoint_b_label %||% "B")
  sprintf("%s - %s", a, b)
}

arm_variant_label <- function(family_label, thickening_method) {
  fam <- as.character(family_label %||% "")
  meth <- as.character(thickening_method %||% "")
  if (!nzchar(fam)) {
    fam <- "Arm"
  }
  if (!nzchar(meth) || identical(meth, "path_only")) {
    return(fam)
  }
  sprintf("%s | %s", fam, meth)
}

build_weighted_igraph_from_lists <- function(adj.list, weight.list = NULL) {
  stopifnot(is.list(adj.list))
  n <- length(adj.list)
  if (!is.list(weight.list) || length(weight.list) != n) {
    weight.list <- lapply(adj.list, function(nb) rep(1, length(nb %||% integer(0))))
  }
  edges <- vector("list", n)
  weights <- vector("list", n)
  idx <- 1L
  for (ii in seq_len(n)) {
    nb <- suppressWarnings(as.integer(adj.list[[ii]] %||% integer(0)))
    ww <- suppressWarnings(as.numeric(weight.list[[ii]] %||% numeric(0)))
    if (length(nb) != length(ww)) {
      ww <- rep(1, length(nb))
    }
    keep <- is.finite(nb) & nb >= 1L & nb <= n & nb > ii
    if (!any(keep)) {
      next
    }
    edges[[idx]] <- cbind(ii, nb[keep])
    weights[[idx]] <- ww[keep]
    idx <- idx + 1L
  }
  edges <- edges[seq_len(max(0L, idx - 1L))]
  weights <- weights[seq_len(max(0L, idx - 1L))]
  if (length(edges) < 1L) {
    g <- igraph::make_empty_graph(n = n, directed = FALSE)
    return(list(graph = g, weights = numeric(0)))
  }
  edge_mat <- do.call(rbind, edges)
  edge_w <- unlist(weights, use.names = FALSE)
  edge_df <- data.frame(
    from = as.character(edge_mat[, 1]),
    to = as.character(edge_mat[, 2]),
    weight = as.numeric(edge_w),
    stringsAsFactors = FALSE
  )
  vertex_df <- data.frame(
    name = as.character(seq_len(n)),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(edge_df[, c("from", "to"), drop = FALSE], directed = FALSE, vertices = vertex_df)
  igraph::E(g)$weight <- as.numeric(edge_w)
  list(graph = g, weights = as.numeric(edge_w))
}

closest_vertex_to_centroid <- function(coords) {
  if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
    return(NA_integer_)
  }
  xyz <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
  if (!is.matrix(xyz) || nrow(xyz) < 1L) {
    return(NA_integer_)
  }
  centroid <- colMeans(xyz[, seq_len(3L), drop = FALSE], na.rm = TRUE)
  d2 <- rowSums((xyz[, seq_len(3L), drop = FALSE] - matrix(centroid, nrow(xyz), 3L, byrow = TRUE))^2)
  d2[!is.finite(d2)] <- Inf
  idx <- which.min(d2)
  if (!is.finite(idx) || length(idx) < 1L) {
    return(NA_integer_)
  }
  as.integer(idx[[1]])
}

decode_arm_params_json <- function(x) {
  txt <- sanitize_arm_json(x)
  out <- tryCatch(jsonlite::fromJSON(txt), error = function(e) list())
  if (is.list(out)) out else list()
}

encode_arm_params_json <- function(x) {
  jsonlite::toJSON(x %||% list(), auto_unbox = TRUE, null = "null")
}

compute_arm_variant <- function(adj.list,
                                weight.list,
                                coords,
                                endpoint_a,
                                endpoint_b,
                                endpoint_a_key,
                                endpoint_b_key,
                                endpoint_a_label,
                                endpoint_b_label,
                                endpoint_a_virtual = FALSE,
                                endpoint_b_virtual = FALSE,
                                thickening_method = c("path_only", "corridor", "tube_hop", "tube_geodesic", "harmonic_hop", "harmonic_geodesic"),
                                corridor_rel_tol = 0.05,
                                corridor_abs_tol = 0,
                                tube_radius = 2) {
  thickening_method <- match.arg(thickening_method)
  uu <- suppressWarnings(as.integer(endpoint_a))
  vv <- suppressWarnings(as.integer(endpoint_b))
  if (!is.list(adj.list) || length(adj.list) < 2L) {
    stop("Graph adjacency list is not available.")
  }
  if (!is.finite(uu) || !is.finite(vv) || uu < 1L || vv < 1L || uu > length(adj.list) || vv > length(adj.list)) {
    stop("Endpoints are not valid vertices in the current graph.")
  }
  if (identical(as.integer(uu), as.integer(vv))) {
    stop("Arm endpoints must be different vertices.")
  }
  graph_info <- build_weighted_igraph_from_lists(adj.list, weight.list)
  g <- graph_info$graph
  ww <- graph_info$weights
  sp <- igraph::shortest_paths(
    g,
    from = as.integer(uu),
    to = as.integer(vv),
    weights = ww,
    output = "vpath"
  )
  path_vertices <- suppressWarnings(as.integer(sp$vpath[[1]]))
  path_vertices <- path_vertices[is.finite(path_vertices) & path_vertices >= 1L]
  if (length(path_vertices) < 2L) {
    stop("Could not find a connected shortest path between the selected endpoints.")
  }

  arm_vertices <- path_vertices
  extended_coords <- numeric(0)
  params <- list()
  parameter_summary <- "weighted shortest path"
  select_path_corridor <- getFromNamespace("select.path.corridor", "gflow")
  select_path_neighborhood <- getFromNamespace("select.path.neighborhood", "gflow")
  tube_radius_num <- suppressWarnings(as.numeric(tube_radius))
  if (!is.finite(tube_radius_num) || tube_radius_num <= 0) {
    tube_radius_num <- 2
  }
  tube_radius_hop <- max(1L, suppressWarnings(as.integer(floor(tube_radius_num))))

  if (identical(thickening_method, "corridor")) {
    arm_vertices <- select_path_corridor(
      graph = g,
      path = path_vertices,
      weights = ww,
      rel.tol = as.numeric(corridor_rel_tol),
      abs.tol = as.numeric(corridor_abs_tol),
      include.path = TRUE
    )
    params <- list(rel.tol = as.numeric(corridor_rel_tol), abs.tol = as.numeric(corridor_abs_tol))
    parameter_summary <- sprintf("corridor rel.tol=%.3f abs.tol=%.3f", as.numeric(corridor_rel_tol), as.numeric(corridor_abs_tol))
  } else if (identical(thickening_method, "tube_hop")) {
    arm_vertices <- select_path_neighborhood(
      graph = g,
      path = path_vertices,
      weights = NA,
      max.dist = tube_radius_hop
    )
    params <- list(radius = tube_radius_hop, distance = "hop")
    parameter_summary <- sprintf("tube radius=%d (hop)", params$radius)
  } else if (identical(thickening_method, "tube_geodesic")) {
    arm_vertices <- select_path_neighborhood(
      graph = g,
      path = path_vertices,
      weights = ww,
      max.dist = tube_radius_num
    )
    params <- list(radius = tube_radius_num, distance = "geodesic")
    parameter_summary <- sprintf("tube radius=%.3f (geodesic)", tube_radius_num)
  } else if (identical(thickening_method, "harmonic_hop") || identical(thickening_method, "harmonic_geodesic")) {
    tube_type <- if (identical(thickening_method, "harmonic_hop")) "hop" else "geodesic"
    tube_radius_use <- if (identical(tube_type, "hop")) tube_radius_hop else tube_radius_num
    he <- gflow::compute.harmonic.extension(
      adj.list = adj.list,
      weight.list = weight.list,
      trajectory = path_vertices,
      tube.radius = tube_radius_use,
      tube.type = tube_type,
      use.edge.weights = TRUE,
      verbose = FALSE
    )
    arm_vertices <- suppressWarnings(as.integer(he$tubular.vertices %||% path_vertices))
    ext_vals <- suppressWarnings(as.numeric(he$extended.coords %||% numeric(0)))
    if (length(ext_vals) == length(arm_vertices)) {
      extended_coords <- ext_vals
      names(extended_coords) <- as.character(arm_vertices)
    }
    params <- list(radius = tube_radius_use, distance = tube_type)
    if (identical(tube_type, "hop")) {
      parameter_summary <- sprintf("harmonic extension radius=%d (%s)", as.integer(tube_radius_use), tube_type)
    } else {
      parameter_summary <- sprintf("harmonic extension radius=%.3f (%s)", tube_radius_use, tube_type)
    }
  }

  arm_vertices <- sort(unique(suppressWarnings(as.integer(arm_vertices))))
  arm_vertices <- arm_vertices[is.finite(arm_vertices) & arm_vertices >= 1L & arm_vertices <= length(adj.list)]
  family_label <- arm_family_label(endpoint_a_label, endpoint_b_label)
  family_id <- sanitize_arm_token_id(
    sprintf("%s__%s", endpoint_a_key, endpoint_b_key),
    fallback = "arm_family"
  )
  label <- arm_variant_label(family_label, thickening_method)
  param_token <- if (identical(thickening_method, "corridor")) {
    sprintf("rt%s_at%s",
      gsub("\\.", "p", sprintf("%.3f", as.numeric(corridor_rel_tol))),
      gsub("\\.", "p", sprintf("%.3f", as.numeric(corridor_abs_tol)))
    )
  } else if (thickening_method %in% c("tube_hop", "tube_geodesic", "harmonic_hop", "harmonic_geodesic")) {
    radius_token <- if (thickening_method %in% c("tube_hop", "harmonic_hop")) {
      sprintf("%d", tube_radius_hop)
    } else {
      sprintf("%.3f", tube_radius_num)
    }
    sprintf("r%s", gsub("\\.", "p", radius_token))
  } else {
    "base"
  }
  arm_id <- sanitize_arm_token_id(
    sprintf("%s__%s__%s", family_id, thickening_method, param_token),
    fallback = "arm"
  )

  list(
    arm_id = arm_id,
    family_id = family_id,
    label = label,
    family_label = family_label,
    endpoint_a = as.integer(uu),
    endpoint_b = as.integer(vv),
    endpoint_a_key = as.character(endpoint_a_key %||% sprintf("v%d", as.integer(uu))),
    endpoint_b_key = as.character(endpoint_b_key %||% sprintf("v%d", as.integer(vv))),
    endpoint_a_label = as.character(endpoint_a_label %||% sprintf("v%d", as.integer(uu))),
    endpoint_b_label = as.character(endpoint_b_label %||% sprintf("v%d", as.integer(vv))),
    endpoint_a_virtual = isTRUE(endpoint_a_virtual),
    endpoint_b_virtual = isTRUE(endpoint_b_virtual),
    path_method = "weighted_shortest_path",
    thickening_method = as.character(thickening_method),
    path_vertices = as.integer(path_vertices),
    arm_vertices = as.integer(arm_vertices),
    arm_coords = extended_coords,
    parameter_summary = parameter_summary,
    params = params,
    source_k = NA_integer_
  )
}

working_arm_rows_from_variant <- function(variant, source_type = "manual", source_dataset_id = "", source_k = NA_integer_) {
  if (!is.list(variant)) {
    return(empty_working_arm_rows())
  }
  data.frame(
    arm_id = as.character(variant$arm_id %||% ""),
    family_id = as.character(variant$family_id %||% ""),
    label = as.character(variant$label %||% variant$family_label %||% ""),
    family_label = as.character(variant$family_label %||% ""),
    endpoint_a = as.integer(variant$endpoint_a %||% NA_integer_),
    endpoint_b = as.integer(variant$endpoint_b %||% NA_integer_),
    endpoint_a_key = as.character(variant$endpoint_a_key %||% ""),
    endpoint_b_key = as.character(variant$endpoint_b_key %||% ""),
    endpoint_a_label = as.character(variant$endpoint_a_label %||% ""),
    endpoint_b_label = as.character(variant$endpoint_b_label %||% ""),
    endpoint_a_virtual = isTRUE(variant$endpoint_a_virtual),
    endpoint_b_virtual = isTRUE(variant$endpoint_b_virtual),
    path_method = as.character(variant$path_method %||% "weighted_shortest_path"),
    thickening_method = as.character(variant$thickening_method %||% "path_only"),
    path_vertices_json = encode_arm_integer_json(variant$path_vertices %||% integer(0)),
    arm_vertices_json = encode_arm_integer_json(variant$arm_vertices %||% integer(0)),
    arm_coords_json = encode_arm_numeric_json(variant$arm_coords %||% numeric(0)),
    parameter_summary = as.character(variant$parameter_summary %||% ""),
    params_json = encode_arm_params_json(variant$params %||% list()),
    source_k = as.integer(source_k %||% variant$source_k %||% NA_integer_),
    accepted = TRUE,
    visible = TRUE,
    source_type = as.character(source_type %||% "manual"),
    source_dataset_id = as.character(source_dataset_id %||% ""),
    manually_added = TRUE,
    updated_at = .gflowui_now(),
    stringsAsFactors = FALSE
  )
}

accepted_visible_working_arm_rows <- function(working_state) {
  rows_df <- if (is.list(working_state) && is.data.frame(working_state$rows)) working_state$rows else empty_working_arm_rows()
  keep <- rows_df$accepted & rows_df$visible
  keep[is.na(keep)] <- FALSE
  rows_df[keep, , drop = FALSE]
}

accepted_hidden_working_arm_rows <- function(working_state) {
  rows_df <- if (is.list(working_state) && is.data.frame(working_state$rows)) working_state$rows else empty_working_arm_rows()
  keep <- rows_df$accepted & !rows_df$visible
  keep[is.na(keep)] <- FALSE
  rows_df[keep, , drop = FALSE]
}
