.gflowui_basin_cache <- new.env(parent = emptyenv())

gflowui_basin_hash <- function(x) {
  path <- tempfile("gflowui-basin-hash-")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeBin(serialize(x, NULL, version = 3L), con)
  close(con)
  unname(tools::md5sum(path))
}

gflowui_basin_mass_provenance <- function(
    mass_kind,
    source_id,
    source_fingerprint,
    authority,
    validator = "gflowui basin source contract",
    validator_version = "1",
    algorithm = "exact full-domain ordered vertex-ID and content comparison",
    evidence_fingerprint) {
  list(
    mass.kind = as.character(mass_kind),
    source.id = as.character(source_id),
    source.fingerprint = as.character(source_fingerprint),
    attestations = list(list(
      claim = "scientific mass meaning and external source/graph/vertex alignment",
      authority = as.character(authority),
      validator = as.character(validator),
      validator.version = as.character(validator_version),
      algorithm = as.character(algorithm),
      evidence.fingerprint = as.character(evidence_fingerprint),
      status = "validated"
    ))
  )
}

gflowui_basin_default_colors <- function(table) {
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(structure(character(), names = character()))
  }
  keys <- paste(table$type, table$basin.id, sep = "|")
  out <- character(length(keys))
  for (direction in c("max", "min")) {
    index <- which(table$type == direction)
    if (length(index) < 1L) {
      next
    }
    palette <- if (direction == "max") "YlOrRd" else "Blues 3"
    out[index] <- grDevices::hcl.colors(
      max(3L, length(index)),
      palette
    )[seq_along(index)]
  }
  stats::setNames(out, keys)
}

gflowui_basin_display_values <- function(
    basin,
    table,
    selected_keys = NULL,
    direction = "max") {
  n <- suppressWarnings(as.integer(basin$n.vertices %||% 0L))
  values <- rep.int("Other basins", n)
  if (!is.data.frame(table) || nrow(table) < 1L ||
      !is.data.frame(basin$assignment)) {
    return(values)
  }
  direction <- match.arg(as.character(direction), c("max", "min"))
  rows <- table[table$type == direction, , drop = FALSE]
  if (nrow(rows) < 1L) {
    return(values)
  }
  keys <- paste(rows$type, rows$basin.id, sep = "|")
  if (!is.null(selected_keys)) {
    keep <- keys %in% as.character(selected_keys)
    rows <- rows[keep, , drop = FALSE]
    keys <- keys[keep]
  }
  if (nrow(rows) < 1L) {
    return(values)
  }
  labels <- as.character(rows$display.label)
  names(labels) <- as.character(rows$basin.id)
  assignment <- basin$assignment
  assignment <- assignment[
    assignment$direction == direction &
      assignment$assignment.status == "assigned",
    ,
    drop = FALSE
  ]
  vertex <- suppressWarnings(as.integer(assignment$vertex))
  valid <- is.finite(vertex) & vertex >= 1L & vertex <= n
  labels.use <- unname(labels[as.character(assignment$basin.id)])
  valid <- valid & !is.na(labels.use)
  values[vertex[valid]] <- labels.use[valid]
  values
}

gflowui_update_basin_row_state <- function(
    selected_keys,
    color_map,
    valid_keys,
    key,
    role,
    checked = NULL,
  value = NULL) {
  selected_keys <- unique(as.character(selected_keys))
  color_names <- names(color_map)
  color_map <- as.character(color_map)
  names(color_map) <- color_names
  valid_keys <- unique(as.character(valid_keys))
  key <- as.character(key %||% "")
  role <- as.character(role %||% "")
  if (!nzchar(key) || !(key %in% valid_keys)) {
    return(list(
      selected_keys = selected_keys,
      color_map = color_map,
      changed = FALSE
    ))
  }
  if (identical(role, "selection")) {
    next_selected <- if (isTRUE(checked)) {
      unique(c(selected_keys, key))
    } else {
      setdiff(selected_keys, key)
    }
    return(list(
      selected_keys = next_selected,
      color_map = color_map,
      changed = !identical(sort(next_selected), sort(selected_keys))
    ))
  }
  if (identical(role, "color")) {
    color <- as.character(value %||% "")
    if (!nzchar(color)) {
      return(list(
        selected_keys = selected_keys,
        color_map = color_map,
        changed = FALSE
      ))
    }
    previous <- unname(color_map[key])
    if (length(previous) < 1L || is.na(previous)) {
      previous <- ""
    }
    color_map[[key]] <- color
    return(list(
      selected_keys = selected_keys,
      color_map = color_map,
      changed = !identical(previous, color)
    ))
  }
  list(
    selected_keys = selected_keys,
    color_map = color_map,
    changed = FALSE
  )
}

gflowui_basin_cache_key <- function(
    adj_list,
    edge_length_list,
    field,
    vertex_mass,
    vertex_id,
    source_key,
    source_fingerprint,
    build_identity) {
  gflowui_basin_hash(list(
    graph = list(
      adjacency = lapply(adj_list, as.integer),
      edge.lengths = lapply(edge_length_list, as.numeric),
      vertex.id = as.character(vertex_id)
    ),
    field = as.numeric(field),
    vertex.mass = if (is.null(vertex_mass)) NULL else as.numeric(vertex_mass),
    source.key = as.character(source_key),
    source.fingerprint = as.character(source_fingerprint),
    gflow.build.id = build_identity$build.id,
    gflow.runtime.id = build_identity$runtime$id,
    method = "trajectory_flow",
    direction = "both",
    method.params = list(
      modulation = "CLOSEST",
      plateau.policy = "connected_exact",
      edge.length.quantile.thld = 1,
      long.edge.fallback = "allow_and_flag",
      store.trajectories = FALSE,
      symmetric.seeding = FALSE,
      tie.breaking = FALSE,
      primary.assignment.policy = "backend_primary"
    )
  ))
}

gflowui_basin_table <- function(summary) {
  table <- summary$basin.table
  if (!is.data.frame(table) || nrow(table) < 1L) {
    table <- summary$maxima
    table <- table[FALSE, , drop = FALSE]
  }
  if (nrow(table) < 1L) {
    table$key <- character()
    table$display.label <- character()
    table$selected <- logical()
    table$color <- character()
    return(table)
  }
  table$key <- paste(table$type, table$basin.id, sep = "|")
  table$display.label <- sprintf(
    "%s Basin %02d",
    ifelse(table$type == "max", "Maximum", "Minimum"),
    as.integer(table$rank)
  )
  table$selected <- TRUE
  table$color <- unname(gflowui_basin_default_colors(table)[table$key])
  table
}

gflowui_estimate_basin_overlay <- function(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k = 6L,
    vertex_mass = NULL,
    top_k_max = NULL,
    top_k_min = NULL,
    rank_by = "auto",
    vertex_id = NULL,
    vertex_mass_provenance = NULL,
    source_key = "",
    source_fingerprint = NULL) {
  if (!is.list(adj_list) || length(adj_list) < 1L) {
    stop("The selected graph has no adjacency data.", call. = FALSE)
  }
  if (!is.list(edge_length_list) ||
      length(edge_length_list) != length(adj_list)) {
    stop("The selected graph has no aligned edge lengths.", call. = FALSE)
  }
  n_vertices <- length(adj_list)
  field <- suppressWarnings(as.numeric(field))
  if (length(field) != n_vertices) {
    stop(
      "The selected estimate is not aligned with the displayed graph.",
      call. = FALSE
    )
  }
  if (any(!is.finite(field))) {
    stop(
      "The selected estimate must provide one finite value for every graph vertex.",
      call. = FALSE
    )
  }
  for (index in seq_len(n_vertices)) {
    neighbors <- suppressWarnings(as.integer(adj_list[[index]]))
    weights <- suppressWarnings(as.numeric(edge_length_list[[index]]))
    if (length(neighbors) != length(weights) ||
        any(!is.finite(neighbors)) ||
        any(neighbors < 1L | neighbors > n_vertices) ||
        any(!is.finite(weights)) ||
        any(weights < 0)) {
      stop(
        "The selected graph has invalid or misaligned adjacency and edge lengths.",
        call. = FALSE
      )
    }
  }

  if (is.null(vertex_id)) {
    vertex_id <- as.character(seq_len(n_vertices))
  }
  if (is.factor(vertex_id) || length(vertex_id) != n_vertices ||
      anyNA(vertex_id) || any(!nzchar(as.character(vertex_id))) ||
      anyDuplicated(as.character(vertex_id))) {
    stop(
      "The selected source has invalid, duplicate, or misaligned vertex IDs.",
      call. = FALSE
    )
  }
  vertex_id <- enc2utf8(as.character(vertex_id))

  if (!is.null(vertex_mass)) {
    vertex_mass <- suppressWarnings(as.numeric(vertex_mass))
    if (length(vertex_mass) != n_vertices ||
        any(!is.finite(vertex_mass)) ||
        any(vertex_mass < 0) ||
        sum(vertex_mass) <= 0) {
      stop(
        "The selected estimate has invalid full-domain vertex mass.",
        call. = FALSE
      )
    }
  } else if (!is.null(vertex_mass_provenance)) {
    stop(
      "Mass provenance cannot be supplied without vertex mass.",
      call. = FALSE
    )
  }

  direction <- match.arg(as.character(direction), c("both", "max", "min"))
  top_k <- suppressWarnings(as.integer(top_k))
  if (!is.finite(top_k) || top_k < 0L) {
    top_k <- 6L
  }
  normalize_top_k <- function(value, fallback) {
    value <- suppressWarnings(as.integer(value %||% fallback))
    if (!is.finite(value) || value < 0L) fallback else value
  }
  top_k_max <- normalize_top_k(top_k_max, if (direction == "min") 0L else top_k)
  top_k_min <- normalize_top_k(top_k_min, if (direction == "max") 0L else top_k)
  rank_by <- as.character(rank_by %||% "auto")

  source_fingerprint <- as.character(
    source_fingerprint %||% gflowui_basin_hash(list(
      source.key = source_key,
      field = field,
      vertex.id = vertex_id
    ))
  )
  build_identity <- gflow::get.gflow.build.identity()
  cache_key <- gflowui_basin_cache_key(
    adj_list,
    edge_length_list,
    field,
    vertex_mass,
    vertex_id,
    source_key,
    source_fingerprint,
    build_identity
  )
  cache_hit <- exists(cache_key, envir = .gflowui_basin_cache, inherits = FALSE)
  basin <- if (cache_hit) {
    get(cache_key, envir = .gflowui_basin_cache, inherits = FALSE)
  } else {
    value <- gflow::create.basin.complex(
      adj.list = adj_list,
      edge.length.list = edge_length_list,
      field = field,
      method = "trajectory_flow",
      direction = "both",
      vertex.mass = vertex_mass,
      method.params = list(
        modulation = "CLOSEST",
        plateau.policy = "connected_exact",
        edge.length.quantile.thld = 1,
        long.edge.fallback = "allow_and_flag",
        store.trajectories = FALSE,
        symmetric.seeding = FALSE,
        tie.breaking = FALSE,
        primary.assignment.policy = "backend_primary"
      ),
      simplify.params = list(),
      verbose = FALSE,
      vertex.id = vertex_id,
      vertex.mass.provenance = vertex_mass_provenance
    )
    assign(cache_key, value, envir = .gflowui_basin_cache)
    value
  }
  if (!is.list(basin) || !identical(as.character(basin$status), "ok")) {
    detail <- as.character(
      basin$diagnostics$message %||%
        basin$diagnostics$error %||%
        "The basin backend did not return a usable result."
    )
    stop(detail, call. = FALSE)
  }

  summary <- summary(
    basin,
    rank.by = rank_by,
    top.k.max = top_k_max,
    top.k.min = top_k_min,
    include.vertex.lists = FALSE
  )
  table <- gflowui_basin_table(summary)
  display_direction <- if (direction == "min") "min" else "max"
  values <- gflowui_basin_display_values(
    basin,
    table,
    selected_keys = table$key,
    direction = display_direction
  )
  direction_rows <- basin$basin.table$type == display_direction
  resolved <- summary$rank.resolved[[display_direction]]
  ranking_labels <- c(
    primary.support.mass = "primary mass",
    raw.allocated.mass = "membership-allocated mass",
    retained.support.mass = "retained coverage mass",
    raw.support.mass = "raw coverage mass",
    primary.support.size = "primary support size",
    retained.support.size = "retained support size",
    raw.support.size = "raw support size"
  )
  ranking <- unname(ranking_labels[[resolved]] %||% resolved)

  legacy_table <- table[table$type == display_direction, , drop = FALSE]
  legacy_table <- data.frame(
    rank = as.integer(legacy_table$rank),
    basin = as.character(legacy_table$basin.id),
    type = as.character(legacy_table$type),
    mass = suppressWarnings(as.numeric(legacy_table$primary.support.mass)),
    support = suppressWarnings(as.integer(
      legacy_table$primary.support.size
    )),
    extremum.vertex = suppressWarnings(as.integer(
      legacy_table$extremum.vertex
    )),
    extremum.vertex.id = as.character(legacy_table$extremum.vertex.id),
    extremum.value = suppressWarnings(as.numeric(
      legacy_table$extremum.value
    )),
    key = as.character(legacy_table$key),
    color = as.character(legacy_table$color),
    selected = as.logical(legacy_table$selected),
    stringsAsFactors = FALSE
  )

  list(
    values = values,
    values_max = gflowui_basin_display_values(
      basin,
      table,
      table$key,
      "max"
    ),
    values_min = gflowui_basin_display_values(
      basin,
      table,
      table$key,
      "min"
    ),
    table = if (direction == "both") table else legacy_table,
    summary = summary,
    top_k = as.integer(if (display_direction == "max") top_k_max else top_k_min),
    top_k_max = as.integer(top_k_max),
    top_k_min = as.integer(top_k_min),
    basin_count = as.integer(if (direction == "both") {
      nrow(basin$basin.table)
    } else {
      sum(direction_rows)
    }),
    basin_count_max = as.integer(sum(basin$basin.table$type == "max")),
    basin_count_min = as.integer(sum(basin$basin.table$type == "min")),
    direction = direction,
    ranking = ranking,
    ranking_resolved = summary$rank.resolved,
    basin = basin,
    cache_key = cache_key,
    cache_hit = cache_hit,
    build_identity = build_identity,
    source_fingerprint = source_fingerprint
  )
}
