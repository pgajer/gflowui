.gflowui_basin_cache <- new.env(parent = emptyenv())
.gflowui_basin_prominence_cache <- new.env(parent = emptyenv())

gflowui_basin_hash <- function(x) {
  path <- tempfile("gflowui-basin-hash-")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeBin(serialize(x, NULL, version = 3L), con)
  close(con)
  unname(tools::md5sum(path))
}

gflowui_basin_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

gflowui_basin_file_sha256 <- function(path) {
  path <- as.character(path %||% "")
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    stop("The basin source asset is unavailable.", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE)
}

gflowui_basin_field_fingerprint <- function(values) {
  gflowui_basin_sha256(list(
    schema = "gflowui_basin_field_v1",
    values = as.numeric(values)
  ))
}

gflowui_basin_graph_identity <- function(
    adj_list,
    edge_length_list,
    vertex_id,
    graph_id,
    graph_k,
    source_vertex_id = NULL,
    declared_display_vertex_fingerprint = NULL) {
  graph_id <- as.character(graph_id %||% "")
  graph_k <- suppressWarnings(as.integer(graph_k %||% NA_integer_))
  vertex_id <- enc2utf8(as.character(vertex_id %||% character()))
  source_vertex_id <- enc2utf8(as.character(
    source_vertex_id %||% vertex_id
  ))
  if (!is.list(adj_list) || !is.list(edge_length_list) ||
      length(adj_list) < 1L || length(edge_length_list) != length(adj_list) ||
      length(vertex_id) != length(adj_list) || anyNA(vertex_id) ||
      any(!nzchar(vertex_id)) || anyDuplicated(vertex_id) ||
      length(source_vertex_id) != length(adj_list) ||
      anyNA(source_vertex_id) || any(!nzchar(source_vertex_id)) ||
      anyDuplicated(source_vertex_id) ||
      length(graph_id) != 1L || !nzchar(graph_id) ||
      length(graph_k) != 1L || !is.finite(graph_k) || graph_k < 1L) {
    stop(
      "The displayed graph lacks a complete basin-alignment identity.",
      call. = FALSE
    )
  }
  adjacency <- lapply(adj_list, as.integer)
  weights <- lapply(edge_length_list, as.double)
  valid <- vapply(seq_along(adjacency), function(index) {
    neighbors <- adjacency[[index]]
    edge_lengths <- weights[[index]]
    length(neighbors) == length(edge_lengths) &&
      all(is.finite(neighbors)) &&
      all(neighbors >= 1L & neighbors <= length(adjacency)) &&
      all(is.finite(edge_lengths)) &&
      all(edge_lengths >= 0)
  }, logical(1))
  if (!all(valid)) {
    stop(
      "The displayed graph has invalid adjacency or edge lengths.",
      call. = FALSE
    )
  }
  display_vertex_fingerprint <- gflowui_basin_sha256(list(
    schema = "gflowui_basin_display_vertices_v1",
    vertex.id = vertex_id
  ))
  declared_display_vertex_fingerprint <- as.character(
    declared_display_vertex_fingerprint %||% display_vertex_fingerprint
  )
  if (length(declared_display_vertex_fingerprint) != 1L ||
      !identical(
        declared_display_vertex_fingerprint,
        display_vertex_fingerprint
      )) {
    stop(
      "The displayed graph's ordered vertex IDs do not match its manifest.",
      call. = FALSE
    )
  }
  topology_fingerprint <- gflowui_basin_sha256(list(
    schema = "hmp_graph_heat_topology_v1",
    vertex.id = source_vertex_id,
    adj.list = adjacency,
    weight.list = weights
  ))
  list(
    contract.version = "gflowui_basin_graph_identity/1",
    graph.id = graph_id,
    graph.k = graph_k,
    graph.fingerprint = gflowui_basin_sha256(list(
      schema = "hmp_graph_heat_graph_v1",
      graph.id = graph_id,
      graph.k = graph_k,
      topology.fingerprint = topology_fingerprint
    )),
    topology.fingerprint = topology_fingerprint,
    vertex.id.fingerprint = gflowui_basin_sha256(list(
      schema = "hmp_graph_heat_vertices_v1",
      vertex.id = source_vertex_id
    )),
    source.vertex.id = source_vertex_id,
    display.vertex.id.fingerprint = display_vertex_fingerprint,
    vertex.id = vertex_id
  )
}

gflowui_validate_basin_source_alignment <- function(
    source_contract,
    graph_identity,
    field,
    source_fingerprint) {
  required <- c(
    "contract.version", "algorithm", "graph.id", "graph.k",
    "graph.fingerprint", "vertex.id.fingerprint",
    "display.vertex.id.fingerprint", "source.vertex.id",
    "field.fingerprint"
  )
  if (!is.list(source_contract) ||
      !all(required %in% names(source_contract))) {
    stop(
      paste(
        "The selected estimate does not provide the required source-side",
        "graph and ordered-vertex alignment contract."
      ),
      call. = FALSE
    )
  }
  if (!is.list(graph_identity) ||
      !all(c(
        "graph.id", "graph.k", "graph.fingerprint",
        "vertex.id.fingerprint"
      ) %in% names(graph_identity))) {
    stop(
      "The displayed graph does not provide a verifiable basin identity.",
      call. = FALSE
    )
  }
  scalar_string <- function(value, label) {
    value <- as.character(value %||% "")
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop(sprintf("The source contract has no valid %s.", label), call. = FALSE)
    }
    enc2utf8(value)
  }
  source <- list(
    contract.version = scalar_string(
      source_contract$contract.version, "contract version"
    ),
    algorithm = scalar_string(source_contract$algorithm, "algorithm"),
    graph.id = scalar_string(source_contract$graph.id, "graph ID"),
    graph.k = suppressWarnings(as.integer(source_contract$graph.k)),
    graph.fingerprint = scalar_string(
      source_contract$graph.fingerprint, "graph fingerprint"
    ),
    vertex.id.fingerprint = scalar_string(
      source_contract$vertex.id.fingerprint, "vertex fingerprint"
    ),
    display.vertex.id.fingerprint = scalar_string(
      source_contract$display.vertex.id.fingerprint,
      "display-vertex fingerprint"
    ),
    source.vertex.id = enc2utf8(as.character(
      source_contract$source.vertex.id %||% character()
    )),
    field.fingerprint = scalar_string(
      source_contract$field.fingerprint, "field fingerprint"
    ),
    source.asset.fingerprint = scalar_string(
      source_contract$source.asset.fingerprint %||% source_fingerprint,
      "source-asset fingerprint"
    ),
    source.id = scalar_string(
      source_contract$source.id %||% "estimate", "source ID"
    ),
    source.field.fingerprint = scalar_string(
      source_contract$source.field.fingerprint %||%
        source_contract$field.fingerprint,
      "source field fingerprint"
    )
  )
  if (!is.finite(source$graph.k) || source$graph.k < 1L) {
    stop("The source contract has no valid graph k.", call. = FALSE)
  }
  if (length(source$source.vertex.id) != length(field) ||
      anyNA(source$source.vertex.id) ||
      any(!nzchar(source$source.vertex.id)) ||
      anyDuplicated(source$source.vertex.id)) {
    stop(
      "The source contract has invalid ordered source vertex IDs.",
      call. = FALSE
    )
  }
  computed_field_fingerprint <- gflowui_basin_field_fingerprint(field)
  comparisons <- c(
    graph.id = identical(
      source$graph.id,
      as.character(graph_identity$graph.id)
    ),
    graph.k = identical(
      source$graph.k,
      suppressWarnings(as.integer(graph_identity$graph.k))
    ),
    graph.fingerprint = identical(
      source$graph.fingerprint,
      as.character(graph_identity$graph.fingerprint)
    ),
    vertex.id.fingerprint = identical(
      source$vertex.id.fingerprint,
      as.character(graph_identity$vertex.id.fingerprint)
    ),
    source.vertex.id = identical(
      source$source.vertex.id,
      graph_identity$source.vertex.id
    ),
    display.vertex.id.fingerprint = identical(
      source$display.vertex.id.fingerprint,
      as.character(graph_identity$display.vertex.id.fingerprint)
    ),
    field.fingerprint = identical(
      source$field.fingerprint,
      computed_field_fingerprint
    )
  )
  if (any(!comparisons)) {
    stop(
      sprintf(
        "Basin source alignment failed: %s.",
        paste(names(comparisons)[!comparisons], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  evidence <- list(
    source.graph.id = source$graph.id,
    selected.graph.id = as.character(graph_identity$graph.id),
    source.graph.k = source$graph.k,
    selected.graph.k = suppressWarnings(as.integer(graph_identity$graph.k)),
    source.graph.fingerprint = source$graph.fingerprint,
    selected.graph.fingerprint = as.character(
      graph_identity$graph.fingerprint
    ),
    source.vertex.id.fingerprint = source$vertex.id.fingerprint,
    selected.vertex.id.fingerprint = as.character(
      graph_identity$vertex.id.fingerprint
    ),
    source.display.vertex.id.fingerprint =
      source$display.vertex.id.fingerprint,
    selected.display.vertex.id.fingerprint = as.character(
      graph_identity$display.vertex.id.fingerprint
    ),
    source.field.fingerprint = source$field.fingerprint,
    selected.field.fingerprint = computed_field_fingerprint,
    source.asset.fingerprint = source$source.asset.fingerprint
  )
  list(
    status = "validated",
    contract.version = source$contract.version,
    validator = "gflowui basin source contract",
    validator.version = "2",
    algorithm = source$algorithm,
    source.id = source$source.id,
    source.fingerprint = as.character(source_fingerprint),
    evidence = evidence,
    evidence.fingerprint = gflowui_basin_sha256(list(
      schema = "gflowui_basin_alignment_evidence/2",
      contract = source,
      evidence = evidence
    ))
  )
}

gflowui_basin_mass_provenance <- function(
    mass_kind,
    source_id,
    source_fingerprint,
    authority,
    validator = "gflowui basin source contract",
    validator_version = "2",
    algorithm = "exact full-domain ordered vertex-ID and content comparison",
    evidence_fingerprint,
    contract_version = "gflowui_basin_source_contract/2",
    evidence = NULL,
    validation_status = "validated") {
  list(
    mass.kind = as.character(mass_kind),
    attestations = list(list(
      claim = "scientific mass meaning and external source/graph/vertex alignment",
      authority = as.character(authority),
      validator = as.character(validator),
      validator.version = as.character(validator_version),
      algorithm = as.character(algorithm),
      evidence.fingerprint = as.character(evidence_fingerprint),
      status = as.character(validation_status),
      contract.version = as.character(contract_version),
      source.id = as.character(source_id),
      source.fingerprint = as.character(source_fingerprint),
      evidence = evidence
    ))
  )
}

gflowui_basin_construction_identity <- function(
    project_id,
    graph_set_id,
    graph_identity,
    source_key,
    source_fingerprint,
    field,
    vertex_mass,
    vertex_mass_provenance,
    alignment_validation,
    build_identity) {
  record <- list(
    schema = "gflowui_basin_construction_identity/2",
    project.id = as.character(project_id %||% ""),
    graph.set.id = as.character(graph_set_id %||% ""),
    graph = graph_identity,
    source.key = as.character(source_key %||% ""),
    source.fingerprint = as.character(source_fingerprint %||% ""),
    field.fingerprint = gflowui_basin_field_fingerprint(field),
    mass.fingerprint = if (is.null(vertex_mass)) {
      NULL
    } else {
      gflowui_basin_field_fingerprint(vertex_mass)
    },
    mass.provenance = vertex_mass_provenance,
    alignment.validation = alignment_validation,
    construction = list(
      method = "trajectory_flow",
      direction = "both",
      modulation = "CLOSEST",
      plateau.policy = "connected_exact",
      edge.length.quantile.thld = 1,
      long.edge.fallback = "allow_and_flag",
      store.trajectories = FALSE,
      symmetric.seeding = FALSE,
      tie.breaking = FALSE,
      primary.assignment.policy = "backend_primary"
    ),
    gflow.build.id = as.character(build_identity$build.id %||% ""),
    gflow.runtime.id = as.character(build_identity$runtime$id %||% "")
  )
  list(
    record = record,
    fingerprint = gflowui_basin_sha256(record)
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

gflowui_basin_layer_specs <- function(
    basin_display,
    visible_vertices,
    point_size,
    opacity,
    unselected_color = "#D1D5DB",
    unselected_opacity = 0.28) {
  if (!is.list(basin_display) ||
      !inherits(basin_display$basin, "basin_complex") ||
      !is.data.frame(basin_display$table)) {
    return(list())
  }
  visible_vertices <- suppressWarnings(as.integer(visible_vertices))
  visible_vertices <- visible_vertices[
    is.finite(visible_vertices) &
      visible_vertices >= 1L &
      visible_vertices <= basin_display$basin$n.vertices
  ]
  table <- basin_display$table
  selected <- table[as.logical(table$selected), , drop = FALSE]
  specs <- list()
  maximum <- selected[selected$type == "max", , drop = FALSE]
  values <- as.character(basin_display$values_max %||% character())
  for (index in seq_len(nrow(maximum))) {
    row <- maximum[index, , drop = FALSE]
    vertices <- visible_vertices[
      values[visible_vertices] == as.character(row$display.label)
    ]
    if (length(vertices) > 0L) {
      specs[[length(specs) + 1L]] <- list(
        kind = "maximum_fill",
        key = as.character(row$key),
        name = as.character(row$display.label),
        vertices = vertices,
        color = as.character(row$color),
        size = as.numeric(point_size),
        opacity = as.numeric(opacity)
      )
    }
  }
  other <- visible_vertices[values[visible_vertices] == "Other basins"]
  if (length(other) > 0L) {
    specs[[length(specs) + 1L]] <- list(
      kind = "background",
      key = "background",
      name = "Other basins",
      vertices = other,
      color = as.character(unselected_color),
      size = as.numeric(point_size),
      opacity = as.numeric(unselected_opacity)
    )
  }
  minimum <- selected[selected$type == "min", , drop = FALSE]
  assignment <- basin_display$basin$assignment
  assignment <- assignment[
    assignment$direction == "min" &
      assignment$assignment.status == "assigned",
    ,
    drop = FALSE
  ]
  for (index in seq_len(nrow(minimum))) {
    row <- minimum[index, , drop = FALSE]
    vertices <- suppressWarnings(as.integer(
      assignment$vertex[assignment$basin.id == row$basin.id]
    ))
    vertices <- intersect(vertices[is.finite(vertices)], visible_vertices)
    if (length(vertices) > 0L) {
      specs[[length(specs) + 1L]] <- list(
        kind = "minimum_halo",
        key = as.character(row$key),
        name = paste0(as.character(row$display.label), " halo"),
        vertices = vertices,
        color = as.character(row$color),
        size = max(4, as.numeric(point_size) * 1.65),
        opacity = 1,
        line.width = 5,
        rgl.size = max(7, as.numeric(point_size) * 2.1),
        rgl.opacity = max(0.25, min(0.7, as.numeric(opacity) * 0.6))
      )
    }
  }
  specs
}

gflowui_add_plotly_basin_layers <- function(plot, specs, coords) {
  for (spec in specs) {
    vertices <- suppressWarnings(as.integer(spec$vertices))
    if (length(vertices) < 1L) {
      next
    }
    if (identical(spec$kind, "minimum_halo")) {
      plot <- plotly::add_trace(
        plot,
        type = "scatter3d",
        mode = "markers",
        x = coords[vertices, 1],
        y = coords[vertices, 2],
        z = coords[vertices, 3],
        key = vertices,
        customdata = vertices,
        name = spec$name,
        text = sprintf("%s<br>vertex=%d", spec$name, vertices),
        hoverinfo = "text",
        marker = list(
          size = spec$size,
          color = "rgba(255,255,255,0)",
          opacity = 1,
          line = list(color = spec$color, width = spec$line.width)
        ),
        showlegend = TRUE
      )
    } else {
      plot <- plotly::add_trace(
        plot,
        type = "scatter3d",
        mode = "markers",
        x = coords[vertices, 1],
        y = coords[vertices, 2],
        z = coords[vertices, 3],
        key = vertices,
        customdata = vertices,
        name = spec$name,
        legendgroup = spec$name,
        text = sprintf("%s<br>vertex=%d", spec$name, vertices),
        hoverinfo = "text",
        marker = list(
          size = spec$size,
          color = spec$color,
          opacity = spec$opacity
        ),
        showlegend = TRUE
      )
    }
  }
  plot
}

gflowui_draw_rgl_basin_layers <- function(coords, specs) {
  ids <- integer()
  for (spec in specs) {
    if (!identical(spec$kind, "minimum_halo")) {
      next
    }
    vertices <- suppressWarnings(as.integer(spec$vertices))
    vertices <- vertices[
      is.finite(vertices) & vertices >= 1L & vertices <= nrow(coords)
    ]
    if (length(vertices) < 1L) {
      next
    }
    id <- rgl::points3d(
      coords[vertices, , drop = FALSE],
      col = grDevices::adjustcolor(
        spec$color,
        alpha.f = spec$rgl.opacity
      ),
      size = spec$rgl.size
    )
    ids <- c(ids, as.integer(id))
  }
  ids
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
    build_identity,
    vertex_mass_provenance = NULL,
    alignment_validation = NULL) {
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
    vertex.mass.provenance = vertex_mass_provenance,
    alignment.validation = alignment_validation,
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

gflowui_basin_table <- function(summary, prominence_complex = NULL) {
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
    table$prominence <- numeric()
    return(table)
  }
  table$prominence <- NA_real_
  if (inherits(prominence_complex, "basin_complex") &&
      identical(as.character(prominence_complex$status %||% ""), "ok") &&
      is.data.frame(prominence_complex$basin.table)) {
    prominence.table <- prominence_complex$basin.table
    table.key <- paste(table$type, table$extremum.vertex, sep = "|")
    prominence.key <- paste(
      prominence.table$type,
      prominence.table$extremum.vertex,
      sep = "|"
    )
    if (anyDuplicated(prominence.key)) {
      stop(
        "The canonical prominence complex returned duplicate extrema.",
        call. = FALSE
      )
    }
    matched <- match(table.key, prominence.key)
    if (anyNA(matched)) {
      stop(
        paste(
          "The trajectory-flow and canonical merge-tree extrema do not align;",
          "prominence cannot be reported safely."
        ),
        call. = FALSE
      )
    }
    table$prominence <- as.numeric(
      prominence.table$persistence[matched]
    )
  }
  table$key <- paste(table$type, table$basin.id, sep = "|")
  table$display.label <- paste0(
    ifelse(table$type == "max", "M", "m"),
    as.integer(table$rank)
  )
  table$selected <- FALSE
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
    source_fingerprint = NULL,
    alignment_validation = NULL) {
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
    build_identity,
    vertex_mass_provenance = vertex_mass_provenance,
    alignment_validation = alignment_validation
  )
  cache_hit <- exists(cache_key, envir = .gflowui_basin_cache, inherits = FALSE)
  if (isTRUE(cache_hit)) {
    cached <- get(cache_key, envir = .gflowui_basin_cache, inherits = FALSE)
    cache_hit <- is.list(cached) &&
      identical(as.character(cached$status %||% ""), "ok")
    if (!isTRUE(cache_hit)) {
      rm(list = cache_key, envir = .gflowui_basin_cache)
    }
  }
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
  if (!isTRUE(cache_hit)) {
    assign(cache_key, basin, envir = .gflowui_basin_cache)
  }

  prominence_cache_key <- paste0("merge-tree-prominence|", cache_key)
  prominence_cache_hit <- exists(
    prominence_cache_key,
    envir = .gflowui_basin_prominence_cache,
    inherits = FALSE
  )
  if (isTRUE(prominence_cache_hit)) {
    cached <- get(
      prominence_cache_key,
      envir = .gflowui_basin_prominence_cache,
      inherits = FALSE
    )
    prominence_cache_hit <- inherits(cached, "basin_complex") &&
      identical(as.character(cached$status %||% ""), "ok")
    if (!isTRUE(prominence_cache_hit)) {
      rm(
        list = prominence_cache_key,
        envir = .gflowui_basin_prominence_cache
      )
    }
  }
  prominence_complex <- if (prominence_cache_hit) {
    get(
      prominence_cache_key,
      envir = .gflowui_basin_prominence_cache,
      inherits = FALSE
    )
  } else {
    gflow::create.basin.complex(
      adj.list = adj_list,
      edge.length.list = edge_length_list,
      field = field,
      method = "superlevel_merge_tree",
      direction = "both",
      vertex.mass = vertex_mass,
      method.params = list(),
      simplify.params = list(),
      verbose = FALSE,
      vertex.id = vertex_id,
      vertex.mass.provenance = vertex_mass_provenance
    )
  }
  if (!inherits(prominence_complex, "basin_complex") ||
      !identical(as.character(prominence_complex$status %||% ""), "ok")) {
    detail <- as.character(
      prominence_complex$diagnostics$message %||%
        prominence_complex$diagnostics$error %||%
        "The canonical merge tree did not return usable prominence values."
    )
    stop(detail, call. = FALSE)
  }
  if (!isTRUE(prominence_cache_hit)) {
    assign(
      prominence_cache_key,
      prominence_complex,
      envir = .gflowui_basin_prominence_cache
    )
  }

  summary <- summary(
    basin,
    rank.by = rank_by,
    top.k.max = top_k_max,
    top.k.min = top_k_min,
    include.vertex.lists = FALSE
  )
  table <- gflowui_basin_table(summary, prominence_complex)
  display_direction <- if (direction == "min") "min" else "max"
  values <- gflowui_basin_display_values(
    basin,
    table,
    selected_keys = table$key[table$selected],
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
      table$key[table$selected],
      "max"
    ),
    values_min = gflowui_basin_display_values(
      basin,
      table,
      table$key[table$selected],
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
    prominence_complex = prominence_complex,
    prominence_method = "superlevel_merge_tree",
    prominence_cache_hit = prominence_cache_hit,
    cache_key = cache_key,
    cache_hit = cache_hit,
    build_identity = build_identity,
    source_fingerprint = source_fingerprint
  )
}
