# Session-owned registry: requests can retrieve only previously registered groups.
gflowui_edge_registry <- function() {
  assets <- gflowui_lru_cache(4L, 96 * 1024^2)
  entries <- list()
  list(
    read = function(path) assets(gflowui_file_version(path), function() readRDS(path)),
    register = function(edges, version, ordinal, vertex_ids) {
      key <- digest::digest(list(version, ordinal, vertex_ids), algo="xxhash64")
      entries <<- entries[setdiff(names(entries),key)]
      entries[key] <<- list(edges)
      while(length(entries)>1L && (length(entries)>16L ||
        sum(vapply(entries,object.size,numeric(1)))>96*1024^2)) entries <<- entries[-1L]
      key
    },
    get = function(key) entries[[key]]
  )
}

# Standalone callers retain eager rendering; the app supplies a lazy registry.
# Edge indices refer to graph row order, before component or dCST filtering.
gflowui_add_saved_edge_overlays <- function(plot, coords, visible, path = NULL,
                                           registry = NULL, vertex_ids = NULL) {
  if (is.null(path) || length(path) != 1L || !nzchar(path) || !file.exists(path))
    return(plot)
  groups <- if(is.null(registry)) readRDS(path) else registry$read(path)
  if (!is.list(groups)) stop("Saved edge overlays must be a list of edge groups.")
  keep <- seq_len(nrow(coords)) %in% visible
  for (ordinal in seq_along(groups)) {
    group <- groups[[ordinal]]
    edges <- as.matrix(group$edges)
    if (!is.numeric(edges) || ncol(edges) != 2L || any(!is.finite(edges)) ||
        any(edges != floor(edges)) || any(edges < 1L | edges > nrow(coords)))
      stop("Saved edge overlay has invalid graph vertex indices.")
    selected <- keep[edges[, 1]] & keep[edges[, 2]]
    if (!any(selected)) next
    metadata <- NULL
    if(is.null(registry)) {
      edges <- edges[selected,,drop=FALSE]
      xyz <- matrix(NA_real_, nrow(edges) * 3L, 3L)
      xyz[seq.int(1L, nrow(xyz), 3L), ] <- coords[edges[, 1], , drop = FALSE]
      xyz[seq.int(2L, nrow(xyz), 3L), ] <- coords[edges[, 2], , drop = FALSE]
    } else {
      key <- registry$register(edges, gflowui_file_version(path), ordinal,
        vertex_ids %||% seq_len(nrow(coords)))
      metadata <- list(gflowui_edges=list(key=key, count=nrow(edges)))
      # A one-point line keeps the legend entry, without drawing a segment.
      xyz <- coords[visible[[1L]],1:3,drop=FALSE]
    }
    plot <- plotly::add_trace(plot, type = "scatter3d", mode = "lines",
      x = xyz[, 1], y = xyz[, 2], z = xyz[, 3], inherit = FALSE,
      name = as.character(group$label), showlegend = TRUE, meta=metadata,
      visible = if (isTRUE(group$visible)) TRUE else "legendonly",
      hoverinfo = "name", line = list(color = group$color, width = group$width))
    plot <- plotly::layout(plot, showlegend = TRUE)
  }
  plot
}
