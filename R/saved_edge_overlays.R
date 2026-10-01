# Optional, project-supplied edge groups for the Plotly reference layout.
# Endpoints refer to the original graph vertex order, before display filtering.
gflowui_add_saved_edge_overlays <- function(plot, coords, visible, path = NULL) {
  if (is.null(path) || length(path) != 1L || !nzchar(path) || !file.exists(path))
    return(plot)
  groups <- readRDS(path)
  if (!is.list(groups)) stop("Saved edge overlays must be a list of edge groups.")
  for (group in groups) {
    edges <- as.matrix(group$edges)
    if (!is.numeric(edges) || ncol(edges) != 2L || any(!is.finite(edges)) ||
        any(edges != floor(edges)) || any(edges < 1L | edges > nrow(coords)))
      stop("Saved edge overlay has invalid graph vertex indices.")
    edges <- edges[edges[, 1] %in% visible & edges[, 2] %in% visible, , drop = FALSE]
    if (!nrow(edges)) next
    xyz <- matrix(NA_real_, nrow(edges) * 3L, 3L)
    xyz[seq.int(1L, nrow(xyz), 3L), ] <- coords[edges[, 1], , drop = FALSE]
    xyz[seq.int(2L, nrow(xyz), 3L), ] <- coords[edges[, 2], , drop = FALSE]
    plot <- plotly::add_trace(plot, type = "scatter3d", mode = "lines",
      x = xyz[, 1], y = xyz[, 2], z = xyz[, 3], inherit = FALSE,
      name = as.character(group$label), showlegend = TRUE,
      visible = if (isTRUE(group$visible)) TRUE else "legendonly",
      hoverinfo = "name", line = list(color = group$color, width = group$width))
    # Plotly otherwise hides a lone legend-only group, making it untoggleable.
    plot <- plotly::layout(plot, showlegend = TRUE)
  }
  plot
}
