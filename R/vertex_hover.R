# Optional original-abundance profiles, matched by stable graph vertex IDs.
gflowui_validate_vertex_hover <- function(asset) {
  ids <- asset$sample_ids
  taxa <- asset$taxon_names
  if (!is.character(ids) || !length(ids) || anyNA(ids) ||
      any(!nzchar(ids)) || anyDuplicated(ids) || !is.character(taxa) ||
      !length(taxa) || anyNA(taxa) || any(!nzchar(taxa)) || anyDuplicated(taxa) ||
      !is.list(asset$indices) || !is.list(asset$abundances) ||
      length(asset$indices) != length(ids) || length(asset$abundances) != length(ids))
    stop("Invalid vertex-hover abundance identities.")
  for (i in seq_along(ids)) {
    j <- asset$indices[[i]]; v <- asset$abundances[[i]]
    if (!is.numeric(j) || !is.numeric(v) || !length(v) || length(j) != length(v) ||
        any(!is.finite(j)) || any(j != floor(j) | j < 1 | j > length(taxa)) ||
        anyDuplicated(j) || any(!is.finite(v)) || any(v <= 0 | v > 1 + 1e-12) ||
        any(diff(v) > 0) || abs(sum(v) - 1) > 1e-10)
      stop("Invalid ranked relative abundances for vertex ID: ", ids[[i]])
  }
  asset
}

gflowui_vertex_hover_asset <- local({
  key <- NULL; cached <- NULL
  function(manifest) {
    spec <- manifest$metadata$vertex_hover
    path <- spec$abundances_file
    if (is.null(path) || length(path) != 1L || !nzchar(path)) return(NULL)
    if (!grepl("^(/|[A-Za-z]:)", path)) path <- file.path(manifest$project_root, path)
    if (!file.exists(path)) stop("Vertex-hover abundance file is missing: ", path)
    info <- file.info(path)
    next_key <- paste(normalizePath(path), info$size, as.numeric(info$mtime), sep = "|")
    if (!identical(next_key, key)) {
      cached <<- gflowui_validate_vertex_hover(readRDS(path))
      key <<- next_key
    }
    cached
  }
})

gflowui_hover_top_n <- function(value, maximum) {
  n <- suppressWarnings(as.integer(value))
  if (length(n) != 1L || is.na(n)) n <- 4L
  max(1L, min(maximum, n))
}

gflowui_vertex_hover_text <- function(vertex_ids, asset, top_n = 4L) {
  if (is.null(asset)) return(NULL)
  if (is.null(vertex_ids) || anyNA(vertex_ids) || anyDuplicated(vertex_ids))
    stop("Abundance hover requires unique graph vertex IDs.")
  rows <- match(vertex_ids, asset$sample_ids)
  n <- gflowui_hover_top_n(top_n, length(asset$taxon_names))
  escape <- function(x) as.character(htmltools::htmlEscape(x))
  vapply(seq_along(vertex_ids), function(i) {
    heading <- sprintf("<b>Vertex ID:</b> %s<br>Vertex number: %d", escape(vertex_ids[[i]]), i)
    if (is.na(rows[[i]])) return(paste0(heading, "<br>Relative abundances unavailable"))
    row <- rows[[i]]; values <- asset$abundances[[row]]
    take <- seq_len(min(n, length(values)))
    names <- gsub("_", " ", asset$taxon_names[asset$indices[[row]][take]], fixed = TRUE)
    percentage <- vapply(100 * values[take], function(x)
      format(signif(x, 4L), trim = TRUE, scientific = FALSE), character(1))
    entries <- sprintf("%d. %s: %s%%", take, escape(names), percentage)
    note <- if (length(values) < n) sprintf("<br><i>Only %d nonzero phylotype%s</i>",
      length(values), if (length(values) == 1L) "" else "s") else ""
    paste0(heading, "<br><b>Relative abundances (top ", n, "):</b><br>",
      paste(entries, collapse = "<br>"), note)
  }, character(1), USE.NAMES = FALSE)
}

# Apply to each marker layer after construction so filtering, categorical
# splits, endpoints, basin layers and subject overlays use original indices.
# Keep text labels, click keys, marker data and existing hover context intact.
gflowui_add_vertex_hover <- function(plot, hover) {
  if (is.null(hover)) return(plot)
  for (i in seq_along(plot$x$attrs)) {
    trace <- plot$x$attrs[[i]]
    if (!identical(trace$type, "scatter3d") || is.null(trace$mode) ||
        !grepl("markers", trace$mode, fixed = TRUE) ||
        identical(trace$hoverinfo, "skip") || identical(trace$visible, "legendonly")) next
    vertices <- trace$customdata
    if (!is.numeric(vertices) || !length(vertices) || any(!is.finite(vertices)) ||
        any(vertices != floor(vertices) | vertices < 1L | vertices > length(hover))) next
    details <- trace$hovertext
    if (is.null(details)) details <- trace$text
    if (is.null(details) || !is.character(details)) details <- ""
    if (length(details) == 1L) details <- rep(details, length(vertices))
    if (length(details) != length(vertices)) stop("Hover context does not match marker vertices.")
    # Remove the old numeric vertex prefix; the new heading already supplies it.
    details <- sub("^vertex=[0-9]+(<br>)?", "", details)
    details[is.na(details)] <- ""
    trace$hovertext <- paste0(hover[vertices], ifelse(nzchar(details), paste0("<br>", details), ""))
    trace$hoverinfo <- "text"
    trace$hovertemplate <- "%{hovertext}<extra></extra>"
    plot$x$attrs[[i]] <- trace
  }
  plot
}

gflowui_vertex_hover_controls <- function(manifest, value = NULL, renderer = "plotly") {
  if (!identical(renderer, "plotly")) return(NULL)
  asset <- gflowui_vertex_hover_asset(manifest)
  if (is.null(asset)) return(NULL)
  shiny::div(class = "gf-graph-row gf-graph-layout-row",
    shiny::span(class = "gf-graph-row-label", "Phylotypes on hover:"),
    shiny::numericInput("graph_hover_top_n", label = NULL,
      value = gflowui_hover_top_n(value, length(asset$taxon_names)),
      min = 1L, max = length(asset$taxon_names), step = 1L, width = "105px"))
}
