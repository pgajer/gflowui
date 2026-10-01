# Dedicated controls are available when both merged dominant CST levels exist.
gflowui_dcst_options <- function(sources, level = "dcst_level1", group = "") {
  keys <- c("dcst_level1", "dcst_level2")
  if (!all(keys %in% names(sources))) return(NULL)
  if (length(level) != 1L || !level %in% keys) level <- keys[[1L]]
  values <- as.character(sources[[level]]$values)
  groups <- sort(unique(values[!is.na(values) & nzchar(values)]))
  counts <- vapply(groups, function(x) sum(values == x, na.rm = TRUE), integer(1))
  rank <- order(-counts, groups)
  groups <- groups[rank]
  counts <- counts[rank]
  # Encode the level in each choice so a level change resets the selection.
  ids <- paste(level, groups, sep = ":")
  choices <- c("All dCSTs" = "__all__", stats::setNames(ids,
    sprintf("%s (%s vertices)", groups, format(counts, big.mark = ",", trim = TRUE))))
  if (length(group) != 1L || !group %in% ids) group <- "__all__"
  list(level = level, group = group, choices = choices,
       selected_label = if (group %in% ids) groups[match(group, ids)] else NULL)
}

gflowui_dcst_focus <- function(st, keep_idx, level, group = "",
                               mode = "gray", background = "#b3b3b3") {
  opt <- gflowui_dcst_options(st$sources, level, group)
  if (is.null(opt) || is.null(opt$selected_label))
    return(list(st = st, keep_idx = keep_idx, note = ""))
  values <- as.character(st$sources[[opt$level]]$values)
  selected <- !is.na(values) & values == opt$selected_label
  if (identical(mode, "hide")) {
    keep_idx <- intersect(keep_idx, which(selected))
  } else {
    tryCatch(grDevices::col2rgb(background), error = function(e) {
      stop("Invalid nonselected vertex color.")
    })
    palette <- st$graph_set$color_assets$categorical_palettes[[opt$level]]
    if (is.null(palette)) {
      groups <- sort(unique(values[!is.na(values)]))
      palette <- stats::setNames(grDevices::hcl.colors(length(groups), "Dark 3"), groups)
    }
    other <- "Other dCSTs"
    while (other %in% values) other <- paste0(other, " ")
    values[!selected] <- other
    palette[other] <- background
    st$sources[[opt$level]]$values <- values
    st$graph_set$color_assets$categorical_palettes[[opt$level]] <- palette
  }
  list(st = st, keep_idx = keep_idx,
       note = sprintf("Selected %s: %s vertices in this view. Other vertices %s. Coordinates unchanged.",
         opt$selected_label, format(sum(selected[keep_idx]), big.mark = ","),
         if (identical(mode, "hide")) "hidden" else "recolored"))
}
