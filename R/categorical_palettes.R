# A project-supplied palette fixes category colors across subsets and graph sets.
gflowui_explicit_categorical_palette <- function(values, palette = NULL) {
  if (is.null(palette) || length(palette) == 0L) return(NULL)
  if (is.list(palette)) palette <- unlist(palette, use.names = TRUE)
  if (!is.character(palette) || is.null(names(palette)) ||
      anyNA(names(palette)) || any(!nzchar(names(palette))) ||
      anyDuplicated(names(palette)) || anyNA(palette)) {
    stop("Categorical palettes must be named character vectors with unique category names.")
  }
  tryCatch(grDevices::col2rgb(palette), error = function(e) {
    stop("Categorical palette contains an invalid color.")
  })
  vv <- as.character(values)
  vv[is.na(vv) | !nzchar(vv)] <- "NA"
  present <- unique(vv)
  known <- names(palette)[names(palette) %in% present]
  unknown <- setdiff(present, known)
  lev <- c(known, unknown)
  cols <- c(palette[known], stats::setNames(rep("#808080", length(unknown)), unknown))
  list(values = vv, levels = lev, colors = cols)
}
