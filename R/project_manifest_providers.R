# Opening behavior and taxonomy are declared by the manifest, never inferred
# from a project identifier, display name, or a developer's filesystem.
gflowui_project_open_defaults <- function(manifest) {
  defaults <- manifest$defaults %||% list()
  list(
    set_id = .as_scalar_chr(defaults$open_graph_set_id, ""),
    k = suppressWarnings(as.integer(defaults$open_graph_k %||% NA_integer_)[1L]),
    open_panels = defaults$open_panels %||% NULL
  )
}

gflowui_manifest_taxonomy_map <- function(spec, project_root) {
  path <- .normalize_project_path(spec$taxonomy_map_file %||% "", project_root)
  if (!nzchar(path)) return(NULL)
  tx <- readRDS(path)
  if (!is.character(tx) || is.null(names(tx)) || anyNA(names(tx)) ||
      any(!nzchar(names(tx))) || anyDuplicated(names(tx))) {
    stop("taxonomy_map_file must contain a character vector with unique feature names.",
         call. = FALSE)
  }
  tx
}
