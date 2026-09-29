gflowui_ec_favorites_record <- function(ids, graph_ids) {
  list(schema_version=1L, selection_type="graph favorites",
    updated_at=format(Sys.time(), tz="UTC", usetz=TRUE),
    favorite_graph_ids=sort(unique(as.character(ids))),
    available_graph_ids=sort(unique(as.character(graph_ids))),
    unselected_graph_ids=sort(setdiff(graph_ids, ids)),
    note="Unselected does not mean rejected or reviewed. This file authorizes no deletion.")
}

gflowui_ec_read_favorites <- function(root) {
  file <- file.path(root, "favorites.json")
  if(!file.exists(file)) return(character())
  record <- jsonlite::read_json(file, simplifyVector=TRUE)
  if(!identical(record$schema_version, 1L) || is.null(record$favorite_graph_ids))
    stop("Invalid favorites file; existing file has been preserved.")
  ids <- unlist(record$favorite_graph_ids, use.names=FALSE)
  if(length(ids) && (!is.character(ids) || anyNA(ids) || any(!nzchar(ids))))
    stop("Invalid favorite graph IDs; existing file has been preserved.")
  unique(as.character(ids))
}

gflowui_ec_save_favorites <- function(root, ids, graph_ids) {
  file <- file.path(root, "favorites.json")
  temp <- tempfile(".favorites-", tmpdir=root)
  on.exit(unlink(temp), add=TRUE)
  jsonlite::write_json(gflowui_ec_favorites_record(ids, graph_ids), temp,
    pretty=TRUE, auto_unbox=TRUE)
  if(!file.rename(temp, file)) stop("Could not save favorites.")
  invisible(file)
}

gflowui_ec_export_favorites <- function(root, graph_ids, directory=path.expand("~/Downloads")) {
  directory <- normalizePath(directory, mustWork=TRUE)
  file <- tempfile(paste0("suitesparse-favorites-", format(Sys.time(), "%Y-%m-%d_%H%M%S"), "-"),
    tmpdir=directory, fileext=".json")
  jsonlite::write_json(gflowui_ec_favorites_record(gflowui_ec_read_favorites(root), graph_ids),
    file, pretty=TRUE, auto_unbox=TRUE)
  normalizePath(file, mustWork=TRUE)
}
