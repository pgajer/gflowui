# Named endpoint sets belong to a dataset, independently of graphs and layouts.
gflowui_endpoint_scope <- function(graph_set, k, project_id) {
  namespace <- as.character(graph_set$endpoint_vertex_namespace %||% project_id)
  list(key = digest::digest(list("dataset", namespace), algo = "sha256"),
    label = namespace, namespace = namespace)
}

# Preserve all existing alternatives while lifting the former graph/k boundary.
gflowui_endpoint_store_upgrade <- function(store, preferred_scope = NULL) {
  if (identical(store$version, 2L)) return(store)
  old_active <- store$active
  store$active <- list()
  preferred <- old_active[[preferred_scope %||% ""]]
  for (id in names(store$sets)) {
    set <- store$sets[[id]]
    scope <- gflowui_endpoint_scope(list(endpoint_vertex_namespace = set$namespace), NULL, NULL)
    set$legacy_scope <- set$scope
    set$legacy_scope_label <- set$scope_label
    set$scope <- scope$key; set$scope_label <- scope$label
    store$sets[[id]] <- set
    if (is.null(store$active[[scope$key]]) || identical(id, preferred))
      store$active[[scope$key]] <- id
  }
  store$version <- 2L
  store
}

gflowui_endpoint_ids <- function(ids) {
  ids <- enc2utf8(as.character(ids %||% character()))
  if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) return(NULL)
  ids
}

gflowui_endpoint_set_project <- function(set, ids) {
  ids <- gflowui_endpoint_ids(ids)
  if (is.null(ids)) stop("Shared endpoint sets require unique stable vertex IDs.")
  state <- set$state
  rows <- state$rows
  at <- match(rows$vertex_id, ids)
  state$rows <- rows[!is.na(at), setdiff(names(rows), "vertex_id"), drop = FALSE]
  state$rows$vertex <- as.integer(at[!is.na(at)])
  state$shared_set_id <- set$id
  state$shared_revision <- set$revision
  state$shared_missing <- sum(is.na(at))
  state$shared_total <- nrow(rows)
  state
}

gflowui_endpoint_set_update <- function(set, state, ids) {
  ids <- gflowui_endpoint_ids(ids)
  if (is.null(ids)) stop("Shared endpoint sets require unique stable vertex IDs.")
  if (!is.null(state$shared_revision) && !identical(state$shared_revision, set$revision))
    stop("This endpoint set changed in another session. Reload it before editing.")
  rows <- state$rows
  if (anyNA(rows$vertex) || any(rows$vertex < 1L | rows$vertex > length(ids)))
    stop("An endpoint does not belong to the current graph.")
  rows$vertex_id <- ids[rows$vertex]
  old <- set$state$rows
  absent <- old[!old$vertex_id %in% ids, , drop = FALSE]
  # Edits to a subset cannot discard endpoints outside that subset.
  state$rows <- if (nrow(absent)) rbind(rows, absent[, names(rows), drop = FALSE]) else rows
  state$rows <- state$rows[!duplicated(state$rows$vertex_id), , drop = FALSE]
  state[c("shared_set_id", "shared_revision", "shared_missing", "shared_total")] <- NULL
  set$state <- state
  set$revision <- as.integer(set$revision %||% 0L) + 1L
  set$updated_at <- .gflowui_now()
  set
}

gflowui_endpoint_set_new <- function(name, state, ids, scope, provenance) {
  id <- paste0("set_", digest::digest(list(Sys.time(), tempfile(), name), algo = "sha256"))
  empty <- state
  empty$rows <- state$rows[0, , drop = FALSE]
  empty$rows$vertex_id <- character()
  set <- list(id = id, name = name, scope = scope$key, scope_label = scope$label,
    namespace = scope$namespace, provenance = provenance, created_at = .gflowui_now(),
    revision = 0L, state = empty)
  state$shared_revision <- NULL
  gflowui_endpoint_set_update(set, state, ids)
}

gflowui_endpoint_store_read <- function(path) {
  if (file.exists(path)) return(readRDS(path))
  list(version = 2L, sets = list(), active = list(), migrated = character())
}

gflowui_endpoint_store_write <- function(store, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("endpoint-sets-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(store, tmp)
  if (!file.rename(tmp, path)) stop("Could not save endpoint sets.")
  invisible(store)
}

# Import every legacy table independently. The original files are left in place.
gflowui_endpoint_sets_migrate <- function(store, entries) {
  for (entry in entries) {
    if (entry$key %in% store$migrated || is.null(gflowui_endpoint_ids(entry$ids))) next
    state <- entry$state
    if (!is.data.frame(state$rows)) next
    if (any(!is.finite(state$rows$vertex)) || any(state$rows$vertex < 1L |
        state$rows$vertex > length(entry$ids))) next
    set <- gflowui_endpoint_set_new(entry$name, state, entry$ids, entry$scope, entry$provenance)
    set$migration_source <- entry$key
    store$sets[[set$id]] <- set
    if (is.null(store$active[[entry$scope$key]])) store$active[[entry$scope$key]] <- set$id
    store$migrated <- c(store$migrated, entry$key)
  }
  store
}
