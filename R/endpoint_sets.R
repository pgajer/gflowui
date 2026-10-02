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
  if (!is.null(set$membership_ids) && any(!rows$vertex_id %in% set$membership_ids))
    stop("Choose endpoints within this region's saved membership.")
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

# A region revision owns annotations independently of its current graph or fit.
gflowui_endpoint_region_scope <- function(dataset_scope, region) {
  if (is.null(region)) return(dataset_scope)
  list(key=digest::digest(list("region",dataset_scope$namespace,region$id,
      region$membership_fingerprint),algo="sha256"),
    label=paste0(region$label," · revision ",region$revision %||% 1L),
    namespace=dataset_scope$namespace, region_id=region$id,
    membership_fingerprint=region$membership_fingerprint)
}

gflowui_endpoint_region_ensure <- function(store, dataset_scope, region, regions,
    empty_state, provenance=list()) {
  if (is.null(region)) return(store)
  scope <- gflowui_endpoint_region_scope(dataset_scope,region)
  if (!is.null(store$active[[scope$key]])) return(store)
  parent_scope <- dataset_scope
  parent <- regions[[region$parent_id %||% ""]]
  if (!is.null(parent)) {
    store <- gflowui_endpoint_region_ensure(store,dataset_scope,parent,regions,empty_state,provenance)
    parent_scope <- gflowui_endpoint_region_scope(dataset_scope,parent)
  }
  source <- store$sets[[store$active[[parent_scope$key]] %||% ""]]
  state <- if(is.null(source)) empty_state else gflowui_endpoint_set_project(source,region$vertex_ids)
  state$is_modified <- FALSE
  state$base_dataset_id <- state$base_dataset_label <- NA_character_
  state$last_snapshot_id <- state$last_snapshot_label <- NA_character_
  set <- gflowui_endpoint_set_new(paste(scope$label,"— Endpoints"),state,
    region$vertex_ids,scope,provenance)
  set$region_id <- region$id
  set$membership_ids <- region$vertex_ids
  set$membership_fingerprint <- region$membership_fingerprint
  set$inherited_from <- list(set_id=source$id,revision=source$revision,scope=parent_scope$key,
    at=.gflowui_now(),vertex_ids=set$state$rows$vertex_id)
  set$row_provenance <- source$row_provenance[intersect(names(source$row_provenance),set$state$rows$vertex_id)]
  set$display <- source$display %||% gflowui_endpoint_display_defaults()
  store$sets[[set$id]] <- set
  store$active[[scope$key]] <- set$id
  store
}

gflowui_endpoint_display_defaults <- function() list(label_size=1,label_offset="1x",
  marker_size="1x",marker_color="#ef4444")

# Screen-space annotations keep long and short labels at the same font size,
# independently of depth in the 3D scene.
gflowui_endpoint_annotations <- function(xyz, labels, size) {
  lapply(seq_along(labels),function(i) list(x=xyz[i,1],y=xyz[i,2],z=xyz[i,3],
    text=as.character(htmltools::htmlEscape(labels[i])),showarrow=FALSE,
    xanchor="center",yanchor="bottom",font=list(size=max(8,12*size),color="#111827")))
}
