# A region is a frozen set of dataset IDs, independent of graph row numbers.
gflowui_atlas_region <- function(label, ids, universe, definition, views = list()) {
  ids <- unique(as.character(ids))
  if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || !all(ids %in% universe))
    stop("Region membership must contain known dataset vertex IDs.")
  fingerprint <- digest::digest(sort(ids), algo = "sha256")
  list(id = paste0("region_", substr(digest::digest(list(definition, fingerprint)), 1, 16)),
       label = label, vertex_ids = ids, membership_fingerprint = fingerprint,
       definition = definition, distance_scope = "within_region", views = views,
       created_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

gflowui_atlas_anchor <- function(asset, anchor, size, metric = "hellinger") {
  ids <- asset$sample_ids
  a <- match(anchor, ids)
  if (is.na(a)) stop("Choose an anchor using its dataset vertex ID.")
  if (length(size) != 1L || !is.finite(size) || size != floor(size) || size < 1L || size > length(ids))
    stop("Neighborhood size must be an integer between 1 and the dataset size (including the anchor).")
  size <- as.integer(size)
  ref <- numeric(length(asset$taxon_names))
  ref[asset$indices[[a]]] <- asset$abundances[[a]]
  # One source only: linear in the nonzero abundance table, no dense pair matrix.
  d <- vapply(seq_along(ids), function(i) {
    x <- asset$abundances[[i]]; j <- asset$indices[[i]]; y <- ref[j]
    if (metric == "euclidean") return(sqrt(max(0, sum(x*x) + sum(ref*ref) - 2*sum(x*y))))
    if (metric == "hellinger") return(sqrt(max(0, 1 - sum(sqrt(x*y)))))
    if (metric != "jensen_shannon") stop("Unknown neighborhood metric.")
    m <- (x+y)/2
    positive <- y > 0
    v <- sum(x * log(x/m)) + sum(y[positive] * log(y[positive]/m[positive])) +
      sum(ref[-j]) * log(2)
    sqrt(max(0, v/2))
  }, numeric(1))
  # Force anchor inclusion even when several observations coincide exactly.
  ord <- c(a, setdiff(order(d, ids), a))
  list(ids = ids[head(ord, size)], distances = d[head(ord, size)],
       radius = max(d[head(ord, size)]), metric = metric)
}

gflowui_atlas_import <- function(source, universe, namespace) {
  sets <- source$graph_sets
  keys <- vapply(sets, function(gs) paste(gs$anchor, gs$neighborhood, sep = " / "), "")
  if (any(keys == " / ")) stop("Import requires anchor and neighborhood fields on every view.")
  regions <- lapply(unique(keys), function(key) {
    views <- sets[keys == key]
    memberships <- lapply(views, function(gs) {
      if (!file.exists(gs$graph_file)) stop("Missing graph asset: ", gs$graph_file)
      as.character(readRDS(gs$graph_file)$vertex_ids)
    })
    ids <- memberships[[1]]
    if (!length(ids) || anyDuplicated(ids) ||
        !all(vapply(memberships, function(x) setequal(x, ids) && !anyDuplicated(x), logical(1))))
      stop("Imported views disagree on region membership: ", key)
    views <- lapply(views, function(gs) {
      gs$endpoint_vertex_namespace <- namespace
      gs$endpoint_scope_id <- NULL
      gs
    })
    gflowui_atlas_region(paste0(key, " samples"), ids, universe,
      list(type = "import", source_project = source$project_id,
           anchor = views[[1]]$anchor, size = length(ids),
           selection = "Original reference-phylotype abundance ranking; preserved, not reselected",
           source_metadata = source$metadata), views)
  })
  stats::setNames(regions, vapply(regions, `[[`, "", "id"))
}

gflowui_atlas_manifest <- function(manifest, region, view_id = "__preview__", parent_set = NULL) {
  if (length(parent_set) == 1L && parent_set %in% vapply(manifest$graph_sets, `[[`, "", "id")) {
    manifest$defaults$reference_graph_set_id <- parent_set
    manifest$defaults$graph_set_id <- parent_set
  }
  if (is.null(region) || identical(view_id, "__preview__")) return(manifest)
  ids <- vapply(region$views, `[[`, "", "id")
  index <- match(view_id, ids)
  if (is.na(index)) return(manifest)
  gs <- region$views[[index]]
  if (is.null(gs)) return(manifest)
  # Dataset annotations follow stable IDs into local fits; local numeric sources
  # and graph diagnostics remain available alongside the inherited categories.
  parent_id <- parent_set %||% manifest$defaults$reference_graph_set_id %||% manifest$defaults$graph_set_id
  parent_index <- match(parent_id, vapply(manifest$graph_sets, `[[`, "", "id"))
  if (length(parent_index) == 1L && !is.na(parent_index)) {
    ca <- manifest$graph_sets[[parent_index]]$color_assets
    columns <- intersect(c("dcst_level1", "dcst_level2", "dcst_level3"),
      ca$vector_columns %||% ca$preferred_order)
    if (length(columns) && nzchar(ca$metadata_file %||% "")) {
      ca$vector_columns <- ca$preferred_order <- columns
      ca$match_vertex_ids <- TRUE
      ca$inherited_metadata <- NULL
      gs$color_assets$inherited_metadata <- ca
      for (column in columns) {
        gs$color_assets$categorical_palettes[[column]] <- ca$categorical_palettes[[column]]
      }
    }
  }
  manifest$graph_sets <- list(gs)
  manifest$defaults$reference_graph_set_id <- gs$id
  manifest$defaults$reference_k <- gs$selected_k %||% 1L
  # The Local views menu selects a complete saved view. No duplicate selectors.
  manifest$metadata$graph_selector_schema <- list(fields = list())
  source_meta <- region$definition$source_metadata
  if (is.list(source_meta$endpoint_label_provider))
    manifest$metadata$endpoint_label_provider <- source_meta$endpoint_label_provider
  manifest$endpoint_runs <- list()
  manifest$condexp_sets <- list()
  manifest$occupation_density_sets <- list()
  manifest
}

gflowui_atlas_save <- function(regions, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("atlas-", dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(version = 1L, regions = regions), tmp)
  if (!file.rename(tmp, path)) stop("Could not save the atlas.")
  invisible(regions)
}

# Short transaction lock: prevents independent sessions/jobs losing each other's updates.
gflowui_atlas_update <- function(path, update) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  lock<-paste0(path,".lock")
  if(!dir.create(lock,showWarnings=FALSE)) {
    owner_file<-file.path(lock,"owner.rds")
    owner<-if(file.exists(owner_file))tryCatch(readRDS(owner_file),error=function(e)NULL) else NULL
    age<-as.numeric(difftime(Sys.time(),file.info(lock)$mtime,units="secs"))
    alive<-!is.null(owner) && isTRUE(tryCatch(tools::pskill(owner$pid,signal=0L),error=function(e)FALSE))
    if(alive || is.na(age) || (is.null(owner)&&age<30))stop("Atlas is being updated; retry shortly.")
    unlink(lock,recursive=TRUE)
    if(!dir.create(lock,showWarnings=FALSE))stop("Atlas is being updated; retry shortly.")
  }
  saveRDS(list(pid=Sys.getpid(),created=Sys.time()),file.path(lock,"owner.rds"))
  on.exit(unlink(lock,recursive=TRUE))
  regions<-if(file.exists(path))readRDS(path)$regions else list()
  next_regions<-update(regions)
  if(!identical(next_regions,regions))gflowui_atlas_save(next_regions,path)
  next_regions
}
