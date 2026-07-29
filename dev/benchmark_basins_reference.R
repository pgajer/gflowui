#!/usr/bin/env Rscript

gflow.root <- "/Users/pgajer/current_projects/gflow"
gflowui.root <- "/Users/pgajer/current_projects/gflowui"
project.root <- paste0(
  "/Users/pgajer/current_projects/vaginal_community_trajectory_types/",
  "analysis_output/hmp_subject15_k03_gflowui_project_20260727"
)
graph.file <- file.path(
  project.root,
  "data/graph/symmetric_knn_k03.rds"
)
density.file <- file.path(
  project.root,
  "data/occupation_density/subject15_k03_eta_basin_path.rds"
)

pkgload::load_all(gflow.root, quiet = TRUE)
pkgload::load_all(gflowui.root, quiet = TRUE)

graph.asset <- readRDS(graph.file)
graph <- graph.asset$X.graphs$k03
density.asset <- readRDS(density.file)
path.index <- 4L
source.field <- as.numeric(density.asset$probability.mass[, path.index])
field <- gflowui:::gflowui_normalize_density(source.field)
vertex.id <- enc2utf8(as.character(graph.asset$vertex_ids))

stopifnot(
  length(graph$adj_list) == 6529L,
  length(graph$weight_list) == 6529L,
  length(field) == 6529L,
  length(vertex.id) == 6529L,
  all(is.finite(field)),
  !anyDuplicated(vertex.id),
  abs(sum(field) - 1) < 1e-8
)

source.fingerprint <- gflowui:::gflowui_basin_hash(list(
  graph.asset = unname(tools::md5sum(graph.file)),
  density.asset = unname(tools::md5sum(density.file)),
  path.index = path.index,
  vertex.id = vertex.id,
  field = field
))
mass.provenance <- gflowui:::gflowui_basin_mass_provenance(
  mass_kind = "occupation_probability",
  source_id = sprintf("%s#eta-index-%d", density.file, path.index),
  source_fingerprint = source.fingerprint,
  authority = "registered Subject 15 gflowui project assets",
  validator = "gflowui reference basin benchmark",
  validator_version = "1",
  algorithm = paste(
    "exact graph-asset vertex IDs, graph and density asset fingerprints,",
    "path index, and full ordered field comparison"
  ),
  evidence_fingerprint = source.fingerprint,
  evidence = list(
    selected.path.index = path.index,
    source.asset.fingerprint = gflowui:::gflowui_basin_file_sha256(
      density.file
    ),
    selected.field.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(field)
  )
)

profile <- function(expression) {
  memory.file <- tempfile("gflowui-basin-rprofmem-")
  on.exit(unlink(memory.file), add = TRUE)
  gc()
  Rprofmem(memory.file)
  elapsed <- system.time(value <- force(expression))
  Rprofmem(NULL)
  lines <- readLines(memory.file, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  bytes <- bytes[is.finite(bytes)]
  list(
    value = value,
    elapsed.seconds = unname(elapsed[["elapsed"]]),
    allocation.bytes = sum(bytes),
    largest.allocation.bytes = if (length(bytes)) max(bytes) else 0
  )
}

rm(list = ls(gflowui:::.gflowui_basin_cache, all.names = TRUE),
   envir = gflowui:::.gflowui_basin_cache)

uncached <- profile(gflowui:::gflowui_estimate_basin_overlay(
  adj_list = graph$adj_list,
  edge_length_list = graph$weight_list,
  field = field,
  direction = "both",
  top_k_max = 6L,
  top_k_min = 6L,
  rank_by = "auto",
  vertex_mass = field,
  vertex_id = vertex.id,
  vertex_mass_provenance = mass.provenance,
  source_key = "subject15_eta_index_4",
  source_fingerprint = source.fingerprint
))

cached <- profile(gflowui:::gflowui_estimate_basin_overlay(
  adj_list = graph$adj_list,
  edge_length_list = graph$weight_list,
  field = field,
  direction = "both",
  top_k_max = 6L,
  top_k_min = 6L,
  rank_by = "auto",
  vertex_mass = field,
  vertex_id = vertex.id,
  vertex_mass_provenance = mass.provenance,
  source_key = "subject15_eta_index_4",
  source_fingerprint = source.fingerprint
))

with.trajectories <- profile(gflow::create.basin.complex(
  adj.list = graph$adj_list,
  edge.length.list = graph$weight_list,
  field = field,
  method = "trajectory_flow",
  direction = "both",
  vertex.mass = field,
  method.params = list(
    modulation = "CLOSEST",
    plateau.policy = "connected_exact",
    edge.length.quantile.thld = 1,
    long.edge.fallback = "allow_and_flag",
    store.trajectories = TRUE,
    symmetric.seeding = FALSE,
    tie.breaking = FALSE,
    primary.assignment.policy = "backend_primary"
  ),
  simplify.params = list(),
  verbose = FALSE,
  vertex.id = vertex.id,
  vertex.mass.provenance = mass.provenance
))

without <- uncached$value$basin
with <- with.trajectories$value
source.field.after <- as.numeric(
  readRDS(density.file)$probability.mass[, path.index]
)
report <- list(
  measured.at = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S %Z",
    tz = "America/New_York"
  ),
  project.id = "hmp_subject15_k03_heat_basin_path",
  vertices = length(field),
  path.index = path.index,
  field.finite = all(is.finite(field)),
  source.asset.field.exact.identity =
    identical(source.field, source.field.after),
  adapter.normalization.exact.identity = identical(source.field, field),
  adapter.normalization.maximum.absolute.difference =
    max(abs(source.field - field)),
  constructed.field.exact.identity =
    identical(field, without$field$input.values),
  constructed.mass.exact.identity =
    identical(field, without$field$vertex.mass.input),
  source.to.constructed.maximum.absolute.difference =
    max(abs(source.field - without$field$input.values)),
  direction = without$direction,
  assignment.rows = nrow(without$assignment),
  assignments.per.direction = table(without$assignment$direction),
  maximum.basins = uncached$value$basin_count_max,
  minimum.basins = uncached$value$basin_count_min,
  rank.resolved = uncached$value$ranking_resolved,
  without.trajectory.storage = list(
    elapsed.seconds = uncached$elapsed.seconds,
    allocation.bytes = uncached$allocation.bytes,
    largest.allocation.bytes = uncached$largest.allocation.bytes,
    object.bytes = as.numeric(object.size(without))
  ),
  cache.hit = list(
    confirmed = isTRUE(cached$value$cache_hit),
    elapsed.seconds = cached$elapsed.seconds,
    allocation.bytes = cached$allocation.bytes
  ),
  with.trajectory.storage = list(
    elapsed.seconds = with.trajectories$elapsed.seconds,
    allocation.bytes = with.trajectories$allocation.bytes,
    largest.allocation.bytes = with.trajectories$largest.allocation.bytes,
    object.bytes = as.numeric(object.size(with))
  ),
  object.size.ratio = as.numeric(object.size(with)) /
    as.numeric(object.size(without)),
  gflow.build.identity = without$provenance$build.identity
)

dput(report)
