#!/usr/bin/env Rscript

gflow.root <- "/Users/pgajer/current_projects/gflow"
gflowui.root <- "/Users/pgajer/current_projects/gflowui"
project.id <- "hmp_subject15_k03_heat_basin_path"
output.file <- file.path(
  gflowui.root,
  "dev/basins_renderer_final_state_qa_2026-07-28.md"
)

pkgload::load_all(gflow.root, quiet = TRUE)
pkgload::load_all(gflowui.root, quiet = TRUE)

registry <- gflowui::list_projects()
index <- match(project.id, registry$id)
stopifnot(is.finite(index))
manifest <- gflowui:::gflowui_read_manifest(registry$manifest_file[[index]])
graph.set <- manifest$graph_sets[[1L]]
graph.asset <- readRDS(graph.set$graph_file)
graph <- graph.asset$X.graphs$k03
contract <- graph.set$basin_source_contract
graph.identity <- gflowui:::gflowui_basin_graph_identity(
  graph$adj_list,
  graph$weight_list,
  graph.asset$vertex_ids,
  contract$graph.id,
  3L,
  source_vertex_id = contract$source.vertex.id,
  declared_display_vertex_fingerprint =
    contract$display.vertex.id.fingerprint
)

density <- gflowui::gflowui_evaluate_occupation_density(
  manifest = manifest,
  set_id = manifest$occupation_density_sets[[1L]]$id,
  subject_id = "15",
  method_id = "graph_heat_kernel",
  mode = "parameters",
  parameters = list(eta_index = 4L, display_mode = "density")
)
alignment <- gflowui:::gflowui_validate_basin_source_alignment(
  density$alignment_contract,
  graph.identity,
  density$values,
  density$source_fingerprint
)
source.fingerprint <- gflowui:::gflowui_basin_sha256(list(
  schema = "gflowui_basin_source_request/2",
  source.key = "occupation_density_active",
  source.asset.fingerprint = density$source_fingerprint,
  field.fingerprint =
    gflowui:::gflowui_basin_field_fingerprint(density$values),
  alignment.evidence.fingerprint = alignment$evidence.fingerprint
))
provenance <- gflowui:::gflowui_basin_mass_provenance(
  mass_kind = "occupation_probability",
  source_id = alignment$source.id,
  source_fingerprint = source.fingerprint,
  authority = sprintf("gflowui project manifest %s", project.id),
  validator = alignment$validator,
  validator_version = alignment$validator.version,
  algorithm = alignment$algorithm,
  evidence_fingerprint = alignment$evidence.fingerprint,
  contract_version = alignment$contract.version,
  evidence = alignment$evidence,
  validation_status = alignment$status
)
result <- gflowui:::gflowui_estimate_basin_overlay(
  graph$adj_list,
  graph$weight_list,
  density$values,
  direction = "both",
  top_k_max = 6L,
  top_k_min = 6L,
  rank_by = "auto",
  vertex_mass = density$values,
  vertex_id = graph.asset$vertex_ids,
  vertex_mass_provenance = provenance,
  source_key = "occupation_density_active",
  source_fingerprint = source.fingerprint,
  alignment_validation = alignment
)
result$table$selected <- TRUE
coords <- as.matrix(readRDS(
  graph.set$layout_assets$grip_layouts$k03$path
))
stopifnot(nrow(coords) == length(graph$adj_list), ncol(coords) >= 3L)

specs <- gflowui:::gflowui_basin_layer_specs(
  result,
  visible_vertices = seq_len(nrow(coords)),
  point_size = 1,
  opacity = 0.85
)
maximum.specs <- specs[vapply(
  specs,
  function(x) identical(x$kind, "maximum_fill"),
  logical(1)
)]
minimum.specs <- specs[vapply(
  specs,
  function(x) identical(x$kind, "minimum_halo"),
  logical(1)
)]

plotly.status <- "skipped: plotly unavailable"
plotly.trace.count <- NA_integer_
plotly.maximum.count <- NA_integer_
plotly.minimum.count <- NA_integer_
if (requireNamespace("plotly", quietly = TRUE)) {
  plot <- gflowui:::gflowui_add_plotly_basin_layers(
    plotly::plot_ly(),
    specs,
    coords
  )
  traces <- plotly::plotly_build(plot)$x$data
  trace.names <- vapply(
    traces,
    function(x) as.character(if (is.null(x$name)) "" else x$name),
    character(1)
  )
  plotly.trace.count <- length(traces)
  plotly.maximum.count <- sum(grepl("^Maximum Basin", trace.names))
  plotly.minimum.count <- sum(grepl("^Minimum Basin.* halo$", trace.names))
  stopifnot(
    plotly.maximum.count == length(maximum.specs),
    plotly.minimum.count == length(minimum.specs)
  )
  plotly.status <- "passed"
}

rgl.status <- "skipped: rgl unavailable"
rgl.layer.count <- NA_integer_
if (requireNamespace("rgl", quietly = TRUE)) {
  old <- options(rgl.useNULL = TRUE)
  on.exit(options(old), add = TRUE)
  device <- rgl::open3d(useNULL = TRUE)
  on.exit(try(rgl::close3d(device), silent = TRUE), add = TRUE)
  ids <- gflowui:::gflowui_draw_rgl_basin_layers(
    coords,
    minimum.specs
  )
  shapes <- rgl::rgl.ids(type = "shapes")
  stopifnot(length(ids) == length(minimum.specs), all(ids %in% shapes$id))
  rgl.layer.count <- length(ids)
  rgl.status <- "passed on null RGL device"
}

build <- gflow::get.gflow.build.identity(refresh = TRUE)
selected.keys <- as.character(result$table$key[result$table$selected])
lines <- c(
  "# Basin Renderer Final-State QA",
  "",
  "Date: 2026-07-28",
  "",
  sprintf("- Project: `%s`", project.id),
  "- Estimate: Subject 15 graph-heat occupation density, path index 4",
  sprintf("- Source asset fingerprint: `%s`", density$source_fingerprint),
  sprintf(
    "- Selected field fingerprint: `%s`",
    gflowui:::gflowui_basin_field_fingerprint(density$values)
  ),
  sprintf("- Alignment evidence fingerprint: `%s`", alignment$evidence.fingerprint),
  sprintf("- gflow build ID: `%s`", build$build.id),
  sprintf("- gflow runtime ID: `%s`", build$runtime$id),
  sprintf("- Selected basin keys: `%s`", paste(selected.keys, collapse = "`, `")),
  sprintf(
    "- Layer specifications: %d maximum fills, %d minimum halos",
    length(maximum.specs),
    length(minimum.specs)
  ),
  sprintf(
    "- Plotly: %s; %s total traces, %s maximum fills, %s minimum halos",
    plotly.status,
    plotly.trace.count,
    plotly.maximum.count,
    plotly.minimum.count
  ),
  sprintf("- RGL: %s; %s minimum marker layers", rgl.status, rgl.layer.count),
  "- Diagnostics: no construction, alignment, Plotly-build, or RGL-layer errors",
  "",
  "## Reproduction",
  "",
  "```sh",
  paste(
    "Rscript",
    paste0(
      "/Users/pgajer/current_projects/vaginal_community_trajectory_types/",
      "analysis/291_register_hmp_subject15_k03_gflowui_project.R"
    )
  ),
  "cd /Users/pgajer/current_projects/gflowui",
  "Rscript dev/qa_basins_renderer_reference.R",
  paste0(
    "Rscript -e 'pkgload::load_all(\".\", quiet=TRUE); ",
    "gflowui::run_gflowui(host=\"127.0.0.1\", port=3867, ",
    "launch.browser=FALSE)'"
  ),
  "```"
)
writeLines(lines, output.file, useBytes = TRUE)
cat("Wrote", output.file, "\n")
