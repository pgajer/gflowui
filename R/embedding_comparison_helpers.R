# Dedicated read-only adapter; no graph reconstruction or embedding optimization.
gflowui_ec_active <- function(manifest) {
  is.list(manifest) && is.list(manifest$metadata$embedding_comparison) &&
    identical(as.integer(manifest$metadata$embedding_comparison$schema_version), 1L)
}

gflowui_ec_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) default else as.character(x[[1L]])
}

gflowui_ec_number <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.numeric(x) || !is.finite(x)) NA_real_ else as.numeric(x)
}

gflowui_ec_json <- function(file) jsonlite::fromJSON(file, simplifyVector = FALSE)

gflowui_ec_asset <- function(root, asset) {
  if (!is.list(asset) || !is.character(asset$path) || length(asset$path) != 1L ||
      !nzchar(asset$path) || grepl("(^/|^[A-Za-z]:|\\\\)", asset$path) ||
      ".." %in% strsplit(asset$path, "/", fixed = TRUE)[[1L]]) stop("Unsafe embedding asset path.")
  root <- normalizePath(root, mustWork = TRUE)
  file <- normalizePath(file.path(root, asset$path), mustWork = TRUE)
  if (!startsWith(file, paste0(root, "/")) || dir.exists(file) ||
      !identical(digest::digest(file = file, algo = "sha256"), asset$sha256)) {
    stop(sprintf("Missing, changed or invalid embedding asset: %s", asset$path))
  }
  file
}

gflowui_ec_methods <- function() c(
  metric_mds = "Metric MDS", metric_mds_edge_kk = "Metric MDS + edge-KK",
  weighted_grip = "Weighted GRIP", isomap_graph = "Isomap (original graph)",
  umap = "UMAP", lle = "LLE", pacmap = "PaCMAP", localmap = "LocalMAP",
  trimap = "TriMAP (landmark features)", trimap_graph = "TriMAP (graph distances)",
  phate = "PHATE", largevis = "LargeVis", ncvis = "NCVis", lgs = "LGS",
  lgs_paper = "LGS (experimental paper-form)"
)

gflowui_ec_metrics <- function() c(
  chord_error = "Euclidean error (lower is better)",
  relative_stress = "Relative-distance stress (lower is better)",
  path_error = "Fixed-path error (lower is better)",
  edge_error = "Edge error (lower is better)",
  distance_rank_correlation = "Distance-rank correlation (higher is better)"
)

gflowui_ec_definitions <- function() list(
  version = "suitesparse-pilot-v1; suitesparse-uniform-pairs-v1",
  evaluation = "Original pilot: exact unordered within-component pairs. Expanded graphs: up to 20,000 shared uniform pairs per component, with N/m weighting of component sums; exclude cross-component pairs. Edge and streamed neighborhood scores remain exact.",
  chord_error = "sqrt(sum((s*r-d)^2)/sum(d^2)); s=sum(r*d)/sum(r^2), separately per component.",
  relative_stress = "mean(((s*r-d)/d)^2); s=sum(r/d)/sum((r/d)^2), separately per component.",
  path_error = "sqrt(sum((p-d)^2)/sum(d^2)), identity scale; p follows fixed original shortest routes.",
  edge_error = "sqrt(mean((embedded unit-edge length-1)^2)), identity scale.",
  distance_rank_correlation = "Spearman correlation of graph and chord distances, average tied ranks; per-component when disconnected.",
  neighborhoods = "Trustworthiness penalizes false neighbors; continuity penalizes lost neighbors. k=5,10,20,50 where k<n/2; lexical-ID ties.",
  interpretation = "Do not combine fitted-scale and identity-scale measures into a single ranking. Fixed-path preservation alone does not establish unfolding.",
  uncertainty = "Expanded distance intervals: approximate 95% paired percentile bootstrap, 200 draws, refitted scales, finite-population-corrected component-sum deviations. Conditional on fixed coordinates, not optimizer variability. Observed seed ranges are not confidence intervals. Reverse-ID tie sensitivity changes convention, not layout."
)

gflowui_ec_seed_ranges <- function(table, graph_id, metric = "chord_error") {
  rows <- table[table$graph_id == graph_id, , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  groups <- unique(rows[c("method", "settings")])
  do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    rr <- rows[rows$method == groups$method[i] & rows$settings == groups$settings[i], , drop = FALSE]
    values <- rr[[metric]][rr$status == "completed" & is.finite(rr[[metric]])]
    data.frame(Method = groups$method[i], Settings = groups$settings[i],
      Available = length(values), Listed = nrow(rr),
      Mean = if (length(values)) mean(values) else NA_real_,
      Minimum = if (length(values)) min(values) else NA_real_,
      Maximum = if (length(values)) max(values) else NA_real_, check.names = FALSE)
  }))
}

gflowui_ec_load_index <- function(root) {
  root <- normalizePath(root, mustWork = TRUE)
  index <- gflowui_ec_json(file.path(root, "viewer_manifest.json"))
  if (!identical(index$kind, "gflowui_embedding_comparison") ||
      !identical(as.integer(index$schema_version), 1L) || !length(index$graphs)) stop("Unsupported embedding viewer manifest.")
  graph_ids <- vapply(index$graphs, function(x) gflowui_ec_text(x$id), "")
  run_ids <- vapply(index$runs, function(x) gflowui_ec_text(x$id), "")
  if (any(!nzchar(graph_ids)) || anyDuplicated(graph_ids) || any(!nzchar(run_ids)) || anyDuplicated(run_ids)) {
    stop("Missing or duplicate graph/run identity.")
  }
  index$root <- root
  index$manifest_sha256 <- digest::digest(file=file.path(root,"viewer_manifest.json"),algo="sha256")
  names(index$graphs) <- graph_ids
  names(index$runs) <- run_ids
  index$results <- list()
  rows <- lapply(index$runs, function(run) {
    if (!run$graph_id %in% graph_ids) stop("Unknown graph in embedding run.")
    if (!run$status %in% c("completed", "resource_limited", "unavailable", "unsupported", "failed", "cancelled", "invalid_input")) {
      stop("Unknown embedding run status.")
    }
    result <- NULL
    if (identical(run$status, "completed")) {
      result <- gflowui_ec_json(gflowui_ec_asset(root, run$result))
      if (!identical(result$status, "completed") || !identical(as.integer(result$dimension), 3L) ||
          !identical(result$graph_sha256, index$graphs[[run$graph_id]]$graph_sha256) ||
          !identical(result$method, run$method) || !identical(result$seed, run$seed)) stop("Embedding result identity mismatch.")
      index$results[[run$id]] <<- result
    }
    method <- unname(gflowui_ec_methods()[run$method])
    if (is.na(method)) method <- run$method
    settings <- if (identical(run$method, "lle")) sprintf("%s landmarks", gflowui_ec_text(run$parameters, "unknown")) else "fixed pilot settings"
    if (run$method %in% c("pacmap","localmap","trimap","phate","largevis","ncvis")) {
      settings <- sprintf("%s landmarks; fixed backend settings", gflowui_ec_text(run$parameters,"unknown"))
    }
    if (identical(run$method,"trimap_graph")) settings <- "graph distances; 400 iterations"
    if (identical(run$method,"lgs_paper")) {
      k <- unlist(run$locality$component_k,use.names=FALSE)
      fractions <- vapply(run$locality$component_fraction,gflowui_ec_number,0.)
      eligible <- which(k>0)
      values <- unique(sprintf("%s (%.3g%%)",k[eligible],100*fractions[eligible]))
      settings <- if (length(values)) paste0("k=",paste(values,collapse="; "),"; 60 epochs") else "locality unavailable"
    }
    term <- if (is.null(result)) gflowui_ec_text(run$reason, run$status) else {
      details <- vapply(result$components, function(c) {
        if (isTRUE(c$small_component_placement)) return("")
        detail <- if (is.list(c$details$metadata)) c$details$metadata else c$details
        gflowui_ec_text(detail$termination, "termination not reported")
      }, "")
      paste(unique(details[nzchar(details)]), collapse = "; ")
    }
    summary <- result$summary
    row <- data.frame(id = run$id, graph_id = run$graph_id, method = method,
      method_id = run$method, settings = settings, seed = gflowui_ec_number(run$seed),
      status = run$status, termination = term,
      input_type = gflowui_ec_text(result$input_type, "unavailable"),
      chord_error = gflowui_ec_number(summary$chord_error), relative_stress = gflowui_ec_number(summary$relative_stress),
      path_error = gflowui_ec_number(summary$path_error), edge_error = gflowui_ec_number(summary$edge_error),
      distance_rank_correlation = gflowui_ec_number(summary$distance_rank_correlation),
      elapsed_seconds = gflowui_ec_number(run$elapsed_seconds), memory_mib = gflowui_ec_number(run$peak_rss_bytes)/1024^2,
      stringsAsFactors = FALSE)
    row$evaluation <- if(is.null(result)) "unavailable" else gflowui_ec_text(summary$evaluation$mode,"exact")
    row$evaluated_pairs <- gflowui_ec_number(if(is.null(summary$evaluation))summary$n_pairs else summary$evaluation$pair_count)
    row$population_pairs <- gflowui_ec_number(summary$n_pairs)
    for(key in names(gflowui_ec_metrics())) {
      row[[paste0(key,"_lower")]] <- gflowui_ec_number(summary$intervals[[key]]$lower)
      row[[paste0(key,"_upper")]] <- gflowui_ec_number(summary$intervals[[key]]$upper)
    }
    row
  })
  index$table <- do.call(rbind, rows)
  index
}

gflowui_ec_graph <- function(index, id) {
  spec <- index$graphs[[id]]
  if (is.null(spec)) stop("Unknown graph selection.")
  graph <- gflowui_ec_json(gflowui_ec_asset(index$root, spec$file))
  ids <- unlist(graph$vertex_ids, use.names = FALSE)
  n <- gflowui_ec_number(graph$n_vertices)
  if (!identical(graph$graph_id, id) || !identical(graph$graph_sha256, spec$graph_sha256) ||
      !is.character(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) || length(ids) != n) stop("Invalid graph vertex identity.")
  valid_edge <- function(x) is.list(x) && length(x) == 2L && all(vapply(x, is.numeric, TRUE)) &&
    all(is.finite(unlist(x))) && all(unlist(x) == floor(unlist(x))) &&
    x[[1L]] >= 0 && x[[1L]] < x[[2L]] && x[[2L]] < n
  if (!all(vapply(graph$edges, valid_edge, TRUE))) stop("Invalid graph edge indices.")
  edges <- if (length(graph$edges)) matrix(unlist(graph$edges), ncol = 2, byrow = TRUE) else matrix(integer(), 0, 2)
  if (anyDuplicated(data.frame(edges)) || nrow(edges) != graph$n_edges || graph$edge_length != 1) stop("Invalid graph edge contract.")
  labels <- unlist(graph$component_labels, use.names = FALSE)
  if (length(labels) != n || !is.numeric(labels) || anyNA(labels) || any(labels != floor(labels)) ||
      any(labels < 0 | labels >= graph$n_components)) stop("Invalid component membership.")
  graph$ids <- ids; graph$edge_matrix <- edges + 1L; graph$labels <- labels
  graph
}

gflowui_ec_coordinates <- function(file, ids, expected_ids) {
  z <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  if (!identical(names(z), c("x", "y", "z")) || !all(vapply(z, is.numeric, TRUE)) ||
      nrow(z) != length(expected_ids) || any(!is.finite(as.matrix(z))) ||
      !identical(ids, expected_ids) || anyDuplicated(ids)) stop("Coordinates must be finite n-by-3 in exact graph vertex order.")
  as.matrix(z)
}

gflowui_ec_load_run <- function(index, graph, run_id) {
  run <- index$runs[[run_id]]
  if (is.null(run) || !identical(run$graph_id, graph$graph_id) || !identical(run$status, "completed")) stop("No completed layout for this selection.")
  # Revalidate every geometry artifact at the load boundary, never reuse stale coordinates.
  result <- gflowui_ec_json(gflowui_ec_asset(index$root, run$result))
  if (!identical(result$graph_sha256, graph$graph_sha256)) stop("Layout belongs to a different graph.")
  ids <- unlist(gflowui_ec_json(gflowui_ec_asset(index$root, run$vertices)), use.names = FALSE)
  raw <- gflowui_ec_coordinates(gflowui_ec_asset(index$root, run$raw_coordinates), ids, graph$ids)
  display <- gflowui_ec_coordinates(gflowui_ec_asset(index$root, run$display_coordinates), ids, graph$ids)
  if (!identical(run$raw_coordinates$sha256, result$coords_sha256) ||
      !identical(run$display_coordinates$sha256, result$display_sha256)) stop("Coordinate/result checksum mismatch.")
  diag <- gflowui_ec_json(gflowui_ec_asset(index$root, run$diagnostics))
  if (!identical(diag$coordinates_sha256, result$coords_sha256)) stop("Stale plot diagnostics.")
  list(run = run, result = result, raw = raw, display = display, diagnostics = diag)
}

gflowui_ec_export <- function(index, settings, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  stage <- tempfile("embedding-bundle-"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  files <- list(list(path = "viewer_manifest.json", sha256 = index$manifest_sha256))
  files <- c(files, lapply(index$graphs, `[[`, "file"), index$artifacts, index$indexes)
  for (run in index$runs) for (key in c("manifest","result","raw_coordinates","display_coordinates","vertices","diagnostics")) {
    if (is.list(run[[key]])) files <- c(files,list(run[[key]]))
  }
  paths <- character()
  for (spec in files) {
    source <- gflowui_ec_asset(index$root, spec)
    dest <- file.path(stage,spec$path);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    if (!file.copy(source,dest,overwrite=TRUE)) stop("Could not copy export asset.")
    paths <- c(paths,spec$path)
  }
  jsonlite::write_json(gflowui_ec_definitions(),file.path(stage,"metric_definitions.json"),auto_unbox=TRUE,pretty=TRUE)
  jsonlite::write_json(settings,file.path(stage,"figure_specifications.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  utils::write.csv(index$table,file.path(stage,"all_runs.csv"),row.names=FALSE,na="")
  paths <- unique(c(paths,"metric_definitions.json","figure_specifications.json","all_runs.csv"))
  if(!is.null(settings$graph_id)) {
    figures <- gflowui_ec_publication_render(index$table,settings,stage)
    script <- c("# Run from the extracted bundle root; requires jsonlite and base R.",
      "render_figures <-",deparse(gflowui_ec_publication_render),
      "render_figures(read.csv('all_runs.csv',check.names=FALSE), jsonlite::fromJSON('figure_specifications.json'), '.')")
    writeLines(script,file.path(stage,"rebuild_publication_figures.R"))
    table <- index$table[index$table$graph_id==settings$graph_id & index$table$status=="completed",,drop=FALSE]
    figure_manifest <- lapply(names(gflowui_ec_metrics()),function(key)list(metric=key,
      population=settings$graph_id,available_replicates=sum(is.finite(table[[key]])),
      status=if(any(is.finite(table[[key]])))"exported" else "unavailable: no finite completed scores",
      formats=c("PDF","SVG"),uncertainty="Approximate conditional pair-sampling intervals when provided; no intervals for exact scores."))
    jsonlite::write_json(figure_manifest,file.path(stage,"publication_figure_manifest.json"),auto_unbox=TRUE,pretty=TRUE)
    paths <- c(paths,figures,"rebuild_publication_figures.R","publication_figure_manifest.json")
  }
  hashes <- setNames(lapply(paths,function(p) digest::digest(file=file.path(stage,p),algo="sha256")),paths)
  jsonlite::write_json(hashes,file.path(stage,"bundle_checksums.json"),auto_unbox=TRUE,pretty=TRUE)
  zipfile <- tempfile(paste0("suitesparse-",format(Sys.time(),"%Y%m%d-%H%M%S"),"-"),tmpdir=output_dir,fileext=".zip")
  zip::zipr(zipfile,files=c(paths,"bundle_checksums.json"),root=stage,include_directories=FALSE,mode="mirror")
  normalizePath(zipfile,mustWork=TRUE)
}
