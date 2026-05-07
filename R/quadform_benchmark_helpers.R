quadform_stage_key <- function(dataset_id, setting_id, stage) {
  paste(
    as.character(dataset_id %||% ""),
    as.character(setting_id %||% ""),
    as.character(stage %||% ""),
    sep = "||"
  )
}

quadform_split_stage_key <- function(key) {
  parts <- strsplit(as.character(key %||% ""), "||", fixed = TRUE)[[1]]
  if (length(parts) != 3L) {
    return(list(dataset_id = "", setting_id = "", stage = ""))
  }
  list(dataset_id = parts[[1]], setting_id = parts[[2]], stage = parts[[3]])
}

quadform_safe_token <- function(x, fallback = "asset") {
  out <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(x %||% "")))
  out <- gsub("^_+|_+$", "", out)
  if (!nzchar(out)) {
    out <- fallback
  }
  out
}

quadform_first_col <- function(df, candidates) {
  if (!is.data.frame(df) || length(candidates) < 1L) {
    return("")
  }
  nm <- names(df)
  low <- tolower(gsub("[._]+", "_", nm))
  for (cand in candidates) {
    idx <- match(tolower(gsub("[._]+", "_", cand)), low)
    if (!is.na(idx)) {
      return(nm[[idx]])
    }
  }
  ""
}

quadform_read_csv <- function(path) {
  pp <- as.character(path %||% "")
  if (!nzchar(pp) || !file.exists(pp)) {
    return(data.frame())
  }
  out <- tryCatch(utils::read.csv(pp, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (!is.data.frame(out)) {
    return(data.frame())
  }
  rownames(out) <- NULL
  out
}

quadform_normalize_existing_path <- function(path, root = "") {
  pp <- as.character(path %||% "")
  if (!nzchar(pp)) {
    return("")
  }
  if (!grepl("^(/|~|[A-Za-z]:[/\\\\])", pp, perl = TRUE) && nzchar(root)) {
    pp <- file.path(root, pp)
  }
  normalizePath(path.expand(pp), mustWork = FALSE)
}

quadform_required_files <- function(run_dir) {
  root <- normalizePath(path.expand(run_dir), mustWork = TRUE)
  list(
    manifest_rds = file.path(root, "quadform_benchmark_manifest.rds"),
    manifest_json = file.path(root, "quadform_benchmark_manifest.json"),
    dataset_manifest_file = file.path(root, "dataset_manifest.csv"),
    metrics_file = file.path(root, "metrics.csv"),
    dataset_assets_file = file.path(root, "dataset_assets.csv"),
    graph_assets_file = file.path(root, "graph_assets.csv"),
    layout_assets_file = file.path(root, "layout_assets.csv"),
    graph_diagnostics_file = file.path(root, "graph_diagnostics.csv")
  )
}

quadform_is_benchmark_manifest <- function(manifest) {
  is.list(manifest) &&
    is.list(manifest$metadata) &&
    is.list(manifest$metadata$quadform_benchmark)
}

quadform_benchmark_metadata <- function(manifest) {
  if (!quadform_is_benchmark_manifest(manifest)) {
    return(NULL)
  }
  manifest$metadata$quadform_benchmark
}

quadform_summarize_csv_file <- function(path, key_cols = character(0)) {
  tbl <- quadform_read_csv(path)
  if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
    return(list(n_rows = 0L, columns = character(0)))
  }
  out <- list(
    n_rows = as.integer(nrow(tbl)),
    columns = names(tbl)
  )
  for (col in key_cols) {
    if (col %in% names(tbl)) {
      vals <- unique(as.character(tbl[[col]]))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      out[[paste0("n_", col)]] <- as.integer(length(vals))
    }
  }
  out
}

quadform_discover_benchmark_artifacts <- function(project_root) {
  root <- normalizePath(path.expand(project_root[1]), mustWork = TRUE)
  files <- quadform_required_files(root)
  missing <- names(files)[!file.exists(unlist(files, use.names = FALSE))]
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Missing quadform benchmark manifest file(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  bench_manifest <- tryCatch(readRDS(files$manifest_rds), error = function(e) NULL)
  if (!is.list(bench_manifest)) {
    stop("Could not read quadform_benchmark_manifest.rds.", call. = FALSE)
  }

  graph_summary <- quadform_summarize_csv_file(files$graph_assets_file, c("dataset_id", "setting_id", "stage"))
  layout_summary <- quadform_summarize_csv_file(files$layout_assets_file, c("dataset_id", "setting_id", "stage"))
  dataset_summary <- quadform_summarize_csv_file(files$dataset_assets_file, c("dataset_id"))
  metrics_summary <- quadform_summarize_csv_file(files$metrics_file, c("dataset_id", "setting_id", "target"))

  graph_assets <- quadform_read_csv(files$graph_assets_file)
  first_graph <- if (nrow(graph_assets) > 0L && "graph_asset_file" %in% names(graph_assets)) {
    quadform_normalize_existing_path(graph_assets$graph_asset_file[[1]], root = root)
  } else {
    ""
  }

  list(
    profile = "quadform_benchmark",
    project_root = root,
    graph_sets = list(gflowui_normalize_graph_set_manifest(list(
      id = "quadform_benchmark",
      label = "Quadform Benchmark",
      data_type_id = "quadform_benchmark",
      data_type_label = "Quadform Benchmark",
      graph_file = first_graph,
      k_values = 1L,
      n_samples = NA_integer_,
      n_features = 3L,
      source = "quadform_benchmark_compat"
    ))),
    condexp_sets = list(),
    endpoint_runs = list(),
    metadata = list(
      quadform_benchmark = list(
        version = as.character(bench_manifest$version %||% "1"),
        project = as.character(bench_manifest$project %||% "quadform_benchmark"),
        mode = as.character(bench_manifest$mode %||% ""),
        run_dir = root,
        manifest_rds = normalizePath(files$manifest_rds, mustWork = FALSE),
        manifest_json = normalizePath(files$manifest_json, mustWork = FALSE),
        dataset_manifest_file = normalizePath(files$dataset_manifest_file, mustWork = FALSE),
        metrics_file = normalizePath(files$metrics_file, mustWork = FALSE),
        dataset_assets_file = normalizePath(files$dataset_assets_file, mustWork = FALSE),
        graph_assets_file = normalizePath(files$graph_assets_file, mustWork = FALSE),
        layout_assets_file = normalizePath(files$layout_assets_file, mustWork = FALSE),
        graph_diagnostics_file = normalizePath(files$graph_diagnostics_file, mustWork = FALSE),
        summary = list(
          datasets = dataset_summary,
          graph_assets = graph_summary,
          layout_assets = layout_summary,
          metrics = metrics_summary
        ),
        key_fields = c("dataset_id", "setting_id", "stage"),
        generated_layout_cache = "gflowui"
      )
    ),
    artifacts = list(),
    defaults = list(
      graph_set_id = "quadform_benchmark"
    )
  )
}

quadform_normalize_stage_table <- function(tbl, root = "", path_col = "") {
  if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
    return(data.frame())
  }
  out <- tbl
  for (cc in c("dataset_id", "setting_id", "stage")) {
    if (!(cc %in% names(out))) {
      out[[cc]] <- ""
    }
    out[[cc]] <- as.character(out[[cc]])
  }
  out$quadform_stage_key <- quadform_stage_key(out$dataset_id, out$setting_id, out$stage)
  if (nzchar(path_col) && path_col %in% names(out)) {
    out[[path_col]] <- vapply(
      out[[path_col]],
      quadform_normalize_existing_path,
      character(1),
      root = root
    )
  }
  rownames(out) <- NULL
  out
}

quadform_index_from_metadata <- function(qb) {
  if (!is.list(qb)) {
    return(list(error = "Quadform benchmark metadata is missing."))
  }
  run_dir <- quadform_normalize_existing_path(qb$run_dir %||% "")
  if (!nzchar(run_dir) || !dir.exists(run_dir)) {
    return(list(error = "Quadform benchmark run directory is missing."))
  }

  graph_assets <- quadform_normalize_stage_table(
    quadform_read_csv(qb$graph_assets_file),
    root = run_dir,
    path_col = "graph_asset_file"
  )
  layout_assets <- quadform_normalize_stage_table(
    quadform_read_csv(qb$layout_assets_file),
    root = run_dir,
    path_col = "layout_asset_file"
  )
  dataset_assets <- quadform_read_csv(qb$dataset_assets_file)
  if (is.data.frame(dataset_assets) && nrow(dataset_assets) > 0L && "dataset_asset_file" %in% names(dataset_assets)) {
    dataset_assets$dataset_asset_file <- vapply(
      dataset_assets$dataset_asset_file,
      quadform_normalize_existing_path,
      character(1),
      root = run_dir
    )
  }
  dataset_manifest <- quadform_read_csv(qb$dataset_manifest_file)
  metrics <- quadform_read_csv(qb$metrics_file)
  diagnostics <- quadform_read_csv(qb$graph_diagnostics_file)

  bench_manifest <- tryCatch(readRDS(qb$manifest_rds), error = function(e) NULL)
  graph_settings <- if (is.list(bench_manifest) && is.data.frame(bench_manifest$graph_settings)) {
    bench_manifest$graph_settings
  } else if (is.data.frame(metrics) && nrow(metrics) > 0L) {
    metrics[!duplicated(metrics[, c("dataset_id", "setting_id"), drop = FALSE]), , drop = FALSE]
  } else {
    data.frame()
  }
  if (is.data.frame(graph_settings) && nrow(graph_settings) > 0L && "stage" %in% names(graph_settings)) {
    names(graph_settings)[names(graph_settings) == "stage"] <- "default_stage"
  }

  index <- graph_assets
  if (nrow(index) > 0L && is.data.frame(graph_settings) && nrow(graph_settings) > 0L) {
    join_cols <- intersect(c("dataset_id", "setting_id"), intersect(names(index), names(graph_settings)))
    if (length(join_cols) == 2L) {
      index <- merge(index, graph_settings, by = join_cols, all.x = TRUE, sort = FALSE, suffixes = c("", "_setting"))
    }
  }
  if (nrow(index) > 0L && is.data.frame(dataset_manifest) && nrow(dataset_manifest) > 0L && "dataset_id" %in% names(dataset_manifest)) {
    dm <- dataset_manifest
    names(dm) <- gsub("\\.", "_", names(dm))
    add_cols <- setdiff(names(dm), names(index))
    index <- merge(index, dm[, c("dataset_id", add_cols), drop = FALSE], by = "dataset_id", all.x = TRUE, sort = FALSE)
  }

  index$quadform_stage_key <- quadform_stage_key(index$dataset_id, index$setting_id, index$stage)
  rownames(index) <- NULL

  list(
    error = NULL,
    run_dir = run_dir,
    graph_assets = graph_assets,
    layout_assets = layout_assets,
    dataset_assets = dataset_assets,
    dataset_manifest = dataset_manifest,
    metrics = metrics,
    diagnostics = diagnostics,
    graph_settings = graph_settings,
    index = index
  )
}

quadform_stage_key_from_row <- function(row) {
  if (!is.data.frame(row) || nrow(row) < 1L) {
    return("")
  }
  quadform_stage_key(row$dataset_id[[1]], row$setting_id[[1]], row$stage[[1]])
}

quadform_exact_graph_row <- function(index, key) {
  ga <- if (is.list(index)) index$graph_assets else data.frame()
  if (!is.data.frame(ga) || nrow(ga) < 1L || !("quadform_stage_key" %in% names(ga))) {
    return(list(status = "missing_index", row = data.frame()))
  }
  hit <- which(as.character(ga$quadform_stage_key) == as.character(key))
  if (length(hit) == 1L) {
    return(list(status = "ok", row = ga[hit, , drop = FALSE]))
  }
  list(
    status = if (length(hit) < 1L) "no_match" else "ambiguous",
    row = if (length(hit) > 0L) ga[hit, , drop = FALSE] else data.frame()
  )
}

quadform_exact_layout_row <- function(index, key, method = "weighted_grip") {
  la <- if (is.list(index)) index$layout_assets else data.frame()
  if (!is.data.frame(la) || nrow(la) < 1L || !("quadform_stage_key" %in% names(la))) {
    return(list(status = "missing_index", row = data.frame()))
  }
  hit <- which(as.character(la$quadform_stage_key) == as.character(key))
  if ("method" %in% names(la) && nzchar(as.character(method %||% ""))) {
    hit <- hit[tolower(as.character(la$method[hit])) == tolower(as.character(method))]
  }
  if (length(hit) == 1L) {
    return(list(status = "ok", row = la[hit, , drop = FALSE]))
  }
  list(
    status = if (length(hit) < 1L) "no_match" else "ambiguous",
    row = if (length(hit) > 0L) la[hit, , drop = FALSE] else data.frame()
  )
}

quadform_parse_graph_asset <- function(path) {
  pp <- as.character(path %||% "")
  if (!nzchar(pp) || !file.exists(pp)) {
    return(list(status = "missing_graph", message = "Graph asset file is missing."))
  }
  obj <- tryCatch(readRDS(pp), error = function(e) e)
  if (inherits(obj, "error")) {
    return(list(status = "error", message = conditionMessage(obj)))
  }
  adj <- obj$adj_list %||% obj$adj.list
  weight <- obj$weight_list %||% obj$weight.list %||% obj$edge.length.list
  if (!is.list(adj) || length(adj) < 1L) {
    return(list(status = "error", message = "Graph asset does not contain adj_list."))
  }
  if (!is.list(weight) || length(weight) != length(adj)) {
    return(list(status = "error", message = "Graph asset does not contain weight_list matching adj_list."))
  }
  list(
    status = "ok",
    obj = obj,
    adj_list = adj,
    weight_list = weight,
    n_vertices = length(adj)
  )
}

quadform_parse_layout_asset <- function(path) {
  pp <- as.character(path %||% "")
  if (!nzchar(pp) || !file.exists(pp)) {
    return(list(status = "missing_layout", message = "Layout asset file is missing."))
  }
  obj <- tryCatch(readRDS(pp), error = function(e) e)
  if (inherits(obj, "error")) {
    return(list(status = "error", message = conditionMessage(obj)))
  }
  coords <- if (is.list(obj) && !is.null(obj$coords)) obj$coords else obj
  if (is.data.frame(coords)) {
    coords <- as.matrix(coords)
  } else {
    coords <- suppressWarnings(as.matrix(coords))
  }
  if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
    return(list(status = "error", message = "Layout asset does not contain a 3-column coordinate matrix."))
  }
  num <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
  if (!is.matrix(num) || ncol(num) < 3L || !any(is.finite(num))) {
    return(list(status = "error", message = "Layout coordinates are not numeric."))
  }
  num <- num[, seq_len(3L), drop = FALSE]
  num[!is.finite(num)] <- 0
  list(status = "ok", obj = obj, coords = num)
}

quadform_generated_layout_cache_path <- function(project_id, key, params = list()) {
  parts <- quadform_split_stage_key(key)
  base <- file.path(
    gflowui_projects_data_dir(),
    "quadform_layout_cache",
    quadform_safe_token(project_id, "project"),
    quadform_safe_token(parts$dataset_id, "dataset"),
    quadform_safe_token(parts$setting_id, "setting")
  )
  file.path(base, sprintf("%s_weighted_grip_3d.rds", quadform_safe_token(parts$stage, "stage")))
}

quadform_weighted_layout_fun <- function() {
  if (!requireNamespace("grip", quietly = TRUE) ||
      !exists("grip.layout.weighted", envir = asNamespace("grip"), inherits = FALSE)) {
    return(NULL)
  }
  get("grip.layout.weighted", envir = asNamespace("grip"), inherits = FALSE)
}

quadform_generate_weighted_layout <- function(graph_asset_path, output_path, params = list(), weighted_layout_fun = quadform_weighted_layout_fun()) {
  graph <- quadform_parse_graph_asset(graph_asset_path)
  if (!identical(graph$status, "ok")) {
    return(list(status = graph$status, message = graph$message %||% "Graph asset unavailable."))
  }
  if (!is.function(weighted_layout_fun)) {
    return(list(
      status = "unavailable",
      message = "Package `grip` with `grip.layout.weighted()` is required for weighted layout generation."
    ))
  }
  defaults <- list(dim = 3L, rounds = 8L, final_rounds = 12L, seed = 6L)
  params_use <- defaults
  if (is.list(params) && length(params) > 0L) {
    params_use[names(params)] <- params
  }
  layout <- tryCatch(
    do.call(
      weighted_layout_fun,
      c(list(adj_list = graph$adj_list, weight_list = graph$weight_list), params_use)
    ),
    error = function(e) e
  )
  if (inherits(layout, "error")) {
    return(list(status = "error", message = conditionMessage(layout)))
  }
  coords <- quadform_parse_layout_asset_structure(layout)
  if (!identical(coords$status, "ok")) {
    return(list(status = "error", message = coords$message))
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(
      method = "weighted_grip",
      coords = coords$coords,
      params = params_use,
      graph_asset_file = normalizePath(graph_asset_path, mustWork = FALSE),
      created_at = .gflowui_now()
    ),
    output_path
  )
  list(status = "ok", path = normalizePath(output_path, mustWork = FALSE), coords = coords$coords)
}

quadform_parse_layout_asset_structure <- function(obj) {
  coords <- if (is.list(obj) && !is.null(obj$coords)) obj$coords else obj
  if (is.data.frame(coords)) {
    coords <- as.matrix(coords)
  } else {
    coords <- suppressWarnings(as.matrix(coords))
  }
  if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
    return(list(status = "error", message = "Layout result did not contain 3D coordinates."))
  }
  num <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
  if (!is.matrix(num) || ncol(num) < 3L || !any(is.finite(num))) {
    return(list(status = "error", message = "Layout result coordinates are not numeric."))
  }
  num <- num[, seq_len(3L), drop = FALSE]
  num[!is.finite(num)] <- 0
  list(status = "ok", coords = num)
}

quadform_selector_label <- function(x) {
  switch(
    as.character(x),
    surface = "Surface",
    n = "n",
    seed = "Seed",
    graph_family = "Graph family",
    k = "k",
    radius_rank = "Radius rank",
    k_scale = "k scale",
    radius_rule = "Radius rule",
    radius_factor = "Radius factor",
    prune_method = "Pruning",
    stage = "Stage",
    as.character(x)
  )
}

quadform_family_param_fields <- function(family) {
  fam <- as.character(family %||% "")
  if (fam %in% c("sknn", "mknn", "iknn")) {
    return("k")
  }
  if (identical(fam, "fixed_radius")) {
    return("radius_rank")
  }
  if (identical(fam, "adaptive_radius")) {
    return(c("k_scale", "radius_rule", "radius_factor"))
  }
  character(0)
}

quadform_order_values <- function(values, field) {
  vals <- unique(as.character(values %||% character(0)))
  vals <- vals[!is.na(vals) & nzchar(vals) & vals != "NA"]
  if (length(vals) < 1L) {
    return(character(0))
  }
  num_fields <- c("n", "seed", "k", "radius_rank", "k_scale", "radius_factor")
  if (field %in% num_fields) {
    num <- suppressWarnings(as.numeric(vals))
    if (all(is.finite(num))) {
      vals <- vals[order(num)]
    } else {
      vals <- sort(vals)
    }
  } else if (identical(field, "stage")) {
    pref <- c("raw", "raw.repaired", "pruned", "pruned.repaired", "repaired.pruned", "final")
    vals <- c(pref[pref %in% vals], sort(setdiff(vals, pref)))
  } else if (identical(field, "graph_family")) {
    pref <- c("adaptive_radius", "fixed_radius", "iknn", "mknn", "sknn")
    vals <- c(pref[pref %in% vals], sort(setdiff(vals, pref)))
  } else {
    vals <- sort(vals)
  }
  unique(vals)
}

quadform_selector_state <- function(index_df, input_values = list()) {
  if (!is.data.frame(index_df) || nrow(index_df) < 1L) {
    return(list(error = "No benchmark graph-stage rows are available.", fields = list(), row = data.frame()))
  }
  candidate <- index_df
  fields <- list()

  add_field <- function(field) {
    if (!(field %in% names(candidate))) {
      return(invisible(NULL))
    }
    vals <- quadform_order_values(candidate[[field]], field)
    if (length(vals) < 1L) {
      return(invisible(NULL))
    }
    input_val <- as.character(input_values[[field]] %||% "")
    selected <- if (input_val %in% vals) input_val else vals[[1]]
    fields[[length(fields) + 1L]] <<- list(
      id = field,
      input_id = paste0("quadform_", field),
      label = quadform_selector_label(field),
      choices = stats::setNames(vals, vals),
      selected = selected
    )
    candidate <<- candidate[as.character(candidate[[field]]) == selected, , drop = FALSE]
    invisible(NULL)
  }

  for (field in c("surface", "n", "seed", "graph_family")) {
    add_field(field)
  }
  family_selected <- ""
  for (field_spec in fields) {
    if (identical(field_spec$id, "graph_family")) {
      family_selected <- as.character(field_spec$selected %||% "")
    }
  }
  for (field in quadform_family_param_fields(family_selected)) {
    add_field(field)
  }
  for (field in c("prune_method", "stage")) {
    add_field(field)
  }

  status <- if (nrow(candidate) == 1L) {
    "ok"
  } else if (nrow(candidate) < 1L) {
    "no_match"
  } else {
    "ambiguous"
  }
  list(
    error = NULL,
    status = status,
    fields = fields,
    row = if (nrow(candidate) > 0L) candidate[seq_len(min(50L, nrow(candidate))), , drop = FALSE] else data.frame(),
    n_matches = as.integer(nrow(candidate)),
    key = if (nrow(candidate) == 1L) quadform_stage_key_from_row(candidate[1, , drop = FALSE]) else ""
  )
}

quadform_selection_mode_field <- function(selected = "manual") {
  sel <- as.character(selected %||% "manual")
  if (!sel %in% c("manual", "optimal")) {
    sel <- "manual"
  }
  list(
    id = "selection_mode",
    input_id = "quadform_selection_mode",
    label = "Selection mode",
    choices = c(Manual = "manual", Optimal = "optimal"),
    selected = sel
  )
}

quadform_metric_target_field <- function(metrics, selected = "") {
  vals <- if (is.data.frame(metrics) && "target" %in% names(metrics)) {
    quadform_order_values(metrics$target, "target")
  } else {
    character(0)
  }
  if (length(vals) < 1L) {
    vals <- "surface"
  }
  sel <- as.character(selected %||% "")
  if (!sel %in% vals) {
    sel <- vals[[1]]
  }
  list(
    id = "metric_target",
    input_id = "quadform_metric_target",
    label = "Optimal target",
    choices = stats::setNames(vals, vals),
    selected = sel
  )
}

quadform_add_any_choice <- function(vals, any_label = "Any") {
  vals <- vals[!is.na(vals) & nzchar(vals) & vals != "NA"]
  c(stats::setNames("", any_label), stats::setNames(vals, vals))
}

quadform_metric_settings_index <- function(index_df, metrics) {
  if (!is.data.frame(metrics) || nrow(metrics) < 1L) {
    return(data.frame())
  }
  out <- metrics
  for (cc in c("dataset_id", "setting_id")) {
    if (!(cc %in% names(out))) {
      out[[cc]] <- ""
    }
    out[[cc]] <- as.character(out[[cc]])
  }
  if (!is.data.frame(index_df) || nrow(index_df) < 1L) {
    return(out)
  }
  setting_cols <- setdiff(names(index_df), c("stage", "quadform_stage_key", "graph_asset_file"))
  setting_cols <- unique(c("dataset_id", "setting_id", setting_cols))
  setting_cols <- intersect(setting_cols, names(index_df))
  setting_index <- index_df[, setting_cols, drop = FALSE]
  setting_index <- setting_index[!duplicated(setting_index[, c("dataset_id", "setting_id"), drop = FALSE]), , drop = FALSE]
  add_cols <- setdiff(names(setting_index), names(out))
  if (length(add_cols) < 1L) {
    return(out)
  }
  merge(out, setting_index[, c("dataset_id", "setting_id", add_cols), drop = FALSE],
    by = c("dataset_id", "setting_id"), all.x = TRUE, sort = FALSE
  )
}

quadform_optimal_selector_state <- function(index_df, metrics, input_values = list()) {
  if (!is.data.frame(index_df) || nrow(index_df) < 1L) {
    return(list(error = "No benchmark graph-stage rows are available.", fields = list(), row = data.frame()))
  }
  if (!is.data.frame(metrics) || nrow(metrics) < 1L) {
    return(list(error = "No benchmark metric rows are available.", fields = list(), row = data.frame()))
  }

  metric_index <- quadform_metric_settings_index(index_df, metrics)
  candidate <- metric_index
  fields <- list(quadform_selection_mode_field("optimal"))

  target_field <- quadform_metric_target_field(candidate, input_values$metric_target)
  fields[[length(fields) + 1L]] <- target_field
  if ("target" %in% names(candidate)) {
    candidate <- candidate[as.character(candidate$target) == as.character(target_field$selected), , drop = FALSE]
  }

  add_required_field <- function(field) {
    if (!(field %in% names(candidate))) {
      return(invisible(NULL))
    }
    vals <- quadform_order_values(candidate[[field]], field)
    if (length(vals) < 1L) {
      return(invisible(NULL))
    }
    input_val <- as.character(input_values[[field]] %||% "")
    selected <- if (input_val %in% vals) input_val else vals[[1]]
    fields[[length(fields) + 1L]] <<- list(
      id = field,
      input_id = paste0("quadform_", field),
      label = quadform_selector_label(field),
      choices = stats::setNames(vals, vals),
      selected = selected
    )
    candidate <<- candidate[as.character(candidate[[field]]) == selected, , drop = FALSE]
    invisible(NULL)
  }

  add_optional_field <- function(field) {
    if (!(field %in% names(candidate))) {
      return(invisible(NULL))
    }
    vals <- quadform_order_values(candidate[[field]], field)
    if (length(vals) < 1L) {
      return(invisible(NULL))
    }
    input_val <- as.character(input_values[[field]] %||% "")
    selected <- if (input_val %in% vals) input_val else ""
    fields[[length(fields) + 1L]] <<- list(
      id = field,
      input_id = paste0("quadform_", field),
      label = quadform_selector_label(field),
      choices = quadform_add_any_choice(vals),
      selected = selected
    )
    if (nzchar(selected)) {
      candidate <<- candidate[as.character(candidate[[field]]) == selected, , drop = FALSE]
    }
    invisible(NULL)
  }

  for (field in c("surface", "n")) {
    add_required_field(field)
  }
  for (field in c("seed", "graph_family", "prune_method")) {
    add_optional_field(field)
  }

  err_col <- quadform_first_col(candidate, c("rel_rms_error", "error", "rel_abs_error_median"))
  if (!nzchar(err_col)) {
    return(list(
      error = "Metric table does not contain an error column for optimal selection.",
      fields = fields,
      row = data.frame()
    ))
  }
  err <- suppressWarnings(as.numeric(candidate[[err_col]]))
  keep <- is.finite(err)
  if (!any(keep)) {
    return(list(
      error = NULL,
      status = "no_metric",
      fields = fields,
      row = data.frame(),
      n_matches = 0L,
      key = "",
      mode = "optimal"
    ))
  }
  candidate <- candidate[keep, , drop = FALSE]
  err <- err[keep]
  ord <- order(err, as.character(candidate$dataset_id), as.character(candidate$setting_id), na.last = TRUE)
  best_metric <- candidate[ord[[1]], , drop = FALSE]
  best_stage <- as.character(best_metric$stage[[1]] %||% "")

  stage_rows <- index_df[
    as.character(index_df$dataset_id) == as.character(best_metric$dataset_id[[1]]) &
      as.character(index_df$setting_id) == as.character(best_metric$setting_id[[1]]),
    ,
    drop = FALSE
  ]
  if (!is.data.frame(stage_rows) || nrow(stage_rows) < 1L) {
    return(list(
      error = NULL,
      status = "missing_graph",
      fields = fields,
      row = data.frame(),
      n_matches = 0L,
      key = "",
      mode = "optimal",
      optimal_metric = best_metric,
      error_column = err_col
    ))
  }

  stage_vals <- quadform_order_values(stage_rows$stage, "stage")
  stage_selected <- as.character(input_values$stage %||% "")
  if (!stage_selected %in% stage_vals) {
    stage_selected <- if (best_stage %in% stage_vals) best_stage else stage_vals[[1]]
  }
  fields[[length(fields) + 1L]] <- list(
    id = "stage",
    input_id = "quadform_stage",
    label = quadform_selector_label("stage"),
    choices = stats::setNames(stage_vals, stage_vals),
    selected = stage_selected
  )
  selected_row <- stage_rows[as.character(stage_rows$stage) == stage_selected, , drop = FALSE]
  selected_row <- selected_row[1, , drop = FALSE]

  list(
    error = NULL,
    status = "ok",
    fields = fields,
    row = selected_row,
    n_matches = as.integer(nrow(candidate)),
    key = quadform_stage_key_from_row(selected_row),
    mode = "optimal",
    optimal_metric = best_metric,
    error_column = err_col
  )
}

quadform_benchmark_selector_state <- function(index_df, metrics, input_values = list()) {
  mode <- as.character(input_values$selection_mode %||% "manual")
  if (identical(mode, "optimal")) {
    return(quadform_optimal_selector_state(index_df, metrics, input_values))
  }
  out <- quadform_selector_state(index_df, input_values)
  out$mode <- "manual"
  out$fields <- c(list(quadform_selection_mode_field("manual")), out$fields)
  out
}
