make_quadform_fixture <- function(root, missing_graph = FALSE, missing_layout = FALSE, layout_as_list = TRUE) {
  run <- file.path(root, "run")
  dir.create(run, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run, "assets", "datasets"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run, "assets", "graphs", "paraboloid_n10_seed001", "g0001"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run, "assets", "layouts", "paraboloid_n10_seed001", "g0001"), recursive = TRUE, showWarnings = FALSE)

  dataset_id <- "paraboloid_n10_seed001"
  setting_id <- "g0001"
  stage <- "raw.repaired"
  dataset_file <- file.path(run, "assets", "datasets", paste0(dataset_id, ".rds"))
  graph_file <- file.path(run, "assets", "graphs", dataset_id, setting_id, "raw_repaired.rds")
  layout_file <- file.path(run, "assets", "layouts", dataset_id, setting_id, "raw_repaired_weighted_grip_3d.rds")

  X <- cbind(seq_len(10), seq_len(10) / 10, (seq_len(10) / 10)^2)
  saveRDS(list(dataset_id = dataset_id, X_embed = X), dataset_file)

  adj <- list(2L, c(1L, 3L), c(2L, 4L), c(3L, 5L), c(4L, 6L), c(5L, 7L), c(6L, 8L), c(7L, 9L), c(8L, 10L), 9L)
  weight <- lapply(adj, function(nb) rep(1, length(nb)))
  saveRDS(
    list(
      dataset_id = dataset_id,
      setting_id = setting_id,
      stage = stage,
      adj_list = adj,
      weight_list = weight,
      n_vertices = length(adj)
    ),
    graph_file
  )

  layout <- cbind(seq_len(10), cos(seq_len(10)), sin(seq_len(10)))
  saveRDS(if (isTRUE(layout_as_list)) list(coords = layout) else layout, layout_file)

  dataset_manifest <- data.frame(
    surface = "paraboloid",
    index.k = 2L,
    n = 10L,
    seed = 1L,
    dataset_id = dataset_id,
    stringsAsFactors = FALSE
  )
  utils::write.csv(dataset_manifest, file.path(run, "dataset_manifest.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(dataset_id = dataset_id, dataset_asset_file = dataset_file, stringsAsFactors = FALSE),
    file.path(run, "dataset_assets.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      dataset_id = dataset_id,
      setting_id = setting_id,
      stage = stage,
      graph_family = "sknn",
      graph_asset_file = graph_file,
      n_vertices = 10L,
      n_edges = 9L,
      n_components = 1L,
      stringsAsFactors = FALSE
    ),
    file.path(run, "graph_assets.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      dataset_id = dataset_id,
      setting_id = setting_id,
      stage = stage,
      method = "weighted_grip",
      layout_asset_file = layout_file,
      n_vertices = 10L,
      stringsAsFactors = FALSE
    ),
    file.path(run, "layout_assets.csv"),
    row.names = FALSE
  )
  metrics <- rbind(
    data.frame(
      dataset_id = dataset_id,
      surface = "paraboloid",
      index_k = 2L,
      n = 10L,
      seed = 1L,
      graph_family = "sknn",
      k = 3L,
      prune_method = "none",
      radius_rank = NA_integer_,
      k_scale = NA_integer_,
      radius_rule = NA_character_,
      radius_factor = NA_real_,
      stage = stage,
      setting_id = setting_id,
      status = "ok",
      error = NA_character_,
      scale = 1,
      rel_rms_error = 0.1,
      rel_abs_error_median = 0.08,
      rel_abs_error_q95 = 0.2,
      distortion_q05 = 0.9,
      distortion_median = 1,
      distortion_q95 = 1.1,
      pearson_cor = 0.99,
      spearman_cor = 0.98,
      target = "surface",
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset_id = dataset_id,
      surface = "paraboloid",
      index_k = 2L,
      n = 10L,
      seed = 1L,
      graph_family = "sknn",
      k = 3L,
      prune_method = "none",
      radius_rank = NA_integer_,
      k_scale = NA_integer_,
      radius_rule = NA_character_,
      radius_factor = NA_real_,
      stage = stage,
      setting_id = setting_id,
      status = "ok",
      error = NA_character_,
      scale = 1,
      rel_rms_error = 0.2,
      rel_abs_error_median = 0.16,
      rel_abs_error_q95 = 0.3,
      distortion_q05 = 0.8,
      distortion_median = 1,
      distortion_q95 = 1.2,
      pearson_cor = 0.97,
      spearman_cor = 0.96,
      target = "sample_oracle",
      stringsAsFactors = FALSE
    )
  )
  utils::write.csv(metrics, file.path(run, "metrics.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(
      dataset_id = dataset_id,
      setting_id = setting_id,
      stage = stage,
      n_vertices = 10L,
      n_edges_raw_repaired = 9L,
      n_components_raw_repaired = 1L,
      stringsAsFactors = FALSE
    ),
    file.path(run, "graph_diagnostics.csv"),
    row.names = FALSE
  )

  graph_settings <- data.frame(
    dataset_id = dataset_id,
    setting_id = setting_id,
    surface = "paraboloid",
    index_k = 2L,
    n = 10L,
    seed = 1L,
    graph_family = "sknn",
    k = 3L,
    radius_rank = NA_integer_,
    k_scale = NA_integer_,
    radius_rule = NA_character_,
    radius_factor = NA_real_,
    prune_method = "none",
    stage = stage,
    stringsAsFactors = FALSE
  )
  manifest <- list(
    version = "1",
    project = "quadform_test",
    mode = "test",
    run_dir = run,
    dataset_manifest_file = file.path(run, "dataset_manifest.csv"),
    metrics_file = file.path(run, "metrics.csv"),
    graph_diagnostics_file = file.path(run, "graph_diagnostics.csv"),
    dataset_assets_file = file.path(run, "dataset_assets.csv"),
    graph_assets_file = file.path(run, "graph_assets.csv"),
    layout_assets_file = file.path(run, "layout_assets.csv"),
    graph_settings = graph_settings
  )
  saveRDS(manifest, file.path(run, "quadform_benchmark_manifest.rds"))
  writeLines("{}", file.path(run, "quadform_benchmark_manifest.json"))

  if (isTRUE(missing_graph)) {
    unlink(graph_file, force = TRUE)
  }
  if (isTRUE(missing_layout)) {
    unlink(layout_file, force = TRUE)
  }

  list(
    run = run,
    dataset_id = dataset_id,
    setting_id = setting_id,
    stage = stage,
    key = gflowui:::quadform_stage_key(dataset_id, setting_id, stage),
    dataset_file = dataset_file,
    graph_file = graph_file,
    layout_file = layout_file
  )
}

with_quadform_project_sandbox <- function(code) {
  db_dir <- tempfile("gflowui-quadform-projects-")
  dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("gflowui.projects_data_dir", NULL)
  options(gflowui.projects_data_dir = db_dir)
  on.exit({
    if (is.null(old_opt)) {
      options(gflowui.projects_data_dir = NULL)
    } else {
      options(gflowui.projects_data_dir = old_opt)
    }
    unlink(db_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  force(code)
}

test_that("quadform benchmark discovery stores paths and summaries, not expanded graph sets", {
  root <- tempfile("quadform-discovery-")
  fx <- make_quadform_fixture(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  discovered <- gflowui:::quadform_discover_benchmark_artifacts(fx$run)

  expect_equal(discovered$profile, "quadform_benchmark")
  expect_true(is.list(discovered$metadata$quadform_benchmark))
  expect_equal(discovered$metadata$quadform_benchmark$summary$graph_assets$n_rows, 1L)
  expect_equal(length(discovered$graph_sets), 1L)
  expect_equal(discovered$graph_sets[[1]]$id, "quadform_benchmark")
})

test_that("quadform normalized index joins selector metadata and resolves exact rows", {
  root <- tempfile("quadform-index-")
  fx <- make_quadform_fixture(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  discovered <- gflowui:::quadform_discover_benchmark_artifacts(fx$run)
  idx <- gflowui:::quadform_index_from_metadata(discovered$metadata$quadform_benchmark)

  expect_null(idx$error)
  expect_true(all(c("surface", "n", "seed", "k", "prune_method") %in% names(idx$index)))
  sel <- gflowui:::quadform_selector_state(idx$index, list(
    surface = "paraboloid",
    n = "10",
    seed = "1",
    graph_family = "sknn",
    k = "3",
    prune_method = "none",
    stage = "raw.repaired"
  ))
  expect_equal(sel$status, "ok")
  expect_equal(sel$key, fx$key)

  graph_row <- gflowui:::quadform_exact_graph_row(idx, fx$key)
  layout_row <- gflowui:::quadform_exact_layout_row(idx, fx$key)
  expect_equal(graph_row$status, "ok")
  expect_equal(layout_row$status, "ok")
  expect_equal(graph_row$row$graph_asset_file[[1]], normalizePath(fx$graph_file, mustWork = FALSE))
  expect_equal(layout_row$row$layout_asset_file[[1]], normalizePath(fx$layout_file, mustWork = FALSE))
})

test_that("quadform optimal selector picks smallest metric error and reports parameters", {
  root <- tempfile("quadform-optimal-")
  fx <- make_quadform_fixture(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  setting2 <- "g0002"
  graph2 <- file.path(fx$run, "assets", "graphs", fx$dataset_id, setting2, "raw_repaired.rds")
  layout2 <- file.path(fx$run, "assets", "layouts", fx$dataset_id, setting2, "raw_repaired_weighted_grip_3d.rds")
  dir.create(dirname(graph2), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(layout2), recursive = TRUE, showWarnings = FALSE)
  file.copy(fx$graph_file, graph2)
  file.copy(fx$layout_file, layout2)

  graph_assets <- utils::read.csv(file.path(fx$run, "graph_assets.csv"), stringsAsFactors = FALSE)
  graph_assets <- rbind(
    graph_assets,
    data.frame(
      dataset_id = fx$dataset_id,
      setting_id = setting2,
      stage = fx$stage,
      graph_family = "sknn",
      graph_asset_file = graph2,
      n_vertices = 10L,
      n_edges = 9L,
      n_components = 1L,
      stringsAsFactors = FALSE
    )
  )
  utils::write.csv(graph_assets, file.path(fx$run, "graph_assets.csv"), row.names = FALSE)

  layout_assets <- utils::read.csv(file.path(fx$run, "layout_assets.csv"), stringsAsFactors = FALSE)
  layout_assets <- rbind(
    layout_assets,
    data.frame(
      dataset_id = fx$dataset_id,
      setting_id = setting2,
      stage = fx$stage,
      method = "weighted_grip",
      layout_asset_file = layout2,
      n_vertices = 10L,
      stringsAsFactors = FALSE
    )
  )
  utils::write.csv(layout_assets, file.path(fx$run, "layout_assets.csv"), row.names = FALSE)

  metrics <- utils::read.csv(file.path(fx$run, "metrics.csv"), stringsAsFactors = FALSE)
  best <- metrics[metrics$target == "surface", , drop = FALSE][1, , drop = FALSE]
  best$setting_id <- setting2
  best$k <- 5L
  best$rel_rms_error <- 0.01
  metrics <- rbind(metrics, best)
  utils::write.csv(metrics, file.path(fx$run, "metrics.csv"), row.names = FALSE)

  manifest <- readRDS(file.path(fx$run, "quadform_benchmark_manifest.rds"))
  gs2 <- manifest$graph_settings[1, , drop = FALSE]
  gs2$setting_id <- setting2
  gs2$k <- 5L
  manifest$graph_settings <- rbind(manifest$graph_settings, gs2)
  saveRDS(manifest, file.path(fx$run, "quadform_benchmark_manifest.rds"))

  discovered <- gflowui:::quadform_discover_benchmark_artifacts(fx$run)
  idx <- gflowui:::quadform_index_from_metadata(discovered$metadata$quadform_benchmark)
  sel <- gflowui:::quadform_benchmark_selector_state(idx$index, idx$metrics, list(
    selection_mode = "optimal",
    metric_target = "surface",
    surface = "paraboloid",
    n = "10"
  ))

  expect_equal(sel$status, "ok")
  expect_equal(sel$mode, "optimal")
  expect_equal(as.character(sel$optimal_metric$setting_id[[1]]), setting2)
  expect_equal(as.character(sel$row$setting_id[[1]]), setting2)
  expect_equal(as.character(sel$row$k[[1]]), "5")
  expect_equal(sel$key, gflowui:::quadform_stage_key(fx$dataset_id, setting2, fx$stage))
})

test_that("quadform parsers read graph stages and matrix/list layouts", {
  root <- tempfile("quadform-parsers-")
  fx <- make_quadform_fixture(root, layout_as_list = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  graph <- gflowui:::quadform_parse_graph_asset(fx$graph_file)
  expect_equal(graph$status, "ok")
  expect_true(is.list(graph$adj_list))
  expect_true(is.list(graph$weight_list))

  layout_list <- gflowui:::quadform_parse_layout_asset(fx$layout_file)
  expect_equal(layout_list$status, "ok")
  expect_equal(dim(layout_list$coords), c(10L, 3L))

  matrix_file <- file.path(root, "layout_matrix.rds")
  saveRDS(matrix(seq_len(30), nrow = 10, ncol = 3), matrix_file)
  layout_matrix <- gflowui:::quadform_parse_layout_asset(matrix_file)
  expect_equal(layout_matrix$status, "ok")
  expect_equal(dim(layout_matrix$coords), c(10L, 3L))
})

test_that("quadform missing listed assets produce clear missing states", {
  root <- tempfile("quadform-missing-")
  fx <- make_quadform_fixture(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  unlink(fx$layout_file, force = TRUE)
  expect_equal(gflowui:::quadform_parse_layout_asset(fx$layout_file)$status, "missing_layout")

  unlink(fx$graph_file, force = TRUE)
  expect_equal(gflowui:::quadform_parse_graph_asset(fx$graph_file)$status, "missing_graph")
})

test_that("quadform layout generation uses weighted function and caches result", {
  root <- tempfile("quadform-generate-")
  fx <- make_quadform_fixture(root, missing_layout = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  with_quadform_project_sandbox({
    cache_path <- gflowui:::quadform_generated_layout_cache_path("quadform_test", fx$key)
    fake_weighted <- function(adj_list, weight_list, dim, rounds, final_rounds, seed) {
      cbind(seq_along(adj_list), seq_along(adj_list) * 0, seq_along(adj_list) * 0 + 1)
    }

    out <- gflowui:::quadform_generate_weighted_layout(
      graph_asset_path = fx$graph_file,
      output_path = cache_path,
      weighted_layout_fun = fake_weighted
    )

    expect_equal(out$status, "ok")
    expect_true(file.exists(cache_path))
    parsed <- gflowui:::quadform_parse_layout_asset(cache_path)
    expect_equal(parsed$status, "ok")
    expect_equal(dim(parsed$coords), c(10L, 3L))
  })
})

test_that("quadform layout generation is unavailable without weighted GRIP", {
  root <- tempfile("quadform-generate-unavailable-")
  fx <- make_quadform_fixture(root, missing_layout = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  with_quadform_project_sandbox({
    out <- gflowui:::quadform_generate_weighted_layout(
      graph_asset_path = fx$graph_file,
      output_path = gflowui:::quadform_generated_layout_cache_path("quadform_test", fx$key),
      weighted_layout_fun = NULL
    )

    expect_equal(out$status, "unavailable")
    expect_match(out$message, "grip.layout.weighted", fixed = TRUE)
  })
})

test_that("quadform benchmark project opens and view state settles", {
  root <- tempfile("quadform-app-")
  fx <- make_quadform_fixture(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  with_quadform_project_sandbox({
    gflowui::register_project(
      project_root = fx$run,
      project_id = "quadform_test",
      project_name = "Quadform Test",
      profile = "quadform_benchmark",
      overwrite = TRUE
    )

    shiny::testServer(gflowui:::app_server, {
      open_project("quadform_test")
      settled <- FALSE
      for (ii in seq_len(12)) {
        if (!isTRUE(session$flushReact())) {
          settled <- TRUE
          break
        }
      }
      expect_true(settled)
      expect_true(quadform_project_active())
      st <- quadform_view_state()
      expect_equal(st$status, "ok")
      expect_equal(st$key, fx$key)
      expect_equal(sort(as.character(st$metrics$target)), c("sample_oracle", "surface"))

      controls_html <- paste(as.character(output$workflow_controls), collapse = "")
      expect_match(controls_html, "Quadform Benchmark", fixed = TRUE)
      expect_match(controls_html, "Selection mode", fixed = TRUE)
      expect_match(controls_html, "Optimal", fixed = TRUE)
      expect_match(controls_html, "Surface", fixed = TRUE)
      expect_match(controls_html, "Graph family", fixed = TRUE)
      expect_false(grepl("Data Type:", controls_html, fixed = TRUE))
    })
  })
})

test_that("quadform app reports listed missing graph without reconstruction", {
  root <- tempfile("quadform-app-missing-")
  fx <- make_quadform_fixture(root, missing_graph = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  with_quadform_project_sandbox({
    gflowui::register_project(
      project_root = fx$run,
      project_id = "quadform_missing_graph",
      project_name = "Quadform Missing Graph",
      profile = "quadform_benchmark",
      overwrite = TRUE
    )

    shiny::testServer(gflowui:::app_server, {
      open_project("quadform_missing_graph")
      session$flushReact()
      st <- quadform_view_state()
      expect_equal(st$status, "missing_graph")
      expect_false(file.exists(fx$graph_file))
    })
  })
})
