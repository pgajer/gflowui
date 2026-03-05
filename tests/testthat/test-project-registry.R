test_that("register/list/unregister roundtrip persists manifest and registry", {
  db_dir <- tempfile("gflowui-projects-")
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

  project_root <- tempfile("external-project-")
  dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

  reg_result <- gflowui::register_project(
    project_root = project_root,
    project_name = "Test Project",
    profile = "custom",
    scan_results = FALSE
  )

  reg <- gflowui::list_projects()
  expect_equal(nrow(reg), 1L)
  expect_equal(reg$id[[1]], reg_result$project_id)
  expect_equal(reg$label[[1]], "Test Project")
  expect_false(reg$has_graphs[[1]])
  expect_false(reg$has_condexp[[1]])
  expect_false(reg$has_endpoints[[1]])
  expect_true(file.exists(reg_result$manifest_file))

  listed <- gflowui::list_projects(include_manifests = TRUE)
  expect_true(is.list(listed$manifests))
  expect_true(reg_result$project_id %in% names(listed$manifests))
  expect_equal(listed$manifests[[reg_result$project_id]]$project_name, "Test Project")

  removed <- gflowui::unregister_project(reg_result$project_id)
  expect_true(isTRUE(removed))
  expect_equal(nrow(gflowui::list_projects()), 0L)
  expect_false(file.exists(reg_result$manifest_file))
})


test_that("discover_project_artifacts finds symptoms_restart outputs", {
  root <- tempfile("symptoms-restart-")
  results_root <- file.path(root, "results")

  dir.create(file.path(results_root, "asv_hv_k_gcv_sweep", "top20"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), file.path(results_root, "asv_hv_k_gcv_sweep", "top20", "iknn.selection.rds"))
  dir.create(file.path(results_root, "asv_hv_k_gcv_sweep", "top20", "layouts_3d_rds"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(matrix(seq_len(12), nrow = 4, ncol = 3), file.path(results_root, "asv_hv_k_gcv_sweep", "top20", "layouts_3d_rds", "top20_k05_layout3d.rds"))

  dir.create(file.path(results_root, "asv_full_graph_hv_criteria_k_selection"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), file.path(results_root, "asv_full_graph_hv_criteria_k_selection", "asv.full.iknn.selection.rds"))

  dir.create(file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "fits"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(k = c(5L, 7L), gcv = c(0.42, 0.39)),
    file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "vag_odor_gcv_by_k.csv"),
    row.names = FALSE
  )
  saveRDS(
    list(dummy = TRUE),
    file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "fits", "vag_odor_fit_k05.rds")
  )

  dir.create(file.path(results_root, "asv_full_graph_evenness_endpoints_k05"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(k = c(5L), endpoint.count = c(4L)),
    file.path(results_root, "asv_full_graph_evenness_endpoints_k05", "evenness.endpoint.summary.k05.csv"),
    row.names = FALSE
  )

  discovered <- gflowui::discover_project_artifacts(root, profile = "symptoms_restart")

  expect_equal(discovered$profile, "symptoms_restart")
  expect_true(length(discovered$graph_sets) >= 1L)
  expect_true(any(vapply(discovered$graph_sets, function(x) identical(x$id, "top20"), logical(1))))
  expect_true(any(vapply(discovered$graph_sets, function(x) identical(x$id, "all"), logical(1))))
  top20 <- discovered$graph_sets[[which(vapply(discovered$graph_sets, function(x) identical(x$id, "top20"), logical(1)))[1]]]
  expect_equal(top20$data_type_id, "top20")
  expect_equal(top20$data_type_label, "ASV-top20")
  expect_equal(top20$n_features, 20L)
  expect_true(is.list(top20$optimal_k_artifacts))
  expect_true("response_gcv" %in% names(top20$optimal_k_artifacts))
  expect_true(is.list(top20$layout_assets$presets))
  expect_true(is.list(top20$layout_assets$grip_layouts))
  expect_true(length(top20$layout_assets$grip_layouts) >= 1L)
  expect_true(any(vapply(top20$layout_assets$grip_layouts, function(x) identical(as.integer(x$k), 5L), logical(1))))

  expect_equal(length(discovered$condexp_sets), 1L)
  expect_equal(discovered$condexp_sets[[1]]$id, "vag_odor_binary")
  expect_true(5L %in% discovered$condexp_sets[[1]]$k_values)

  expect_equal(length(discovered$endpoint_runs), 1L)
  expect_equal(discovered$endpoint_runs[[1]]$id, "evenness_k05")
})


test_that("discover_project_artifacts finds agp_restart outputs", {
  root <- tempfile("agp-restart-")
  base <- file.path(root, "results", "asv_hv_k_gcv_sweep")

  dir.create(file.path(base, "shared_graphs_all_asv"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), file.path(base, "shared_graphs_all_asv", "iknn.selection.rds"))

  dir.create(file.path(base, "top20"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(k.requested = c(7L, 12L, 30L)),
    file.path(base, "top20", "k.status.csv"),
    row.names = FALSE
  )

  bench_dir <- file.path(base, "ibs_ibd_benchmark_k071230")
  dir.create(bench_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), file.path(bench_dir, "ibs_ibd_conditional_expectation.long.rds"))
  utils::write.csv(
    data.frame(k = c(7L, 12L), outcome = c("ibs", "ibd")),
    file.path(bench_dir, "ibs_ibd_gcv_summary.csv"),
    row.names = FALSE
  )

  endpoint_dir <- file.path(base, "evenness_endpoints_k07")
  dir.create(file.path(endpoint_dir, "per_k"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), file.path(endpoint_dir, "per_k", "evenness_k07_endpoints.rds"))
  utils::write.csv(
    data.frame(k = c(7L), endpoint.method = "evenness"),
    file.path(endpoint_dir, "evenness_endpoint_summary.csv"),
    row.names = FALSE
  )

  discovered <- gflowui::discover_project_artifacts(root, profile = "agp_restart")

  expect_equal(discovered$profile, "agp_restart")
  expect_true(any(vapply(discovered$graph_sets, function(x) identical(x$id, "shared_all_asv"), logical(1))))
  shared <- discovered$graph_sets[[which(vapply(discovered$graph_sets, function(x) identical(x$id, "shared_all_asv"), logical(1)))[1]]]
  expect_equal(shared$data_type_id, "shared_all_asv")
  expect_equal(shared$data_type_label, "ASV")
  expect_true(is.list(shared$layout_assets$presets))
  expect_true(any(vapply(discovered$condexp_sets, function(x) identical(x$id, "ibs_ibd_benchmark_k071230"), logical(1))))
  expect_true(any(vapply(discovered$endpoint_runs, function(x) identical(x$id, "evenness_endpoints_k07"), logical(1))))
  expect_equal(discovered$defaults$graph_set_id, "shared_all_asv")
})


test_that("register_project normalizes graph-set metadata and infers layout variants", {
  db_dir <- tempfile("gflowui-projects-meta-")
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

  project_root <- tempfile("external-project-meta-")
  dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

  graph_file <- file.path(project_root, "graph_set.rds")
  saveRDS(list(dummy = TRUE), graph_file)
  html_file <- file.path(project_root, "graph_point_1.5x_color_degree_k07.html")
  writeLines("<html><body>variant</body></html>", html_file)
  layout_file <- file.path(project_root, "set_a_k07_layout3d.rds")
  saveRDS(matrix(seq_len(15), nrow = 5, ncol = 3), layout_file)

  gflowui::register_project(
    project_root = project_root,
    project_name = "Meta Project",
    project_id = "meta_project",
    profile = "custom",
    scan_results = FALSE,
    graph_sets = list(list(
      id = "set_a",
      label = "Set A",
      graph_file = graph_file,
      html_file = html_file,
      k_values = c(7L, 9L)
    )),
    overwrite = TRUE
  )

  listed <- gflowui::list_projects(include_manifests = TRUE)
  manifest <- listed$manifests[["meta_project"]]
  gs <- manifest$graph_sets[[1]]

  expect_equal(gs$data_type_id, "set_a")
  expect_equal(gs$data_type_label, "Set A")
  expect_true(is.list(gs$layout_assets$presets))
  expect_equal(gs$layout_assets$presets$renderer, "rglwidget")
  expect_true(is.list(gs$layout_assets$variants))
  expect_true(length(gs$layout_assets$variants) >= 1L)
  expect_true(is.list(gs$layout_assets$grip_layouts))
  expect_true(length(gs$layout_assets$grip_layouts) >= 1L)
  expect_true(any(vapply(gs$layout_assets$grip_layouts, function(x) identical(as.integer(x$k), 7L), logical(1))))

  vv <- gs$layout_assets$variants[[1]]
  expect_equal(vv$renderer, "html")
  expect_equal(vv$vertex_layout, "point")
  expect_equal(vv$vertex_size, "1.5x")
  expect_equal(vv$color_by, "degree_k07")
  expect_equal(vv$k, 7L)
  expect_true(file.exists(vv$path))
})
