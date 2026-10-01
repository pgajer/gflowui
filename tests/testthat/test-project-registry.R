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


test_that("build_project_spec_iknn_3x3 normalizes rich project inputs", {
  root <- tempfile("iknn-3x3-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  graph_file <- file.path(root, "results", "iknn.selection.rds")
  dir.create(dirname(graph_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), graph_file)

  metadata_file <- file.path(root, "data", "S_asv.rda")
  dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
  mt.asv <- data.frame(
    CST = c("I", "II"),
    subCST = c("I-A", "II-A"),
    stringsAsFactors = FALSE
  )
  save(mt.asv, file = metadata_file)

  report_file <- file.path(root, "reports", "iknn_report.pdf")
  dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("report", report_file, useBytes = TRUE)

  labels_file <- file.path(root, "results", "landmark_labels.csv")
  utils::write.csv(
    data.frame(vertex = c(1L, 2L), label = c("L1", "L2")),
    labels_file,
    row.names = FALSE
  )

  X <- matrix(seq_len(6), nrow = 3, ncol = 2)
  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(list(
      id = "all",
      label = "All Samples",
      graph_file = graph_file,
      k_values = 3:5
    )),
    X = X,
    factor_sets = list(list(
      id = "cst",
      label = "CST",
      metadata_file = metadata_file,
      columns = c("CST", "subCST")
    )),
    ordered_factor_sets = list(list(
      id = "subject_order",
      columns = c("subject_id", "visit_order")
    )),
    cont_vars_sets = list(list(
      id = "clinical",
      columns = c("pH", "age")
    )),
    landmark_pts_runs = list(list(
      id = "dcst_landmarks",
      label = "DCST Landmarks",
      landmark_labels_csv = labels_file,
      k_values = 5L
    )),
    doc_sets = list(list(
      id = "report_pdf",
      label = "Report PDF",
      path = report_file
    )),
    defaults = list(graph_set_id = "all")
  )

  expect_equal(spec$profile, "iknn_3x3")
  expect_true(file.exists(spec$metadata$reference_data$path))
  expect_equal(spec$metadata$reference_data$n_samples, 3L)
  expect_equal(spec$metadata$reference_data$n_features, 2L)
  expect_equal(
    sort(vapply(spec$metadata$annotation_sets, function(x) as.character(x$value_type), character(1))),
    c("continuous", "factor", "ordered_factor")
  )
  expect_equal(spec$artifacts$documents[[1]]$id, "report_pdf")
  expect_equal(spec$endpoint_runs[[1]]$kind, "landmark_points")
  expect_equal(
    spec$endpoint_runs[[1]]$labels_csv,
    normalizePath(labels_file, mustWork = FALSE)
  )
  expect_equal(spec$defaults$graph_set_id, "all")
})


test_that("register_project supports project_spec precedence and 3x3 alias", {
  db_dir <- tempfile("gflowui-projects-iknn-")
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

  root <- tempfile("iknn-project-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  graph_file <- file.path(root, "results", "iknn.selection.rds")
  dir.create(dirname(graph_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(dummy = TRUE), graph_file)

  report_file <- file.path(root, "reports", "iknn_report.pdf")
  dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("report", report_file, useBytes = TRUE)

  labels_file <- file.path(root, "results", "landmark_labels.csv")
  utils::write.csv(
    data.frame(vertex = c(1L, 2L), label = c("L1", "L2")),
    labels_file,
    row.names = FALSE
  )

  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(list(
      id = "all",
      label = "All Samples",
      graph_file = graph_file,
      k_values = 3:5
    )),
    landmark_pts_runs = list(list(
      id = "dcst_landmarks",
      label = "DCST Landmarks",
      landmark_labels_csv = labels_file,
      k_values = 5L
    )),
    doc_sets = list(list(
      id = "report_pdf",
      label = "Report PDF",
      path = report_file
    ))
  )

  wrong_graph <- file.path(root, "results", "wrong.selection.rds")
  saveRDS(list(dummy = FALSE), wrong_graph)

  reg_result <- gflowui::register_project(
    project_root = root,
    project_id = "iknn_project",
    project_name = "IKNN Project",
    profile = "3x3",
    project_spec = spec,
    graph_sets = list(list(
      id = "wrong",
      label = "Wrong Graph",
      graph_file = wrong_graph,
      k_values = 7L
    )),
    metadata = list(extra = list(flag = TRUE)),
    defaults = list(graph_set_id = "all"),
    overwrite = TRUE
  )

  manifest <- reg_result$manifest
  expect_equal(manifest$profile, "iknn_3x3")
  expect_equal(manifest$graph_sets[[1]]$id, "all")
  expect_equal(manifest$endpoint_runs[[1]]$id, "dcst_landmarks")
  expect_true(isTRUE(manifest$metadata$extra$flag))
  expect_equal(manifest$defaults$graph_set_id, "all")
  expect_true(is.list(manifest$artifacts$documents))

  listed <- gflowui::list_projects(include_manifests = TRUE)
  expect_equal(listed$manifests[["iknn_project"]]$profile, "iknn_3x3")
})


test_that("register_project normalizes top-level landmark_pts_runs alias", {
  db_dir <- tempfile("gflowui-projects-landmarks-")
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

  project_root <- tempfile("external-project-landmarks-")
  dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

  graph_file <- file.path(project_root, "graph_set.rds")
  saveRDS(list(dummy = TRUE), graph_file)

  labels_file <- file.path(project_root, "landmark_labels.csv")
  utils::write.csv(
    data.frame(vertex = c(1L, 2L), label = c("A", "B")),
    labels_file,
    row.names = FALSE
  )

  reg_result <- gflowui::register_project(
    project_root = project_root,
    project_name = "Landmark Project",
    project_id = "landmark_project",
    profile = "iknn_3x3",
    scan_results = FALSE,
    graph_sets = list(list(
      id = "set_a",
      label = "Set A",
      graph_file = graph_file,
      k_values = c(5L, 7L)
    )),
    landmark_pts_runs = list(list(
      id = "lm_k05",
      label = "Landmarks (k=5)",
      landmark_labels_csv = labels_file,
      k_values = 5L
    )),
    overwrite = TRUE
  )

  expect_equal(reg_result$manifest$profile, "iknn_3x3")
  expect_equal(reg_result$manifest$endpoint_runs[[1]]$id, "lm_k05")
  expect_equal(reg_result$manifest$endpoint_runs[[1]]$kind, "landmark_points")
})


test_that("register_project normalizes graph-set metadata and ignores legacy html variants", {
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
  expect_equal(gs$layout_assets$presets$color_by, "vertex_degree")
  expect_equal(gs$layout_assets$presets$vertex_color, "#111827")
  expect_true(is.list(gs$layout_assets$variants))
  expect_length(gs$layout_assets$variants, 0L)
  expect_true(is.list(gs$layout_assets$grip_layouts))
  expect_true(length(gs$layout_assets$grip_layouts) >= 1L)
  expect_true(any(vapply(gs$layout_assets$grip_layouts, function(x) identical(as.integer(x$k), 7L), logical(1))))
})


test_that("normalize_graph_set_manifest preserves solid-color presets", {
  root <- tempfile("solid-color-preset-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  graph_file <- file.path(root, "graph_set.rds")
  saveRDS(list(dummy = TRUE), graph_file)

  gs <- gflowui:::gflowui_normalize_graph_set_manifest(list(
    id = "set_a",
    label = "Set A",
    graph_file = graph_file,
    layout_assets = list(
      presets = list(
        color_by = "solid_color",
        vertex_color = "#374151"
      )
    )
  ))

  expect_equal(gs$layout_assets$presets$color_by, "solid_color")
  expect_equal(gs$layout_assets$presets$vertex_color, "#374151")
})
