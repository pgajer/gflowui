test_that("opening defaults depend on the manifest, not project identity", {
  for (id in c("agp", "symptoms", "renamed_project")) {
    m <- list(project_id = id, project_name = id, defaults = list(
      open_graph_set_id = "second", open_graph_k = 7L,
      open_panels = "workflow_endpoint_structure"))
    expect_identical(gflowui_project_open_defaults(m), list(
      set_id = "second", k = 7L, open_panels = "workflow_endpoint_structure"))
  }
  expect_identical(gflowui_project_open_defaults(NULL),
    list(set_id = "", k = NA_integer_, open_panels = NULL))
})

test_that("folder names do not activate dataset importers", {
  root <- withr::local_tempdir()
  for (name in c("AGP", "symptoms", "unrelated")) {
    path <- file.path(root, name)
    dir.create(path)
    got <- discover_project_artifacts(path)
    expect_identical(got$profile, "custom")
    expect_length(got$graph_sets, 0L)
  }
  expect_error(discover_project_artifacts(root, "agp_restart"), "arg")
  expect_error(discover_project_artifacts(root, "symptoms_restart"), "arg")
})

test_that("explicit providers work for arbitrary project IDs and roots", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir = file.path(root, "registry")))
  X <- matrix(c(.9, .1, .2, .8), nrow = 2, byrow = TRUE,
    dimnames = list(c("s2", "s1"), c("feature1", "feature2")))
  tx <- c(feature1 = "Lactobacillus iners", feature2 = "Lactobacillus crispatus")
  rows <- data.frame(vertex = 1:2, subject_id = "P1", sample_id = c("s2", "s1"),
    week = 1L, day = 1:2, time_order = 1:2, visit_label = c("W1D1", "W1D2"),
    graph_set_id = "", representation_id = "")
  saveRDS(X, file.path(root, "matrix.rds"))
  saveRDS(tx, file.path(root, "taxonomy.rds"))
  saveRDS(rows, file.path(root, "subjects.rds"))
  manifest <- list(project_root = root, graph_sets = list(list(id = "main")),
    metadata = list(endpoint_label_provider = list(matrix_file = "matrix.rds",
      taxonomy_map_file = "taxonomy.rds"),
      subject_provider = list(rows_file = "subjects.rds")))
  shiny::testServer(app_server, {
    for (id in c("arbitrary_id", "symptoms", "agp")) {
      provider <- build_live_endpoint_label_provider(id, manifest)
      expect_identical(provider$X_by_graph_set$main, X)
      expect_identical(provider$sample_ids_by_graph_set$main, rownames(X))
      expect_identical(provider$taxonomy_map, tx)
      subjects <- build_live_subject_provider(id, manifest)
      expect_equal(subjects$rows, rows)
      # An identifier alone supplies neither labels nor subject data.
      empty <- list(project_root = root)
      expect_null(build_live_endpoint_label_provider(id, empty))
      expect_null(build_live_subject_provider(id, empty))
    }
  })
  saveRDS(c("unnamed taxonomy"), file.path(root, "taxonomy.rds"))
  expect_error(gflowui_manifest_taxonomy_map(
    manifest$metadata$endpoint_label_provider, root), "unique feature names")
})

test_that("retirement preserves assets, manifest and other registrations", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir = file.path(root, "registry")))
  asset <- file.path(root, "graph.rds")
  saveRDS(list(important = TRUE), asset)
  first <- register_project(root, "old", "Old", profile = "custom",
    scan_results = FALSE, graph_sets = list(list(id = "g", graph_file = asset)))
  register_project(root, "keep", "Keep", profile = "custom", scan_results = FALSE)
  original <- list_projects(include_manifests = TRUE)
  env <- new.env()
  sys.source(testthat::test_path("..", "..", "dev", "retire_project_registration.R"), env)
  archive <- env$retire_project_registration("old")
  expect_identical(list_projects()$id, "keep")
  expect_true(file.exists(first$manifest_file))
  expect_identical(readRDS(asset), list(important = TRUE))
  expect_identical(list_projects(include_manifests = TRUE)$manifests$keep,
    original$manifests$keep)
  expect_identical(readRDS(file.path(archive, "registration.rds"))$manifest,
    original$manifests$old)
})

test_that("color metadata requires explicit files, objects and columns", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  annotations <- data.frame(group = c("a", "b"))
  save(annotations, file = file.path(root, "data", "S_asv.rda"))
  h <- gflowui_make_server_renderer_helpers(new.env(), function() NULL)
  manifest <- list(project_root = root)
  expect_length(h$collect_reference_metadata_sources(manifest, list(), 2L), 0L)
  graph <- list(color_assets = list(metadata_file = "data/S_asv.rda",
    metadata_object = "annotations", vector_columns = "group"))
  out <- h$collect_reference_metadata_sources(manifest, graph, 2L)
  expect_identical(out$group$values, c("a", "b"))
})
