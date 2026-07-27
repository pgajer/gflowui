test_that("project specs preserve occupation-density assets", {
  root <- tempfile("gflowui-occupation-spec-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(),
    occupation_density_sets = list(list(
      id = "subject_od",
      subject_ids = c("S1", "S2")
    ))
  )

  expect_equal(spec$occupation_density_sets[[1L]]$id, "subject_od")
  expect_equal(spec$occupation_density_sets[[1L]]$subject_ids, c("S1", "S2"))
})

test_that("selected occupation-density estimates load and normalize", {
  root <- tempfile("gflowui-occupation-fit-")
  dir.create(file.path(root, "fits"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  rho <- array(
    c(1, 2, 1, 4),
    dim = c(2L, 1L, 2L),
    dimnames = list(
      vertex = c("v1", "v2"),
      subject = "S1",
      selector = c("minimum_brier", "minimum_bernoulli_nll")
    )
  )
  saveRDS(
    list(
      subject.id = "S1",
      rho = rho,
      selected = data.frame(
        subject.id = rep("S1", 2L),
        selector = c("minimum_brier", "minimum_bernoulli_nll"),
        graph.k = c(5L, 7L)
      )
    ),
    file.path(root, "fits", "selected.rds")
  )
  manifest <- list(
    project_root = root,
    occupation_density_sets = list(list(
      id = "subject_od",
      methods = list(list(
        id = "graph_heat_kernel",
        selected_fit_file = "fits/selected.rds"
      ))
    ))
  )

  result <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject_od",
    subject_id = "S1",
    method_id = "graph_heat_kernel",
    mode = "selected",
    selector = "minimum_bernoulli_nll"
  )

  expect_equal(result$values, c(0.2, 0.8))
  expect_equal(result$selected$graph.k, 7L)
})

test_that("precomputed heat paths expose every time and current top-K basins", {
  root <- tempfile("gflowui-precomputed-path-")
  dir.create(file.path(root, "paths"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  path <- list(
    subject.id = "15",
    settings = list(graph.k = 3L),
    probability.mass = matrix(
      c(
        0.40, 0.30, 0.20, 0.10,
        0.10, 0.20, 0.30, 0.40
      ),
      nrow = 4L,
      ncol = 2L
    ),
    path.summary = data.frame(
      eta.index = 1:2,
      eta = c(0.5, 1.5),
      log.eta = log(c(0.5, 1.5)),
      mean.brier = c(0.2, 0.1),
      brier.selected = c(FALSE, TRUE)
    ),
    field.index = data.frame(
      field.id = c("eta_01", "eta_02"),
      path.parameter.index = 1:2
    ),
    raw.basins = data.frame(
      path.parameter.index = c(1L, 1L, 1L, 2L, 2L),
      raw.basin.id = c("a", "b", "c", "d", "e"),
      basin.mass = c(0.5, 0.4, 0.1, 0.6, 0.4),
      support.count = c(2L, 1L, 1L, 2L, 2L),
      peak.vertex.id = c(1L, 3L, 4L, 3L, 1L)
    ),
    assignments = data.frame(
      field.id = rep(c("eta_01", "eta_02"), each = 4L),
      point.id = rep(1:4, 2L),
      raw.basin.id = c("a", "a", "b", "c", "e", "e", "d", "d")
    )
  )
  saveRDS(path, file.path(root, "paths", "subject15.rds"))

  manifest <- list(
    project_root = root,
    occupation_density_sets = list(list(
      id = "subject15_path",
      methods = list(list(
        id = "graph_heat_kernel",
        source = "precomputed_path",
        path_file = "paths/subject15.rds"
      ))
    ))
  )

  density <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject15_path",
    subject_id = "15",
    method_id = "graph_heat_kernel",
    mode = "parameters",
    parameters = list(eta_index = 1L, display_mode = "density")
  )
  expect_equal(density$values, c(0.4, 0.3, 0.2, 0.1))
  expect_equal(density$selected_eta_index, 2L)
  expect_equal(density$selected$eta, 0.5)
  expect_identical(density$color_type, "numeric")

  basins <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject15_path",
    subject_id = "15",
    method_id = "graph_heat_kernel",
    mode = "parameters",
    parameters = list(
      eta_index = 1L,
      display_mode = "top_k_basins",
      top_k = 2L
    )
  )
  expect_identical(basins$color_type, "categorical")
  expect_equal(basins$top_k, 2L)
  expect_equal(basins$basin_count, 3L)
  expect_equal(basins$basin_table$basin, c("a", "b"))
  expect_equal(
    basins$values,
    c(
      "Basin 01 (mass 0.500)",
      "Basin 01 (mass 0.500)",
      "Basin 02 (mass 0.400)",
      "Other basins"
    )
  )
})
