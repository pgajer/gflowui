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

test_that("density colors use a bounded log scale without changing raw mass", {
  mass <- c(0, 1e-12, 1e-8, 1e-6, 1e-4, 1e-2, NA_real_)
  encoded <- gflowui:::gflowui_numeric_color_encoding(
    mass,
    transform = "density_log10",
    title = "Probability mass"
  )

  expect_equal(encoded$raw_values, mass)
  expect_equal(encoded$floor_value, 1e-8)
  expect_equal(encoded$mapped_values[1:3], rep(-8, 3))
  expect_true(all(diff(encoded$mapped_values[3:6]) > 0))
  expect_equal(encoded$color_limits, c(-8, -2))
  expect_match(encoded$colorbar$title, "log10 color", fixed = TRUE)
  expect_match(encoded$colorbar$ticktext[[1L]], "<=", fixed = TRUE)

  uniform <- gflowui:::gflowui_numeric_color_encoding(
    rep(0.25, 4),
    transform = "density_log10",
    title = "Probability mass"
  )
  expect_equal(uniform$mapped_values, rep(log10(0.25), 4))
  expect_equal(
    uniform$color_limits,
    c(log10(0.25) - 6, log10(0.25))
  )

  identity <- gflowui:::gflowui_numeric_color_encoding(
    mass,
    transform = "identity",
    title = "Value"
  )
  expect_equal(identity$mapped_values, mass)
  expect_identical(identity$colorbar$title, "Value")
})

test_that("density palettes expose selectable endpoints and midpoint", {
  expect_equal(
    gflowui:::gflowui_density_palette(),
    c("#FDE725", "#C51B1D")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(midpoint = "blue"),
    c("#FDE725", "#2563EB", "#C51B1D")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(
      low = "blue",
      midpoint = "white",
      high = "orange"
    ),
    c("#2563EB", "#F8FAFC", "#F97316")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(
      low = "unknown",
      midpoint = "unknown",
      high = "unknown"
    ),
    c("#FDE725", "#C51B1D")
  )

  scale <- gflowui:::gflowui_plotly_colorscale(
    gflowui:::gflowui_density_palette(midpoint = "blue")
  )
  expect_equal(vapply(scale, `[[`, character(1), 1L), c("0", "0.5", "1"))
  expect_equal(
    vapply(scale, `[[`, character(1), 2L),
    c("#FDE725", "#2563EB", "#C51B1D")
  )
})

test_that("strict graph-local density extrema are ranked deterministically", {
  adj_list <- list(
    c(2L, 3L),
    c(1L, 3L),
    c(1L, 2L, 4L),
    c(3L, 5L),
    4L,
    integer(0)
  )
  extrema <- gflowui:::gflowui_density_local_extrema(
    c(5, 1, 3, 0, 4, 9),
    adj_list
  )

  expect_equal(
    extrema,
    data.frame(
      vertex = c(1L, 5L, 4L, 2L),
      value = c(5, 4, 0, 1),
      type = c("maximum", "maximum", "minimum", "minimum"),
      rank = c(1L, 2L, 1L, 2L),
      label = c("M_1", "M_2", "m_1", "m_2"),
      stringsAsFactors = FALSE
    )
  )

  plateau <- gflowui:::gflowui_density_local_extrema(
    c(0, 0, 1),
    list(2L, c(1L, 3L), 2L)
  )
  expect_equal(plateau$vertex, 3L)
  expect_equal(plateau$label, "M_1")
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
