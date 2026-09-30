test_that("uniform display preserves distance ratios and constant axes", {
  h <- gflowui:::gflowui_make_server_renderer_helpers(new.env(), function(...) NULL)
  x <- cbind(c(0, 1, 2, 7), c(0, 20, 3, -5), c(4, 4, 4, 4))
  y <- h$normalize_coord_matrix(x, "uniform")
  centered <- sweep(x, 2L, colMeans(x), "-")
  expect_equal(as.vector(dist(y)) * max(abs(centered)), as.vector(dist(x)))
  expect_equal(colMeans(y), rep(0, 3), tolerance = 1e-14)
  expect_equal(y[, 3], rep(0, 4))
  expect_equal(h$normalize_coord_matrix(matrix(3, 4, 3), "uniform"), matrix(0, 4, 3))
  expect_equal(h$normalize_coord_matrix(x), h$normalize_coord_matrix(x, "axis"))
  x[1, 1] <- NA_real_
  expect_error(h$normalize_coord_matrix(x, "uniform"), "finite numeric")
})

test_that("layout manifest retains the shape-preserving setting", {
  gs <- gflowui:::gflowui_normalize_graph_sets_manifest(list(list(
    id = "test", label = "Test", layout_assets = list(coordinate_normalization = "uniform",
      presets = list(legend_position = "bottom"))
  )))
  expect_identical(gs[[1]]$layout_assets$coordinate_normalization, "uniform")
  expect_identical(gs[[1]]$layout_assets$presets$legend_position, "bottom")
})

test_that("compiled display components retain first-vertex labels and isolates", {
  skip_if_not_installed("igraph")
  adj <- list(c(3L, 4L), integer(), 1L, 1L, 6L, 5L, integer())
  expect_identical(gflowui:::gflowui_display_component_ids(adj), c(1L, 2L, 1L, 1L, 5L, 5L, 7L))
  expect_identical(gflowui:::gflowui_display_component_ids(adj),
                   dgraphs::graph.connected.components(adj))
  expect_identical(gflowui:::gflowui_display_component_ids(list(integer())), 1L)
})
