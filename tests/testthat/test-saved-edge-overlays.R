test_that("saved repair overlays respect vertex filtering and legend visibility", {
  skip_if_not_installed("plotly")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  edges <- matrix(c(1, 2, 2, 3), 2, 2, byrow = TRUE)
  saveRDS(list(list(edges = edges, label = "MST repair", color = "#d1495b",
                   width = 5, visible = TRUE)), path)
  coords <- matrix(seq_len(9), 3, 3)
  draw <- function(visible) plotly::plotly_build(gflowui_add_saved_edge_overlays(
    plotly::plot_ly(), coords, visible, path))$x$data[[1]]
  all <- draw(1:3)
  expect_equal(as.numeric(all$x), c(1, 2, NA, 2, 3))
  expect_equal(all$line$color, "#d1495b")
  expect_true(all$visible)
  expect_equal(as.numeric(draw(1:2)$x), c(1, 2))
  saveRDS(list(list(edges = edges, label = "Original edges", color = "gray",
                   width = 1, visible = FALSE)), path)
  expect_identical(draw(1:3)$visible, "legendonly")
  expect_true(plotly::plotly_build(gflowui_add_saved_edge_overlays(
    plotly::plot_ly(), coords, 1:3, path))$x$layout$showlegend)
})

test_that("saved overlays reject endpoints outside the graph", {
  skip_if_not_installed("plotly")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(list(list(edges = matrix(c(1, 4), 1, 2))), path)
  expect_error(gflowui_add_saved_edge_overlays(plotly::plot_ly(),
    matrix(seq_len(9), 3, 3), 1:3, path), "invalid graph vertex indices")
})
