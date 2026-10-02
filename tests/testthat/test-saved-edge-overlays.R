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

test_that("lazy edges retain exact pairs without expanded geometry", {
  skip_if_not_installed("plotly")
  path<-tempfile(fileext=".rds");on.exit(unlink(path))
  pairs<-matrix(c(1,2,2,3),2,2,byrow=TRUE)
  saveRDS(list(list(edges=pairs,label="Edges",visible=FALSE,color="gray",width=1)),path)
  registry<-gflowui_edge_registry();coords<-matrix(1:9,3,3)
  draw<-function(ids=letters[1:3],visible=1:3)plotly::plotly_build(gflowui_add_saved_edge_overlays(
    plotly::plot_ly(),coords,visible,path,registry,ids))$x$data[[1]]
  a<-draw();key<-a$meta$gflowui_edges$key
  expect_length(a$x,1L)
  expect_identical(a$visible,"legendonly")
  expect_equal(registry$get(key),pairs)
  expect_identical(draw(visible=1:2)$meta$gflowui_edges$key,key)
  expect_false(identical(draw(rev(letters[1:3]))$meta$gflowui_edges$key,key))
  expect_null(registry$get("unregistered"))
  expect_identical(gflowui_scene_edge_keys(list(data=list(a))),key)
  expect_length(gflowui_scene_edge_keys(list(data=list(list()))),0L)
  saveRDS(list(list(edges=pairs[1,,drop=FALSE],label="Changed",visible=FALSE,color="gray",width=1)),path)
  b<-draw();expect_false(identical(b$meta$gflowui_edges$key,key))
  expect_equal(registry$get(b$meta$gflowui_edges$key),pairs[1,,drop=FALSE])
})
