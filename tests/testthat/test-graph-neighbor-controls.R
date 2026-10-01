test_that("graphs without a neighbor parameter hide k controls but retain state", {
  state <- list(neighbor_parameter = FALSE, k_choices = "1", k_selected = 1L,
    optimal_choices = c("Criterion" = "criterion"), optimal_selected = "criterion")
  html <- as.character(gflowui:::gflowui_graph_neighbor_controls(state))
  expect_match(html, 'id="graph_k"', fixed = TRUE)
  expect_match(html, 'display: none;', fixed = TRUE)
  expect_match(html, 'aria-hidden="true"', fixed = TRUE)
  expect_match(html, 'Set Reference', fixed = TRUE)
  expect_false(grepl('>k:</span>', html, fixed = TRUE))
  expect_false(grepl('Optimal k:', html, fixed = TRUE))
  expect_false(grepl('graph_optimal_method', html, fixed = TRUE))
  state$neighbor_parameter <- NULL
  default_html <- as.character(gflowui:::gflowui_graph_neighbor_controls(state))
  state$neighbor_parameter <- TRUE
  expect_identical(default_html, as.character(gflowui:::gflowui_graph_neighbor_controls(state)))
  expect_match(default_html, '>k:</span>', fixed = TRUE)
  expect_match(default_html, 'Optimal k:', fixed = TRUE)
  expect_false(grepl('display: none;', default_html, fixed = TRUE))
})

test_that("manifest normalization preserves the explicit neighbor flag", {
  gs <- gflowui:::gflowui_normalize_graph_sets_manifest(list(list(id="full", label="Full",
    neighbor_parameter=FALSE), list(id="knn", label="Neighbors")))
  expect_false(gflowui:::gflowui_has_neighbor_parameter(gs[[1]]))
  expect_true(gflowui:::gflowui_has_neighbor_parameter(gs[[2]]))
})
