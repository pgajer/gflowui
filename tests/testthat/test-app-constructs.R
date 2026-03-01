test_that("application object builds", {
  app <- gflowui::gflowui_app()
  expect_s3_class(app, "shiny.appobj")
})


test_that("adapter stubs return expected shape", {
  skip_if_not_installed("gflow")
  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph_stub(x, kmin = 5, kmax = 9, method = "both")
  expect_true(is.list(g))
  expect_true("selected.k" %in% names(g))

  fit <- gflowui:::gflow_fit_condexp_stub(g, y = x[, 1])
  expect_equal(length(fit$fitted.values), nrow(x))

  ep <- gflowui:::gflow_detect_endpoints_stub(g)
  expect_true("endpoints" %in% names(ep))
})
