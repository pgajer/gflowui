test_that("application object builds", {
  app <- gflowui::gflowui_app()
  expect_s3_class(app, "shiny.appobj")
})


test_that("graph adapter returns expected shape", {
  skip_if_not_installed("gflow")
  has_builder <- exists(
    "build.iknn.graphs.and.selectk",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  skip_if_not(has_builder)

  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph(x, kmin = 5, kmax = 9, method = "edit")
  expect_true(is.list(g))
  expect_true("selected.k" %in% names(g))
  expect_true("selected.graph" %in% names(g))
  expect_true("adj.list" %in% names(g))

  fit <- gflowui:::gflow_fit_condexp_stub(g, y = x[, 1])
  expect_equal(length(fit$fitted.values), nrow(x))

  ep <- gflowui:::gflow_detect_endpoints_stub(g)
  expect_true("endpoints" %in% names(ep))
})
