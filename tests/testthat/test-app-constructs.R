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
  has_fit <- exists(
    "fit.rdgraph.regression",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  has_refit <- exists(
    "refit.rdgraph.regression",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  skip_if_not(has_fit && has_refit)

  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph(x, kmin = 5, kmax = 9, method = "edit")
  expect_true(is.list(g))
  expect_true("selected.k" %in% names(g))
  expect_true("selected.graph" %in% names(g))
  expect_true("adj.list" %in% names(g))

  fit <- gflowui:::gflow_fit_condexp(
    graph_obj = g,
    X = x,
    y = x[, 1],
    feature.matrix = x[, 2:3, drop = FALSE],
    fit.args = list(max.iterations = 3L, n.eigenpairs = 10L, verbose.level = 0L),
    refit.args = list(per.column.gcv = FALSE)
  )
  expect_equal(length(fit$fitted.values), nrow(x))
  expect_true(is.matrix(fit$feature.fitted.values))
  expect_equal(nrow(fit$feature.fitted.values), nrow(x))
  expect_equal(ncol(fit$feature.fitted.values), 2L)

  ep <- gflowui:::gflow_detect_endpoints_stub(g)
  expect_true("endpoints" %in% names(ep))
})
