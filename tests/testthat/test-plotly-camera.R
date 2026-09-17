test_that("Plotly redraws retain the last interactive camera", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is required for the camera event regression")
  hook_file <- tempfile(fileext = ".js")
  on.exit(unlink(hook_file), add = TRUE)

  shiny::testServer(app_server, {
    widget <- attach_reference_plotly_camera_preserver(plotly::plot_ly())
    writeLines(widget$jsHooks$render[[1L]]$code, hook_file)
  })
  result <- system2(node, c(
    shQuote(test_path("plotly-camera-regression.js")), shQuote(hook_file)
  ), stdout = TRUE, stderr = TRUE)
  expect_equal(attr(result, "status") %||% 0L, 0L, info = paste(result, collapse = "\n"))
})
