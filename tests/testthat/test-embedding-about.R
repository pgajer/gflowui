test_that("graph context omits empty research and escapes source text", {
  expect_null(gflowui_ec_about_ui(NULL))
  info <- list(construction="A <model>", weights="Unit weights", references=list())
  html <- as.character(gflowui_ec_about_ui(info))
  expect_match(html, "About this graph")
  expect_match(html, "A &lt;model&gt;", fixed=TRUE)
  expect_false(grepl("Research using|Use in graph algorithms|No paper|unavailable", html))
  info$references <- list(list(title="Author — Paper (2020)", url="https://example.org/paper",
    description="Clusters observations using graph cuts.", role="application", scope="Original full graph"))
  html <- as.character(gflowui_ec_about_ui(info))
  expect_match(html, 'href="https://example.org/paper"', fixed=TRUE)
  expect_match(html, "Research using the data", fixed=TRUE)
  expect_match(html, "Original full graph", fixed=TRUE)
})

test_that("annotations are optional, hash checked, and require references", {
  root <- tempfile(); dir.create(root)
  index <- list(root=root, graphs=list(A=list(graph_sha256="graph-A")), artifacts=list())
  expect_identical(gflowui_ec_load_annotations(index), list())
  data <- list(schema_version=1L, graphs=list(list(id="A", graph_sha256="graph-A",
    construction="A model", references=list(list(title="Paper", description="Graph analysis",
      role="graph", url="https://example.org/paper")))))
  save <- function() {
    jsonlite::write_json(data, file.path(root,"about.json"), auto_unbox=TRUE)
    index$artifacts[["graph_annotations.json"]] <<- list(path="about.json",
      sha256=digest::digest(file=file.path(root,"about.json"),algo="sha256"))
  }
  save()
  expect_identical(names(gflowui_ec_load_annotations(index)), "A")
  data$graphs[[1]]$references[[1]]$url <- "javascript:alert(1)"; save()
  expect_error(gflowui_ec_load_annotations(index), "safe reference")
  data$graphs[[1]]$graph_sha256 <- "wrong"; save()
  expect_error(gflowui_ec_load_annotations(index), "hash mismatch")
  cat("tampered", file=file.path(root,"about.json"))
  expect_error(gflowui_ec_load_annotations(index), "invalid embedding asset")
})
