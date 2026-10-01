hover_fixture <- function() list(
  sample_ids = c("sample-A", "sample<&B"),
  taxon_names = c("Lactobacillus_iners", "Taxon_<two>", "Taxon_three", "Taxon_four", "Taxon_five"),
  indices = list(1L, c(2L, 5L, 1L, 3L, 4L)),
  abundances = list(1, c(.5, .2, .15, .1, .05)))

test_that("hover matches stable IDs after reordering and never substitutes rows", {
  a <- gflowui_validate_vertex_hover(hover_fixture())
  text <- gflowui_vertex_hover_text(c("sample<&B", "sample-A", "unknown"), a)
  expect_match(text[1], "Vertex ID:</b> sample&lt;&amp;B", fixed = TRUE)
  expect_false(any(grepl("Vertex number", text, fixed = TRUE)))
  expect_match(text[1], "1. Taxon &lt;two&gt;: 50", fixed = TRUE)
  expect_match(text[1], "2. Taxon five: 20", fixed = TRUE)
  expect_match(text[1], "4. Taxon three: 10", fixed = TRUE)
  expect_false(grepl("Taxon four:", text[1], fixed = TRUE))
  expect_match(text[2], "Lactobacillus iners: 100%", fixed = TRUE)
  expect_match(text[2], "Only 1 nonzero phylotype", fixed = TRUE)
  expect_match(text[3], "Relative abundances unavailable", fixed = TRUE)
  two <- gflowui_vertex_hover_text("sample<&B", a, 2)
  expect_match(two, "2. Taxon five", fixed = TRUE)
  expect_false(grepl("3.", two, fixed = TRUE))
  expect_equal(gflowui_hover_top_n(NA, 5L), 4L)
  expect_equal(gflowui_hover_top_n(-1, 5L), 1L)
  expect_equal(gflowui_hover_top_n(99, 5L), 5L)
  expect_null(gflowui_vertex_hover_text(NULL, NULL))
})

test_that("abundance profiles reject invalid identities, values and sorting", {
  a <- hover_fixture(); a$sample_ids[2] <- a$sample_ids[1]
  expect_error(gflowui_validate_vertex_hover(a), "identities")
  a <- hover_fixture(); a$abundances[[2]][1] <- 2
  expect_error(gflowui_validate_vertex_hover(a), "relative abundances")
  a <- hover_fixture(); a$abundances[[2]] <- rev(a$abundances[[2]])
  expect_error(gflowui_validate_vertex_hover(a), "relative abundances")
  a <- hover_fixture(); a$indices[[1]] <- 9L
  expect_error(gflowui_validate_vertex_hover(a), "relative abundances")
})

test_that("all point layers get the same filtered profiles without changing labels or keys", {
  skip_if_not_installed("plotly")
  hover <- c("profile one", "profile two", "profile three")
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, type="scatter3d", mode="markers", x=1:2,y=1:2,z=1:2,
    key=c(3L,1L),customdata=c(3L,1L),text=c("vertex=3<br>group=A","vertex=1<br>group=A"))
  p <- plotly::add_trace(p, type="scatter3d", mode="markers+text", x=2,y=2,z=2,
    key=2L,customdata=2L,text="Visit W1D1",hovertext="Subject S; visit W1D1")
  p <- plotly::add_trace(p, type="scatter3d", mode="lines", x=1:2,y=1:2,z=1:2,hoverinfo="skip")
  p <- plotly::add_trace(p, type="scatter3d", mode="markers", x=1,y=1,z=1,
    customdata=NA_integer_,visible="legendonly",hoverinfo="skip")
  out <- plotly::plotly_build(gflowui_add_vertex_hover(p, hover))$x$data
  expect_equal(as.character(out[[1]]$hovertext), c("profile three<br>group=A", "profile one<br>group=A"))
  expect_equal(as.integer(out[[1]]$customdata), c(3L,1L))
  expect_equal(as.character(out[[2]]$hovertext), "profile two<br>Subject S; visit W1D1")
  expect_equal(as.character(out[[2]]$text), "Visit W1D1")
  expect_equal(unique(as.character(out[[1]]$hovertemplate)), "%{hovertext}<extra></extra>")
  expect_null(out[[3]]$hovertext)
  expect_null(out[[4]]$hovertext)
})

test_that("hover controls are opt-in, default to four, and accept a changed count", {
  path <- tempfile(fileext=".rds"); on.exit(unlink(path)); saveRDS(hover_fixture(),path)
  manifest <- list(metadata=list(vertex_hover=list(abundances_file=path)))
  ui <- as.character(gflowui_vertex_hover_controls(manifest))
  expect_match(ui, 'id="graph_hover_top_n"', fixed=TRUE)
  expect_match(ui, 'value="4"', fixed=TRUE)
  expect_match(as.character(gflowui_vertex_hover_controls(manifest,2)), 'value="2"', fixed=TRUE)
  expect_null(gflowui_vertex_hover_controls(list()))
  expect_null(gflowui_vertex_hover_controls(manifest,renderer="rglwidget"))
})
