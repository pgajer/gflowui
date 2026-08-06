test_that("event navigator previews locally and commits through one structured input", {
  path <- system.file(
    "app", "www", "basin-tree-interaction.js",
    package = "gflowui"
  )
  expect_true(nzchar(path))
  javascript <- paste(readLines(path, warn = FALSE), collapse = "\n")
  input.start <- regexpr(
    'document.addEventListener("input"',
    javascript,
    fixed = TRUE
  )[[1L]]
  change.start <- regexpr(
    'document.addEventListener("change"',
    javascript,
    fixed = TRUE
  )[[1L]]
  preview.handler <- substr(javascript, input.start, change.start - 1L)

  expect_match(preview.handler, "showPreview", fixed = TRUE)
  expect_false(grepl("setInputValue", preview.handler, fixed = TRUE))
  expect_match(
    javascript,
    'window.Shiny.setInputValue("basin_tree_event_commit"',
    fixed = TRUE
  )
  expect_false(grepl("basin_tree_level_index", javascript, fixed = TRUE))
  expect_match(javascript, "context_token", fixed = TRUE)
  expect_match(javascript, "event_index", fixed = TRUE)
  expect_match(javascript, "pendingIndex", fixed = TRUE)
  expect_match(
    javascript,
    "state.pendingIndex = null;",
    fixed = TRUE
  )
  expect_match(javascript, "Release to apply to the graph", fixed = TRUE)
  expect_match(javascript, "aria-valuenow", fixed = TRUE)
  expect_match(
    javascript,
    "gflowui-basin-ascent-flow-style",
    fixed = TRUE
  )
  expect_match(javascript, "applyAscentFlowStyle", fixed = TRUE)
  expect_match(javascript, "window.Plotly.restyle", fixed = TRUE)
  expect_match(javascript, "canonical_ascent_flow", fixed = TRUE)
  expect_match(
    javascript,
    "Number.isFinite(opacity) ? opacity : 1",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "Number.isFinite(width) ? width : 2",
    fixed = TRUE
  )
})

test_that("event navigator styling is horizontal and responsive", {
  path <- system.file("app", "www", "styles.css", package = "gflowui")
  expect_true(nzchar(path))
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  navigator.start <- regexpr(
    ".gf-basin-tree-event-navigator",
    css,
    fixed = TRUE
  )[[1L]]
  expect_gt(navigator.start, 0L)
  expect_match(css, ".gf-basin-tree-event-range", fixed = TRUE)
  expect_match(css, ".gf-basin-tree-event-controls", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: auto minmax(8rem, 1fr) auto",
    fixed = TRUE
  )
  expect_false(grepl("writing-mode: vertical", css, fixed = TRUE))
})
