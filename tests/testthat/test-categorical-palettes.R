test_that("explicit palettes preserve colors and order across graph subsets", {
  palette <- c(A = "#123456", B = "#789ABC", C = "red")
  full <- gflowui_explicit_categorical_palette(c("B", "A", "C"), palette)
  subset <- gflowui_explicit_categorical_palette(c("C", "A", "C"), palette)
  expect_identical(full$levels, c("A", "B", "C"))
  expect_identical(subset$levels, c("A", "C"))
  expect_identical(subset$colors, full$colors[c("A", "C")])
  expect_identical(subset$values, c("C", "A", "C"))
  expect_null(gflowui_explicit_categorical_palette("A"))
  expect_identical(gflowui_explicit_categorical_palette(c("unknown", NA), palette)$colors,
                   c(unknown = "#808080", `NA` = "#808080"))
})

test_that("invalid explicit palettes fail clearly", {
  expect_error(gflowui_explicit_categorical_palette("A", "red"), "named character")
  expect_error(gflowui_explicit_categorical_palette("A", c(A="red", A="blue")), "unique")
  expect_error(gflowui_explicit_categorical_palette("A", c(A="notacolor")), "invalid color")
})

test_that("graph manifest normalization preserves shared palettes", {
  palette <- list(dcst_level1 = c(A="#123456", B="#654321"))
  graph <- gflowui_normalize_graph_set_manifest(list(id="one", color_assets=list(
    categorical_palettes=palette)))
  expect_identical(graph$color_assets$categorical_palettes, palette)
})
