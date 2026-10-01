fixture_dcst <- function() list(
  coords = matrix(seq_len(12), 4, 3),
  sources = list(dcst_level1 = list(values = c("A", "B", "A", NA)),
                 dcst_level2 = list(values = c("C", "D", "C", "D"))),
  graph_set = list(color_assets = list(categorical_palettes = list(
    dcst_level1 = c(A = "red", B = "blue"),
    dcst_level2 = c(C = "green", D = "gold")))))

test_that("level changes reset incompatible selections and other projects are untouched", {
  st <- fixture_dcst()
  expect_identical(gflowui_dcst_options(st$sources, "dcst_level2", "dcst_level1:A")$group, "__all__")
  expect_identical(gflowui_dcst_options(st$sources, "dcst_level1", "dcst_level1:A")$selected_label, "A")
  expect_null(gflowui_dcst_options(list()))
  expect_identical(gflowui_dcst_focus(st, 1:4, "dcst_level1")$st, st)
})

test_that("recoloring preserves coordinates and selected palette colors", {
  st <- fixture_dcst()
  focused <- gflowui_dcst_focus(st, 1:4, "dcst_level1", "dcst_level1:A", background = "#555555")
  expect_identical(focused$keep_idx, 1:4)
  expect_identical(focused$st$coords, st$coords)
  expect_identical(focused$st$sources$dcst_level1$values, c("A", "Other dCSTs", "A", "Other dCSTs"))
  expect_identical(focused$st$graph_set$color_assets$categorical_palettes$dcst_level1[c("A", "Other dCSTs")],
                   c(A = "red", `Other dCSTs` = "#555555"))
  expect_identical(st$sources$dcst_level1$values, c("A", "B", "A", NA_character_))
})

test_that("hiding intersects component selection and keeps empty intersections empty", {
  st <- fixture_dcst()
  focused <- gflowui_dcst_focus(st, 2:4, "dcst_level1", "dcst_level1:A", "hide")
  expect_identical(focused$keep_idx, 3L)
  expect_identical(focused$st, st)
  expect_length(gflowui_dcst_focus(st, 2L, "dcst_level1", "dcst_level1:A", "hide")$keep_idx, 0L)
  expect_identical(gflowui_dcst_focus(st, 1:4, "dcst_level2", "dcst_level2:D", "hide")$keep_idx, c(2L, 4L))
})
