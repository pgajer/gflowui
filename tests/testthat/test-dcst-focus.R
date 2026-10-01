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

test_that("table selection shows the union and respects component filtering", {
  st <- fixture_dcst()
  out <- gflowui_dcst_table_focus(st, 2:4, "dcst_level1", c("A", "B"))
  expect_identical(out$keep_idx, 2:3)
  expect_identical(out$st, st)
  expect_identical(gflowui_dcst_table_focus(st, 1:4, "dcst_level1")$keep_idx, 1:4)
  expect_length(gflowui_dcst_table_focus(st, 1:4, "dcst_level1", "absent")$keep_idx, 0)
  selection <- list(project = "AGP", level = "dcst_level1", groups = c("A", "B"))
  expect_identical(gflowui_dcst_table_groups(selection, "AGP", "dcst_level1"), c("A", "B"))
  expect_length(gflowui_dcst_table_groups(selection, "other", "dcst_level1"), 0)
  expect_length(gflowui_dcst_table_groups(selection, "AGP", "dcst_level2"), 0)
})

test_that("table rows are size ordered and expose checkboxes and current colors", {
  st <- fixture_dcst()
  opt <- gflowui_dcst_options(st$sources)
  expect_identical(opt$groups, c("A", "B"))
  expect_equal(unname(opt$counts), c(2, 1))
  html <- as.character(gflowui_dcst_table_ui(opt,
    st$graph_set$color_assets$categorical_palettes, "AGP"))
  expect_match(html, 'type="checkbox"', fixed = TRUE)
  expect_match(html, 'type="color" value="#FF0000"', fixed = TRUE)
  expect_match(html, 'Show all / clear selection', fixed = TRUE)
})
