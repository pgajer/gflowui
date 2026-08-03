basin_label_fixture_table <- function() {
  data.frame(
    key = c("max|a", "max|b", "max|c", "min|d", "min|e"),
    type = c("max", "max", "max", "min", "min"),
    basin.id = c("a", "b", "c", "d", "e"),
    extremum.vertex = c(3L, 1L, 2L, 5L, 4L),
    extremum.value = c(10, 20, 15, 3, 1),
    primary.support.mass = c(0.9, 0.1, 0.5, 0.2, 0.8),
    primary.support.size = c(2L, 9L, 5L, 8L, 3L),
    rank = c(1L, 3L, 2L, 2L, 1L),
    color = c("#111111", "#222222", "#333333", "#444444", "#555555"),
    selected = FALSE,
    stringsAsFactors = FALSE
  )
}

labels_by_id <- function(table) {
  stats::setNames(as.character(table$display.label), table$basin.id)
}

test_that("global basin labels are direction-specific and basis-dependent", {
  table <- basin_label_fixture_table()

  by.mass <- gflowui:::gflowui_basin_apply_label_basis_table(
    table,
    "primary.support.mass"
  )
  expect_identical(
    labels_by_id(by.mass)[c("a", "b", "c", "d", "e")],
    c(a = "M1", b = "M3", c = "M2", d = "m2", e = "m1")
  )
  expect_identical(by.mass$label.basis, rep("primary.support.mass", 5L))

  by.support <- gflowui:::gflowui_basin_apply_label_basis_table(
    table,
    "primary.support.size"
  )
  expect_identical(
    labels_by_id(by.support)[c("a", "b", "c", "d", "e")],
    c(a = "M3", b = "M1", c = "M2", d = "m1", e = "m2")
  )

  by.value <- gflowui:::gflowui_basin_apply_label_basis_table(
    table,
    "extremum.value"
  )
  expect_identical(
    labels_by_id(by.value)[c("a", "b", "c", "d", "e")],
    c(a = "M3", b = "M1", c = "M2", d = "m2", e = "m1")
  )
})

test_that("basin label ties use canonical extremum-vertex order", {
  table <- basin_label_fixture_table()
  table$primary.support.mass[table$type == "max"] <- 1
  labelled <- gflowui:::gflowui_basin_apply_label_basis_table(
    table,
    "primary.support.mass"
  )
  expect_identical(
    labels_by_id(labelled)[c("a", "b", "c")],
    c(a = "M3", b = "M1", c = "M2")
  )
})

test_that("unavailable label measures are exposed and resolved explicitly", {
  table <- basin_label_fixture_table()
  table$primary.support.mass <- NA_real_
  resolution <- gflowui:::gflowui_basin_resolve_label_basis(
    table,
    "primary.support.mass"
  )
  expect_false(resolution$available[["primary.support.mass"]])
  expect_identical(resolution$resolved, "primary.support.size")
  expect_true(isTRUE(resolution$fallback))
  expect_match(resolution$message, "unavailable", fixed = TRUE)
  expect_match(
    resolution$message,
    "Trajectory-flow basin support",
    fixed = TRUE
  )
})

test_that("changing label basis preserves basin identity and analysis rank", {
  table <- basin_label_fixture_table()
  result <- gflowui:::gflowui_basin_prepare_analysis_result(list(
    all_table = table,
    table = table
  ))
  original.keys <- result$all_table$key
  original.rank <- result$all_table$rank

  relabelled <- gflowui:::gflowui_basin_apply_label_basis(
    result,
    "primary.support.size"
  )
  expect_identical(relabelled$all_table$key, original.keys)
  expect_identical(relabelled$all_table$rank, original.rank)
  expect_identical(relabelled$label_basis, "primary.support.size")
  expect_false(identical(
    result$all_table$display.label,
    relabelled$all_table$display.label
  ))
})
