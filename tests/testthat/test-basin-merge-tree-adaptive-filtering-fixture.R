test_that("Subject 15 adaptive-filtering fixture is complete and pinned", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  provenance <- utils::read.csv(
    test_path(
      "fixtures",
      "basin_merge_tree_subject15_maxima_provenance.csv"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(fixture), 352L)
  expect_true(all(fixture$direction == "max"))
  expect_true(all(fixture$component == 1L))
  expect_identical(anyDuplicated(fixture$trajectory_basin_id), 0L)
  expect_identical(anyDuplicated(fixture$canonical_branch_id), 0L)
  expect_identical(
    fixture$trajectory_basin_id,
    fixture$canonical_branch_id
  )
  expect_equal(sum(fixture$is_component_survivor), 1L)
  expect_true(all(
    fixture$parent_canonical_branch_id[
      !fixture$is_component_survivor
    ] %in% fixture$canonical_branch_id
  ))
  expect_equal(
    provenance$fixture_schema,
    "gflowui_basin_merge_tree_adaptive_fixture/1"
  )
  expect_equal(
    provenance$source_zip_sha256,
    "15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275"
  )
})

test_that("Subject 15 fixture reproduces the bounded rank-17 evidence", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  mass <- sort(fixture$primary_support_mass, decreasing = TRUE)
  gap <- log10(mass[-length(mass)]) - log10(mass[-1L])

  expect_equal(sum(mass), 1.0000000000000087, tolerance = 3e-16)
  expect_equal(mass[[17L]], 0.0122134243817115)
  expect_equal(mass[[18L]], 1.40305377913392e-15)
  expect_equal(which.max(gap), 17L)
  expect_equal(gap[[17L]], 12.9397631299771, tolerance = 1e-12)
  expect_equal(sum(mass[seq_len(17L)]), 0.99999999999992595)
  expect_equal(
    sum(mass[seq_len(17L)]) / sum(mass),
    0.99999999999991729
  )
})
