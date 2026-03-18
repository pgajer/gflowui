testthat::test_that("compute_arm_variant builds path and tube-lens corridor variants", {
  adj <- list(
    c(2L),
    c(1L, 3L, 5L, 6L),
    c(2L, 4L, 5L),
    c(3L),
    c(2L, 3L),
    c(2L)
  )
  weights <- list(
    1,
    c(1, 1, 1, 2),
    c(1, 1, 1),
    1,
    c(1, 1),
    2
  )
  coords <- cbind(
    x = c(0, 1, 2, 3, 1.5, 1),
    y = c(0, 0, 0, 0, 0.8, -1),
    z = c(0, 0, 0, 0, 0, 0)
  )

  path_only <- gflowui:::compute_arm_variant(
    adj.list = adj,
    weight.list = weights,
    coords = coords,
    endpoint_a = 1L,
    endpoint_b = 4L,
    endpoint_a_key = "v1",
    endpoint_b_key = "v4",
    endpoint_a_label = "A",
    endpoint_b_label = "B",
    thickening_method = "path_only"
  )

  testthat::expect_equal(path_only$path_vertices, c(1L, 2L, 3L, 4L))
  testthat::expect_equal(path_only$arm_vertices, c(1L, 2L, 3L, 4L))
  testthat::expect_equal(unname(path_only$arm_metrics$harmonic_t), c(0, 1 / 3, 2 / 3, 1))
  testthat::expect_true(nzchar(path_only$arm_id))

  corridor <- gflowui:::compute_arm_variant(
    adj.list = adj,
    weight.list = weights,
    coords = coords,
    endpoint_a = 1L,
    endpoint_b = 4L,
    endpoint_a_key = "v1",
    endpoint_b_key = "v4",
    endpoint_a_label = "A",
    endpoint_b_label = "B",
    thickening_method = "tube_lens_corridor",
    path_relative_radius = 0.35
  )

  testthat::expect_true(all(c(1L, 2L, 3L, 4L) %in% corridor$arm_vertices))
  testthat::expect_true(5L %in% corridor$arm_vertices)
  testthat::expect_true("harmonic_t" %in% names(corridor$arm_metrics))
  testthat::expect_equal(length(corridor$arm_metrics$harmonic_t), length(corridor$arm_vertices))
})

testthat::test_that("tube-lens excess corridor carries excess-filtered selection", {
  adj <- list(
    c(2L),
    c(1L, 3L, 5L, 6L),
    c(2L, 4L, 5L),
    c(3L),
    c(2L, 3L),
    c(2L)
  )
  weights <- list(
    1,
    c(1, 1, 1, 2),
    c(1, 1, 1),
    1,
    c(1, 1),
    2
  )
  coords <- cbind(
    x = c(0, 1, 2, 3, 1.5, 1),
    y = c(0, 0, 0, 0, 0.8, -1),
    z = c(0, 0, 0, 0, 0, 0)
  )

  strict_corridor <- gflowui:::compute_arm_variant(
    adj.list = adj,
    weight.list = weights,
    coords = coords,
    endpoint_a = 1L,
    endpoint_b = 4L,
    endpoint_a_key = "v1",
    endpoint_b_key = "v4",
    endpoint_a_label = "A",
    endpoint_b_label = "B",
    thickening_method = "tube_lens_excess_corridor",
    path_relative_radius = 0.35,
    excess_tolerance = 0.5
  )

  testthat::expect_true(all(c(1L, 2L, 3L, 4L) %in% strict_corridor$arm_vertices))
  testthat::expect_false(6L %in% strict_corridor$arm_vertices)
  testthat::expect_true(is.finite(strict_corridor$params$excess.tolerance))
  testthat::expect_match(strict_corridor$parameter_summary, "tube-lens excess corridor")
})

testthat::test_that("working arm state sanitizes and preserves visible rows", {
  variant <- list(
    arm_id = "arm_a",
    family_id = "fam_a",
    label = "Arm A",
    family_label = "Arm A",
    endpoint_a = 1L,
    endpoint_b = 3L,
    endpoint_a_key = "v1",
    endpoint_b_key = "v3",
    endpoint_a_label = "A",
    endpoint_b_label = "B",
    endpoint_a_virtual = FALSE,
    endpoint_b_virtual = FALSE,
    path_method = "weighted_shortest_path",
    thickening_method = "path_only",
    path_vertices = c(1L, 2L, 3L),
    arm_vertices = c(1L, 2L, 3L),
    arm_coords = c(0, 0.5, 1),
    path_arc_length = stats::setNames(c(0, 0.5, 1), c("1", "2", "3")),
    arm_metrics = list(
      t_balance = stats::setNames(c(0, 0.5, 1), c("1", "2", "3")),
      harmonic_t = stats::setNames(c(0, 0.5, 1), c("1", "2", "3")),
      distance_to_path = stats::setNames(c(0, 0, 0), c("1", "2", "3")),
      excess = stats::setNames(c(0, 0, 0), c("1", "2", "3"))
    ),
    parameter_summary = "weighted shortest path",
    params = list(),
    source_k = 7L
  )

  rows <- gflowui:::working_arm_rows_from_variant(variant)
  state <- gflowui:::sanitize_working_arm_state(
    list(
      project_id = "demo",
      graph_set_id = "shared_all_asv",
      k = 7L,
      rows = rows
    )
  )

  visible <- gflowui:::accepted_visible_working_arm_rows(state)
  hidden <- gflowui:::accepted_hidden_working_arm_rows(state)

  testthat::expect_equal(nrow(visible), 1L)
  testthat::expect_equal(nrow(hidden), 0L)
  testthat::expect_equal(visible$label[[1]], "Arm A")
  testthat::expect_equal(gflowui:::decode_arm_integer_json(visible$path_vertices_json[[1]]), c(1L, 2L, 3L))
})
