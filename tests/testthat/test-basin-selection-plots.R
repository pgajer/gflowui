test_that("ranked and cumulative plot specifications are scientifically scoped", {
  ranked <- gflowui:::gflowui_basin_new_plot_specs(
    features = c("support", "mass", "prominence"),
    mode = "ranked",
    first_id = 4L,
    construction_fingerprint = "plot-fingerprint"
  )
  expect_length(ranked, 3L)
  expect_true(all(vapply(
    ranked,
    function(spec) identical(spec$kind, "ranked"),
    logical(1)
  )))

  cumulative <- gflowui:::gflowui_basin_new_plot_specs(
    features = c("support", "mass", "prominence", "extremum_value"),
    mode = "cumulative",
    first_id = 9L,
    construction_fingerprint = "plot-fingerprint"
  )
  expect_length(cumulative, 2L)
  expect_identical(
    vapply(cumulative, function(spec) spec$features[[1L]], character(1)),
    c("support", "mass")
  )
  expect_true(all(vapply(
    cumulative,
    function(spec) identical(spec$kind, "cumulative"),
    logical(1)
  )))
})

test_that("ranked and cumulative curves are direction-specific and tie-complete", {
  data <- data.frame(
    key = paste0("key-", 1:7),
    type = c(rep("max", 4), rep("min", 3)),
    label = c("M1", "M2", "M3", "M4", "m1", "m2", "m3"),
    canonical_basin_id = paste0("basin-", 1:7),
    membership = c("core", "core", "hidden", "hidden", rep("not_applicable", 3)),
    selected = FALSE,
    support = c(8, 4, 4, 2, 5, 3, 2),
    mass = c(0.5, 0.2, 0.2, 0.1, 0.6, 0.3, 0.1),
    extremum_value = c(9, 8, 7, 6, 1, 2, 3),
    prominence = c(4, 3, 2, 1, 4, 2, 1),
    stringsAsFactors = FALSE
  )

  ranked <- gflowui:::gflowui_basin_ranked_curve(data, "mass")
  expect_identical(ranked$position[ranked$type == "max"], 1:4)
  expect_equal(ranked$value[ranked$type == "max"], c(0.5, 0.2, 0.2, 0.1))
  expect_identical(
    ranked$canonical_basin_id[ranked$type == "max"][2:3],
    c("basin-2", "basin-3")
  )

  cumulative <- gflowui:::gflowui_basin_cumulative_curve(data, "mass")
  maxima <- cumulative[cumulative$type == "max", , drop = FALSE]
  expect_identical(maxima$position, c(1L, 3L, 4L))
  expect_equal(maxima$value, c(0.5, 0.9, 1))
  expect_identical(
    nrow(gflowui:::gflowui_basin_cumulative_curve(data, "prominence")),
    0L
  )
})

test_that("proposal-aware default plot cards render with threshold overlays", {
  data <- data.frame(
    key = paste0("key-", 1:5),
    type = rep("max", 5),
    label = paste0("M", 1:5),
    rank = 1:5,
    canonical_basin_id = paste0("basin-", 1:5),
    membership = c("core", "core", "hidden", "hidden", "hidden"),
    inclusion_reasons = "",
    pinned = FALSE,
    selected = FALSE,
    visibility = "displayed",
    support = c(20, 10, 5, 2, 1),
    mass = c(0.6, 0.25, 0.1, 0.05, 0),
    extremum_value = 5:1,
    prominence = 5:1,
    extremum_value_rank = 1:5,
    support_rank = 1:5,
    mass_rank = 1:5,
    prominence_rank = 1:5,
    stringsAsFactors = FALSE
  )
  overlay <- list(
    available = TRUE,
    filter.mode = "auto",
    core.ids = c("basin-1", "basin-2"),
    boundary = 2L,
    mass.cutoff = 0.16,
    minimum.mass = NA_real_,
    coverage.target = 0.8,
    core.budget = 4L
  )
  specs <- gflowui:::gflowui_basin_default_plot_specs("render-defaults")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output, width = 8, height = 6)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  for (spec in specs) {
    expect_no_error(gflowui:::gflowui_draw_basin_plot(
      data,
      spec,
      point_size = 0.5,
      selection_overlay = overlay,
      show_selection_thresholds = TRUE,
      x_scale = spec$x_scale,
      y_scale = spec$y_scale
    ))
  }
})
