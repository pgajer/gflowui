test_that("project specs preserve occupation-density assets", {
  root <- tempfile("gflowui-occupation-spec-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(),
    occupation_density_sets = list(list(
      id = "subject_od",
      subject_ids = c("S1", "S2")
    ))
  )

  expect_equal(spec$occupation_density_sets[[1L]]$id, "subject_od")
  expect_equal(spec$occupation_density_sets[[1L]]$subject_ids, c("S1", "S2"))
})

test_that("selected occupation-density estimates load and normalize", {
  root <- tempfile("gflowui-occupation-fit-")
  dir.create(file.path(root, "fits"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  rho <- array(
    c(1, 2, 1, 4),
    dim = c(2L, 1L, 2L),
    dimnames = list(
      vertex = c("v1", "v2"),
      subject = "S1",
      selector = c("minimum_brier", "minimum_bernoulli_nll")
    )
  )
  saveRDS(
    list(
      subject.id = "S1",
      rho = rho,
      selected = data.frame(
        subject.id = rep("S1", 2L),
        selector = c("minimum_brier", "minimum_bernoulli_nll"),
        graph.k = c(5L, 7L)
      )
    ),
    file.path(root, "fits", "selected.rds")
  )
  manifest <- list(
    project_root = root,
    occupation_density_sets = list(list(
      id = "subject_od",
      methods = list(list(
        id = "graph_heat_kernel",
        selected_fit_file = "fits/selected.rds"
      ))
    ))
  )

  result <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject_od",
    subject_id = "S1",
    method_id = "graph_heat_kernel",
    mode = "selected",
    selector = "minimum_bernoulli_nll"
  )

  expect_equal(result$values, c(0.2, 0.8))
  expect_equal(result$selected$graph.k, 7L)
})

test_that("density colors use a bounded log scale without changing raw mass", {
  mass <- c(0, 1e-12, 1e-8, 1e-6, 1e-4, 1e-2, NA_real_)
  encoded <- gflowui:::gflowui_numeric_color_encoding(
    mass,
    transform = "density_log10",
    title = "Probability mass"
  )

  expect_equal(encoded$raw_values, mass)
  expect_equal(encoded$floor_value, 1e-8)
  expect_equal(encoded$mapped_values[1:3], rep(-8, 3))
  expect_true(all(diff(encoded$mapped_values[3:6]) > 0))
  expect_equal(encoded$color_limits, c(-8, -2))
  expect_match(encoded$colorbar$title, "log10 color", fixed = TRUE)
  expect_match(encoded$colorbar$ticktext[[1L]], "<=", fixed = TRUE)

  uniform <- gflowui:::gflowui_numeric_color_encoding(
    rep(0.25, 4),
    transform = "density_log10",
    title = "Probability mass"
  )
  expect_equal(uniform$mapped_values, rep(log10(0.25), 4))
  expect_equal(
    uniform$color_limits,
    c(log10(0.25) - 6, log10(0.25))
  )

  identity <- gflowui:::gflowui_numeric_color_encoding(
    mass,
    transform = "identity",
    title = "Value"
  )
  expect_equal(identity$mapped_values, mass)
  expect_identical(identity$colorbar$title, "Value")
})

test_that("density palettes expose selectable endpoints and midpoint", {
  expect_equal(
    gflowui:::gflowui_density_palette(),
    c("#FDE725", "#C51B1D")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(midpoint = "blue"),
    c("#FDE725", "#2563EB", "#C51B1D")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(
      low = "blue",
      midpoint = "white",
      high = "orange"
    ),
    c("#2563EB", "#F8FAFC", "#F97316")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(
      low = "unknown",
      midpoint = "unknown",
      high = "unknown"
    ),
    c("#FDE725", "#C51B1D")
  )
  expect_equal(
    gflowui:::gflowui_density_palette(
      midpoint = "blue",
      low_alpha = 0.25,
      midpoint_alpha = 0.5,
      high_alpha = 0
    ),
    c("#FDE72540", "#2563EB80", "#C51B1D00")
  )

  scale <- gflowui:::gflowui_plotly_colorscale(
    gflowui:::gflowui_density_palette(midpoint = "blue")
  )
  expect_equal(vapply(scale, `[[`, character(1), 1L), c("0", "0.5", "1"))
  expect_equal(
    vapply(scale, `[[`, character(1), 2L),
    c(
      "rgba(253,231,37,1.0000)",
      "rgba(37,99,235,1.0000)",
      "rgba(197,27,29,1.0000)"
    )
  )
})

test_that("generic estimate basins support mass and support-size ranking", {
  adj_list <- list(
    c(2L, 4L),
    c(1L, 3L, 5L),
    c(2L, 6L),
    c(1L, 5L, 7L),
    c(2L, 4L, 6L, 8L),
    c(3L, 5L, 9L),
    c(4L, 8L),
    c(5L, 7L, 9L),
    c(6L, 8L)
  )
  edge_length_list <- lapply(
    adj_list,
    function(neighbors) rep(1, length(neighbors))
  )
  field <- c(0, 1, 0, 1, 3, 1, 0, 1, 2)

  support_ranked <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "max",
    top_k = 1L
  )
  expect_identical(support_ranked$ranking, "primary support size")
  expect_identical(support_ranked$top_k, 1L)
  expect_identical(support_ranked$basin_count, 2L)
  expect_equal(support_ranked$table$support, 8L)
  expect_true(any(support_ranked$values == "Other basins"))

  mass_ranked <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "max",
    top_k = 2L,
    vertex_mass = field + 1
  )
  expect_identical(mass_ranked$ranking, "primary mass")
  expect_identical(mass_ranked$top_k, 2L)
  expect_true(all(is.finite(mass_ranked$table$mass)))

  expect_error(
    gflowui:::gflowui_estimate_basin_overlay(
      adj_list,
      edge_length_list,
      replace(field, 1L, NA_real_),
      direction = "min",
      top_k = 2L
    ),
    "one finite value for every graph vertex"
  )
  cache.before <- ls(
    envir = gflowui:::.gflowui_basin_cache,
    all.names = TRUE
  )
  expect_error(
    gflowui:::gflowui_estimate_basin_overlay(
      adj_list,
      edge_length_list,
      replace(field, 2L, Inf),
      source_key = "must-not-cache"
    ),
    "one finite value for every graph vertex"
  )
  expect_identical(
    ls(envir = gflowui:::.gflowui_basin_cache, all.names = TRUE),
    cache.before
  )
})

test_that("canonical basin overlay computes both directions and reuses cache", {
  storage <- tempfile("gflowui-basin-cache-test-")
  dir.create(storage)
  withr::local_options(gflowui.basin_storage_dir = storage)
  withr::defer(unlink(storage, recursive = TRUE, force = TRUE))
  rm(
    list = ls(gflowui:::.gflowui_basin_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_cache
  )
  rm(
    list = ls(gflowui:::.gflowui_basin_prominence_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_prominence_cache
  )
  withr::defer({
    rm(
      list = ls(gflowui:::.gflowui_basin_cache, all.names = TRUE),
      envir = gflowui:::.gflowui_basin_cache
    )
    rm(
      list = ls(
        gflowui:::.gflowui_basin_prominence_cache,
        all.names = TRUE
      ),
      envir = gflowui:::.gflowui_basin_prominence_cache
    )
  })
  construction.fingerprint <- paste(rep("c", 64L), collapse = "")
  adj_list <- list(
    c(2L, 4L),
    c(1L, 3L, 5L),
    c(2L, 6L),
    c(1L, 5L, 7L),
    c(2L, 4L, 6L, 8L),
    c(3L, 5L, 9L),
    c(4L, 8L),
    c(5L, 7L, 9L),
    c(6L, 8L)
  )
  edge_length_list <- lapply(
    adj_list,
    function(neighbors) rep(1, length(neighbors))
  )
  field <- c(0, 1, 0, 1, 3, 1, 0, 1, 2)
  vertex_id <- paste0("sample-", seq_along(field))
  provenance <- gflowui:::gflowui_basin_mass_provenance(
    mass_kind = "occupation_probability",
    source_id = "fixture",
    source_fingerprint = "fixture-source",
    authority = "fixture manifest",
    evidence_fingerprint = "fixture-evidence"
  )

  first <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 0L,
    top_k_min = 2L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )
  second <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 2L,
    top_k_min = 0L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )

  expect_identical(first$direction, "both")
  expect_identical(first$cache_source, "miss")
  expect_true(isTRUE(first$disk_cache_written))
  expect_true(file.exists(first$disk_cache_path))
  expect_equal(nrow(first$summary$maxima), 0L)
  expect_equal(nrow(first$summary$minima), 2L)
  expect_true(isTRUE(second$cache_hit))
  expect_identical(second$cache_source, "memory")
  expect_identical(first$cache_key, second$cache_key)
  expect_equal(nrow(second$summary$minima), 0L)
  expect_true(all(c("max", "min") %in% first$basin$assignment$direction))
  expect_identical(
    first$basin$graph.input$vertex.id,
    vertex_id
  )
  expect_identical(
    first$summary$mass.provenance$upstream.attestations[[1L]]$authority,
    "fixture manifest"
  )

  rm(
    list = ls(gflowui:::.gflowui_basin_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_cache
  )
  rm(
    list = ls(gflowui:::.gflowui_basin_prominence_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_prominence_cache
  )
  disk <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 2L,
    top_k_min = 2L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )
  expect_true(isTRUE(disk$cache_hit))
  expect_true(isTRUE(disk$disk_cache_hit))
  expect_identical(disk$cache_source, "disk")

  envelope <- readRDS(disk$disk_cache_path)
  envelope$schema <- "gflowui_basin_disk_cache/obsolete"
  saveRDS(envelope, disk$disk_cache_path)
  rm(
    list = ls(gflowui:::.gflowui_basin_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_cache
  )
  rm(
    list = ls(gflowui:::.gflowui_basin_prominence_cache, all.names = TRUE),
    envir = gflowui:::.gflowui_basin_prominence_cache
  )
  invalidated <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 2L,
    top_k_min = 2L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )
  expect_identical(invalidated$cache_source, "miss")
  expect_true(isTRUE(invalidated$disk_cache_written))
  expect_match(invalidated$disk_cache_reason, "invalidated", fixed = TRUE)
  expect_identical(
    readRDS(invalidated$disk_cache_path)$schema,
    "gflowui_basin_disk_cache/1"
  )

  changed.fingerprint <- paste(rep("d", 64L), collapse = "")
  changed <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 2L,
    top_k_min = 2L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = changed.fingerprint
  )
  expect_identical(changed$cache_source, "miss")
  expect_false(identical(changed$cache_key, invalidated$cache_key))
  expect_false(identical(changed$disk_cache_path, invalidated$disk_cache_path))

  empty <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 0L,
    top_k_min = 0L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )
  restored <- gflowui:::gflowui_estimate_basin_overlay(
    adj_list,
    edge_length_list,
    field,
    direction = "both",
    top_k_max = 2L,
    top_k_min = 2L,
    vertex_mass = field + 1,
    vertex_id = vertex_id,
    vertex_mass_provenance = provenance,
    source_key = "fixture",
    source_fingerprint = "fixture-source",
    construction_fingerprint = construction.fingerprint
  )
  expect_equal(nrow(empty$table), 0L)
  expect_true(all(empty$values_max == "Other basins"))
  expect_true(all(empty$values_min == "Other basins"))
  expect_equal(nrow(restored$table), 4L)
  expect_true(all(is.finite(restored$table$prominence)))
  expect_true(all(restored$table$prominence >= 0))
  expect_s3_class(restored$prominence_complex, "basin_complex")
  expect_identical(restored$prominence_method, "superlevel_merge_tree")
  expect_gte(nrow(restored$all_table), nrow(restored$table))
  expect_false(any(restored$table$selected))
  expect_true(all(restored$values_max == "Other basins"))
  expect_true(all(restored$values_min == "Other basins"))
  expect_equal(
    restored$table$display.label[restored$table$type == "max"],
    paste0("M", seq_len(sum(restored$table$type == "max")))
  )
  expect_equal(
    restored$table$display.label[restored$table$type == "min"],
    paste0("m", seq_len(sum(restored$table$type == "min")))
  )
  expect_true(isTRUE(empty$cache_hit))
  expect_true(isTRUE(restored$cache_hit))
})

test_that("basin plot helpers preserve all, listed, and selected scopes", {
  table <- data.frame(
    key = c("max|a", "max|b", "min|c"),
    type = c("max", "max", "min"),
    display.label = c("M1", "M2", "m1"),
    primary.support.size = c(8L, 5L, 7L),
    primary.support.mass = c(0.4, 0.2, 0.3),
    extremum.value = c(1.2, 1.0, 0.1),
    prominence = c(0.8, 0.5, 0.6),
    stringsAsFactors = FALSE
  )
  result <- list(
    all_table = table,
    table = table[c(1L, 3L), , drop = FALSE]
  )
  expect_equal(
    nrow(gflowui:::gflowui_basin_plot_data(result, "all", "both")),
    3L
  )
  expect_equal(
    nrow(gflowui:::gflowui_basin_plot_data(result, "listed", "both")),
    2L
  )
  selected <- gflowui:::gflowui_basin_plot_data(
    result,
    "selected",
    "max",
    selected_keys = "max|b"
  )
  expect_equal(selected$key, "max|b")
  all.plot.data <- gflowui:::gflowui_basin_plot_data(
    result,
    "all",
    "both"
  )
  expect_true(all(c(
    "extremum_value_rank",
    "support_rank",
    "mass_rank",
    "prominence_rank"
  ) %in% names(all.plot.data)))
  expect_equal(all.plot.data$extremum_value_rank, c(1L, 2L, 1L))
  expect_equal(all.plot.data$support_rank, c(1L, 2L, 1L))
  expect_equal(all.plot.data$mass_rank, c(1L, 2L, 1L))
  expect_equal(all.plot.data$prominence_rank, c(1L, 2L, 1L))
  expect_true(all(c(
    "Extremum value rank",
    "Support rank",
    "Mass rank",
    "Prominence rank"
  ) %in% names(gflowui:::gflowui_basin_plot_feature_choices())))
  rank.table <- data.frame(
    key = c("max|a", "max|b", "min|c", "min|d"),
    type = c("max", "max", "min", "min"),
    display.label = c("M1", "M2", "m1", "m2"),
    primary.support.size = c(5L, 8L, 4L, 7L),
    primary.support.mass = c(0.2, 0.4, 0.1, 0.3),
    extremum.value = c(1.0, 1.2, -0.5, -0.2),
    prominence = c(0.4, 0.8, 0.3, 0.6),
    stringsAsFactors = FALSE
  )
  ranked.plot.data <- gflowui:::gflowui_basin_plot_data(
    list(all_table = rank.table, table = rank.table),
    "all",
    "both"
  )
  expect_equal(ranked.plot.data$extremum_value_rank, c(2L, 1L, 1L, 2L))
  expect_equal(ranked.plot.data$support_rank, c(2L, 1L, 2L, 1L))
  expect_equal(ranked.plot.data$mass_rank, c(2L, 1L, 2L, 1L))
  expect_equal(ranked.plot.data$prominence_rank, c(2L, 1L, 2L, 1L))

  histograms <- gflowui:::gflowui_basin_new_plot_specs(
    c("support", "mass", "prominence"),
    "histograms",
    first_id = 4L
  )
  expect_length(histograms, 3L)
  expect_equal(vapply(histograms, `[[`, integer(1), "id"), 4:6)
  pairs <- gflowui:::gflowui_basin_new_plot_specs(
    c("support", "mass", "prominence"),
    "pairs"
  )
  expect_length(pairs, 3L)
  matrix.spec <- gflowui:::gflowui_basin_new_plot_specs(
    c("support", "mass", "extremum_value", "prominence"),
    "matrix"
  )
  expect_length(matrix.spec, 1L)
  expect_length(matrix.spec[[1L]]$features, 4L)
  expect_true(all(
    gflowui:::gflowui_basin_plot_scale_map(
      matrix.spec[[1L]],
      x_scale = "log10"
    ) == "log10"
  ))

  existing <- gflowui:::gflowui_basin_new_plot_specs(
    c("support", "mass"),
    "histograms",
    construction_fingerprint = "field-a"
  )
  overlapping <- gflowui:::gflowui_basin_new_plot_specs(
    c("support", "mass", "prominence"),
    "histograms",
    construction_fingerprint = "field-a"
  )
  filtered <- gflowui:::gflowui_basin_filter_new_plot_specs(
    existing,
    overlapping
  )
  expect_length(filtered$specs, 1L)
  expect_identical(filtered$specs[[1L]]$features, "prominence")
  expect_equal(filtered$skipped, 2L)

  repeated.pairs <- gflowui:::gflowui_basin_filter_new_plot_specs(
    pairs,
    gflowui:::gflowui_basin_new_plot_specs(
      c("prominence", "mass", "support"),
      "pairs"
    )
  )
  expect_length(repeated.pairs$specs, 0L)
  expect_equal(repeated.pairs$skipped, 3L)

  current.field <- gflowui:::gflowui_basin_filter_new_plot_specs(
    existing,
    gflowui:::gflowui_basin_new_plot_specs(
      c("support", "mass"),
      "histograms",
      construction_fingerprint = "field-b"
    )
  )
  expect_length(current.field$specs, 2L)

  log.table <- gflowui:::gflowui_basin_plot_data(
    result,
    "all",
    "both"
  )
  log.table$prominence[[2L]] <- 0
  log.spec <- list(kind = "scatter", features = c("mass", "prominence"))
  scaled <- gflowui:::gflowui_basin_plot_scaled_data(
    log.table,
    log.spec,
    x_scale = "raw",
    y_scale = "log10"
  )
  expect_equal(nrow(scaled), 2L)
  expect_equal(attr(scaled, "gflowui_nonpositive_excluded"), 1L)
  expect_equal(
    scaled$prominence,
    log10(log.table$prominence[c(1L, 3L)])
  )
  expect_identical(
    gflowui:::gflowui_basin_plot_axis_label("mass", "log10"),
    "log10(Mass)"
  )

  negative.log.geometry <- gflowui:::gflowui_basin_histogram_geometry(
    c(-8, -7.5, -7, -6, -5, -4, -3, -2, -1),
    bins = 5L
  )
  expect_equal(negative.log.geometry$y_limits, c(0, 1.05))
  expect_true(any(negative.log.geometry$height > 0))
  expect_true(all(
    negative.log.geometry$height >= negative.log.geometry$y_limits[[1L]] &
      negative.log.geometry$height <= negative.log.geometry$y_limits[[2L]]
  ))

  plot.file <- tempfile(fileext = ".png")
  grDevices::png(plot.file, width = 900, height = 650)
  expect_no_error(gflowui:::gflowui_draw_basin_plot(
    data = log.table,
    spec = log.spec,
    point_color = "type",
    y_scale = "log10"
  ))
  grDevices::dev.off()
  expect_gt(file.info(plot.file)$size, 0)
  unlink(plot.file)
})

test_that("basin export bundles contain the complete raw table and provenance", {
  storage <- tempfile("gflowui-basin-index-test-")
  dir.create(storage)
  withr::local_options(gflowui.basin_storage_dir = storage)
  withr::defer(unlink(storage, recursive = TRUE, force = TRUE))
  all.table <- data.frame(
    basin.id = c("basin-max-a", "basin-max-b", "basin-min-c"),
    extremum.id = c("extremum-max-a", "extremum-max-b", "extremum-min-c"),
    parent.basin.id = NA_character_,
    key = c("max|a", "max|b", "min|c"),
    display.label = c("M1", "M2", "m1"),
    type = c("max", "max", "min"),
    rank = c(1L, 2L, 1L),
    method = "trajectory_flow",
    rank.measure = "primary.support.mass",
    extremum.vertex = c(2L, 4L, 7L),
    extremum.vertex.id = c("sample-2", "sample-4", "sample-7"),
    extremum.value = c(1.2, 1.0, 0.1),
    primary.support.size = c(8L, 5L, 7L),
    primary.support.mass = c(0.4, 0.2, 0.3),
    prominence = c(0.8, 0.5, 0.6),
    raw.support.size = c(9L, 6L, 8L),
    raw.support.mass = c(0.45, 0.24, 0.34),
    retained.support.size = c(8L, 5L, 7L),
    retained.support.mass = c(0.4, 0.2, 0.3),
    raw.allocated.mass = c(0.4, 0.2, 0.3),
    assignment.status = "assigned",
    retention.status = "retained",
    selected = c(TRUE, FALSE, TRUE),
    color = c("#111111", "#222222", "#333333"),
    stringsAsFactors = FALSE
  )
  result <- list(
    table = all.table[1L, , drop = FALSE],
    all_table = all.table,
    project_id = "fixture-project",
    graph_set_id = "fixture-set",
    graph_k = 3L,
    source_key = "fixture-estimate",
    source_label = "Fixture estimate",
    source_fingerprint = "source-fingerprint",
    rank_by = "auto",
    ranking_resolved = c(
      max = "primary.support.mass",
      min = "primary.support.mass"
    ),
    prominence_method = "superlevel_merge_tree",
    construction_identity = list(
      fingerprint = paste(rep("a", 64L), collapse = ""),
      record = list(
        project.id = "fixture-project",
        graph.set.id = "fixture-set",
        source.key = "fixture-estimate",
        source.fingerprint = "source-fingerprint",
        graph = list(
          graph.k = 3L,
          graph.fingerprint = "graph-fingerprint",
          topology.fingerprint = "topology-fingerprint",
          vertex.id.fingerprint = "vertex-fingerprint",
          display.vertex.id.fingerprint = "display-vertex-fingerprint"
        ),
        construction = list(
          method = "trajectory_flow",
          direction = "both",
          modulation = "CLOSEST",
          plateau.policy = "connected_exact"
        )
      )
    ),
    summary = list(mass.provenance = NULL)
  )
  characteristics <- gflowui:::gflowui_basin_export_characteristics(result)
  expect_equal(nrow(characteristics), 3L)
  expect_equal(
    characteristics$extremum_basin,
    c("M1", "M2", "m1")
  )
  expect_false(any(c("selected", "color", "internal_key") %in%
    names(characteristics)))

  destination <- tempfile("gflowui-basin-export-test-")
  dir.create(destination)
  on.exit(unlink(destination, recursive = TRUE, force = TRUE), add = TRUE)
  exported.at <- as.POSIXct(
    "2026-07-29 12:34:56",
    tz = "America/New_York"
  )
  saved <- gflowui:::gflowui_write_basin_export_bundle(
    result,
    destination,
    exported_at = exported.at
  )
  expect_true(file.exists(saved$path))
  expect_identical(dirname(saved$path), normalizePath(destination))
  expect_equal(saved$row_count, 3L)
  expect_true(isTRUE(saved$indexed))
  expect_match(saved$zip_sha256, "^[a-f0-9]{64}$")
  expected.files <- c(
    "README.txt",
    "basin_analysis.rds",
    "basin_characteristics.csv",
    "basin_column_definitions.csv",
    "basin_internal_mapping.csv",
    "basin_provenance.json"
  )
  archive <- utils::unzip(saved$path, list = TRUE)
  expect_setequal(archive$Name, expected.files)

  extracted <- tempfile("gflowui-basin-export-extracted-")
  dir.create(extracted)
  on.exit(unlink(extracted, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(saved$path, exdir = extracted)
  csv <- utils::read.csv(
    file.path(extracted, "basin_characteristics.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(csv), nrow(result$all_table))
  expect_gt(nrow(csv), nrow(result$table))
  expect_equal(csv$mass, result$all_table$primary.support.mass)
  expect_equal(csv$prominence, result$all_table$prominence)
  rds <- readRDS(file.path(extracted, "basin_analysis.rds"))
  expect_equal(nrow(rds$basin_characteristics), 3L)
  provenance <- jsonlite::fromJSON(
    file.path(extracted, "basin_provenance.json")
  )
  expect_equal(provenance$counts$total, 3L)
  expect_true(isTRUE(provenance$export_scope$top_k_ignored))
  expect_identical(provenance$export_scope$coordinate_scale, "raw")
  matched <- gflowui:::gflowui_find_basin_export(
    result$construction_identity$fingerprint
  )
  expect_true(isTRUE(matched$found))
  expect_identical(matched$path, saved$path)
  expect_identical(matched$zip_sha256, saved$zip_sha256)
  expect_false(isTRUE(gflowui:::gflowui_validate_basin_export_bundle(
    saved$path,
    expected_fingerprint = paste(rep("b", 64L), collapse = "")
  )$valid))
  expect_false(isTRUE(gflowui:::gflowui_validate_basin_export_bundle(
    saved$path,
    expected_sha256 = paste(rep("0", 64L), collapse = "")
  )$valid))

  second <- gflowui:::gflowui_write_basin_export_bundle(
    result,
    destination,
    exported_at = exported.at
  )
  expect_false(identical(saved$path, second$path))
  expect_true(file.exists(second$path))
  expect_identical(
    gflowui:::gflowui_find_basin_export(
      result$construction_identity$fingerprint
    )$path,
    second$path
  )
  connection <- file(second$path, open = "ab")
  writeBin(as.raw(0L), connection)
  close(connection)
  fallback <- gflowui:::gflowui_find_basin_export(
    result$construction_identity$fingerprint
  )
  expect_true(isTRUE(fallback$found))
  expect_identical(fallback$path, saved$path)
  expect_error(
    gflowui:::gflowui_write_basin_export_bundle(
      result,
      file.path(destination, "missing")
    ),
    "does not exist",
    fixed = TRUE
  )
})

test_that("basin inspector row updates preserve explicit selection and colors", {
  keys <- c("max|basin_a", "min|basin_b")
  colors <- stats::setNames(c("#DC2626", "#2563EB"), keys)

  unchecked <- gflowui:::gflowui_update_basin_row_state(
    selected_keys = keys,
    color_map = colors,
    valid_keys = keys,
    key = keys[[1L]],
    role = "selection",
    checked = FALSE
  )
  expect_true(unchecked$changed)
  expect_equal(unchecked$selected_keys, keys[[2L]])
  expect_equal(unchecked$color_map, colors)

  recolored <- gflowui:::gflowui_update_basin_row_state(
    selected_keys = unchecked$selected_keys,
    color_map = unchecked$color_map,
    valid_keys = keys,
    key = keys[[2L]],
    role = "color",
    value = "#16A34A"
  )
  expect_true(recolored$changed)
  expect_equal(recolored$selected_keys, keys[[2L]])
  expect_equal(unname(recolored$color_map[keys[[2L]]]), "#16A34A")

  ignored <- gflowui:::gflowui_update_basin_row_state(
    selected_keys = recolored$selected_keys,
    color_map = recolored$color_map,
    valid_keys = keys,
    key = "max|unknown",
    role = "selection",
    checked = TRUE
  )
  expect_false(ignored$changed)
  expect_equal(ignored$selected_keys, recolored$selected_keys)
})

test_that("basin cache identity distinguishes build and runtime changes", {
  adjacency <- list(2L, 1L)
  weights <- list(1, 1)
  arguments <- list(
    adj_list = adjacency,
    edge_length_list = weights,
    field = c(0, 1),
    vertex_mass = NULL,
    vertex_id = c("a", "b"),
    source_key = "fixture",
    source_fingerprint = "source-1"
  )
  identity <- list(
    build.id = "build-a",
    runtime = list(id = "runtime-a")
  )
  first <- do.call(
    gflowui:::gflowui_basin_cache_key,
    c(arguments, list(build_identity = identity))
  )
  identity$build.id <- "build-b"
  second <- do.call(
    gflowui:::gflowui_basin_cache_key,
    c(arguments, list(build_identity = identity))
  )
  identity$build.id <- "build-a"
  identity$runtime$id <- "runtime-b"
  third <- do.call(
    gflowui:::gflowui_basin_cache_key,
    c(arguments, list(build_identity = identity))
  )
  expect_false(identical(first, second))
  expect_false(identical(first, third))
})

test_that("occupation-density alignment rejects mismatched source contracts", {
  adjacency <- list(c(2L), c(1L, 3L), c(2L, 4L), 3L)
  weights <- lapply(adjacency, function(x) rep(1, length(x)))
  display_id <- paste0("display-", 1:4)
  source_id <- as.character(1:4)
  graph <- gflowui:::gflowui_basin_graph_identity(
    adjacency,
    weights,
    display_id,
    graph_id = "graph-k03",
    graph_k = 3L,
    source_vertex_id = source_id
  )
  field <- c(0.1, 0.2, 0.3, 0.4)
  contract <- list(
    contract.version = "occupation-fixture/1",
    algorithm = "fixture exact comparison",
    graph.id = graph$graph.id,
    graph.k = graph$graph.k,
    graph.fingerprint = graph$graph.fingerprint,
    vertex.id.fingerprint = graph$vertex.id.fingerprint,
    display.vertex.id.fingerprint = graph$display.vertex.id.fingerprint,
    source.vertex.id = source_id,
    field.fingerprint = gflowui:::gflowui_basin_field_fingerprint(field),
    source.field.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(field),
    source.asset.fingerprint = "occupation-asset",
    source.id = "occupation-density"
  )
  validated <- gflowui:::gflowui_validate_basin_source_alignment(
    contract,
    graph,
    field,
    "occupation-asset"
  )
  expect_identical(validated$status, "validated")

  mutations <- list(
    graph.id = "wrong-graph",
    graph.k = 4L,
    graph.fingerprint = "wrong-graph-fingerprint",
    vertex.id.fingerprint = "wrong-vertex-fingerprint",
    display.vertex.id.fingerprint = "wrong-display-fingerprint",
    source.vertex.id = rev(source_id),
    field.fingerprint = "wrong-field-fingerprint"
  )
  for (name in names(mutations)) {
    changed <- contract
    changed[[name]] <- mutations[[name]]
    expect_error(
      gflowui:::gflowui_validate_basin_source_alignment(
        changed,
        graph,
        field,
        "occupation-asset"
      ),
      "alignment failed|ordered source vertex IDs"
    )
  }
})

test_that("conditional-expectation alignment requires the same graph contract", {
  adjacency <- list(2L, c(1L, 3L), 2L)
  weights <- lapply(adjacency, function(x) rep(1, length(x)))
  graph <- gflowui:::gflowui_basin_graph_identity(
    adjacency,
    weights,
    paste0("display-", 1:3),
    graph_id = "condexp-graph",
    graph_k = 5L,
    source_vertex_id = as.character(1:3)
  )
  field <- c(0.2, 0.8, 0.4)
  contract <- list(
    contract.version = "condexp-fixture/1",
    algorithm = "fixture exact comparison",
    graph.id = graph$graph.id,
    graph.k = graph$graph.k,
    graph.fingerprint = graph$graph.fingerprint,
    vertex.id.fingerprint = graph$vertex.id.fingerprint,
    display.vertex.id.fingerprint = graph$display.vertex.id.fingerprint,
    source.vertex.id = graph$source.vertex.id,
    field.fingerprint = gflowui:::gflowui_basin_field_fingerprint(field),
    source.asset.fingerprint = "condexp-asset",
    source.id = "conditional-expectation"
  )
  expect_identical(
    gflowui:::gflowui_validate_basin_source_alignment(
      contract, graph, field, "condexp-asset"
    )$status,
    "validated"
  )
  expect_error(
    gflowui:::gflowui_validate_basin_source_alignment(
      NULL, graph, field, "condexp-asset"
    ),
    "required source-side"
  )
  contract$graph.k <- 7L
  expect_error(
    gflowui:::gflowui_validate_basin_source_alignment(
      contract, graph, field, "condexp-asset"
    ),
    "graph.k"
  )
})

test_that("basin cache identity includes typed provenance and alignment evidence", {
  args <- list(
    adj_list = list(2L, 1L),
    edge_length_list = list(1, 1),
    field = c(0, 1),
    vertex_mass = c(0.25, 0.75),
    vertex_id = c("a", "b"),
    source_key = "fixture",
    source_fingerprint = "source",
    build_identity = list(
      build.id = "build",
      runtime = list(id = "runtime")
    )
  )
  provenance <- gflowui:::gflowui_basin_mass_provenance(
    mass_kind = "occupation_probability",
    source_id = "fixture",
    source_fingerprint = "source",
    authority = "authority-A",
    algorithm = "algorithm-A",
    evidence_fingerprint = "evidence-A",
    contract_version = "contract-A",
    evidence = list(source.graph.id = "graph-A")
  )
  alignment <- list(
    status = "validated",
    contract.version = "contract-A",
    algorithm = "algorithm-A",
    evidence.fingerprint = "alignment-A"
  )
  base <- do.call(
    gflowui:::gflowui_basin_cache_key,
    c(args, list(
      vertex_mass_provenance = provenance,
      alignment_validation = alignment
    ))
  )
  variants <- list()
  variants$authority <- provenance
  variants$authority$attestations[[1L]]$authority <- "authority-B"
  variants$contract <- provenance
  variants$contract$attestations[[1L]]$contract.version <- "contract-B"
  variants$algorithm <- provenance
  variants$algorithm$attestations[[1L]]$algorithm <- "algorithm-B"
  variants$evidence <- provenance
  variants$evidence$attestations[[1L]]$evidence.fingerprint <- "evidence-B"
  for (variant in variants) {
    changed <- do.call(
      gflowui:::gflowui_basin_cache_key,
      c(args, list(
        vertex_mass_provenance = variant,
        alignment_validation = alignment
      ))
    )
    expect_false(identical(base, changed))
  }
  for (field in c("status", "contract.version", "algorithm",
                  "evidence.fingerprint")) {
    changed.alignment <- alignment
    changed.alignment[[field]] <- paste0(changed.alignment[[field]], "-B")
    changed <- do.call(
      gflowui:::gflowui_basin_cache_key,
      c(args, list(
        vertex_mass_provenance = provenance,
        alignment_validation = changed.alignment
      ))
    )
    expect_false(identical(base, changed))
  }
})

test_that("failed basin objects are never restored as cache hits", {
  adjacency <- list(2L, c(1L, 3L), 2L)
  weights <- lapply(adjacency, function(x) rep(1, length(x)))
  field <- c(0, 2, 1)
  build <- gflow::get.gflow.build.identity()
  key <- gflowui:::gflowui_basin_cache_key(
    adjacency,
    weights,
    field,
    NULL,
    paste0("cache-", 1:3),
    "failed-object-fixture",
    "failed-object-source",
    build
  )
  assign(
    key,
    list(status = "error", diagnostics = list(message = "old failure")),
    envir = gflowui:::.gflowui_basin_cache
  )
  result <- gflowui:::gflowui_estimate_basin_overlay(
    adjacency,
    weights,
    field,
    vertex_id = paste0("cache-", 1:3),
    source_key = "failed-object-fixture",
    source_fingerprint = "failed-object-source"
  )
  expect_false(isTRUE(result$cache_hit))
  expect_identical(
    get(key, envir = gflowui:::.gflowui_basin_cache)$status,
    "ok"
  )
})

test_that("construction identity changes for same-key fields and graph inputs", {
  graph <- list(
    graph.id = "graph",
    graph.k = 3L,
    graph.fingerprint = "graph-A",
    vertex.id.fingerprint = "vertices-A",
    display.vertex.id.fingerprint = "display-A"
  )
  identity <- list(build.id = "build", runtime = list(id = "runtime"))
  make_identity <- function(field, graph_identity = graph) {
    gflowui:::gflowui_basin_construction_identity(
      project_id = "project",
      graph_set_id = "set",
      graph_identity = graph_identity,
      source_key = "same-key",
      source_fingerprint = "source",
      field = field,
      vertex_mass = NULL,
      vertex_mass_provenance = NULL,
      alignment_validation = list(
        status = "validated",
        evidence.fingerprint = "evidence"
      ),
      build_identity = identity
    )
  }
  first <- make_identity(c(0, 1))
  expect_false(identical(
    first$fingerprint,
    make_identity(c(1, 0))$fingerprint
  ))
  graph$graph.fingerprint <- "graph-B"
  expect_false(identical(
    first$fingerprint,
    make_identity(c(0, 1), graph)$fingerprint
  ))
})

test_that("Plotly basin layers contain selected fills and minimum halos", {
  skip_if_not_installed("plotly")
  adjacency <- list(
    c(2L, 4L), c(1L, 3L, 5L), c(2L, 6L),
    c(1L, 5L), c(2L, 4L, 6L), c(3L, 5L)
  )
  weights <- lapply(adjacency, function(x) rep(1, length(x)))
  result <- gflowui:::gflowui_estimate_basin_overlay(
    adjacency,
    weights,
    c(0, 1, 0, 1, 3, 1),
    top_k_max = 2L,
    top_k_min = 2L,
    vertex_id = paste0("plotly-", 1:6),
    source_key = "plotly-layer-fixture"
  )
  result$table$selected <- TRUE
  selected.keys <- result$table$key[result$table$selected]
  result$values_max <- gflowui:::gflowui_basin_display_values(
    result$basin,
    result$table,
    selected.keys,
    "max"
  )
  result$values_min <- gflowui:::gflowui_basin_display_values(
    result$basin,
    result$table,
    selected.keys,
    "min"
  )
  coords <- cbind(1:6, (1:6)^2, (1:6)^3)
  specs <- gflowui:::gflowui_basin_layer_specs(
    result,
    visible_vertices = 1:6,
    point_size = 3,
    opacity = 0.8
  )
  expect_true(any(vapply(
    specs, function(x) identical(x$kind, "maximum_fill"), logical(1)
  )))
  expect_true(any(vapply(
    specs, function(x) identical(x$kind, "minimum_halo"), logical(1)
  )))
  plot <- gflowui:::gflowui_add_plotly_basin_layers(
    plotly::plot_ly(),
    specs,
    coords
  )
  traces <- plotly::plotly_build(plot)$x$data
  trace.names <- vapply(traces, function(x) as.character(x$name), character(1))
  expect_true(any(grepl("^M[0-9]+$", trace.names)))
  expect_true(any(grepl("^m[0-9]+ halo$", trace.names)))
  halos <- traces[grepl(" halo$", trace.names)]
  expect_true(all(vapply(
    halos,
    function(x) identical(x$marker$color, "rgba(255,255,255,0)") &&
      is.list(x$marker$line) && x$marker$line$width == 5,
    logical(1)
  )))
})

test_that("actual RGL basin layers contain the selected minimum vertices", {
  skip_if_not_installed("rgl")
  old <- options(rgl.useNULL = TRUE)
  on.exit(options(old), add = TRUE)
  device <- rgl::open3d(useNULL = TRUE)
  on.exit(try(rgl::close3d(device), silent = TRUE), add = TRUE)
  coords <- cbind(1:4, c(0, 1, 0, 1), c(1, 1, 2, 2))
  specs <- list(list(
    kind = "minimum_halo",
    key = "min|fixture",
    name = "m1 halo",
    vertices = c(2L, 4L),
    color = "#2563EB",
    rgl.size = 8,
    rgl.opacity = 0.5
  ))
  before <- rgl::rgl.ids(type = "shapes")
  ids <- gflowui:::gflowui_draw_rgl_basin_layers(coords, specs)
  after <- rgl::rgl.ids(type = "shapes")
  expect_length(ids, 1L)
  expect_true(all(ids %in% after$id))
  expect_gt(nrow(after), nrow(before))
})

test_that("strict graph-local density extrema are ranked deterministically", {
  adj_list <- list(
    c(2L, 3L),
    c(1L, 3L),
    c(1L, 2L, 4L),
    c(3L, 5L),
    4L,
    integer(0)
  )
  extrema <- gflowui:::gflowui_density_local_extrema(
    c(5, 1, 3, 0, 4, 9),
    adj_list
  )

  expect_equal(
    extrema,
    data.frame(
      vertex = c(1L, 5L, 4L, 2L),
      value = c(5, 4, 0, 1),
      type = c("maximum", "maximum", "minimum", "minimum"),
      rank = c(1L, 2L, 1L, 2L),
      label = c("M1", "M2", "m1", "m2"),
      stringsAsFactors = FALSE
    )
  )

  plateau <- gflowui:::gflowui_density_local_extrema(
    c(0, 0, 1),
    list(2L, c(1L, 3L), 2L)
  )
  expect_equal(plateau$vertex, 3L)
  expect_equal(plateau$label, "M1")
})

test_that("precomputed heat paths expose every time and current top-K basins", {
  root <- tempfile("gflowui-precomputed-path-")
  dir.create(file.path(root, "paths"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  source_vertex_id <- as.character(1:4)
  vertex_fingerprint <- gflowui:::gflowui_basin_sha256(list(
    schema = "hmp_graph_heat_vertices_v1",
    vertex.id = source_vertex_id
  ))
  graph_fingerprint <- paste(rep("a", 64L), collapse = "")
  display_vertex_fingerprint <- paste(rep("b", 64L), collapse = "")
  path <- list(
    contract.id = "fixture-path/1",
    subject.id = "15",
    settings = list(
      contract.id = "fixture-path/1",
      graph.id = "fixture-graph",
      graph.k = 3L
    ),
    selected = data.frame(
      graph.id = "fixture-graph",
      graph.k = 3L,
      graph.fingerprint = graph_fingerprint,
      vertex.fingerprint = vertex_fingerprint,
      stringsAsFactors = FALSE
    ),
    spectral.coordinates = data.frame(point.id = source_vertex_id),
    probability.mass = matrix(
      c(
        0.40, 0.30, 0.20, 0.10,
        0.10, 0.20, 0.30, 0.40
      ),
      nrow = 4L,
      ncol = 2L
    ),
    path.summary = data.frame(
      eta.index = 1:2,
      eta = c(0.5, 1.5),
      log.eta = log(c(0.5, 1.5)),
      mean.brier = c(0.2, 0.1),
      brier.selected = c(FALSE, TRUE)
    ),
    field.index = data.frame(
      field.id = c("eta_01", "eta_02"),
      path.parameter.index = 1:2
    ),
    raw.basins = data.frame(
      path.parameter.index = c(1L, 1L, 1L, 2L, 2L),
      raw.basin.id = c("a", "b", "c", "d", "e"),
      basin.mass = c(0.5, 0.4, 0.1, 0.6, 0.4),
      support.count = c(2L, 1L, 1L, 2L, 2L),
      peak.vertex.id = c(1L, 3L, 4L, 3L, 1L)
    ),
    assignments = data.frame(
      field.id = rep(c("eta_01", "eta_02"), each = 4L),
      point.id = rep(1:4, 2L),
      raw.basin.id = c("a", "a", "b", "c", "e", "e", "d", "d")
    )
  )
  saveRDS(path, file.path(root, "paths", "subject15.rds"))

  manifest <- list(
    project_root = root,
    occupation_density_sets = list(list(
      id = "subject15_path",
      basin_source_contract = list(
        contract.version = "fixture-path/1",
        graph.id = "fixture-graph",
        graph.k = 3L,
        graph.fingerprint = graph_fingerprint,
        vertex.id.fingerprint = vertex_fingerprint,
        source.vertex.id = source_vertex_id,
        display.vertex.id.fingerprint = display_vertex_fingerprint,
        algorithm = "fixture exact alignment"
      ),
      methods = list(list(
        id = "graph_heat_kernel",
        source = "precomputed_path",
        path_file = "paths/subject15.rds"
      ))
    ))
  )

  density <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject15_path",
    subject_id = "15",
    method_id = "graph_heat_kernel",
    mode = "parameters",
    parameters = list(eta_index = 1L, display_mode = "density")
  )
  expect_equal(density$values, c(0.4, 0.3, 0.2, 0.1))
  expect_equal(density$selected_eta_index, 2L)
  expect_equal(density$selected$eta, 0.5)
  expect_identical(density$color_type, "numeric")

  basins <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject15_path",
    subject_id = "15",
    method_id = "graph_heat_kernel",
    mode = "parameters",
    parameters = list(
      eta_index = 1L,
      display_mode = "top_k_basins",
      top_k = 2L
    )
  )
  expect_identical(basins$color_type, "categorical")
  expect_equal(basins$top_k, 2L)
  expect_equal(basins$basin_count, 3L)
  expect_equal(basins$basin_table$basin, c("a", "b"))
  expect_equal(
    basins$values,
    c(
      "Basin 01 (mass 0.500)",
      "Basin 01 (mass 0.500)",
      "Basin 02 (mass 0.400)",
      "Other basins"
    )
  )
})
