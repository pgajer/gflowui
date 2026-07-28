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
    source_fingerprint = "fixture-source"
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
    source_fingerprint = "fixture-source"
  )

  expect_identical(first$direction, "both")
  expect_equal(nrow(first$summary$maxima), 0L)
  expect_equal(nrow(first$summary$minima), 2L)
  expect_true(isTRUE(second$cache_hit))
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
    source_fingerprint = "fixture-source"
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
    source_fingerprint = "fixture-source"
  )
  expect_equal(nrow(empty$table), 0L)
  expect_true(all(empty$values_max == "Other basins"))
  expect_true(all(empty$values_min == "Other basins"))
  expect_equal(nrow(restored$table), 4L)
  expect_true(isTRUE(empty$cache_hit))
  expect_true(isTRUE(restored$cache_hit))
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
      label = c("M_1", "M_2", "m_1", "m_2"),
      stringsAsFactors = FALSE
    )
  )

  plateau <- gflowui:::gflowui_density_local_extrema(
    c(0, 0, 1),
    list(2L, c(1L, 3L), 2L)
  )
  expect_equal(plateau$vertex, 3L)
  expect_equal(plateau$label, "M_1")
})

test_that("precomputed heat paths expose every time and current top-K basins", {
  root <- tempfile("gflowui-precomputed-path-")
  dir.create(file.path(root, "paths"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  path <- list(
    subject.id = "15",
    settings = list(graph.k = 3L),
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
