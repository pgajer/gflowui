test_that("optimal-k display resolver prefers set-specific PDFs from figures dir", {
  root <- tempfile("optimal-k-resolver-")
  csv_dir <- file.path(root, "results", "vag_odor_asv_graph_gcv_sweep", "hv20")
  fig_dir <- file.path(root, "results", "vag_odor_asv_graph_gcv_sweep", "figures")
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  csv_path <- file.path(csv_dir, "vag_odor_gcv_by_k.csv")
  utils::write.csv(
    data.frame(k = c(5L, 6L, 7L), gcv = c(0.32, 0.28, 0.31)),
    csv_path,
    row.names = FALSE
  )

  hv20_pdf <- file.path(fig_dir, "hv20_vag_odor_gcv_vs_k.pdf")
  combo_pdf <- file.path(fig_dir, "hv20_hv30_hv50_all_vag_odor_gcv_vs_k.pdf")
  writeLines("dummy", hv20_pdf)
  writeLines("dummy", combo_pdf)

  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  tokens <- helpers$graph_alias_tokens("top20", "ASV HV20")

  picked <- helpers$resolve_optimal_k_display_path(
    path = csv_path,
    set_tokens = tokens,
    method_id = "response_gcv",
    cache_dir = file.path(root, "cache")
  )

  expect_equal(basename(picked), "hv20_vag_odor_gcv_vs_k.pdf")
})


test_that("optimal-k display resolver generates cached PDF from CSV when needed", {
  root <- tempfile("optimal-k-cache-")
  csv_dir <- file.path(root, "results")
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  csv_path <- file.path(csv_dir, "response_gcv_by_k.csv")
  utils::write.csv(
    data.frame(k = c(4L, 5L, 6L, 7L), gcv = c(0.42, 0.39, 0.37, 0.4)),
    csv_path,
    row.names = FALSE
  )

  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  out_pdf <- helpers$resolve_optimal_k_display_path(
    path = csv_path,
    set_tokens = c("all"),
    method_id = "response_gcv",
    cache_dir = file.path(root, "cache")
  )

  expect_true(nzchar(out_pdf))
  expect_true(file.exists(out_pdf))
  expect_match(basename(out_pdf), "\\.pdf$")
})

test_that("large graphs default to point vertex layout", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  expect_equal(
    helpers$default_vertex_layout_for_graph(preset = NA_character_, n_vertices = 500L),
    "point"
  )
  expect_equal(
    helpers$default_vertex_layout_for_graph(preset = "sphere", n_vertices = 24378L),
    "point"
  )
  expect_equal(
    helpers$default_vertex_layout_for_graph(preset = "sphere", n_vertices = 500L),
    "sphere"
  )
  expect_equal(
    helpers$default_vertex_layout_for_graph(preset = "point", n_vertices = 500L),
    "point"
  )
})


test_that("project metadata inference is silent when optional AGP metadata is missing", {
  root <- tempfile("graph-dims-agp-")
  dir.create(file.path(root, "results", "asv_full_graph_hv_criteria_k_selection"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  utils::write.csv(
    data.frame(n.samples = 24378L, graph.features = 999L),
    file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "summary.across.criteria.csv"),
    row.names = FALSE
  )

  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  expect_silent({
    dims <- helpers$infer_graph_dims_from_project_metadata(
      project_root = root,
      set_id = "shared_all_asv"
    )
  })
  expect_equal(dims$n_samples, 24378L)
  expect_equal(dims$n_features, 999L)
})

test_that("project metadata inference reads AGP shared-graph run metadata", {
  root <- tempfile("graph-dims-agp-shared-")
  dir.create(file.path(root, "results", "asv_hv_k_gcv_sweep"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  saveRDS(
    list(asv.samples = 24378L, asv.features = 955L, sample_set.count = 24378L),
    file.path(root, "results", "asv_hv_k_gcv_sweep", "run.metadata.rds")
  )

  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  dims <- helpers$infer_graph_dims_from_project_metadata(
    project_root = root,
    set_id = "shared_all_asv"
  )

  expect_equal(dims$n_samples, 24378L)
  expect_equal(dims$n_features, 955L)
})

test_that("graph selection resolver prefers sticky k over reference fallback", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  manifest <- list(
    defaults = list(
      graph_set_id = "shared_all_asv",
      reference_graph_set_id = "shared_all_asv",
      reference_k = 7L
    ),
    graph_sets = list(
      list(id = "shared_all_asv", label = "Shared All ASV", k_values = c(6L, 7L, 8L))
    )
  )

  resolved <- helpers$resolve_graph_selection(
    manifest = manifest,
    graph_sets = manifest$graph_sets,
    input_set_id = "",
    input_k = NA_integer_,
    sticky_set_id = "shared_all_asv",
    sticky_k = 6L
  )

  expect_equal(resolved$set_id, "shared_all_asv")
  expect_equal(resolved$k_selected, 6L)
})

test_that("graph selection resolver falls back to reference k when sticky k is invalid", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  manifest <- list(
    defaults = list(
      graph_set_id = "shared_all_asv",
      reference_graph_set_id = "shared_all_asv",
      reference_k = 7L
    ),
    graph_sets = list(
      list(id = "shared_all_asv", label = "Shared All ASV", k_values = c(6L, 7L, 8L))
    )
  )

  resolved <- helpers$resolve_graph_selection(
    manifest = manifest,
    graph_sets = manifest$graph_sets,
    input_set_id = "",
    input_k = NA_integer_,
    sticky_set_id = "shared_all_asv",
    sticky_k = 999L
  )

  expect_equal(resolved$set_id, "shared_all_asv")
  expect_equal(resolved$k_selected, 7L)
})

test_that("graph selection resolver honors preferred project-open default before reference", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  manifest <- list(
    defaults = list(
      graph_set_id = "shared_all_asv",
      reference_graph_set_id = "shared_all_asv",
      reference_k = 7L
    ),
    graph_sets = list(
      list(id = "shared_all_asv", label = "Shared All ASV", k_values = c(6L, 7L, 8L))
    )
  )

  resolved <- helpers$resolve_graph_selection(
    manifest = manifest,
    graph_sets = manifest$graph_sets,
    input_set_id = "",
    input_k = NA_integer_,
    preferred_default_set_id = "shared_all_asv",
    preferred_default_k = 6L,
    sticky_set_id = "",
    sticky_k = NA_integer_
  )

  expect_equal(resolved$set_id, "shared_all_asv")
  expect_equal(resolved$k_selected, 6L)
})
