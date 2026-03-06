test_that("spectral layout selection honors requested graph family", {
  root <- tempfile("renderer-helper-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  hv50_path <- file.path(root, "fit_hv50_k10.rds")
  all_path <- file.path(root, "fit_all_k10.rds")
  hv50_coords <- matrix(seq_len(15), nrow = 5, ncol = 3)
  all_coords <- matrix(seq_len(15) + 100, nrow = 5, ncol = 3)

  saveRDS(
    list(
      fitted.values = seq_len(5),
      spectral = list(eigenvectors = cbind(rep(0, 5), hv50_coords))
    ),
    hv50_path
  )
  saveRDS(
    list(
      fitted.values = seq_len(5),
      spectral = list(eigenvectors = cbind(rep(0, 5), all_coords))
    ),
    all_path
  )

  manifest <- list(
    condexp_sets = list(list(
      id = "condexp",
      family_runs = list(
        list(family = "hv50", fit_files = hv50_path),
        list(family = "all", fit_files = all_path)
      )
    ))
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  picked_all <- renderer_helpers$collect_reference_condexp_sources(
    manifest = manifest,
    set_id = "all",
    k_use = 10L,
    n_vertices = 5L
  )
  expect_true(is.matrix(picked_all$spectral_coords))
  expect_equal(unname(picked_all$spectral_coords), unname(all_coords))

  picked_top50 <- renderer_helpers$collect_reference_condexp_sources(
    manifest = manifest,
    set_id = "top50",
    k_use = 10L,
    n_vertices = 5L
  )
  expect_true(is.matrix(picked_top50$spectral_coords))
  expect_equal(unname(picked_top50$spectral_coords), unname(hv50_coords))
})

test_that("binary fit-file condexp adds rel.y.hat color source", {
  root <- tempfile("renderer-condexp-rel-fit-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  fit_path <- file.path(root, "fit_all_k10.rds")
  y <- c(0, 1, 1, 0, 1)
  yhat <- c(0.1, 0.8, 0.7, 0.2, 0.9)
  saveRDS(list(y = y, fitted.values = yhat), fit_path)

  manifest <- list(
    condexp_sets = list(list(
      id = "vag_odor_binary",
      outcomes = "vag_odor",
      family_runs = list(
        list(family = "all", fit_files = fit_path)
      )
    ))
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  out <- renderer_helpers$collect_reference_condexp_sources(
    manifest = manifest,
    set_id = "all",
    k_use = 10L,
    n_vertices = length(y)
  )

  labels <- vapply(out$sources, function(src) {
    if (!is.list(src) || is.null(src$label)) {
      return("")
    }
    as.character(src$label[[1]])
  }, character(1))
  rel_idx <- which(labels == "VAG_ODOR rel.y.hat")
  expect_equal(length(rel_idx), 1L)
  rel_vals <- out$sources[[rel_idx]]$values
  expect_equal(rel_vals, yhat / mean(y))
})

test_that("rel.y.hat is added only for binary long-table outcomes", {
  root <- tempfile("renderer-condexp-rel-long-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  long_path <- file.path(root, "condexp.long.rds")
  n <- 4L
  tbl <- rbind(
    data.frame(
      k = 7L,
      outcome = "ibs",
      y_fitted = c(0.2, 0.4, 0.6, 0.8),
      y_observed = c(0, 1, 0, 1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      k = 7L,
      outcome = "severity_score",
      y_fitted = c(0.1, 0.2, 0.3, 0.4),
      y_observed = c(0.2, 0.4, 0.6, 0.8),
      stringsAsFactors = FALSE
    )
  )
  saveRDS(tbl, long_path)

  manifest <- list(
    condexp_sets = list(list(
      id = "ibs_ibd_benchmark",
      long_table_file = long_path
    ))
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  out <- renderer_helpers$collect_reference_condexp_sources(
    manifest = manifest,
    set_id = "shared_all_asv",
    k_use = 7L,
    n_vertices = n
  )

  labels <- vapply(out$sources, function(src) {
    if (!is.list(src) || is.null(src$label)) {
      return("")
    }
    as.character(src$label[[1]])
  }, character(1))
  expect_true("IBS rel.y.hat" %in% labels)
  expect_false("Severity score rel.y.hat" %in% labels)
})


test_that("project layout manifest matrix resolves set and k specific layout", {
  root <- tempfile("renderer-layout-manifest-")
  dir.create(file.path(root, "results", "asv_hv_k_gcv_sweep"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  layout_top20 <- file.path(root, "layout_top20_k10.rds")
  layout_top30 <- file.path(root, "layout_top30_k10.rds")
  saveRDS(matrix(seq_len(18), nrow = 6, ncol = 3), layout_top20)
  saveRDS(matrix(seq_len(18) + 50, nrow = 6, ncol = 3), layout_top30)

  manifest_csv <- file.path(root, "results", "asv_hv_k_gcv_sweep", "asv_layouts_html_manifest.csv")
  utils::write.csv(
    data.frame(
      set.tag = c("top20", "top30"),
      k = c(10L, 10L),
      sphere.scale = c(0.5, 0.5),
      size.tag = c("s0p50", "s0p50"),
      file = c("unused_top20.html", "unused_top30.html"),
      layout.file = c(layout_top20, layout_top30),
      status = c("written", "written"),
      stringsAsFactors = FALSE
    ),
    manifest_csv,
    row.names = FALSE
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  mat <- renderer_helpers$project_layout_manifest_matrix(
    project_root = root,
    spec = list(set_id = "top20", k_ref = 10L),
    size_label = "0.5x"
  )

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(6L, 3L))
  expect_equal(unname(mat), unname(readRDS(layout_top20)))
})


test_that("grip layout matrix resolver uses explicit layout_assets metadata", {
  root <- tempfile("renderer-grip-layout-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  k10 <- file.path(root, "set_k10_layout3d.rds")
  k12 <- file.path(root, "set_k12_layout3d.rds")
  mat10 <- matrix(seq_len(18), nrow = 6, ncol = 3)
  mat12 <- matrix(seq_len(18) + 200, nrow = 6, ncol = 3)
  saveRDS(mat10, k10)
  saveRDS(mat12, k12)

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  graph_set <- list(
    layout_assets = list(
      grip_layouts = list(
        k10 = list(k = 10L, path = k10, source = "grip.layout"),
        k12 = list(k = 12L, path = k12, source = "grip.layout")
      )
    )
  )

  got10 <- renderer_helpers$grip_layout_matrix_for_graph_set(graph_set, k_ref = 10L)
  expect_true(is.matrix(got10))
  expect_equal(unname(got10), unname(mat10))

  got11 <- renderer_helpers$grip_layout_matrix_for_graph_set(graph_set, k_ref = 11L)
  expect_true(is.matrix(got11))
  expect_equal(unname(got11), unname(mat10))
})


test_that("endpoint evenness sources are exposed from per-k endpoint bundles", {
  root <- tempfile("renderer-endpoint-evenness-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  bundle <- file.path(root, "evenness_k07_endpoints.rds")
  raw_vals <- c(0.10, 0.20, 0.30, 0.40, 0.50)
  fit_vals <- c(0.15, 0.25, 0.35, 0.45, 0.55)
  saveRDS(
    list(
      k = 7L,
      sample.evenness = raw_vals,
      fitted.evenness = fit_vals
    ),
    bundle
  )

  manifest <- list(
    endpoint_runs = list(list(
      id = "evenness_k07",
      per_k_bundles = bundle
    )),
    defaults = list(endpoint_run_id = "evenness_k07")
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  out <- renderer_helpers$collect_reference_endpoint_sources(
    manifest = manifest,
    k_use = 7L,
    n_vertices = 5L
  )

  expect_true("endpoint_evenness_condexp" %in% names(out))
  expect_true("endpoint_evenness" %in% names(out))
  expect_equal(out$endpoint_evenness_condexp$label, "Evenness CondExp")
  expect_equal(out$endpoint_evenness$label, "Evenness")
  expect_equal(out$endpoint_evenness_condexp$values, fit_vals)
  expect_equal(out$endpoint_evenness$values, raw_vals)
})


test_that("endpoint evenness condexp expands LCC vectors to full graph", {
  root <- tempfile("renderer-endpoint-lcc-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  bundle <- file.path(root, "evenness_endpoints_k05_bundle.rds")
  saveRDS(
    list(
      k = 5L,
      sample.evenness.raw = c(1, 2, 3, 4, 5, 6),
      fitted.evenness.lcc = c(0.2, 0.4, 0.6, 0.8),
      lcc = list(lcc.index.global = c(1L, 2L, 3L, 4L))
    ),
    bundle
  )

  manifest <- list(
    endpoint_runs = list(list(
      id = "evenness_k05",
      bundle_file = bundle
    )),
    defaults = list(endpoint_run_id = "evenness_k05")
  )

  adj_list <- list(
    c(2L, 3L),
    c(1L, 3L),
    c(1L, 2L, 4L),
    c(3L),
    c(6L),
    c(5L)
  )

  rv <- new.env(parent = emptyenv())
  rv$reference.layout.cache <- list()
  rv$reference.html.cache <- list()
  rv$html.resource.map <- list()

  graph_helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui:::gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = graph_helpers$current_reference_info
  )

  out <- renderer_helpers$collect_reference_endpoint_sources(
    manifest = manifest,
    k_use = 5L,
    n_vertices = 6L,
    reference_adj_list = adj_list
  )

  expect_true("endpoint_evenness_condexp" %in% names(out))
  expect_true("endpoint_evenness" %in% names(out))
  expect_equal(
    out$endpoint_evenness_condexp$values,
    c(0.2, 0.4, 0.6, 0.8, NA_real_, NA_real_)
  )
  expect_equal(out$endpoint_evenness$values, c(1, 2, 3, 4, 5, 6))
})
