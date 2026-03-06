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
