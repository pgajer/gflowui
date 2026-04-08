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

test_that("graph selection resolver supports grouped selector schemas", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  manifest <- list(
    defaults = list(
      graph_set_id = "ct_biological__kegg__ge_1pct__hellinger_pca",
      reference_graph_set_id = "ct_biological__kegg__ge_1pct__hellinger_pca",
      reference_k = 5L
    ),
    metadata = list(
      graph_selector_schema = list(
        summary_label = "Graph family",
        fields = list(
          list(
            id = "dataset",
            field = "dataset_id",
            label = "Sample set",
            order = c("ct_clearance", "ct_biological"),
            labels = c(
              ct_clearance = "CT clearance",
              ct_biological = "CT biological"
            )
          ),
          list(
            id = "modality",
            field = "modality_id",
            label = "Feature space",
            order = c("vog_cluster", "kegg"),
            labels = c(
              vog_cluster = "VOG cluster",
              kegg = "KEGG"
            )
          ),
          list(
            id = "screen",
            field = "screen_name",
            label = "Prevalence screen"
          ),
          list(
            id = "representation",
            field = "representation",
            label = "Representation"
          )
        )
      )
    ),
    graph_sets = list(
      list(
        id = "ct_clearance__kegg__ge_1pct__relative_abundance_pca",
        label = "CT clearance / KEGG / ge_1pct / relative_abundance_pca",
        dataset_id = "ct_clearance",
        modality_id = "kegg",
        screen_name = "ge_1pct",
        representation = "relative_abundance_pca",
        k_values = c(5L, 6L)
      ),
      list(
        id = "ct_clearance__kegg__ge_1pct__hellinger_pca",
        label = "CT clearance / KEGG / ge_1pct / hellinger_pca",
        dataset_id = "ct_clearance",
        modality_id = "kegg",
        screen_name = "ge_1pct",
        representation = "hellinger_pca",
        k_values = c(4L, 5L)
      ),
      list(
        id = "ct_biological__kegg__ge_1pct__hellinger_pca",
        label = "CT biological / KEGG / ge_1pct / hellinger_pca",
        dataset_id = "ct_biological",
        modality_id = "kegg",
        screen_name = "ge_1pct",
        representation = "hellinger_pca",
        k_values = c(5L, 6L)
      )
    )
  )

  resolved <- helpers$resolve_graph_selection(
    manifest = manifest,
    graph_sets = manifest$graph_sets,
    input_selector_values = list(
      graph_selector_dataset = "ct_clearance",
      graph_selector_modality = "kegg",
      graph_selector_screen = "ge_1pct",
      graph_selector_representation = "hellinger_pca"
    ),
    input_k = NA_integer_
  )

  expect_true(isTRUE(resolved$grouped_selector_enabled))
  expect_equal(resolved$set_id, "ct_clearance__kegg__ge_1pct__hellinger_pca")
  expect_equal(resolved$k_selected, 5L)
  expect_length(resolved$selector_fields, 4L)
  expect_equal(unname(resolved$selector_fields[[1]]$choices), c("ct_clearance", "ct_biological"))
  expect_equal(as.character(resolved$selector_fields[[4]]$selected), "hellinger_pca")
  expect_match(resolved$selector_summary_value, "CT clearance", fixed = TRUE)
})

test_that("graph selector schema ignores fields absent from graph sets", {
  rv <- new.env(parent = emptyenv())
  helpers <- gflowui:::gflowui_make_server_graph_structure_helpers(rv = rv)

  manifest <- list(
    metadata = list(
      graph_selector_schema = list(
        fields = list(
          list(id = "dataset", field = "dataset_id", label = "Dataset"),
          list(id = "missing", field = "not_present", label = "Missing")
        )
      )
    )
  )
  graph_sets <- list(
    list(id = "one", label = "One", dataset_id = "ct_clearance", k_values = 5L)
  )

  schema <- helpers$graph_selector_schema(manifest, graph_sets)
  expect_true(isTRUE(schema$enabled))
  expect_length(schema$fields, 1L)
  expect_equal(schema$fields[[1]]$field, "dataset_id")
})
