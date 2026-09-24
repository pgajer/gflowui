method_names_manifest <- function() {
  list(
    defaults = list(graph_set_id = "nearest", reference_graph_set_id = "nearest", reference_k = 4L),
    metadata = list(graph_selector_schema = list(fields = list(
      list(id = "method", field = "graph_method", label = "Graph / embedding method",
        order = c("sknn", "ian"),
        labels = c(sknn = "Symmetric kNN (EXP-037)", ian = "IAN (EXP-038)"))
    ))),
    graph_sets = list(
      list(id = "nearest", label = "Nearest neighbors", graph_method = "sknn", k_values = 4L),
      list(id = "adaptive", label = "Adaptive neighbors", graph_method = "ian", k_values = 4L)
    )
  )
}

test_that("renaming method labels preserves method values, graphs and selection", {
  helpers <- gflowui_make_server_graph_structure_helpers(new.env())
  manifest <- method_names_manifest()
  renamed <- helpers$rename_graph_method_entries(manifest, c(sknn = "  Nearest neighbors  ", ian = "Adaptive"))
  expect_identical(renamed$graph_sets, manifest$graph_sets)
  expect_identical(renamed$defaults, manifest$defaults)
  expect_identical(helpers$graph_method_name_choices(renamed), c("Nearest neighbors" = "sknn", Adaptive = "ian"))
  for (method in c("sknn", "ian")) {
    resolve <- function(m) helpers$resolve_graph_selection(m, m$graph_sets,
      input_selector_values = list(graph_selector_method = method), input_k = "4")
    expect_identical(resolve(renamed)$set_id, resolve(manifest)$set_id)
    expect_identical(resolve(renamed)$k_selected, resolve(manifest)$k_selected)
  }
  expect_error(helpers$rename_graph_method_entries(manifest, c(sknn = " ", ian = "Adaptive")), "name for every")
  expect_error(helpers$rename_graph_method_entries(manifest, c(sknn = "Same", ian = "same")), "different name")
  expect_error(helpers$rename_graph_method_entries(manifest, c(sknn = "Only one")), "methods have changed")

  manifest$graph_selector_schema <- manifest$metadata$graph_selector_schema
  manifest$metadata <- NULL
  renamed <- helpers$rename_graph_method_entries(manifest, c(sknn = "Nearest", ian = "Adaptive"))
  expect_null(renamed$metadata)
  expect_identical(helpers$graph_method_name_choices(renamed), c(Nearest = "sknn", Adaptive = "ian"))
  manifest$graph_selector_schema$fields <- "graph_method"
  renamed <- helpers$rename_graph_method_entries(manifest, c(sknn = "Nearest", ian = "Adaptive"))
  expect_identical(helpers$graph_method_name_choices(renamed), c(Adaptive = "ian", Nearest = "sknn"))
})

test_that("method name editor saves names across project reopening", {
  # Saves within one timestamp tick must still invalidate the manifest reader.
  testthat::local_mocked_bindings(.gflowui_now=function()"2026-09-24 00:00:00",.package="gflowui")
  root <- tempfile("method-names-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_options(list(gflowui.projects_data_dir = file.path(root, "registry")))
  manifest <- method_names_manifest()
  graph_file <- file.path(root, "graph.rds")
  saveRDS(list(X.graphs = list(list(
    adj_list = list(2L, c(1L, 3L), c(2L, 4L), 3L),
    weight_list = list(1, c(1, 1), c(1, 1), 1)
  )), k.values = 4L, selected.k = 4L), graph_file)
  manifest$graph_sets <- lapply(manifest$graph_sets, function(gs) {
    gs$graph_file <- graph_file
    gs
  })
  register_project(root, project_id = "methods", project_name = "Methods",
    profile = "custom", graph_sets = manifest$graph_sets,
    metadata = manifest$metadata, defaults = manifest$defaults, scan_results = FALSE)

  shiny::testServer(app_server, {
    open_project("methods")
    session$setInputs(graph_selector_method = "ian", graph_k = "4")
    before <- current_graph_selection()
    session$setInputs(graph_method_edit_names = 1L)
    expect_identical(unname(graph_method_name_editor()$choices), c("sknn", "ian"))
    session$setInputs(graph_method_name_1 = "Nearest", graph_method_name_2 = "Adaptive")
    # Typing (and therefore cancelling) leaves the saved labels untouched.
    expect_identical(graph_method_name_choices(active_manifest()), c(
      "Symmetric kNN (EXP-037)" = "sknn", "IAN (EXP-038)" = "ian"))
    session$setInputs(graph_method_save_names = 1L)
    expect_null(graph_method_name_editor())
    expect_identical(graph_method_name_choices(active_manifest()), c(Nearest = "sknn", Adaptive = "ian"))
    expect_identical(current_graph_selection()$set_id, before$set_id)
    expect_identical(current_graph_selection()$k_selected, before$k_selected)

    session$setInputs(graph_method_edit_names = 2L,
      graph_method_name_1 = " ", graph_method_name_2 = "Adaptive")
    session$setInputs(graph_method_save_names = 2L)
    expect_false(is.null(graph_method_name_editor()))
    expect_identical(graph_method_name_choices(active_manifest()), c(Nearest = "sknn", Adaptive = "ian"))
    open_project("methods")
    session$flushReact()
    expect_identical(graph_method_name_choices(active_manifest()), c(Nearest = "sknn", Adaptive = "ian"))
    saved <- gflowui_read_manifest(active_project_context()$row$manifest_file[[1L]])
    expect_identical(graph_method_name_choices(saved), c(Nearest = "sknn", Adaptive = "ian"))
  })
})
