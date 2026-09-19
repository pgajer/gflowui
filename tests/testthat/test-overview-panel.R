test_that("Overview retains its open state alongside other workflow panels", {
  root <- tempfile("overview-panel-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_options(list(gflowui.projects_data_dir = file.path(root, "registry")))
  graph_file <- file.path(root, "graph.rds")
  saveRDS(list(X.graphs = list(list(
    adj_list = list(2L, c(1L, 3L), c(2L, 4L), 3L),
    weight_list = list(1, c(1, 1), c(1, 1), 1)
  )), k.values = 4L, selected.k = 4L), graph_file)
  register_project(root, project_id = "overview", profile = "custom",
    graph_sets = list(list(id = "graph", label = "Graph", graph_file = graph_file, k_values = 4L)),
    defaults = list(graph_set_id = "graph", reference_graph_set_id = "graph", reference_k = 4L),
    metadata = list(overview = list(summary_table = data.frame(Project = "Overview test"))),
    scan_results = FALSE)

  shiny::testServer(app_server, {
    open_project("overview")
    session$flushReact()
    overview_html <- function() {
      html <- htmltools::renderTags(output$workflow_controls)$html
      items <- strsplit(html, '<div class="accordion-item"', fixed = TRUE)[[1L]][-1L]
      items[grepl('data-value="workflow_overview"', items, fixed = TRUE)]
    }
    session$setInputs(workflow_accordion = "workflow_graph_structure")
    expect_match(overview_html(), 'aria-expanded="false"', fixed = TRUE)
    session$setInputs(workflow_accordion = c("workflow_graph_structure", "workflow_overview"))
    expect_match(overview_html(), 'aria-expanded="true"', fixed = TRUE)

    session$setInputs(endpoint_label_size = 1.6)
    expect_match(overview_html(), 'aria-expanded="true"', fixed = TRUE)
    session$setInputs(workflow_accordion = NULL, endpoint_label_size = 1.8)
    expect_match(overview_html(), 'aria-expanded="true"', fixed = TRUE)

    session$setInputs(workflow_accordion = "workflow_overview")
    expect_match(overview_html(), 'aria-expanded="true"', fixed = TRUE)
    session$setInputs(workflow_accordion = character(0))
    expect_match(overview_html(), 'aria-expanded="false"', fixed = TRUE)
  })
})
