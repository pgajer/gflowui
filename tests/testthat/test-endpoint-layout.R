test_that("Endpoint Layout is nested in Endpoints and retains its open state on edits", {
  root <- tempfile("endpoint-layout-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_options(list(gflowui.projects_data_dir = file.path(root, "registry")))
  graph_file <- file.path(root, "graph.rds")
  saveRDS(list(X.graphs = list(list(
    adj_list = list(2L, c(1L, 3L), c(2L, 4L), 3L),
    weight_list = list(1, c(1, 1), c(1, 1), 1)
  )), k.values = 4L, selected.k = 4L), graph_file)
  register_project(root, project_id = "layout", profile = "custom",
    graph_sets = list(list(id = "graph", label = "Graph", graph_file = graph_file, k_values = 4L)),
    defaults = list(graph_set_id = "graph", reference_graph_set_id = "graph", reference_k = 4L),
    scan_results = FALSE)

  shiny::testServer(app_server, {
    open_project("layout")
    session$flushReact()
    panels <- function() {
      html <- htmltools::renderTags(output$workflow_controls)$html
      strsplit(html, '<div class="accordion-item"', fixed = TRUE)[[1L]][-1L]
    }
    panel <- function(id) {
      items <- panels()
      items[grepl(paste0('data-value="', id, '"'), items, fixed = TRUE)]
    }
    expect_length(panel("workflow_endpoint_layout"), 0L)
    expect_match(panel("workflow_endpoint_structure"), 'id="endpoint_layout_details"', fixed = TRUE)
    expect_match(panel("workflow_endpoint_structure"), 'id="endpoint_label_size"', fixed = TRUE)
    layout_is_open <- function() {
      grepl('<details[^>]*id="endpoint_layout_details"[^>]*open="open"',
        panel("workflow_endpoint_structure"))
    }
    expect_false(layout_is_open())
    session$setInputs(workflow_accordion = "workflow_endpoint_structure", endpoint_layout_open = TRUE)
    changes <- list(endpoint_label_size = 1.6, endpoint_label_offset = "2x",
      endpoint_marker_size = "1.50x", endpoint_marker_color = "#3b82f6")
    for (id in names(changes)) {
      do.call(session$setInputs, changes[id])
      expect_true(layout_is_open())
      expect_match(panel("workflow_endpoint_structure"), 'aria-expanded="true"', fixed = TRUE)
    }
    state <- reference_renderer_state()
    expect_equal(state$endpoint_label_size, 1.6)
    expect_equal(state$endpoint_label_offset, 2)
    expect_equal(state$endpoint_marker_size, 1.5)
    expect_identical(state$endpoint_marker_color, "#3b82f6")

    # Shiny temporarily unbinds the accordion while replacing its markup.
    session$setInputs(workflow_accordion = NULL, endpoint_layout_open = NULL, endpoint_label_size = 1.8)
    expect_true(layout_is_open())
    session$setInputs(workflow_accordion = "workflow_endpoint_structure", endpoint_layout_open = FALSE)
    session$setInputs(endpoint_label_size = 2)
    expect_false(layout_is_open())
  })
})
