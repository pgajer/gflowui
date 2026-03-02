app_server <- function(input, output, session) {
  data_state <- mod_data_server("data")
  graph_state <- mod_graph_server("graph", data_state = data_state)
  condexp_state <- mod_condexp_server(
    "condexp",
    data_state = data_state,
    graph_state = graph_state
  )
  viz_state <- mod_visualize_server(
    "viz",
    data_state = data_state,
    graph_state = graph_state,
    condexp_state = condexp_state
  )

  `%||%` <- function(x, y) {
    if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  }

  shiny::observeEvent(input$project_new, {
    shiny::updateTextInput(session, "project_name", value = "Untitled Project")
  })

  shiny::observeEvent(input$project_clone, {
    nm <- input$project_name %||% "Untitled Project"
    shiny::updateTextInput(session, "project_name", value = paste0(nm, " Copy"))
  })

  output$project_status <- shiny::renderText({
    tpl <- switch(
      input$project_template %||% "empty",
      zapps_pressmat = "Template selected: ZAPPS/PreSSMat (placeholder)",
      "Template selected: Empty project"
    )
    sprintf("Active project: %s | %s", input$project_name %||% "Untitled Project", tpl)
  })

  output$chip_backend <- shiny::renderUI({
    backend <- if (requireNamespace("gflow", quietly = TRUE)) {
      sprintf("R + gflow backend: gflow %s", as.character(utils::packageVersion("gflow")))
    } else {
      "R + gflow backend: gflow not installed"
    }
    shiny::span(class = "gf-chip", backend)
  })

  output$chip_renderer <- shiny::renderUI({
    shiny::span(class = "gf-chip", "3D-ready outputs: workspace mode")
  })

  output$chip_project <- shiny::renderUI({
    shiny::span(class = "gf-chip", sprintf("Project: %s", input$project_name %||% "Untitled Project"))
  })

  output$workspace_status <- shiny::renderText({
    dat <- data_state()
    g <- graph_state()
    cfit <- condexp_state()
    vz <- viz_state()

    data.msg <- if (is.null(dat$data)) {
      "Data: not loaded"
    } else {
      sprintf("Data: %d rows x %d columns | Source: %s", nrow(dat$data), ncol(dat$data), dat$source %||% "unknown")
    }

    graph.msg <- sprintf("Graphs: %s", g$status %||% "not run")
    condexp.msg <- sprintf("CondExp: %s", cfit$status %||% "not run")
    viz.msg <- sprintf("Endpoints/View: %s", vz$status %||% "not run")

    paste(data.msg, graph.msg, condexp.msg, viz.msg, sep = "\n\n")
  })

  output$workspace_endpoint_table <- shiny::renderTable({
    vz <- viz_state()
    ep <- vz$endpoint.result
    if (is.null(ep) || length(ep$endpoints) == 0L) {
      return(NULL)
    }

    s <- ep$summary
    if (!is.null(s) && is.data.frame(s) && nrow(s) > 0L) {
      keep <- c(
        "vertex",
        "eccentricity",
        "distance.to.core",
        "distance_to_core",
        "endpoint.rank",
        "endpoint_rank",
        "is.endpoint",
        "is_endpoint"
      )
      cols <- intersect(keep, colnames(s))
      out <- s[, cols, drop = FALSE]

      endpoint.col <- if ("is.endpoint" %in% colnames(out)) {
        "is.endpoint"
      } else if ("is_endpoint" %in% colnames(out)) {
        "is_endpoint"
      } else {
        NULL
      }

      if (!is.null(endpoint.col)) {
        out <- out[which(as.logical(out[[endpoint.col]])), , drop = FALSE]
      } else if ("vertex" %in% colnames(out)) {
        out <- out[out$vertex %in% ep$endpoints, , drop = FALSE]
      }

      if (nrow(out) > 0L) {
        return(utils::head(out, 20L))
      }
    }

    data.frame(endpoint = ep$endpoints, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$workspace_data_table <- shiny::renderTable({
    dat <- data_state()
    if (is.null(dat$data)) {
      return(NULL)
    }
    utils::head(dat$data, 15L)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}
