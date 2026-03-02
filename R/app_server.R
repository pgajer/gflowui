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

  rv <- shiny::reactiveValues(
    project.active = FALSE,
    project.mode = "existing_graphs",
    project.name = "Untitled Project",
    project.show.data = FALSE
  )

  shiny::observeEvent(input$project_mode, {
    mode <- input$project_mode %||% "existing_graphs"
    if (!isTRUE(rv$project.active)) {
      default.name <- switch(
        mode,
        existing_graphs = "ZAPPS PreSSMat",
        clone = "ZAPPS PreSSMat Copy",
        template = "Template Project",
        scratch = "Untitled Project",
        "Untitled Project"
      )
      shiny::updateTextInput(session, "project_name", value = default.name)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_start, {
    mode <- input$project_mode %||% "scratch"
    nm <- input$project_name %||% "Untitled Project"
    rv$project.active <- TRUE
    rv$project.mode <- mode
    rv$project.name <- nm
    rv$project.show.data <- !identical(mode, "existing_graphs")
  })

  shiny::observeEvent(input$add_data_section, {
    rv$project.show.data <- TRUE
  })

  shiny::observeEvent(input$hide_data_section, {
    rv$project.show.data <- FALSE
  })

  output$project_status <- shiny::renderText({
    if (!isTRUE(rv$project.active)) {
      return("Project hub active. Choose mode and click 'Enter Workspace'.")
    }
    mode.txt <- switch(
      rv$project.mode,
      existing_graphs = sprintf("Existing: %s", input$project_existing %||% "unknown"),
      clone = sprintf("Clone of: %s", input$project_existing %||% "unknown"),
      template = sprintf("Template: %s", input$project_template %||% "unknown"),
      scratch = "From scratch",
      "Unknown mode"
    )
    sprintf("Workspace: %s | %s", rv$project.name %||% "Untitled Project", mode.txt)
  })

  output$workflow_controls <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(
        shiny::div(
          class = "gf-sidebar-note",
          shiny::strong("Workspace Locked"),
          shiny::p("Select a project mode and click 'Enter Workspace' to reveal workflow controls.")
        )
      )
    }

    actions <- NULL
    if (identical(rv$project.mode, "existing_graphs") && !isTRUE(rv$project.show.data)) {
      actions <- shiny::actionButton(
        "add_data_section",
        "Add Data",
        class = "btn-light gf-btn-wide"
      )
    } else if (identical(rv$project.mode, "existing_graphs") && isTRUE(rv$project.show.data)) {
      actions <- shiny::actionButton(
        "hide_data_section",
        "Hide Data Section",
        class = "btn-light gf-btn-wide"
      )
    }

    panels <- list()
    open.panels <- c("workflow_graph")

    if (isTRUE(rv$project.show.data)) {
      panels <- c(
        panels,
        list(bslib::accordion_panel("Data", value = "workflow_data", mod_data_ui("data")))
      )
      open.panels <- c("workflow_data", open.panels)
    }

    panels <- c(
      panels,
      list(
        bslib::accordion_panel("Graph(s) Construction", value = "workflow_graph", shiny::tagList(
          mod_graph_ui("graph"),
          shiny::hr(),
          mod_visualize_ui("viz")
        )),
        bslib::accordion_panel(
          "Conditional Expectation Estimation",
          value = "workflow_condexp",
          mod_condexp_ui("condexp")
        ),
        bslib::accordion_panel("Analysis", value = "workflow_analysis", shiny::div(
          class = "gf-analysis-placeholder",
          shiny::p("Analysis tools section placeholder."),
          shiny::p("Future versions will expose downstream comparison, trajectory summaries, and reporting workflows.")
        )),
        bslib::accordion_panel("Run Monitor", value = "workflow_monitor", shiny::div(
          class = "gf-status-block",
          shiny::verbatimTextOutput("run_monitor")
        ))
      )
    )

    shiny::div(
      class = "gf-sidebar-panel gf-accordion-wrap",
      if (!is.null(actions)) shiny::div(class = "gf-workflow-actions", actions),
      do.call(
        bslib::accordion,
        c(
          list(id = "workflow_accordion", open = open.panels, multiple = TRUE),
          panels
        )
      )
    )
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
    shiny::span(class = "gf-chip", "3D viewport: reserved")
  })

  output$chip_project <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(shiny::span(class = "gf-chip", "Project: not started"))
    }
    shiny::span(class = "gf-chip", sprintf("Project: %s", rv$project.name %||% "Untitled Project"))
  })

  output$run_monitor <- shiny::renderText({
    dat <- data_state()
    g <- graph_state()
    cfit <- condexp_state()
    vz <- viz_state()

    proj.msg <- if (!isTRUE(rv$project.active)) {
      "Project: not started"
    } else {
      sprintf("Project: %s | Mode: %s", rv$project.name %||% "Untitled Project", rv$project.mode %||% "unknown")
    }

    data.msg <- if (is.null(dat$data)) {
      "Data: not loaded"
    } else {
      sprintf("Data: %d rows x %d columns | Source: %s", nrow(dat$data), ncol(dat$data), dat$source %||% "unknown")
    }

    graph.msg <- sprintf("Graphs: %s", g$status %||% "not run")
    condexp.msg <- sprintf("CondExp: %s", cfit$status %||% "not run")
    viz.msg <- sprintf("Endpoints/View: %s", vz$status %||% "not run")

    paste(proj.msg, data.msg, graph.msg, condexp.msg, viz.msg, sep = "\n\n")
  })

  output$workspace_view <- shiny::renderUI({
    vz <- viz_state()
    ep <- vz$endpoint.result
    n.ep <- if (is.null(ep) || is.null(ep$endpoints)) 0L else length(ep$endpoints)

    shiny::div(
      class = "gf-viewer-canvas",
      shiny::div(
        class = "gf-viewer-overlay",
        shiny::h3("3D Viewport"),
        shiny::p("This area is reserved for interactive graph and conditional expectation rendering."),
        shiny::p(sprintf("Active project: %s", if (isTRUE(rv$project.active)) rv$project.name else "none")),
        shiny::p(sprintf("Detected endpoints available: %d", n.ep))
      )
    )
  })
}
