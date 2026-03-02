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

  default_projects <- data.frame(
    id = c("zapps_pressmat", "vmrc_demo"),
    label = c("ZAPPS/PreSSMat", "VMRC Demo Cohort"),
    origin = c("existing", "existing"),
    has_graphs = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  template_catalog <- data.frame(
    id = c("zapps_pressmat_template", "empty_template"),
    label = c("ZAPPS/PreSSMat", "Empty template"),
    has_graphs = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  registry_columns <- c("id", "label", "origin", "has_graphs")
  registry_path <- file.path(
    tools::R_user_dir("gflowui", which = "data"),
    "projects",
    "registry.rds"
  )

  sanitize_registry <- function(x) {
    if (!is.data.frame(x) || nrow(x) < 1L) {
      return(default_projects)
    }

    missing_cols <- setdiff(registry_columns, names(x))
    if (length(missing_cols) > 0L) {
      return(default_projects)
    }

    out <- x[, registry_columns, drop = FALSE]
    out$id <- as.character(out$id)
    out$label <- as.character(out$label)
    out$origin <- as.character(out$origin)
    out$has_graphs <- as.logical(out$has_graphs)
    out <- out[stats::complete.cases(out[, c("id", "label", "origin")]), , drop = FALSE]
    out <- out[nzchar(out$id) & nzchar(out$label), , drop = FALSE]
    out <- out[!duplicated(out$id), , drop = FALSE]

    if (nrow(out) < 1L) {
      return(default_projects)
    }
    rownames(out) <- NULL
    out
  }

  load_registry <- function() {
    if (!file.exists(registry_path)) {
      return(default_projects)
    }

    loaded <- tryCatch(readRDS(registry_path), error = function(e) NULL)
    sanitize_registry(loaded)
  }

  save_registry <- function(registry_df) {
    dir.create(dirname(registry_path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(
      pattern = "registry-",
      tmpdir = dirname(registry_path),
      fileext = ".rds"
    )

    tryCatch(
      {
        saveRDS(registry_df, file = tmp)

        moved <- file.rename(tmp, registry_path)
        if (!moved) {
          moved <- file.copy(tmp, registry_path, overwrite = TRUE)
        }
        if (!moved) {
          stop("Could not write registry file.")
        }
      },
      error = function(e) {
        warning(
          sprintf("gflowui: failed to persist project registry: %s", conditionMessage(e)),
          call. = FALSE
        )
      },
      finally = {
        if (file.exists(tmp)) {
          unlink(tmp, force = TRUE)
        }
      }
    )
    invisible(NULL)
  }

  project_registry <- shiny::reactiveVal(load_registry())

  rv <- shiny::reactiveValues(
    project.active = FALSE,
    project.id = NULL,
    project.origin = NULL,
    project.name = "Untitled Project",
    project.has.graphs = FALSE,
    project.show.data = FALSE
  )

  make_project_id <- function(label, existing_ids) {
    base <- tolower(gsub("[^a-zA-Z0-9]+", "_", label %||% "project"))
    base <- gsub("^_+|_+$", "", base)
    if (!nzchar(base)) {
      base <- "project"
    }

    candidate <- base
    suffix <- 2L
    while (candidate %in% existing_ids) {
      candidate <- sprintf("%s_%d", base, suffix)
      suffix <- suffix + 1L
    }
    candidate
  }

  open_project <- function(project_id) {
    if (!nzchar(project_id %||% "")) {
      return(invisible(NULL))
    }

    reg <- project_registry()
    idx <- match(project_id, reg$id)
    if (is.na(idx)) {
      return(invisible(NULL))
    }

    row <- reg[idx, , drop = FALSE]
    rv$project.active <- TRUE
    rv$project.id <- row$id[[1]]
    rv$project.origin <- row$origin[[1]]
    rv$project.name <- row$label[[1]]
    rv$project.has.graphs <- isTRUE(row$has_graphs[[1]])
    rv$project.show.data <- !isTRUE(row$has_graphs[[1]])

    invisible(NULL)
  }

  populate_project_select <- function(selected = "") {
    reg <- project_registry()
    choices <- c("Choose a project..." = "")
    if (nrow(reg) > 0) {
      choices <- c(choices, stats::setNames(reg$id, reg$label))
    }

    shiny::updateSelectInput(
      session,
      "project_select",
      choices = choices,
      selected = selected %||% ""
    )
  }

  shiny::observeEvent(project_registry(), {
    reg <- project_registry()
    selected <- input$project_select %||% ""
    if (!selected %in% reg$id) {
      selected <- ""
    }
    populate_project_select(selected = selected)
  }, ignoreInit = FALSE)

  shiny::observeEvent(project_registry(), {
    reg <- sanitize_registry(project_registry())
    save_registry(reg)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_select, {
    project_id <- input$project_select %||% ""
    if (!nzchar(project_id)) {
      return()
    }
    open_project(project_id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_new, {
    reg <- project_registry()

    template_choices <- if (nrow(template_catalog) > 0) {
      stats::setNames(template_catalog$id, template_catalog$label)
    } else {
      c("No templates available" = "")
    }

    clone_choices <- if (nrow(reg) > 0) {
      stats::setNames(reg$id, reg$label)
    } else {
      c("No projects available" = "")
    }

    shiny::showModal(
      shiny::modalDialog(
        title = "New Project",
        easyClose = TRUE,
        shiny::radioButtons(
          "new_project_type",
          label = NULL,
          choices = c(
            "New project from scratch" = "scratch",
            "New project from template" = "template",
            "Clone existing project" = "clone"
          ),
          selected = "scratch"
        ),
        shiny::conditionalPanel(
          condition = "input.new_project_type == 'template'",
          shiny::selectInput(
            "new_project_template",
            "Template",
            choices = template_choices,
            selected = unname(template_choices[[1]])
          )
        ),
        shiny::conditionalPanel(
          condition = "input.new_project_type == 'clone'",
          shiny::selectInput(
            "new_project_clone_source",
            "Project to clone",
            choices = clone_choices,
            selected = unname(clone_choices[[1]])
          )
        ),
        shiny::textInput("new_project_name", "Project name", value = ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_new_project", "Create Project", class = "btn-primary")
        )
      )
    )
  })

  shiny::observeEvent(input$confirm_new_project, {
    kind <- input$new_project_type %||% "scratch"
    reg <- project_registry()

    project_name <- trimws(input$new_project_name %||% "")
    has_graphs <- FALSE

    if (identical(kind, "scratch")) {
      if (!nzchar(project_name)) {
        project_name <- "Untitled Project"
      }
      has_graphs <- FALSE
    }

    if (identical(kind, "template")) {
      template_id <- input$new_project_template %||% ""
      template_idx <- match(template_id, template_catalog$id)
      if (!is.na(template_idx)) {
        has_graphs <- isTRUE(template_catalog$has_graphs[[template_idx]])
        if (!nzchar(project_name)) {
          project_name <- sprintf("%s Project", template_catalog$label[[template_idx]])
        }
      }
      if (!nzchar(project_name)) {
        project_name <- "Template Project"
      }
    }

    if (identical(kind, "clone")) {
      source_id <- input$new_project_clone_source %||% ""
      source_idx <- match(source_id, reg$id)
      if (is.na(source_idx)) {
        shiny::showNotification(
          "No clone source selected.",
          type = "error"
        )
        return()
      }

      has_graphs <- isTRUE(reg$has_graphs[[source_idx]])
      if (!nzchar(project_name)) {
        project_name <- sprintf("%s Copy", reg$label[[source_idx]])
      }
    }

    project_id <- make_project_id(project_name, reg$id)

    updated_registry <- rbind(
      reg,
      data.frame(
        id = project_id,
        label = project_name,
        origin = kind,
        has_graphs = has_graphs,
        stringsAsFactors = FALSE
      )
    )

    project_registry(updated_registry)
    shiny::removeModal()
    populate_project_select(selected = project_id)
    open_project(project_id)
  })

  shiny::observeEvent(input$add_data_section, {
    rv$project.show.data <- TRUE
  })

  shiny::observeEvent(input$hide_data_section, {
    rv$project.show.data <- FALSE
  })

  output$project_status <- shiny::renderText({
    if (!isTRUE(rv$project.active)) {
      return("Select an existing project or click 'New'.")
    }

    origin_txt <- switch(
      rv$project.origin,
      existing = "existing project",
      template = "new project from template",
      clone = "cloned project",
      scratch = "new project from scratch",
      "project"
    )

    graph_txt <- if (isTRUE(rv$project.has.graphs)) "graphs ready" else "graphs not built yet"

    sprintf(
      "Workspace: %s | %s | %s",
      rv$project.name %||% "Untitled Project",
      origin_txt,
      graph_txt
    )
  })

  output$workflow_controls <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(
        shiny::div(
          class = "gf-sidebar-note",
          shiny::strong("Workspace Locked"),
          shiny::p("Select a project from the dropdown or click 'New' to reveal workflow controls.")
        )
      )
    }

    actions <- NULL
    if (isTRUE(rv$project.has.graphs) && !isTRUE(rv$project.show.data)) {
      actions <- shiny::actionButton(
        "add_data_section",
        "Add Data",
        class = "btn-light gf-btn-wide"
      )
    } else if (isTRUE(rv$project.has.graphs) && isTRUE(rv$project.show.data)) {
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
      sprintf(
        "Project: %s | Origin: %s | Graph-ready: %s",
        rv$project.name %||% "Untitled Project",
        rv$project.origin %||% "unknown",
        if (isTRUE(rv$project.has.graphs)) "yes" else "no"
      )
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
