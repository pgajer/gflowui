mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::numericInput(ns("kmin"), "k min", value = 5, min = 1, step = 1),
      shiny::numericInput(ns("kmax"), "k max", value = 25, min = 2, step = 1)
    ),
    shiny::selectInput(
      ns("method"),
      "Selection method",
      choices = c("both", "edit", "mixing", "none"),
      selected = "edit"
    ),
    shiny::selectInput(
      ns("label_col"),
      "Label column (required for mixing/both)",
      choices = c("None" = ""),
      selected = ""
    ),
    shiny::actionButton(
      ns("build"),
      "Build and Select Graph",
      class = "btn-primary gf-btn-wide"
    ),
    shiny::p(
      class = "gf-hint",
      "For mixing-based selection, choose a categorical label column."
    )
  )
}

mod_graph_server <- function(id, data_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(graph = NULL, status = "Graph has not been built yet.")

    shiny::observe({
      dat <- data_state()
      cols <- if (is.null(dat$data)) character(0) else names(dat$data)
      choices <- c("None" = "", cols)
      current <- input$label_col
      if (is.null(current) || !(current %in% cols)) {
        current <- ""
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "label_col",
        choices = choices,
        selected = current
      )
    })

    shiny::observeEvent(input$build, {
      dat <- data_state()
      if (is.null(dat$data)) {
        rv$status <- "Load data first."
        return()
      }
      if (input$kmax < input$kmin) {
        rv$status <- "k max must be >= k min."
        return()
      }

      label_col <- input$label_col
      labels <- NULL

      x_df <- dat$data
      if (nzchar(label_col)) {
        if (!(label_col %in% colnames(x_df))) {
          rv$status <- sprintf("Label column '%s' not found.", label_col)
          return()
        }
        labels <- x_df[[label_col]]
        x_df <- x_df[, setdiff(colnames(x_df), label_col), drop = FALSE]
      }

      if (input$method %in% c("mixing", "both") && is.null(labels)) {
        rv$status <- "Method 'mixing'/'both' requires selecting a label column."
        return()
      }

      if (ncol(x_df) < 1L) {
        rv$status <- "No feature columns available for graph construction."
        return()
      }

      non_numeric <- names(x_df)[!vapply(x_df, is.numeric, logical(1))]
      if (length(non_numeric) > 0L) {
        rv$status <- paste(
          "Feature columns must be numeric.",
          sprintf("Non-numeric: %s", paste(non_numeric, collapse = ", ")),
          sep = "\n"
        )
        return()
      }

      rv$status <- "Running graph selection..."

      res <- tryCatch(
        gflow_build_graph(
          X = as.matrix(x_df),
          kmin = as.integer(input$kmin),
          kmax = as.integer(input$kmax),
          method = input$method,
          labels = labels
        ),
        error = function(e) e
      )

      if (inherits(res, "error")) {
        rv$graph <- NULL
        rv$status <- sprintf("Graph selection failed:\n%s", conditionMessage(res))
        return()
      }

      res$feature.columns <- colnames(x_df)
      res$label.column <- if (nzchar(label_col)) label_col else NULL

      rv$graph <- res
      rv$status <- paste(
        sprintf("Selected k: %s", res$selected.k),
        sprintf("Selected from: %s", res$selected.k.source),
        sprintf("Connectivity: %s", res$connectivity),
        sep = "\n"
      )
    })

    shiny::reactive(list(
      graph = rv$graph,
      status = rv$status
    ))
  })
}
