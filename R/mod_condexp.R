mod_condexp_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("outcome_col"),
      "Outcome column",
      choices = c("Select column" = ""),
      selected = ""
    ),
    shiny::selectizeInput(
      ns("feature_cols"),
      "Additional feature columns to smooth (optional)",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::actionButton(
      ns("fit"),
      "Fit Conditional Expectation",
      class = "btn-primary gf-btn-wide"
    )
  )
}

mod_condexp_server <- function(id, data_state, graph_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(fit = NULL, status = "No conditional expectation fit yet.")

    shiny::observe({
      dat <- data_state()
      cols <- if (is.null(dat$data)) character(0) else names(dat$data)
      current <- input$outcome_col
      if (is.null(current) || !(current %in% cols)) current <- ""

      shiny::updateSelectInput(
        session = session,
        inputId = "outcome_col",
        choices = c("Select column" = "", cols),
        selected = current
      )
      shiny::updateSelectizeInput(
        session = session,
        inputId = "feature_cols",
        choices = cols,
        selected = input$feature_cols[input$feature_cols %in% cols],
        server = TRUE
      )
    })

    shiny::observeEvent(input$fit, {
      dat <- data_state()
      g <- graph_state()

      if (is.null(dat$data)) {
        rv$status <- "Load data first."
        return()
      }
      if (is.null(g$graph)) {
        rv$status <- "Build/select a graph first."
        return()
      }
      if (!nzchar(input$outcome_col) || !(input$outcome_col %in% colnames(dat$data))) {
        rv$status <- "Select a valid outcome column."
        return()
      }

      y <- dat$data[[input$outcome_col]]
      if (!is.numeric(y)) {
        rv$status <- sprintf("Outcome column '%s' must be numeric.", input$outcome_col)
        return()
      }
      if (anyNA(y) || any(!is.finite(y))) {
        rv$status <- sprintf("Outcome column '%s' contains NA/Inf.", input$outcome_col)
        return()
      }

      graph.feature.cols <- g$graph$feature.columns
      if (is.null(graph.feature.cols) || !all(graph.feature.cols %in% colnames(dat$data))) {
        graph.feature.cols <- names(dat$data)[vapply(dat$data, is.numeric, logical(1))]
      }
      if (length(graph.feature.cols) < 1L) {
        rv$status <- "No numeric feature columns available for model fitting."
        return()
      }

      X.df <- dat$data[, graph.feature.cols, drop = FALSE]
      non.numeric <- names(X.df)[!vapply(X.df, is.numeric, logical(1))]
      if (length(non.numeric) > 0L) {
        rv$status <- paste(
          "Graph feature columns must be numeric.",
          sprintf("Non-numeric: %s", paste(non.numeric, collapse = ", ")),
          sep = "\n"
        )
        return()
      }

      feature.cols <- unique(input$feature_cols)
      feature.cols <- feature.cols[feature.cols %in% colnames(dat$data)]
      feature.mat <- NULL
      if (length(feature.cols) > 0L) {
        feature.df <- dat$data[, feature.cols, drop = FALSE]
        non.numeric.feature <- names(feature.df)[!vapply(feature.df, is.numeric, logical(1))]
        if (length(non.numeric.feature) > 0L) {
          rv$status <- paste(
            "Selected feature columns to smooth must be numeric.",
            sprintf("Non-numeric: %s", paste(non.numeric.feature, collapse = ", ")),
            sep = "\n"
          )
          return()
        }
        feature.mat <- as.matrix(feature.df)
      }

      fit <- tryCatch(
        gflow_fit_condexp(
          graph_obj = g$graph,
          X = as.matrix(X.df),
          y = y,
          feature.matrix = feature.mat,
          fit.args = list(
            pca.dim = min(50L, ncol(X.df)),
            n.eigenpairs = min(30L, max(10L, as.integer(g$graph$selected.k))),
            max.iterations = 20L
          ),
          refit.args = list(
            per.column.gcv = FALSE
          )
        ),
        error = function(e) e
      )

      if (inherits(fit, "error")) {
        rv$fit <- NULL
        rv$status <- sprintf("Conditional expectation fit failed:\n%s", conditionMessage(fit))
        return()
      }

      rv$fit <- fit
      rv$status <- paste(
        sprintf("Outcome fit complete for '%s'.", input$outcome_col),
        sprintf("Selected k: %d", as.integer(g$graph$selected.k)),
        sprintf("Vertices: %d", length(fit$fitted.values)),
        if (!is.null(fit$feature.fitted.values)) {
          sprintf("Smoothed feature columns: %d", ncol(as.matrix(fit$feature.fitted.values)))
        } else {
          "Smoothed feature columns: 0"
        },
        sep = "\n"
      )
    })

    shiny::reactive(list(
      fit = rv$fit,
      status = rv$status
    ))
  })
}
