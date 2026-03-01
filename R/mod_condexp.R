mod_condexp_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Conditional Expectations"),
    shiny::textInput(ns("outcome_col"), "Outcome column name", value = "feature_1"),
    shiny::actionButton(ns("fit"), "Fit conditional expectation"),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("status"))
  )
}

mod_condexp_server <- function(id, data_state, graph_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(fit = NULL, status = "No conditional expectation fit yet.")

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
      if (!(input$outcome_col %in% colnames(dat$data))) {
        rv$status <- sprintf("Column '%s' not found.", input$outcome_col)
        return()
      }

      y <- dat$data[[input$outcome_col]]
      fit <- gflow_fit_condexp_stub(graph_obj = g$graph, y = y)
      rv$fit <- fit
      rv$status <- sprintf(
        "Computed placeholder conditional expectation for '%s' over %d vertices.",
        input$outcome_col,
        length(fit$fitted.values)
      )
    })

    output$status <- shiny::renderText(rv$status)

    shiny::reactive(list(
      fit = rv$fit,
      status = rv$status
    ))
  })
}
