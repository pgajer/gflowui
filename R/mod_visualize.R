mod_visualize_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "gf-module-head",
      shiny::h3("Visualization and Endpoints"),
      shiny::p("Run endpoint detection and render summary outputs before exporting final interactive views.")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        class = "gf-panel",
        bslib::card_header("Actions"),
        shiny::actionButton(
          ns("detect_endpoints"),
          "Detect Endpoints",
          class = "btn-secondary gf-btn-wide"
        ),
        shiny::actionButton(
          ns("render"),
          "Render View Summary",
          class = "btn-primary gf-btn-wide"
        )
      ),
      bslib::card(
        class = "gf-panel",
        bslib::card_header("Visualization Status"),
        shiny::div(
          class = "gf-status-block",
          shiny::verbatimTextOutput(ns("status"))
        )
      )
    )
  )
}

mod_visualize_server <- function(id, data_state, graph_state, condexp_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(status = "No visualization run yet.")

    shiny::observeEvent(input$detect_endpoints, {
      g <- graph_state()
      if (is.null(g$graph)) {
        rv$status <- "Build/select a graph first."
        return()
      }
      ep <- gflow_detect_endpoints_stub(g$graph)
      rv$status <- paste(
        "Endpoint detection placeholder completed.",
        sprintf("Endpoints: %s", paste(ep$endpoints, collapse = ", ")),
        sep = "\n"
      )
    })

    shiny::observeEvent(input$render, {
      dat <- data_state()
      fit <- condexp_state()
      if (is.null(dat$data) || is.null(fit$fit)) {
        rv$status <- "Load data and fit conditional expectation first."
        return()
      }
      rv$status <- paste(
        "Visualization placeholder completed.",
        sprintf("Rows: %d", nrow(dat$data)),
        sprintf("Fitted values: %d", length(fit$fit$fitted.values)),
        sep = "\n"
      )
    })

    output$status <- shiny::renderText(rv$status)
  })
}
