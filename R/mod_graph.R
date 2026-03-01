mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Graph Construction and Selection"),
    shiny::numericInput(ns("kmin"), "k min", value = 5, min = 1, step = 1),
    shiny::numericInput(ns("kmax"), "k max", value = 25, min = 2, step = 1),
    shiny::selectInput(
      ns("method"),
      "Selection method",
      choices = c("both", "edit", "mixing", "none"),
      selected = "both"
    ),
    shiny::actionButton(ns("build"), "Build/select graph"),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("status"))
  )
}

mod_graph_server <- function(id, data_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(graph = NULL, status = "Graph has not been built yet.")

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

      rv$status <- "Running graph selection..."

      res <- gflow_build_graph_stub(
        X = as.matrix(dat$data),
        kmin = as.integer(input$kmin),
        kmax = as.integer(input$kmax),
        method = input$method
      )

      rv$graph <- res
      rv$status <- paste(
        sprintf("Selected k: %s", res$selected.k),
        sprintf("Connectivity: %s", res$connectivity),
        sep = "\n"
      )
    })

    output$status <- shiny::renderText(rv$status)

    shiny::reactive(list(
      graph = rv$graph,
      status = rv$status
    ))
  })
}
