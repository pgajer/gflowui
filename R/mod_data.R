mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file"), "Upload CSV", accept = c(".csv", "text/csv")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::checkboxInput(ns("header"), "Header", TRUE),
      shiny::selectInput(
        ns("sep"),
        "Separator",
        choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")
      )
    ),
    shiny::actionButton(
      ns("load_example"),
      "Load Example Matrix",
      class = "btn-primary gf-btn-wide"
    )
  )
}

mod_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(data = NULL, source = NULL)

    shiny::observeEvent(input$load_example, {
      set.seed(1)
      x <- matrix(stats::rnorm(300), nrow = 100, ncol = 3)
      colnames(x) <- c("feature_1", "feature_2", "feature_3")
      rv$data <- as.data.frame(x)
      rv$source <- "example"
    })

    shiny::observeEvent(input$file, {
      req(input$file)
      dat <- utils::read.csv(
        input$file$datapath,
        header = isTRUE(input$header),
        sep = input$sep,
        check.names = FALSE
      )
      rv$data <- dat
      rv$source <- input$file$name
    })

    shiny::reactive(list(
      data = rv$data,
      source = rv$source
    ))
  })
}
