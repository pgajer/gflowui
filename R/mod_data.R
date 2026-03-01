mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "gf-module-head",
      shiny::h3("Data Intake"),
      shiny::p("Upload a matrix-style CSV or load an example dataset to bootstrap graph exploration.")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        class = "gf-panel",
        bslib::card_header("Source"),
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
      ),
      bslib::card(
        class = "gf-panel",
        bslib::card_header("Data Snapshot"),
        shiny::div(
          class = "gf-status-block",
          shiny::verbatimTextOutput(ns("summary"))
        )
      )
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

    output$summary <- shiny::renderText({
      if (is.null(rv$data)) {
        return("No data loaded.")
      }
      paste(
        sprintf("Source: %s", rv$source),
        sprintf("Rows: %d", nrow(rv$data)),
        sprintf("Columns: %d", ncol(rv$data)),
        "",
        "Column names:",
        paste(colnames(rv$data), collapse = ", "),
        sep = "\n"
      )
    })

    shiny::reactive(list(
      data = rv$data,
      source = rv$source
    ))
  })
}
