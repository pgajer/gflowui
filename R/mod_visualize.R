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
        bslib::card_header("Endpoint Settings"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::numericInput(
            ns("core_quantile"),
            "Core quantile",
            value = 0.10,
            min = 0.01,
            max = 0.99,
            step = 0.01
          ),
          shiny::numericInput(
            ns("endpoint_quantile"),
            "Endpoint quantile",
            value = 0.90,
            min = 0.00,
            max = 1.00,
            step = 0.01
          )
        ),
        shiny::checkboxInput(
          ns("use_approx_ecc"),
          "Use approximate eccentricity",
          value = TRUE
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::numericInput(
            ns("n_landmarks"),
            "Landmarks",
            value = 64,
            min = 1,
            step = 1
          ),
          shiny::numericInput(
            ns("max_endpoints"),
            "Max endpoints (0 = no cap)",
            value = 0,
            min = 0,
            step = 1
          )
        ),
        shiny::numericInput(
          ns("seed"),
          "Seed",
          value = 1,
          min = 1,
          step = 1
        ),
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
        ),
        shiny::tableOutput(ns("endpoint_table"))
      )
    )
  )
}

mod_visualize_server <- function(id, data_state, graph_state, condexp_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      status = "No visualization run yet.",
      endpoint.result = NULL
    )

    shiny::observeEvent(input$detect_endpoints, {
      g <- graph_state()
      if (is.null(g$graph)) {
        rv$status <- "Build/select a graph first."
        rv$endpoint.result <- NULL
        return()
      }

      max.endpoints <- suppressWarnings(as.integer(input$max_endpoints))
      if (!is.finite(max.endpoints) || max.endpoints <= 0L) {
        max.endpoints <- NULL
      }

      ep <- tryCatch(
        gflow_detect_endpoints(
          graph_obj = g$graph,
          core.quantile = input$core_quantile,
          endpoint.quantile = input$endpoint_quantile,
          use.approx.eccentricity = isTRUE(input$use_approx_ecc),
          n.landmarks = as.integer(input$n_landmarks),
          max.endpoints = max.endpoints,
          seed = as.integer(input$seed)
        ),
        error = function(e) e
      )

      if (inherits(ep, "error")) {
        rv$endpoint.result <- NULL
        rv$status <- sprintf("Endpoint detection failed:\n%s", conditionMessage(ep))
        return()
      }

      rv$endpoint.result <- ep

      n.ep <- length(ep$endpoints)
      n.core <- length(ep$core.vertices)
      shown <- ep$endpoints
      if (length(shown) > 20L) {
        shown <- shown[seq_len(20L)]
      }
      shown.txt <- if (length(shown) == 0L) {
        "none"
      } else {
        paste(shown, collapse = ", ")
      }
      if (n.ep > 20L) {
        shown.txt <- paste0(shown.txt, ", ...")
      }

      rv$status <- paste(
        "Endpoint detection complete.",
        sprintf("Selected k: %d", as.integer(g$graph$selected.k)),
        sprintf("Endpoints found: %d", n.ep),
        sprintf("Core vertices: %d", n.core),
        sprintf("Endpoint indices: %s", shown.txt),
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
        "Visualization summary ready.",
        sprintf("Rows: %d", nrow(dat$data)),
        sprintf("Fitted values: %d", length(fit$fit$fitted.values)),
        sprintf(
          "Endpoint result: %s",
          if (is.null(rv$endpoint.result)) "not computed" else
            paste(length(rv$endpoint.result$endpoints), "endpoint(s)")
        ),
        sep = "\n"
      )
    })

    output$status <- shiny::renderText(rv$status)

    output$endpoint_table <- shiny::renderTable({
      ep <- rv$endpoint.result
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
  })
}
