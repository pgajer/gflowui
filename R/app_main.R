#' Build the gflowui Shiny application object
#'
#' @return A `shiny.appobj` application.
#' @export
#'
gflowui_app <- function() {
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
}

#' Run the gflowui application
#'
#' @param host Host passed to `shiny::runApp()`.
#' @param port Port passed to `shiny::runApp()`.
#' @param launch.browser Whether to launch a browser.
#'
#' @return Invisibly returns result of `shiny::runApp()`.
#' @export
#'
run_gflowui <- function(
  host = "127.0.0.1",
  port = getOption("shiny.port"),
  launch.browser = interactive()
) {
  # Use positional first argument for broad Shiny compatibility:
  # older Shiny uses `appDir`, newer accepts app objects too.
  shiny::runApp(
    gflowui_app(),
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}
