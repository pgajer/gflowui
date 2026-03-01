app_ui <- function() {
  bslib::page_sidebar(
    title = "gflowui",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    sidebar = bslib::sidebar(
      shiny::h5("Workflow"),
      shiny::tags$ol(
        shiny::tags$li("Load data"),
        shiny::tags$li("Build/select graph"),
        shiny::tags$li("Fit conditional expectations"),
        shiny::tags$li("Visualize and inspect endpoints")
      ),
      shiny::hr(),
      shiny::helpText("This is an MVP scaffold. Backend compute calls are placeholders.")
    ),
    bslib::navset_card_tab(
      id = "main_tabs",
      bslib::nav_panel("Data", mod_data_ui("data")),
      bslib::nav_panel("Graph", mod_graph_ui("graph")),
      bslib::nav_panel("CondExp", mod_condexp_ui("condexp")),
      bslib::nav_panel("Visualize", mod_visualize_ui("viz"))
    )
  )
}
