app_ui <- function() {
  css.path <- system.file("app/www/styles.css", package = "gflowui")
  theme <- bslib::bs_theme(
    version = 5,
    base_font = bslib::font_google("Space Grotesk"),
    heading_font = bslib::font_google("Fraunces"),
    code_font = bslib::font_google("IBM Plex Mono"),
    bg = "#f6f3ea",
    fg = "#1a2f33",
    primary = "#0f8b77",
    secondary = "#d97706",
    success = "#2f9e44",
    info = "#0b6e99",
    warning = "#c2410c",
    danger = "#b91c1c",
    "border-radius" = "1rem",
    "btn-border-radius" = "999px",
    "card-border-radius" = "1rem"
  )

  bslib::page_sidebar(
    title = shiny::div(
      class = "gf-brand",
      shiny::span(class = "gf-brand-mark", "gflowui"),
      shiny::span(class = "gf-brand-sub", "vmrc research workbench")
    ),
    class = "gf-root",
    theme = theme,
    sidebar = bslib::sidebar(
      class = "gf-sidebar",
      shiny::div(
        class = "gf-sidebar-panel",
        shiny::h5("Workflow"),
        shiny::tags$ol(
          class = "gf-step-list",
          shiny::tags$li("Load and inspect data"),
          shiny::tags$li("Build/select graph scale"),
          shiny::tags$li("Fit outcome and feature smoothers"),
          shiny::tags$li("Inspect visualization and endpoints")
        )
      ),
      shiny::div(
        class = "gf-sidebar-note",
        shiny::strong("Development mode"),
        shiny::p(
          "Run from R with gflowui::run_gflowui().",
          "This interface is optimized for iterative method testing."
        )
      )
    ),
    if (nzchar(css.path)) shiny::tags$head(shiny::includeCSS(css.path)),
    shiny::div(
      class = "gf-topband",
      shiny::div(
        class = "gf-topband-copy",
        shiny::h2("Graph Flow Analysis Studio"),
        shiny::p("Biologist-friendly controls for graph selection, conditional expectation fitting, and endpoint diagnostics.")
      ),
      shiny::div(
        class = "gf-topband-chips",
        shiny::span(class = "gf-chip", "R + gflow backend"),
        shiny::span(class = "gf-chip", "3D-ready outputs"),
        shiny::span(class = "gf-chip", "VMRC-friendly workflow")
      )
    ),
    shiny::div(
      class = "gf-tabs",
      bslib::navset_card_tab(
        id = "main_tabs",
        bslib::nav_panel("Data", mod_data_ui("data")),
        bslib::nav_panel("Graph", mod_graph_ui("graph")),
        bslib::nav_panel("CondExp", mod_condexp_ui("condexp")),
        bslib::nav_panel("Visualize", mod_visualize_ui("viz"))
      )
    )
  )
}
