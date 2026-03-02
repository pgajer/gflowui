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
      shiny::span(class = "gf-brand-mark", "gflowui")
    ),
    class = "gf-root",
    theme = theme,
    sidebar = bslib::sidebar(
      class = "gf-sidebar",
      width = 470,
      shiny::div(
        class = "gf-sidebar-panel",
        shiny::h5("Project"),
        shiny::textInput("project_name", "Project name", value = "Untitled Project"),
        shiny::selectInput(
          "project_template",
          "Template",
          choices = c(
            "Empty project" = "empty",
            "ZAPPS/PreSSMat (placeholder)" = "zapps_pressmat"
          ),
          selected = "empty"
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::actionButton("project_new", "New", class = "btn-light gf-btn-wide"),
          shiny::actionButton("project_clone", "Clone", class = "btn-light gf-btn-wide")
        ),
        shiny::div(class = "gf-inline-status", shiny::textOutput("project_status"))
      ),
      shiny::div(
        class = "gf-sidebar-panel gf-accordion-wrap",
        bslib::accordion(
          id = "workflow_accordion",
          open = c("workflow_data"),
          bslib::accordion_panel("Data", value = "workflow_data", mod_data_ui("data")),
          bslib::accordion_panel("Graph(s) Construction", value = "workflow_graph", shiny::tagList(
            mod_graph_ui("graph"),
            shiny::hr(),
            mod_visualize_ui("viz")
          )),
          bslib::accordion_panel("Conditional Expectation Estimation", value = "workflow_condexp", mod_condexp_ui("condexp")),
          bslib::accordion_panel("Analysis", value = "workflow_analysis", shiny::div(
            class = "gf-analysis-placeholder",
            shiny::p("Analysis tools section placeholder."),
            shiny::p("Future versions will expose downstream comparison, trajectory summaries, and reporting workflows.")
          ))
        )
      )
    ),
    if (nzchar(css.path)) shiny::tags$head(shiny::includeCSS(css.path)),
    shiny::div(
      class = "gf-topband",
      shiny::div(
        class = "gf-topband-copy",
        shiny::h2("Graph Flow Analysis Studio"),
        shiny::p("Project-centered workspace for graph construction, endpoint detection, and conditional expectation estimation.")
      ),
      shiny::div(
        class = "gf-topband-chips",
        shiny::uiOutput("chip_backend"),
        shiny::uiOutput("chip_renderer"),
        shiny::uiOutput("chip_project")
      )
    ),
    shiny::div(
      class = "gf-workspace",
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          class = "gf-panel gf-workspace-panel",
          bslib::card_header("Run Monitor"),
          shiny::div(
            class = "gf-status-block",
            shiny::verbatimTextOutput("workspace_status")
          )
        ),
        bslib::card(
          class = "gf-panel gf-workspace-panel",
          bslib::card_header("Endpoint Preview"),
          shiny::tableOutput("workspace_endpoint_table")
        )
      ),
      bslib::layout_columns(
        col_widths = c(12),
        bslib::card(
          class = "gf-panel gf-workspace-panel",
          bslib::card_header("Data Preview"),
          shiny::tableOutput("workspace_data_table")
        )
      )
    )
  )
}
