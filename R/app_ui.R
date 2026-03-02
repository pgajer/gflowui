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
      class = "gf-appbar",
      shiny::div(
        class = "gf-brand",
        shiny::span(class = "gf-brand-mark", "gflowui")
      ),
      shiny::div(
        class = "gf-appbar-chips",
        shiny::uiOutput("chip_backend"),
        shiny::uiOutput("chip_renderer"),
        shiny::uiOutput("chip_project")
      )
    ),
    class = "gf-root",
    theme = theme,
    sidebar = bslib::sidebar(
      class = "gf-sidebar",
      width = 470,
      shiny::div(
        class = "gf-sidebar-panel",
        shiny::h5("Project"),
        shiny::selectInput(
          "project_mode",
          "Start with",
          choices = c(
            "Open existing project (graphs ready)" = "existing_graphs",
            "New project from scratch" = "scratch",
            "New project from template" = "template",
            "Clone existing project" = "clone"
          ),
          selected = "existing_graphs"
        ),
        shiny::conditionalPanel(
          condition = "input.project_mode == 'existing_graphs' || input.project_mode == 'clone'",
          shiny::selectInput(
            "project_existing",
            "Existing project",
            choices = c(
              "ZAPPS/PreSSMat (placeholder)" = "zapps_pressmat",
              "VMRC Demo Cohort (placeholder)" = "vmrc_demo"
            ),
            selected = "zapps_pressmat"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.project_mode == 'template'",
          shiny::selectInput(
            "project_template",
            "Template",
            choices = c(
              "ZAPPS/PreSSMat (placeholder)" = "zapps_pressmat",
              "Empty template" = "empty"
            ),
            selected = "zapps_pressmat"
          )
        ),
        shiny::textInput("project_name", "Project name", value = "Untitled Project"),
        shiny::actionButton(
          "project_start",
          "Enter Workspace",
          class = "btn-primary gf-btn-wide"
        ),
        shiny::div(class = "gf-inline-status", shiny::textOutput("project_status"))
      ),
      shiny::uiOutput("workflow_controls")
    ),
    if (nzchar(css.path)) shiny::tags$head(shiny::includeCSS(css.path)),
    shiny::div(
      class = "gf-viewer-stage",
      shiny::uiOutput("workspace_view")
    )
  )
}
