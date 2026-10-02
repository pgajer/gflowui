gflowui_project_controls_ui <- function(registry, ready = TRUE) {
  registry <- gflowui_order_projects(registry)
  choices <- c("Choose a project..." = "")
  if (nrow(registry) > 0L) {
    choices <- c(choices, stats::setNames(registry$id, registry$label))
  }

  selector <- shiny::selectInput("project_select", label = NULL, choices = choices, selected = "")
  if (!ready) {
    selector <- htmltools::tagQuery(selector)$find("select")$addAttrs(disabled = "disabled")$allTags()
  }

  shiny::div(
    class = "gf-sidebar-panel",
    shiny::h5("Projects"),
    selector,
    shiny::actionButton(
      "project_new", "New", class = "btn-secondary gf-btn-wide",
      disabled = if (!ready) "disabled" else NULL
    )
  )
}

app_ui <- function() {
  css.path <- system.file("app/www/styles.css", package = "gflowui")
  embedding.css.path <- system.file("app/www/embedding-comparison.css", package = "gflowui")
  embedding.js.path <- system.file("app/www/embedding-comparison.js", package = "gflowui")
  density.state.js.path <- system.file(
    "app/www/density-display-state.js",
    package = "gflowui"
  )
  basin.inspector.js.path <- system.file(
    "app/www/basin-inspector-state.js",
    package = "gflowui"
  )
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
        shiny::actionButton("project_manager", "gflowui",
          class = "gf-brand-mark gf-brand-button", title = "Projects: open or reorder",
          `aria-label` = "gflowui: open projects and reorder", `aria-haspopup` = "dialog")
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
      htmltools::tagAppendChild(
        shiny::uiOutput("project_controls"),
        # Render per page request so new sessions see the current registry immediately.
        htmltools::tagFunction(function() {
          # The first server update enables input once the session can accept it.
          gflowui_project_controls_ui(gflowui_load_registry(), ready = FALSE)
        })
      ),
      shiny::uiOutput("workflow_controls"),
      shiny::uiOutput("project_middle_actions"),
      shiny::uiOutput("workspace_actions"),
      shiny::uiOutput("run_monitor_panel")
    ),
    shiny::tags$head(
      if (nzchar(css.path)) shiny::includeCSS(css.path),
      shiny::includeScript(system.file("app/www/project-order.js", package = "gflowui")),
      shiny::includeScript(system.file("app/www/dropdown-defaults.js", package = "gflowui")),
      shiny::includeScript(system.file("app/www/graph-selection.js", package = "gflowui")),
      shiny::includeScript(system.file("app/www/lazy-edges.js", package = "gflowui")),
      shiny::includeScript(system.file("app/www/scene-updates.js", package = "gflowui")),
      shiny::includeScript(system.file("app/www/source-datasets.js", package = "gflowui")),
      if (nzchar(embedding.css.path)) shiny::includeCSS(embedding.css.path),
      if (nzchar(embedding.js.path)) shiny::includeScript(embedding.js.path),
      if (nzchar(density.state.js.path)) {
        shiny::includeScript(density.state.js.path)
      },
      if (nzchar(basin.inspector.js.path)) {
        shiny::includeScript(basin.inspector.js.path)
      }
    ),
    shiny::div(
      class = "gf-viewer-stage",
      shiny::uiOutput("workspace_view"),
      if (requireNamespace("plotly",quietly=TRUE)) shiny::conditionalPanel("input['source_datasets-show'] === true",
        shiny::div(class="gf-sidebar-panel",
          shiny::p("Linked within-dCST coordinates. The dCST selection in Graphs filters both views. Box/lasso or click to select points; coordinate and color options are in Within-dCST 2D."),
          plotly::plotlyOutput("source_datasets-plot",height="420px")))
    )
  )
}
