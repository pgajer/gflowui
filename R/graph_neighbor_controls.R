# Missing flags preserve the existing neighbor-family UI.
gflowui_has_neighbor_parameter <- function(graph_set) {
  !identical(graph_set$neighbor_parameter, FALSE)
}

# Keep the graph asset selector bound for selection/reference state even when
# its internal key does not represent a scientifically meaningful k parameter.
gflowui_graph_neighbor_controls <- function(graph_ui, show_reference = TRUE) {
  enabled <- gflowui_has_neighbor_parameter(graph_ui)
  selector <- shiny::selectInput(
    "graph_k", label = NULL, choices = graph_ui$k_choices,
    selected = if (is.finite(graph_ui$k_selected)) as.character(graph_ui$k_selected) else "",
    width = "105px"
  )
  shiny::tagList(
    shiny::div(
      class = "gf-graph-row gf-graph-row-tight gf-graph-row-k",
      if (enabled) shiny::span(class = "gf-graph-row-label", "k:"),
      if (enabled) selector else shiny::div(style = "display: none;", `aria-hidden` = "true", selector)
    ),
    if (enabled) shiny::div(
      class = "gf-graph-row gf-graph-row-tight gf-graph-row-optimal",
      shiny::span(class = "gf-graph-row-label", "Optimal k:"),
      shiny::selectInput("graph_optimal_method", label = NULL,
        choices = graph_ui$optimal_choices, selected = graph_ui$optimal_selected,
        width = "180px"),
      shiny::actionButton("graph_optimal_show", "Show",
        class = "btn-light btn-sm gf-btn-inline")
    )
  )
}
