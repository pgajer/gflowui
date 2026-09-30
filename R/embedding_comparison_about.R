# Optional, curated graph context, independent of the selected embedding run.
gflowui_ec_load_annotations <- function(index) {
  spec <- index$artifacts[["graph_annotations.json"]]
  if (is.null(spec)) return(list())
  data <- gflowui_ec_json(gflowui_ec_asset(index$root, spec))
  if (!identical(as.integer(data$schema_version), 1L)) stop("Unsupported graph annotations.")
  ids <- vapply(data$graphs, function(g) gflowui_ec_text(g$id), "")
  if (anyDuplicated(ids) || any(!ids %in% names(index$graphs))) stop("Invalid annotation graph identity.")
  for (g in data$graphs) {
    if (!identical(g$graph_sha256, index$graphs[[g$id]]$graph_sha256)) stop("Graph annotation hash mismatch.")
    for (ref in g$references) {
      if (!nzchar(gflowui_ec_text(ref$title)) || !nzchar(gflowui_ec_text(ref$description)) ||
          !grepl("^https?://[^[:space:]]+$", gflowui_ec_text(ref$url)) ||
          !ref$role %in% c("application", "graph", "numerical", "background")) {
        stop("Research annotations require a title, safe reference link, role and explanation.")
      }
    }
  }
  stats::setNames(data$graphs, ids)
}

gflowui_ec_about_ui <- function(info) {
  if (is.null(info)) return(NULL)
  paragraph <- function(label, value) {
    value <- gflowui_ec_text(value)
    if (nzchar(value)) shiny::p(shiny::strong(paste0(label, ": ")), value)
  }
  reference <- function(ref) {
    shiny::tags$li(shiny::tags$a(href=ref$url, target="_blank", rel="noopener noreferrer", ref$title),
      shiny::p(ref$description), paragraph("Scope", ref$scope), paragraph("Example", ref$detail))
  }
  groups <- c(application="Research using the data", graph="Use in graph algorithms",
              numerical="Sparse numerical methods", background="Construction references")
  research <- lapply(names(groups), function(role) {
    refs <- Filter(function(ref) identical(ref$role, role), info$references)
    if (!length(refs)) return(NULL)
    shiny::tagList(shiny::h5(groups[[role]]), shiny::tags$ul(lapply(head(refs, 2L), reference)),
      if (length(refs)>2L) shiny::tags$details(shiny::tags$summary("More references"),
        shiny::tags$ul(lapply(refs[-seq_len(2L)], reference))))
  })
  source <- gflowui_ec_text(info$source_url)
  shiny::tags$details(open=NA, class="ec-about-graph",
    shiny::tags$summary("About this graph"),
    paragraph("Origin and construction", info$construction),
    paragraph("Original weights", info$weights), paragraph("Displayed component", info$component),
    research,
    shiny::tags$details(shiny::tags$summary("Graph conversion and source"),
      paragraph("Conversion", info$conversion),
      if (grepl("^https?://[^[:space:]]+$", source)) shiny::tags$a(href=source,
        target="_blank", rel="noopener noreferrer", "SuiteSparse source record")))
}
