# Project ordering is a user preference, separate from the asset registry.
gflowui_project_order_path <- function() {
  file.path(gflowui_projects_data_dir(), "project-order.rds")
}

gflowui_read_project_order <- function() {
  path <- gflowui_project_order_path()
  if (!file.exists(path)) return(character())
  ids <- tryCatch(suppressWarnings(readRDS(path)), error = function(e) character())
  if (!is.character(ids)) return(character())
  unique(ids[!is.na(ids) & nzchar(ids)])
}

gflowui_order_projects <- function(registry, ids = gflowui_read_project_order()) {
  registry <- gflowui_sanitize_registry(registry)
  ids <- unique(c(intersect(ids, registry$id), registry$id))
  registry[match(ids, registry$id), , drop = FALSE]
}

gflowui_save_project_order <- function(ids, known_ids) {
  if (!is.character(ids) || anyNA(ids) || anyDuplicated(ids) ||
      !setequal(ids, known_ids)) {
    stop("The project list changed unexpectedly. Reopen Projects and try again.", call. = FALSE)
  }
  # Append newly registered projects and omit deleted ones, without rewriting
  # registry rows that may have been refreshed by an external experiment.
  current <- gflowui_load_registry()
  ids <- c(intersect(ids, current$id), setdiff(current$id, ids))
  path <- gflowui_project_order_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("project-order-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(ids, tmp)
  if (!file.rename(tmp, path)) stop("Could not save project order.", call. = FALSE)
  invisible(ids)
}

gflowui_project_manager_ui <- function(registry, token, active_id = "") {
  button <- function(action, label, ...) shiny::tags$button(
    type = "button", `data-project-action` = action, label, ...)
  rows <- lapply(seq_len(nrow(registry)), function(i) {
    id <- registry$id[[i]]; label <- registry$label[[i]]
    shiny::tags$li(
      class = "gf-project-order-row", `data-project-id` = id,
      button("handle", "\u2630", class = "gf-project-drag",
        title = paste("Drag to move", label), `aria-label` = paste("Drag to move", label), tabindex = "-1"),
      shiny::span(class = "gf-project-order-name", label,
        if (identical(id, active_id)) shiny::span(class = "gf-project-current", "Current")),
      button("up", "\u2191", class = "btn btn-light btn-sm", title = "Move up",
        `aria-label` = paste("Move", label, "up"), disabled = if (i == 1L) "disabled" else NULL),
      button("down", "\u2193", class = "btn btn-light btn-sm", title = "Move down",
        `aria-label` = paste("Move", label, "down"), disabled = if (i == nrow(registry)) "disabled" else NULL),
      button("open", if (identical(id, active_id)) "Return" else "Open",
        class = "btn btn-light btn-sm", `aria-label` = paste("Open", label))
    )
  })
  shiny::modalDialog(title = "Projects", size = "l", easyClose = FALSE,
    shiny::div(id = "gf_project_manager", `data-project-token` = as.character(token),
      shiny::p("Drag projects into order, or use the up and down buttons. Save order keeps this order across sessions."),
      shiny::p(class = "gf-hint", "Opening a project leaves any unsaved ordering changes unapplied."),
      if (nrow(registry)) shiny::tags$ol(class = "gf-project-order-list", rows)
        else shiny::p("No projects yet. Close this panel and choose New to create one."),
      shiny::div(class = "gf-project-order-status", `aria-live` = "polite", "")),
    footer = shiny::tagList(
      button("alphabetical", "Sort A\u2013Z", class = "btn btn-light"),
      shiny::modalButton("Cancel"),
      button("save", "Save order", class = "btn btn-primary")))
}

gflowui_project_manager_server <- function(input, session, on_open, active_id) {
  revision <- shiny::reactiveVal(0L)
  panel <- shiny::reactiveVal(NULL)
  sequence <- 0L
  shiny::observeEvent(input$project_manager, {
    sequence <<- sequence + 1L
    registry <- gflowui_load_registry()
    panel(list(token = as.character(sequence), ids = registry$id))
    shiny::showModal(gflowui_project_manager_ui(registry, sequence, active_id()), session = session)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$project_manager_action, {
    event <- input$project_manager_action; current <- panel()
    if (!is.list(event) || is.null(current) || !identical(event$token, current$token)) return()
    if (identical(event$action, "save")) {
      ids <- as.character(unlist(event$ids, use.names = FALSE))
      result <- tryCatch(gflowui_save_project_order(ids, current$ids), error = identity)
      if (inherits(result, "error")) {
        shiny::showNotification(conditionMessage(result), type = "error", session = session)
        return()
      }
      revision(shiny::isolate(revision()) + 1L)
      panel(NULL)
      shiny::removeModal(session = session)
      shiny::showNotification("Project order saved.", type = "message", session = session)
    } else if (identical(event$action, "open") &&
        is.character(event$id) && length(event$id) == 1L && event$id %in% current$ids) {
      if (isTRUE(on_open(event$id))) {
        panel(NULL)
        shiny::removeModal(session = session)
      }
    }
  }, ignoreInit = TRUE)
  revision
}
