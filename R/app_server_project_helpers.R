gflowui_make_server_project_helpers <- function(
    session,
    data_state,
    graph_state,
    condexp_state,
    viz_state) {
  `%||%` <- function(x, y) {
    if (
      is.null(x) ||
      length(x) < 1L ||
      (is.character(x) && length(x) > 0L && !nzchar(x[[1]]))
    ) y else x
  }

  scalar_int <- function(x, default = NA_integer_) {
    vals <- suppressWarnings(as.integer(x))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 1L) {
      dv <- suppressWarnings(as.integer(default))
      if (length(dv) < 1L) {
        return(NA_integer_)
      }
      return(dv[[1]])
    }
    vals[[1]]
  }

  scalar_chr <- function(x, default = "") {
    vals <- as.character(x)
    vals <- vals[!is.na(vals)]
    if (length(vals) < 1L) {
      as.character(default)[[1]]
    } else {
      vals[[1]]
    }
  }

  template_catalog <- data.frame(
    id = c("zapps_pressmat_template", "empty_template"),
    label = c("ZAPPS/PreSSMat", "Empty template"),
    has_graphs = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  project_registry <- shiny::reactiveVal(gflowui_load_registry())

  build_new_project_manifest <- function(
      project_id,
      project_name,
      kind,
      has_graphs,
      source_id = NA_character_,
      source_manifest = NULL) {
    now <- .gflowui_now()

    if (is.list(source_manifest) && length(source_manifest) > 0L) {
      out <- source_manifest
      out$version <- as.character(out$version %||% "1")
      out$project_id <- project_id
      out$project_name <- project_name
      out$created_at <- now
      out$updated_at <- now
      out$cloned_from <- as.character(source_id %||% NA_character_)
      return(out)
    }

    list(
      version = "1",
      project_id = project_id,
      project_name = project_name,
      profile = sprintf("workspace_%s", kind %||% "scratch"),
      project_root = NA_character_,
      created_at = now,
      updated_at = now,
      graph_sets = list(),
      condexp_sets = list(),
      endpoint_runs = list(),
      defaults = list(
        graph_set_id = NA_character_,
        condexp_set_id = NA_character_,
        endpoint_run_id = NA_character_
      ),
      metadata = list(
        has_graphs = isTRUE(has_graphs),
        source_id = as.character(source_id %||% NA_character_)
      )
    )
  }

  rv <- shiny::reactiveValues(
    project.active = FALSE,
    project.id = NULL,
    project.origin = NULL,
    project.name = "Untitled Project",
    project.has.graphs = FALSE,
    project.show.data = FALSE,
    project.dirty = FALSE,
    project.baseline.signature = NULL,
    run.monitor.visible = FALSE,
    run.monitor.note = NULL,
    reference.layout.cache = list(),
    html.resource.map = list(),
    reference.html.cache = list()
  )

  current_state_signature <- function() {
    dat <- data_state()
    g <- graph_state()
    cfit <- condexp_state()
    vz <- viz_state()

    sig <- list(
      data.loaded = !is.null(dat$data),
      data.nrow = if (is.null(dat$data)) 0L else nrow(dat$data),
      data.ncol = if (is.null(dat$data)) 0L else ncol(dat$data),
      data.source = as.character(dat$source %||% ""),
      graph.selected.k = if (is.null(g$graph$selected.k)) NA_integer_ else as.integer(g$graph$selected.k),
      graph.status = as.character(g$status %||% ""),
      condexp.fitted = !is.null(cfit$fit),
      condexp.length = if (is.null(cfit$fit$fitted.values)) 0L else length(cfit$fit$fitted.values),
      condexp.status = as.character(cfit$status %||% ""),
      endpoint.count = if (is.null(vz$endpoint.result$endpoints)) 0L else length(vz$endpoint.result$endpoints),
      viz.status = as.character(vz$status %||% "")
    )

    paste(utils::capture.output(dput(sig)), collapse = "")
  }

  mark_project_clean <- function() {
    rv$project.baseline.signature <- current_state_signature()
    rv$project.dirty <- FALSE
    invisible(NULL)
  }

  save_current_project <- function() {
    if (!isTRUE(rv$project.active) || !nzchar(rv$project.id %||% "")) {
      return(FALSE)
    }

    reg <- project_registry()
    idx <- match(rv$project.id, reg$id)
    if (is.na(idx)) {
      return(FALSE)
    }

    manifest_file <- as.character(reg$manifest_file[[idx]] %||% "")
    if (!nzchar(manifest_file) || identical(manifest_file, "NA")) {
      manifest_file <- gflowui_manifest_path(rv$project.id)
      reg$manifest_file[[idx]] <- normalizePath(manifest_file, mustWork = FALSE)
    }
    manifest <- gflowui_read_manifest(manifest_file)
    if (is.null(manifest)) {
      manifest <- list(
        version = "1",
        project_id = rv$project.id,
        project_name = rv$project.name,
        profile = "workspace",
        project_root = as.character(reg$project_root[[idx]] %||% NA_character_),
        created_at = .gflowui_now(),
        graph_sets = list(),
        condexp_sets = list(),
        endpoint_runs = list(),
        defaults = list(
          graph_set_id = NA_character_,
          condexp_set_id = NA_character_,
          endpoint_run_id = NA_character_,
          reference_graph_set_id = NA_character_,
          reference_k = NA_integer_,
          reference_k_by_outcome = list(),
          reference_reason = NA_character_
        )
      )
    }

    dat <- data_state()
    g <- graph_state()
    cfit <- condexp_state()
    vz <- viz_state()
    now <- .gflowui_now()

    manifest$updated_at <- now
    manifest$workspace_snapshot <- list(
      saved_at = now,
      data = list(
        loaded = !is.null(dat$data),
        source = as.character(dat$source %||% ""),
        nrow = if (is.null(dat$data)) 0L else nrow(dat$data),
        ncol = if (is.null(dat$data)) 0L else ncol(dat$data)
      ),
      graph = list(
        status = as.character(g$status %||% ""),
        selected_k = if (is.null(g$graph$selected.k)) NA_integer_ else as.integer(g$graph$selected.k)
      ),
      condexp = list(
        status = as.character(cfit$status %||% ""),
        fitted = !is.null(cfit$fit)
      ),
      visualize = list(
        status = as.character(vz$status %||% ""),
        endpoint_count = if (is.null(vz$endpoint.result$endpoints)) 0L else length(vz$endpoint.result$endpoints)
      )
    )

    gflowui_write_manifest(manifest, manifest_file)
    reg$updated_at[[idx]] <- now
    project_registry(gflowui_sanitize_registry(reg))
    mark_project_clean()
    TRUE
  }

  close_project <- function() {
    rv$project.active <- FALSE
    rv$project.id <- NULL
    rv$project.origin <- NULL
    rv$project.name <- "Untitled Project"
    rv$project.has.graphs <- FALSE
    rv$project.show.data <- FALSE
    rv$project.dirty <- FALSE
    rv$project.baseline.signature <- NULL
    rv$run.monitor.visible <- FALSE
    rv$run.monitor.note <- NULL
    rv$reference.layout.cache <- list()
    rv$reference.html.cache <- list()
    shiny::updateSelectInput(session, "project_select", selected = "")
    invisible(NULL)
  }

  active_registry_row <- shiny::reactive({
    if (!isTRUE(rv$project.active) || !nzchar(rv$project.id %||% "")) {
      return(NULL)
    }
    reg <- project_registry()
    idx <- match(rv$project.id, reg$id)
    if (is.na(idx)) {
      return(NULL)
    }
    reg[idx, , drop = FALSE]
  })

  active_manifest <- shiny::reactive({
    row <- active_registry_row()
    if (is.null(row)) {
      return(NULL)
    }
    mf <- as.character(row$manifest_file[[1]] %||% "")
    if (!nzchar(mf) || identical(mf, "NA")) {
      return(NULL)
    }
    manifest <- gflowui_read_manifest(mf)
    if (!is.list(manifest)) {
      return(NULL)
    }
    manifest$graph_sets <- gflowui_normalize_graph_sets_manifest(manifest$graph_sets %||% list())
    manifest
  })

  active_project_context <- function() {
    if (!isTRUE(rv$project.active) || !nzchar(rv$project.id %||% "")) {
      return(NULL)
    }
    reg <- project_registry()
    idx <- match(rv$project.id, reg$id)
    if (is.na(idx)) {
      return(NULL)
    }
    list(reg = reg, idx = idx, row = reg[idx, , drop = FALSE])
  }

  load_or_init_active_manifest <- function(ctx) {
    reg <- ctx$reg
    idx <- ctx$idx

    manifest_file <- as.character(reg$manifest_file[[idx]] %||% "")
    if (!nzchar(manifest_file) || identical(manifest_file, "NA")) {
      manifest_file <- gflowui_manifest_path(rv$project.id)
      reg$manifest_file[[idx]] <- normalizePath(manifest_file, mustWork = FALSE)
    }

    manifest <- gflowui_read_manifest(manifest_file)
    if (is.null(manifest)) {
      manifest <- list(
        version = "1",
        project_id = rv$project.id,
        project_name = rv$project.name,
        profile = "workspace",
        project_root = as.character(reg$project_root[[idx]] %||% NA_character_),
        created_at = .gflowui_now(),
        updated_at = .gflowui_now(),
        graph_sets = list(),
        condexp_sets = list(),
        endpoint_runs = list(),
        defaults = list(
          graph_set_id = NA_character_,
          condexp_set_id = NA_character_,
          endpoint_run_id = NA_character_
        )
      )
    }

    if (!is.list(manifest$graph_sets)) {
      manifest$graph_sets <- list()
    }
    manifest$graph_sets <- gflowui_normalize_graph_sets_manifest(manifest$graph_sets)
    if (!is.list(manifest$condexp_sets)) {
      manifest$condexp_sets <- list()
    }
    if (!is.list(manifest$endpoint_runs)) {
      manifest$endpoint_runs <- list()
    }
    if (!is.list(manifest$defaults)) {
      manifest$defaults <- list()
    }
    if (is.null(manifest$defaults$graph_set_id)) {
      manifest$defaults$graph_set_id <- NA_character_
    }
    if (is.null(manifest$defaults$condexp_set_id)) {
      manifest$defaults$condexp_set_id <- NA_character_
    }
    if (is.null(manifest$defaults$endpoint_run_id)) {
      manifest$defaults$endpoint_run_id <- NA_character_
    }
    if (is.null(manifest$defaults$reference_graph_set_id)) {
      manifest$defaults$reference_graph_set_id <- NA_character_
    }
    if (is.null(manifest$defaults$reference_k)) {
      manifest$defaults$reference_k <- NA_integer_
    }
    if (!is.list(manifest$defaults$reference_k_by_outcome)) {
      manifest$defaults$reference_k_by_outcome <- list()
    }
    if (is.null(manifest$defaults$reference_reason)) {
      manifest$defaults$reference_reason <- NA_character_
    }

    list(reg = reg, idx = idx, manifest = manifest, manifest_file = manifest_file)
  }

  save_active_manifest <- function(payload) {
    now <- .gflowui_now()
    payload$manifest$updated_at <- now
    gflowui_write_manifest(payload$manifest, payload$manifest_file)

    reg <- payload$reg
    idx <- payload$idx
    reg$manifest_file[[idx]] <- normalizePath(payload$manifest_file, mustWork = FALSE)
    reg$has_graphs[[idx]] <- isTRUE(length(payload$manifest$graph_sets) > 0L)
    reg$has_condexp[[idx]] <- isTRUE(length(payload$manifest$condexp_sets) > 0L)
    reg$has_endpoints[[idx]] <- isTRUE(length(payload$manifest$endpoint_runs) > 0L)
    reg$updated_at[[idx]] <- now

    project_registry(gflowui_sanitize_registry(reg))
    rv$project.has.graphs <- isTRUE(length(payload$manifest$graph_sets) > 0L)
    rv$reference.layout.cache <- list()
    rv$reference.html.cache <- list()
    mark_project_clean()
    invisible(TRUE)
  }

  upsert_active_graph_set <- function(graph_set, make_default = FALSE) {
    ctx <- active_project_context()
    if (is.null(ctx)) {
      return(FALSE)
    }

    payload <- load_or_init_active_manifest(ctx)
    manifest <- payload$manifest
    graph_sets <- manifest$graph_sets
    ids <- if (length(graph_sets) > 0L) {
      vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    } else {
      character(0)
    }

    graph_set <- gflowui_normalize_graph_set_manifest(graph_set)
    idx <- match(as.character(graph_set$id), ids)
    if (is.na(idx)) {
      graph_sets[[length(graph_sets) + 1L]] <- graph_set
    } else {
      graph_sets[[idx]] <- graph_set
    }

    manifest$graph_sets <- graph_sets
    default_id <- as.character(manifest$defaults$graph_set_id %||% "")
    if (isTRUE(make_default) || !nzchar(default_id)) {
      manifest$defaults$graph_set_id <- as.character(graph_set$id)
    }
    ref_id <- as.character(manifest$defaults$reference_graph_set_id %||% "")
    if (!nzchar(ref_id)) {
      manifest$defaults$reference_graph_set_id <- as.character(graph_set$id)
    }
    if (!is.finite(scalar_int(manifest$defaults$reference_k, default = NA_integer_))) {
      sel_k <- scalar_int(graph_set$selected_k, default = NA_integer_)
      if (is.finite(sel_k)) {
        manifest$defaults$reference_k <- sel_k
      }
    }

    payload$manifest <- manifest
    save_active_manifest(payload)
  }

  make_project_id <- function(label, existing_ids) {
    gflowui_make_project_id(label = label, existing_ids = existing_ids)
  }

  open_project <- function(project_id) {
    if (!nzchar(project_id %||% "")) {
      return(invisible(NULL))
    }

    reg <- project_registry()
    idx <- match(project_id, reg$id)
    if (is.na(idx)) {
      return(invisible(NULL))
    }

    row <- reg[idx, , drop = FALSE]
    rv$project.active <- TRUE
    rv$project.id <- as.character(row$id[[1]] %||% "")
    rv$project.origin <- as.character(row$origin[[1]] %||% "unknown")
    rv$project.name <- as.character(row$label[[1]] %||% "Untitled Project")
    rv$project.has.graphs <- isTRUE(row$has_graphs[[1]])
    rv$project.show.data <- !isTRUE(row$has_graphs[[1]])
    rv$run.monitor.visible <- FALSE
    rv$run.monitor.note <- NULL
    rv$reference.layout.cache <- list()
    rv$reference.html.cache <- list()
    mark_project_clean()

    invisible(NULL)
  }

  populate_project_select <- function(selected = "") {
    reg <- project_registry()
    choices <- c("Choose a project..." = "")
    if (nrow(reg) > 0) {
      choices <- c(choices, stats::setNames(reg$id, reg$label))
    }

    shiny::updateSelectInput(
      session,
      "project_select",
      choices = choices,
      selected = selected %||% ""
    )
  }

  list(
    `%||%` = `%||%`,
    scalar_int = scalar_int,
    scalar_chr = scalar_chr,
    template_catalog = template_catalog,
    project_registry = project_registry,
    rv = rv,
    build_new_project_manifest = build_new_project_manifest,
    current_state_signature = current_state_signature,
    mark_project_clean = mark_project_clean,
    save_current_project = save_current_project,
    close_project = close_project,
    active_registry_row = active_registry_row,
    active_manifest = active_manifest,
    active_project_context = active_project_context,
    load_or_init_active_manifest = load_or_init_active_manifest,
    save_active_manifest = save_active_manifest,
    upsert_active_graph_set = upsert_active_graph_set,
    make_project_id = make_project_id,
    open_project = open_project,
    populate_project_select = populate_project_select
  )
}
