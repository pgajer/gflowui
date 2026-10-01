# UI and persistence adapter for the existing endpoint editor.
gflowui_endpoint_sets_server <- function(id, context, manifest, view, visible_vertices,
    state_dir, legacy_dir, read_ids, empty_state, sanitize_state, snapshot_state,
    legacy_load, legacy_save, changed) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    foreign <- shiny::reactiveVal("")
    dialog <- shiny::reactiveVal(NULL)
    info <- shiny::reactive({
      ctx <- context()
      if (is.null(ctx)) return(NULL)
      sets <- manifest()$graph_sets
      hit <- which(vapply(sets, function(gs) identical(gs$id, ctx$graph_set_id), logical(1)))
      if (!length(hit)) return(NULL)
      gs <- sets[[hit[1]]]
      ids <- gflowui_endpoint_ids(view()$vertex_ids)
      list(ctx = ctx, gs = gs, ids = ids,
        scope = gflowui_endpoint_scope(gs, ctx$k, ctx$project_id),
        path = file.path(state_dir(ctx$project_id), "endpoint_sets", "sets.rds"))
    })
    provenance <- function(i) list(graph_set_id = i$ctx$graph_set_id, graph_label = i$gs$label,
      embedding = i$gs$embedding_route %||% i$gs$label %||% i$gs$id,
      k = i$ctx$k, created_at = .gflowui_now())
    bump <- function() {
      revision(shiny::isolate(revision()) + 1L)
      changed()
    }
    # Read existing files once per path. Never reinterpret saved indices using another graph.
    migrate <- function(store, i, all = FALSE) {
      sets <- manifest()$graph_sets
      entries <- list()
      sets <- sets[order(!vapply(sets, function(gs) identical(gs$id, i$ctx$graph_set_id), logical(1)))]
      for (gs in sets) {
        ks <- gs$k_values %||% gs$selected_k %||% 1L
        if (!all && !any(vapply(ks, function(k)
            identical(gflowui_endpoint_scope(gs, k, i$ctx$project_id)$key, i$scope$key), logical(1)))) next
        base <- legacy_dir(gs$id, project_id = i$ctx$project_id)
        files <- c(file.path(base, "working", "current.rds"),
          list.files(file.path(base, "working", "snapshots"), "\\.rds$", full.names = TRUE),
          list.files(base, "\\.rds$", recursive = TRUE, full.names = TRUE))
        files <- unique(files[file.exists(files)])
        files <- files[grepl("/working/(current\\.rds|snapshots/[^/]+\\.rds)$", files)]
        for (file in setdiff(files, store$migrated)) {
          obj <- tryCatch(readRDS(file), error = function(e) NULL)
          if (!is.list(obj)) next
          k <- as.integer(obj$k %||% gs$selected_k %||% ks[1])
          if (length(k) != 1L || !is.finite(k) || k < 1L) k <- as.integer(ks[1])
          scope <- gflowui_endpoint_scope(gs, k, i$ctx$project_id)
          if (!all && !identical(scope$key, i$scope$key)) next
          ids <- tryCatch(read_ids(gs, k), error = function(e) NULL)
          if (is.null(gflowui_endpoint_ids(ids))) next
          ctx <- list(project_id = i$ctx$project_id, graph_set_id = gs$id, k = k)
          state <- if (is.data.frame(obj$rows)) sanitize_state(obj, ctx) else snapshot_state(obj, ctx)
          entries[[length(entries) + 1L]] <- list(key = file, ids = ids, scope = scope,
            name = paste(gs$embedding_route %||% gs$label %||% gs$id,
              if (basename(file) == "current.rds") "— working table" else paste("—", obj$label %||% basename(file))),
            state = state, provenance = list(graph_set_id = gs$id, graph_label = gs$label,
              embedding = gs$embedding_route %||% gs$label %||% gs$id, k = k,
              created_at = obj$created_at %||% obj$updated_at, imported_from = file))
        }
      }
      gflowui_endpoint_sets_migrate(store, entries)
    }
    get_store <- function(i, all = FALSE) {
      old <- gflowui_endpoint_store_read(i$path)
      store <- migrate(old, i, all)
      if (!identical(old, store)) gflowui_endpoint_store_write(store, i$path)
      store
    }
    current_set <- function(store, i) {
      set <- store$sets[[store$active[[i$scope$key]] %||% ""]]
      if (!is.null(set) && identical(set$namespace, i$scope$namespace)) set else NULL
    }
    load <- function(ctx) {
      revision()
      i <- info()
      if (is.null(i) || is.null(i$ids)) return(legacy_load(ctx))
      store <- get_store(i)
      set <- current_set(store, i)
      if (is.null(set)) {
        state <- empty_state(ctx)
        attr(state, "state_exists") <- FALSE
        return(state)
      }
      state <- sanitize_state(gflowui_endpoint_set_project(set, i$ids), ctx)
      attr(state, "state_exists") <- TRUE
      state
    }
    save <- function(state, ctx) {
      i <- info()
      if (is.null(i) || is.null(i$ids)) return(legacy_save(state, ctx))
      store <- get_store(i)
      set <- current_set(store, i)
      state <- sanitize_state(state, ctx)
      state$updated_at <- .gflowui_now()
      if (is.null(set)) {
        set <- gflowui_endpoint_set_new("Endpoints", state, i$ids, i$scope, provenance(i))
      } else {
        if (!is.null(state$shared_set_id) && !identical(state$shared_set_id, set$id))
          stop("The selected endpoint set changed. Select it again before editing.")
        set <- gflowui_endpoint_set_update(set, state, i$ids)
      }
      # Preserve the originating embedding for each newly added endpoint as well.
      for (sample in set$state$rows$vertex_id) {
        if (is.null(set$row_provenance[[sample]])) set$row_provenance[[sample]] <- provenance(i)
      }
      store$sets[[set$id]] <- set
      store$active[[i$scope$key]] <- set$id
      gflowui_endpoint_store_write(store, i$path)
      bump()
      invisible(TRUE)
    }
    state <- shiny::reactive({
      revision()
      i <- info()
      if (is.null(i) || is.null(i$ids)) return(NULL)
      store <- get_store(i)
      list(info = i, store = store, set = current_set(store, i))
    })
    shiny::observeEvent(info()$scope$key, { foreign("") }, ignoreInit = TRUE)
    output$controls <- shiny::renderUI({
      st <- state()
      ns <- session$ns
      if (is.null(st)) return(shiny::p(class = "gf-hint",
        "Sharing requires unique stable vertex IDs in the graph asset; this table remains local."))
      i <- st$info
      local <- Filter(function(x) identical(x$scope, i$scope$key) &&
        identical(x$namespace, i$scope$namespace), st$store$sets)
      choices <- stats::setNames(vapply(local, `[[`, "", "id"), vapply(local, `[[`, "", "name"))
      set <- st$set
      rows <- if (!is.null(set)) set$state$rows else data.frame()
      shown <- if (nrow(rows)) rows$accepted & rows$visible else logical()
      present <- i$ids[visible_vertices()]
      count <- if (nrow(rows)) sum(shown & rows$vertex_id %in% present) else 0L
      n <- sum(shown)
      foreign_set <- st$store$sets[[foreign()]]
      comparison <- if (!is.null(foreign_set)) {
        rr <- foreign_set$state$rows
        rr <- rr[rr$accepted & rr$visible, , drop = FALSE]
        in_view <- rr$vertex_id %in% present
        shiny::tags$details(open = "open",
          shiny::tags$summary(paste("Comparison:", foreign_set$name)),
          shiny::p(class = "gf-hint", sprintf("%d of %d visible. Read-only; source: %s.",
            sum(in_view), nrow(rr), foreign_set$provenance$graph_label %||% foreign_set$provenance$embedding)),
          shiny::div(class = "gf-endpoint-table-scroll",
            shiny::tags$table(class = "table table-sm",
              shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Vertex ID"),
                shiny::tags$th("Label"), shiny::tags$th("In view"))),
              shiny::tags$tbody(lapply(seq_len(nrow(rr)), function(j) shiny::tags$tr(
                shiny::tags$td(rr$vertex_id[j]), shiny::tags$td(rr$label[j]),
                shiny::tags$td(if (in_view[j]) "Yes" else "No")))))),
          shiny::actionLink(ns("hide_foreign"), "Hide comparison"))
      } else NULL
      shiny::tagList(
        shiny::selectInput(ns("set"), "Endpoint set:", choices = choices,
          selected = set$id %||% character(), width = "100%"),
        shiny::div(class = "gf-endpoint-actions",
          shiny::actionButton(ns("new"), "New", class = "btn-light btn-sm"),
          shiny::actionButton(ns("rename"), "Rename", class = "btn-light btn-sm"),
          shiny::actionButton(ns("duplicate"), "Duplicate", class = "btn-light btn-sm")),
        shiny::p(class = "gf-hint", sprintf("%d of %d endpoints visible; %d outside the current graph, component or filter.", count, n, n - count)),
        if (!is.null(set)) shiny::p(class = "gf-hint", "Created in: ", set$provenance$embedding,
          ". Edits are shared across embeddings of this graph. Detector scores describe the source embedding."),
        shiny::actionLink(ns("browse"), "Show a set from another graph…"),
        comparison)
    })
    shiny::observeEvent(input$set, {
      st <- state()
      if (is.null(st) || identical(input$set, st$set$id)) return()
      set <- st$store$sets[[input$set]]
      if (is.null(set) || !identical(set$scope, st$info$scope$key) ||
          !identical(set$namespace, st$info$scope$namespace)) return()
      store <- get_store(st$info)
      store$active[[st$info$scope$key]] <- set$id
      gflowui_endpoint_store_write(store, st$info$path)
      bump()
    }, ignoreInit = TRUE)
    open_name <- function(action) {
      st <- state()
      if (is.null(st) || (action != "new" && is.null(st$set))) return()
      dialog(list(action = action, scope = st$info$scope$key, set = st$set$id))
      value <- switch(action, new = "Endpoints", rename = st$set$name,
        duplicate = paste(st$set$name, "copy"))
      shiny::showModal(shiny::modalDialog(title = paste(tools::toTitleCase(action), "endpoint set"),
        shiny::textInput(session$ns("name"), "Name", value),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_name"), "Save"))))
    }
    shiny::observeEvent(input$new, open_name("new"))
    shiny::observeEvent(input$rename, open_name("rename"))
    shiny::observeEvent(input$duplicate, open_name("duplicate"))
    shiny::observeEvent(input$confirm_name, {
      st <- state(); d <- dialog(); name <- trimws(input$name %||% "")
      if (is.null(st) || is.null(d) || !identical(d$scope, st$info$scope$key) || !nzchar(name)) return()
      i <- st$info; store <- get_store(i)
      if (d$action == "rename") {
        set <- store$sets[[d$set]]; set$name <- name
      } else if (d$action == "duplicate") {
        # Copy the canonical table, including endpoints absent from the current subset.
        set <- store$sets[[d$set]]
        set$copied_from <- set$id
        set$id <- paste0("set_", digest::digest(list(Sys.time(), tempfile()), algo = "sha256"))
        set$name <- name; set$created_at <- .gflowui_now(); set$revision <- 1L
      } else set <- gflowui_endpoint_set_new(name, empty_state(i$ctx), i$ids, i$scope, provenance(i))
      store$sets[[set$id]] <- set; store$active[[i$scope$key]] <- set$id
      gflowui_endpoint_store_write(store, i$path)
      shiny::removeModal(); bump()
    })
    shiny::observeEvent(input$browse, {
      st <- state(); if (is.null(st)) return()
      i <- st$info; store <- get_store(i, all = TRUE)
      available <- Filter(function(x) !identical(x$scope, i$scope$key) &&
        identical(x$namespace, i$scope$namespace), store$sets)
      choices <- stats::setNames(vapply(available, `[[`, "", "id"),
        vapply(available, function(x) paste(x$name, "—", x$scope_label, "—", x$provenance$embedding), ""))
      dialog(list(action = "browse", scope = i$scope$key))
      shiny::showModal(shiny::modalDialog(title = "Endpoints from another graph",
        shiny::p("This adds a read-only comparison overlay, matched by stable sample ID. Use Copy to make an independent editable set for this graph."),
        if (!length(choices)) shiny::p("No saved endpoint sets from other compatible graphs.") else
          shiny::selectInput(session$ns("other_set"), "Endpoint set", choices, width = "100%"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          if (length(choices)) shiny::actionButton(session$ns("show_other"), "Show overlay"),
          if (length(choices)) shiny::actionButton(session$ns("copy_other"), "Copy to this graph"))))
      bump()
    })
    other_action <- function(copy = FALSE) {
      st <- state(); d <- dialog()
      if (is.null(st) || !identical(d$scope, st$info$scope$key)) return()
      i <- st$info; store <- get_store(i); set <- store$sets[[input$other_set %||% ""]]
      if (is.null(set) || !identical(set$namespace, i$scope$namespace)) return()
      if (copy) {
        set$copied_from <- set$id
        set$id <- paste0("set_", digest::digest(list(Sys.time(), tempfile()), algo = "sha256"))
        set$name <- paste(set$name, "copy")
        set$scope <- i$scope$key; set$scope_label <- i$scope$label
        set$revision <- 1L; set$created_at <- .gflowui_now()
        store$sets[[set$id]] <- set; store$active[[i$scope$key]] <- set$id
        gflowui_endpoint_store_write(store, i$path)
      } else foreign(set$id)
      shiny::removeModal(); bump()
    }
    shiny::observeEvent(input$show_other, other_action())
    shiny::observeEvent(input$copy_other, other_action(TRUE))
    shiny::observeEvent(input$hide_foreign, foreign(""))
    overlay <- shiny::reactive({
      st <- state()
      if (is.null(st) || !nzchar(foreign())) return(NULL)
      set <- st$store$sets[[foreign()]]
      if (is.null(set) || !identical(set$namespace, st$info$scope$namespace)) return(NULL)
      rows <- gflowui_endpoint_set_project(set, st$info$ids)$rows
      rows[rows$accepted & rows$visible, , drop = FALSE]
    })
    list(load = load, save = save, overlay = overlay, state = state)
  })
}
