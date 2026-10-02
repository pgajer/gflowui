# UI and persistence adapter for the existing endpoint editor.
gflowui_endpoint_sets_server <- function(id, context, manifest, view, visible_vertices,
    state_dir, legacy_dir, read_ids, empty_state, sanitize_state, snapshot_state,
    legacy_load, legacy_save, changed, region=function()NULL, regions=function()list(),
    style_inputs=function()NULL, apply_style=function(x)NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    dialog <- shiny::reactiveVal(NULL)
    info <- shiny::reactive({
      ctx <- context()
      if (is.null(ctx)) return(NULL)
      sets <- manifest()$graph_sets
      hit <- which(vapply(sets, function(gs) identical(gs$id, ctx$graph_set_id), logical(1)))
      if (!length(hit)) return(NULL)
      gs <- sets[[hit[1]]]
      ids <- gflowui_endpoint_ids(view()$vertex_ids)
      ds <- gflowui_endpoint_scope(gs, ctx$k, ctx$project_id)
      r <- region()
      draft <- !is.null(r) && !r$id %in% names(regions())
      list(ctx = ctx, gs = gs, ids = ids, region=r, draft=draft, dataset_scope=ds,
        scope = gflowui_endpoint_region_scope(ds,r),
        path = file.path(state_dir(ctx$project_id), "endpoint_sets", "sets.rds"))
    })
    provenance <- function(i) list(graph_set_id = i$ctx$graph_set_id, graph_label = i$gs$label,
      graph_description = i$gs$data_type_label %||% i$gs$label,
      embedding = i$gs$embedding_route %||% i$gs$label %||% i$gs$id,
      k = i$ctx$k, created_at = .gflowui_now())
    source_label <- function(set) {
      original <- Filter(function(gs) identical(gs$id, set$provenance$graph_set_id), manifest()$graph_sets)
      description <- if (length(original)) original[[1]]$data_type_label %||% original[[1]]$label else NULL
      set$provenance$graph_description %||% description %||%
        set$provenance$graph_label %||% set$provenance$embedding %||% "Unknown source"
    }
    bump <- function() {
      revision(shiny::isolate(revision()) + 1L)
      changed()
    }
    # Read existing files once per path. Never reinterpret saved indices using another graph.
    migrate <- function(store, i) {
      sets <- manifest()$graph_sets
      entries <- list()
      sets <- sets[order(!vapply(sets, function(gs) identical(gs$id, i$ctx$graph_set_id), logical(1)))]
      for (gs in sets) {
        ks <- gs$k_values %||% gs$selected_k %||% 1L
        if (!any(vapply(ks, function(k)
            identical(gflowui_endpoint_scope(gs, k, i$ctx$project_id)$key, i$dataset_scope$key), logical(1)))) next
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
          if (!identical(scope$key, i$dataset_scope$key)) next
          ids <- tryCatch(read_ids(gs, k), error = function(e) NULL)
          if (is.null(gflowui_endpoint_ids(ids))) next
          ctx <- list(project_id = i$ctx$project_id, graph_set_id = gs$id, k = k)
          state <- if (is.data.frame(obj$rows)) sanitize_state(obj, ctx) else snapshot_state(obj, ctx)
          entries[[length(entries) + 1L]] <- list(key = file, ids = ids, scope = scope,
            name = paste(gs$embedding_route %||% gs$label %||% gs$id,
              if (basename(file) == "current.rds") "— working table" else paste("—", obj$label %||% basename(file))),
            state = state, provenance = list(graph_set_id = gs$id, graph_label = gs$label,
              graph_description = gs$data_type_label %||% gs$label,
              embedding = gs$embedding_route %||% gs$label %||% gs$id, k = k,
              created_at = obj$created_at %||% obj$updated_at, imported_from = file))
        }
      }
      gflowui_endpoint_sets_migrate(store, entries)
    }
    get_store <- function(i) {
      old <- gflowui_endpoint_store_read(i$path)
      # The first upgrade keeps the current graph's former selection when possible.
      declared <- i$gs$endpoint_scope_id %||% paste0("graph:", i$gs$id)
      previous_scope <- digest::digest(list(declared, as.integer(i$ctx$k), i$scope$namespace), algo = "sha256")
      store <- gflowui_endpoint_store_upgrade(old, previous_scope)
      if (!identical(old$version, 2L) && file.exists(i$path)) {
        backup <- paste0(i$path, ".before_dataset_sharing")
        if (!file.exists(backup) && !file.copy(i$path, backup))
          stop("Could not back up endpoint sets before migration.")
      }
      # Legacy migration always targets the dataset; regional sets are copies.
      store <- migrate(store, i)
      if (!identical(old, store)) gflowui_endpoint_store_write(store, i$path)
      if (isTRUE(i$draft)) return(store)
      regional <- gflowui_endpoint_region_ensure(store,i$dataset_scope,i$region,regions(),empty_state(i$ctx),provenance(i))
      if (!identical(store, regional)) gflowui_endpoint_store_write(regional, i$path)
      regional
    }
    current_set <- function(store, i) {
      set <- store$sets[[store$active[[i$scope$key]] %||% ""]]
      if (!is.null(set) && identical(set$namespace, i$scope$namespace) && identical(set$scope,i$scope$key)) set else NULL
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
      if (isTRUE(i$draft)) stop("Save this region before editing its endpoint sets.")
      if (!is.null(i$region) && is.null(i$ids)) stop("Regional endpoints require unique stable vertex IDs.")
      if (is.null(i) || is.null(i$ids)) return(legacy_save(state, ctx))
      store <- get_store(i)
      set <- current_set(store, i)
      state <- sanitize_state(state, ctx)
      state$updated_at <- .gflowui_now()
      if (!is.null(i$region)) state$is_modified <- FALSE
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
    output$controls <- shiny::renderUI({
      st <- state()
      ns <- session$ns
      if (is.null(st)) return(shiny::p(class = "gf-hint",
        "Sharing requires unique stable vertex IDs in the graph asset; this table remains local."))
      i <- st$info
      if (isTRUE(i$draft)) return(shiny::p(class="gf-hint",
        "Save this region before creating or editing its endpoint sets. The whole-dataset sets are unchanged."))
      local <- Filter(function(x) identical(x$scope, i$scope$key) &&
        identical(x$namespace, i$scope$namespace), st$store$sets)
      choices <- stats::setNames(vapply(local, `[[`, "", "id"), make.unique(vapply(local, function(x) {
        if (!is.null(i$region)) x$name else paste(x$name, "—", source_label(x))
      }, ""), sep = " — alternative "))
      set <- st$set
      rows <- if (!is.null(set)) set$state$rows else data.frame()
      shown <- if (nrow(rows)) rows$accepted & rows$visible else logical()
      present <- i$ids[visible_vertices()]
      count <- if (nrow(rows)) sum(shown & rows$vertex_id %in% present) else 0L
      n <- sum(shown)
      shiny::tagList(
        shiny::selectInput(ns("set"), "Endpoint set:", choices = choices,
          selected = set$id %||% character(), width = "100%"),
        shiny::div(class = "gf-endpoint-actions",
          shiny::actionButton(ns("new"), "New", class = "btn-light btn-sm"),
          shiny::actionButton(ns("rename"), "Rename", class = "btn-light btn-sm"),
          shiny::actionButton(ns("duplicate"), "Duplicate", class = "btn-light btn-sm")),
        shiny::p(class = "gf-hint", sprintf("%d of %d endpoints visible; %d outside the current graph, component or filter.", count, n, n - count)),
        shiny::p(class="gf-hint", if(is.null(i$region))
          "Scope: whole dataset. Shared across its graphs and embeddings." else
          paste0("Scope: ",i$scope$label,". Shared across this region's graphs and embeddings. Changes are saved automatically.")),
        if (!is.null(set)) shiny::p(class = "gf-hint", "Created in: ", source_label(set),
          ". Detector scores describe the source embedding."),
        if (!is.null(i$region)) shiny::actionButton(ns("import"),"Import endpoints from…",class="btn-light btn-sm"))
    })
    shiny::observeEvent(input$set, {
      st <- state()
      if (is.null(st) || isTRUE(st$info$draft) || identical(input$set, st$set$id)) return()
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
      if (is.null(st) || isTRUE(st$info$draft) || (action != "new" && is.null(st$set))) return()
      dialog(list(action = action, scope = st$info$scope$key, set = st$set$id))
      value <- switch(action, new = if(is.null(st$info$region)) "Endpoints" else paste(st$info$scope$label,"— Endpoints"), rename = st$set$name,
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
      if (is.null(st) || isTRUE(st$info$draft) || is.null(d) || !d$action %in% c("new","rename","duplicate") || !identical(d$scope, st$info$scope$key) || !nzchar(name)) return()
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
      if(!is.null(i$region)) {
        set$region_id <- i$region$id; set$membership_ids <- i$region$vertex_ids
        set$membership_fingerprint <- i$region$membership_fingerprint
      }
      set$display <- set$display %||% gflowui_endpoint_display_defaults()
      store$sets[[set$id]] <- set; store$active[[i$scope$key]] <- set$id
      gflowui_endpoint_store_write(store, i$path)
      shiny::removeModal(); bump()
    })
    # Persist display settings independently of per-view working rows.
    display <- shiny::reactive({
      st <- state()
      if (is.null(st$set)) return(style_inputs() %||% gflowui_endpoint_display_defaults())
      st$set$display %||% gflowui_endpoint_display_defaults()
    })
    style_key <- NULL; awaiting_style <- NULL
    shiny::observe({
      st <- state(); if(is.null(st$set)) return()
      key <- paste(st$info$path,st$set$id)
      target <- display(); incoming <- style_inputs()
      if (!identical(style_key,key)) {
        style_key <<- key; awaiting_style <<- target
        apply_style(target); return()
      }
      if(is.null(incoming)) return()
      if(!is.null(awaiting_style)) {
        if(identical(incoming,awaiting_style)) awaiting_style <<- NULL
        return()
      }
      if(identical(incoming,target)) return()
      store <- get_store(st$info)
      set <- current_set(store,st$info)
      if(!identical(set$id,st$set$id)) return()
      set$display <- incoming; store$sets[[set$id]] <- set
      gflowui_endpoint_store_write(store,st$info$path); bump()
    })
    shiny::observeEvent(input$import,{
      st <- state(); if(is.null(st$info$region) || isTRUE(st$info$draft)) return()
      candidates <- Filter(function(x)identical(x$namespace,st$info$scope$namespace) &&
        !identical(x$id,st$set$id),st$store$sets)
      dialog(list(action="import",scope=st$info$scope$key,set=st$set$id))
      shiny::showModal(shiny::modalDialog(title="Import endpoints into this region",
        shiny::p("Only endpoints within the region are added. Existing labels and settings are retained; the source is unchanged."),
        shiny::selectInput(session$ns("import_source"),"Source endpoint set",
          stats::setNames(names(candidates),vapply(candidates,function(x)paste(x$name,x$scope_label,sep=" — "),""))),
        footer=shiny::tagList(shiny::modalButton("Cancel"),shiny::actionButton(session$ns("confirm_import"),"Import"))))
    })
    shiny::observeEvent(input$confirm_import,{
      st <- state(); d <- dialog()
      if(is.null(d)||!identical(d$action,"import")||!identical(d$scope,st$info$scope$key)||!identical(d$set,st$set$id)) return()
      store <- get_store(st$info); source <- store$sets[[input$import_source]]
      if(is.null(source)||!identical(source$namespace,st$info$scope$namespace)) return()
      set <- current_set(store,st$info)
      add <- source$state$rows
      add <- add[add$vertex_id %in% st$info$region$vertex_ids & !add$vertex_id %in% set$state$rows$vertex_id,,drop=FALSE]
      set$state$rows <- rbind(set$state$rows,add)
      set$revision <- set$revision+1L
      for(sample in add$vertex_id) set$row_provenance[[sample]] <- source$row_provenance[[sample]] %||% source$provenance
      set$imports <- c(set$imports,list(list(set_id=source$id,revision=source$revision,vertex_ids=add$vertex_id,at=.gflowui_now())))
      store$sets[[set$id]] <- set; gflowui_endpoint_store_write(store,st$info$path)
      shiny::removeModal(); bump()
    })
    snapshot <- function() {
      st <- state()
      if (isTRUE(st$info$draft)) stop("Save this region before saving its endpoints.")
      if (is.null(st$info$region) || is.null(st$set)) stop("No saved regional endpoint set is selected.")
      store <- get_store(st$info)
      set <- current_set(store, st$info)
      set$copied_from <- set$id
      set$id <- paste0("set_", digest::digest(list(Sys.time(), tempfile()), algo="sha256"))
      set$name <- paste(set$name, "— snapshot", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      set$created_at <- .gflowui_now(); set$revision <- 1L
      store$sets[[set$id]] <- set
      gflowui_endpoint_store_write(store, st$info$path)
      bump()
      list(ok=TRUE, dataset_id=set$id, label=set$name)
    }
    list(load = load, save = save, state = state, display=display, snapshot=snapshot)
  })
}
