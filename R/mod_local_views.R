gflowui_local_views_ui <- function(id, values = list()) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("navigation")),
    shiny::textOutput(ns("context")),
    shiny::checkboxInput(ns("only_members"), "Show only region members in parent preview", values$only_members %||% FALSE),
    shiny::actionButton(ns("whole"), "Return to whole dataset"),
    shiny::hr(),
    shiny::textInput(ns("label"), "Region name", values$label %||% ""),
    shiny::selectInput(ns("selection"), "Define region from", c("Anchor neighborhood"="anchor", "Selected dCSTs"="dcst"), selected=values$selection %||% "anchor"),
    shiny::conditionalPanel(sprintf("input['%s'] === 'anchor'", ns("selection")),
      shiny::textInput(ns("anchor"), "Anchor vertex ID", values$anchor %||% ""),
      shiny::actionButton(ns("clicked"), "Use selected vertex"),
      shiny::uiOutput(ns("endpoint_ui")),
      shiny::uiOutput(ns("anchor_label")),
      shiny::textInput(ns("sizes"), "Neighborhood sizes (including anchor)", values$sizes %||% "500"),
      shiny::selectInput(ns("metric"), "Neighborhood metric", c("Hellinger"="hellinger", "Euclidean abundance"="euclidean", "Jensen–Shannon"="jensen_shannon"), selected=values$metric %||% "hellinger")),
    shiny::conditionalPanel(sprintf("input['%s'] === 'dcst'", ns("selection")),
      shiny::selectInput(ns("level"), "dCST level", c("Level 1"="dcst_level1", "Level 2"="dcst_level2"), selected=values$level %||% "dcst_level1"),
      shiny::uiOutput(ns("groups_ui")),
      shiny::actionButton(ns("table_groups"), "Use checked dCSTs from Graphs"),
      shiny::checkboxInput(ns("separate"), "Create one region per dCST", values$separate %||% FALSE)),
    shiny::actionButton(ns("preview"), "Preview membership"),
    shiny::actionButton(ns("save"), "Save region(s)"),
    shiny::actionButton(ns("clear"), "Clear preview"),
    shiny::downloadButton(ns("members"), "Download membership"),
    shiny::textOutput(ns("status")),
    shiny::hr(),
    shiny::p("New fits will recompute distances within the region. This stage supports membership previews and existing fitted views; new computation jobs are the next stage.", class="gf-hint"),
    shiny::uiOutput(ns("import_ui")))
}

gflowui_local_views_server <- function(id, manifest, view_state, selected_vertex, dcst_selection, endpoint_state = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    regions <- shiny::reactiveVal(list()); drafts <- shiny::reactiveVal(list())
    region_id <- shiny::reactiveVal(""); view_id <- shiny::reactiveVal("__preview__")
    parent_set <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("")
    loaded_project <- NULL
    config <- shiny::reactive(manifest()$metadata$local_views)
    path <- shiny::reactive(file.path(gflowui_projects_data_dir(), "projects", manifest()$project_id, "local_views", "atlas.rds"))
    shiny::observeEvent(manifest()$project_id, {
      if(identical(loaded_project, manifest()$project_id)) return()
      loaded_project <<- manifest()$project_id
      p <- path(); regions(if (file.exists(p)) readRDS(p)$regions else list())
      drafts(list()); region_id(""); view_id("__preview__"); parent_set(NULL); status("")
    })
    region <- shiny::reactive({
      if (identical(region_id(), "__draft__")) return(if(length(drafts())) drafts()[[1L]] else NULL)
      regions()[[region_id()]]
    })
    output$navigation <- shiny::renderUI({
      if (!isTRUE(config()$enabled)) return(shiny::p("Local views require a dataset abundance source in the project configuration."))
      choices <- c("Whole dataset"="", stats::setNames(names(regions()), vapply(regions(), `[[`, "", "label")))
      if (length(drafts())) choices <- c(choices, "Unsaved membership preview"="__draft__")
      r <- region(); views <- r$views %||% list()
      vc <- c("Locate in parent embedding (preview)"="__preview__", stats::setNames(
        vapply(views, `[[`, "", "id"), vapply(views, function(gs) gs$label %||% gs$id, "")))
      shiny::tagList(shiny::selectInput(session$ns("region"), "Data region", choices, selected=region_id()),
        if (!is.null(r)) shiny::selectInput(session$ns("view"), "Region view", vc, selected=view_id()))
    })
    shiny::observeEvent(input$region, { if (!identical(input$region, region_id())) { region_id(input$region); view_id("__preview__") } }, ignoreNULL=TRUE)
    shiny::observeEvent(input$view, {
      if (!identical(input$view, view_id())) {
        if(identical(view_id(),"__preview__") && !identical(input$view,"__preview__"))
          parent_set(view_state()$set_id)
        view_id(input$view)
      }
    }, ignoreNULL=TRUE)
    shiny::observeEvent(input$whole, { region_id(""); view_id("__preview__") })
    shiny::observeEvent(input$clear, { drafts(list()); region_id(""); view_id("__preview__") })
    output$context <- shiny::renderText({
      r <- region(); if (is.null(r)) return("Context: whole dataset.")
      st <- view_state(); present <- sum(r$vertex_ids %in% st$vertex_ids)
      sprintf("%s — %s members; %s present in this graph. %s", r$label, length(r$vertex_ids), present,
        if (identical(view_id(), "__preview__")) "Parent embedding preview: coordinates and distances unchanged. Existing display filters still apply." else "Saved local fit: paths and coordinates belong to this region.")
    })
    output$status <- shiny::renderText(status())
    output$endpoint_ui <- shiny::renderUI({
      rows <- endpoint_state()$set$state$rows
      if(is.null(rows) || !nrow(rows)) return(NULL)
      labels <- if("label" %in% names(rows)) rows$label else rows$vertex_id
      shiny::selectInput(session$ns("endpoint"), "Anchor from active endpoint set",
        c("Choose endpoint"="",stats::setNames(rows$vertex_id,paste(labels,rows$vertex_id,sep=" — "))),
        selected="")
    })
    shiny::observeEvent(input$endpoint, {
      if(nzchar(input$endpoint %||% "")) shiny::updateTextInput(session,"anchor",value=input$endpoint)
    })
    output$anchor_label <- shiny::renderUI({
      anchor <- input$anchor %||% ""
      if(!nzchar(anchor)) return(NULL)
      a <- gflowui_vertex_hover_asset(manifest()); i <- match(anchor,a$sample_ids)
      if(is.na(i)) return(shiny::p("No matching dataset vertex ID.",class="gf-hint"))
      shiny::p(class="gf-hint",paste0(gsub("_"," ",a$taxon_names[a$indices[[i]][1]]),
        " (",format(round(100*a$abundances[[i]][1],2),trim=TRUE),"%); ID: ",anchor))
    })
    shiny::observeEvent(input$clicked, {
      st <- view_state(); idx <- selected_vertex()
      if (length(idx)==1L && is.finite(idx) && idx>=1 && idx<=length(st$vertex_ids))
        shiny::updateTextInput(session,"anchor",value=st$vertex_ids[idx])
      else status("Select a vertex in the 3D view first, or enter a dataset vertex ID.")
    })
    output$groups_ui <- shiny::renderUI({
      st <- view_state(); x <- as.character(st$sources[[input$level %||% "dcst_level1"]]$values)
      counts <- sort(table(x), decreasing=TRUE)
      shiny::selectInput(session$ns("groups"), "dCSTs in current graph", stats::setNames(names(counts), paste0(names(counts), " (", counts, ")")), multiple=TRUE, selected=shiny::isolate(input$groups))
    })
    shiny::observeEvent(input$table_groups, {
      s <- dcst_selection(); shiny::updateSelectInput(session,"level",selected=s$level)
      session$onFlushed(function() shiny::updateSelectInput(session,"groups",selected=as.character(s$groups)), once=TRUE)
    })
    shiny::observeEvent(input$preview, {
      tryCatch({
        if (!isTRUE(config()$enabled)) stop("This project has no atlas dataset configuration.")
        asset <- gflowui_vertex_hover_asset(manifest()); universe <- asset$sample_ids
        if (!length(universe)) stop("Dataset abundance asset is unavailable.")
        label <- trimws(input$label %||% "")
        if (identical(input$selection,"anchor")) {
          sizes <- suppressWarnings(as.numeric(strsplit(trimws(input$sizes), "[,; ]+")[[1]]))
          if (!length(sizes) || anyNA(sizes)) stop("Enter integer neighborhood sizes separated by commas.")
          ds <- lapply(unique(sizes), function(n) {
            nn <- gflowui_atlas_anchor(asset, input$anchor, n, input$metric)
            gflowui_atlas_region(if(nzchar(label)) paste(label,n) else paste(input$anchor,n,input$metric), nn$ids, universe,
              list(type="anchor", anchor=input$anchor, size=n, metric=input$metric, radius=nn$radius, includes_anchor=TRUE))
          })
        } else {
          st <- view_state(); groups <- input$groups
          if (!length(groups)) stop("Select one or more dCSTs.")
          values <- as.character(st$sources[[input$level]]$values)
          if(length(values)!=length(st$vertex_ids)) stop("dCST labels are unavailable for this graph.")
          partitions <- if(isTRUE(input$separate)) as.list(groups) else list(groups)
          ds <- lapply(partitions, function(g) gflowui_atlas_region(
            if(nzchar(label)) paste(label,paste(g,collapse=" + ")) else paste(input$level,paste(g,collapse=" + ")),
            st$vertex_ids[which(values %in% g)], universe,
            list(type="dcst", level=input$level, groups=g, source_graph=st$set_id)))
        }
        drafts(ds); region_id("__draft__"); view_id("__preview__")
        status(sprintf("Prepared %d region(s): %s members. Preview shows the first; Save stores all.",length(ds),paste(vapply(ds,function(x)length(x$vertex_ids),1L),collapse=", ")))
      }, error=function(e) status(conditionMessage(e)))
    })
    shiny::observeEvent(input$save, {
      tryCatch({
        ds <- drafts(); if(!length(ds)) stop("Preview membership before saving.")
        rs <- regions(); for(r in ds) rs[[r$id]] <- r
        gflowui_atlas_save(rs,path()); regions(rs); region_id(ds[[1]]$id); drafts(list())
        status("Region membership saved. Existing views and dataset endpoint annotations remain available.")
      },error=function(e)status(conditionMessage(e)))
    })
    output$members <- shiny::downloadHandler(filename=function() "region-membership.csv", content=function(file) {
      r <- region(); utils::write.csv(data.frame(vertex_id=r$vertex_ids %||% character()),file,row.names=FALSE)
    })
    output$import_ui <- shiny::renderUI({
      if (length(config()$import_manifest)) shiny::actionButton(session$ns("import"),config()$import_label %||% "Import existing local views")
    })
    shiny::observeEvent(input$import, {
      tryCatch({
        source <- readRDS(config()$import_manifest)
        imported <- gflowui_atlas_import(source,gflowui_vertex_hover_asset(manifest())$sample_ids,config()$vertex_namespace)
        # Declare shared files in the registered manifest so Trash preserves them.
        registry <- gflowui::list_projects()
        mp <- registry$manifest_file[registry$id == manifest()$project_id]
        if(length(mp)!=1L) stop("Registered project manifest is unavailable.")
        registered <- readRDS(mp)
        registered$metadata$local_views$asset_paths <- unique(c(
          registered$metadata$local_views$asset_paths,
          gflowui_project_asset_references(source)))
        gflowui_write_manifest(registered,mp)
        rs <- regions(); rs[names(imported)] <- imported
        gflowui_atlas_save(rs,path()); regions(rs)
        status(sprintf("Imported %d regions with %d shared views. Existing asset files were not copied.",length(imported),sum(vapply(imported,function(r)length(r$views),1L))))
      },error=function(e)status(conditionMessage(e)))
    })
    list(form=shiny::reactive(shiny::reactiveValuesToList(input)), manifest=shiny::reactive(gflowui_atlas_manifest(manifest(),region(),view_id(),parent_set())),
         region=region, preview=shiny::reactive(if(identical(view_id(),"__preview__")) region() else NULL),
         only_members=shiny::reactive(isTRUE(input$only_members)), context=shiny::reactive({
           r<-region(); if(is.null(r)) "Whole dataset" else paste(r$label, if(identical(view_id(),"__preview__")) "— parent preview" else "— local fitted view")
         }))
  })
}
