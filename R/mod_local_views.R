gflowui_local_views_ui <- function(id, values = list(), levels = c("Level 1"="dcst_level1", "Level 2"="dcst_level2")) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("show_retired"),"Include retired regions",values$show_retired %||% FALSE),
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
      shiny::selectInput(ns("level"), "CST level", levels, selected=values$level %||% "dcst_level1"),
      shiny::uiOutput(ns("groups_ui")),
      shiny::actionButton(ns("table_groups"), "Use checked dCSTs from Graphs"),
      shiny::checkboxInput(ns("separate"), "Create one region per dCST", values$separate %||% FALSE)),
    shiny::actionButton(ns("preview"), "Preview membership"),
    shiny::actionButton(ns("save"), "Save region(s)"),
    shiny::actionButton(ns("clear"), "Clear preview"),
    shiny::downloadButton(ns("members"), "Download membership"),
    shiny::textOutput(ns("status")),
    shiny::uiOutput(ns("lifecycle")),
    shiny::downloadButton(ns("bundle"),"Download view reproducibility bundle"),
    shiny::hr(),
    gflowui_atlas_calculation_ui(ns("compute"),ns("region"),values$calculation %||% list()),
    shiny::uiOutput(ns("import_ui")))
}

gflowui_local_views_server <- function(id, manifest, view_state, selected_vertex, dcst_selection, endpoint_state = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    regions <- shiny::reactiveVal(list()); drafts <- shiny::reactiveVal(list())
    region_id <- shiny::reactiveVal(""); view_id <- shiny::reactiveVal("__preview__")
    parent_set <- shiny::reactiveVal(NULL)
    nav_state <- shiny::reactiveVal(list(family="whole"))
    nav_memory <- list(); view_memory <- list(); nav_token <- 0L; nav_sent <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("")
    loaded_project <- NULL
    config <- shiny::reactive(manifest()$metadata$local_views)
    path <- shiny::reactive(file.path(gflowui_projects_data_dir(), "projects", manifest()$project_id, "local_views", "atlas.rds"))
    shiny::observeEvent(manifest()$project_id, {
      if(identical(loaded_project, manifest()$project_id)) return()
      loaded_project <<- manifest()$project_id
      p <- path(); regions(if (file.exists(p)) readRDS(p)$regions else list())
      drafts(list()); region_id(""); view_id("__preview__"); parent_set(NULL); status("")
      nav_state(list(family="whole")); nav_memory <<- list(); view_memory <<- list(); nav_sent(NULL)
    })
    calculation <- gflowui_atlas_calculation_server("compute",manifest,shiny::reactive(region()),path,
      publish=function(folder,spec) {
        target_path<-file.path(dirname(dirname(folder)),"atlas.rds")
        existing<-if(file.exists(target_path))readRDS(target_path)$regions else list()
        r<-existing[[spec$region_id]]
        if(is.null(r))stop("Original region revision is missing; result kept unregistered.")
        id<-paste0("atlas_",spec$key)
        if(id %in% vapply(r$views,`[[`,"","id") &&
           identical(r$views[[id]]$graph_file,file.path(folder,"graph.rds")))return(invisible(NULL))
        result<-gflowui_atlas_job_result(folder)
        if(is.null(result))stop("Job assets failed checksum validation.")
        latest<-gflowui_atlas_update(target_path,function(rs) {
          rs[[spec$region_id]]$views[[id]]<-result$graph_set;rs
        })
        gflowui_atlas_atomic(list(atlas_path=target_path,region_id=spec$region_id,view_id=id),file.path(folder,"publication.rds"))
        if(identical(target_path,path()))regions(latest)
      })
    region <- shiny::reactive({
      if (identical(region_id(), "__draft__")) return(if(length(drafts())) drafts()[[1L]] else NULL)
      regions()[[region_id()]]
    })
    region_choices <- shiny::reactive({
      visible <- Filter(function(r) !isTRUE(r$retired) || isTRUE(input$show_retired), regions())
      choices <- c("Whole dataset"="", stats::setNames(names(visible) %||% character(), vapply(visible,gflowui_atlas_region_label,"")))
      if (length(drafts())) choices <- c(choices, "Unsaved membership preview"="__draft__")
      choices
    })
    view_choices <- shiny::reactive({
      views <- region()$views %||% list()
      c("Locate in parent embedding (preview)"="__preview__", stats::setNames(
        vapply(views, `[[`, "", "id"), vapply(views, function(gs) gs$label %||% gs$id, "")))
    })
    nav_catalog <- shiny::reactive(gflowui_atlas_catalog(regions(),isTRUE(input$show_retired)))
    nav_model <- shiny::reactive(gflowui_atlas_navigation_resolve(nav_catalog(),nav_state(),length(drafts())>0L))
    remember_navigation <- function() {
      id<-shiny::isolate(region_id()); model<-shiny::isolate(nav_model())
      state<-if(identical(model$target,id))model$state else gflowui_atlas_navigation_state(shiny::isolate(nav_catalog()),id)
      nav_memory[[state$family]] <<- state
      view_memory[[if(nzchar(id))id else "__whole__"]] <<- shiny::isolate(view_id())
    }
    navigate_region <- function(id, remember=TRUE) {
      if(identical(id,shiny::isolate(region_id())))return(invisible(NULL))
      if(remember)remember_navigation()
      views<-shiny::isolate(regions())[[id]]$views %||% list()
      remembered<-view_memory[[if(nzchar(id))id else "__whole__"]]
      # Compare core memberships in the same embedding route when available.
      # Explicit per-region choices, including parent preview, take precedence.
      next_region<-shiny::isolate(regions())[[id]]
      current_region<-shiny::isolate(region())
      if(is.null(remembered) && identical(next_region$definition$type,"coverage_core") &&
         identical(current_region$definition$type,"coverage_core")) {
        current_view<-Filter(function(v)identical(v$id,shiny::isolate(view_id())),current_region$views)
        route<-if(length(current_view))current_view[[1]]$embedding_route else NULL
        matches<-if(length(route))Filter(function(v)identical(v$embedding_route,route),views) else list()
        if(length(matches)==1L)remembered<-matches[[1]]$id
      }
      remembered<-remembered %||% "__preview__"
      if(!remembered %in% c("__preview__",vapply(views,`[[`,"","id")))remembered<-"__preview__"
      if(remembered!="__preview__" && identical(shiny::isolate(view_id()),"__preview__"))parent_set(shiny::isolate(view_state())$set_id)
      region_id(id);view_id(remembered)
    }
    # External actions (save/revise/import/defaults) also select the matching family.
    shiny::observeEvent(region_id(), {
      id<-region_id()
      if(!identical(shiny::isolate(nav_model())$target,id))nav_state(gflowui_atlas_navigation_state(nav_catalog(),id))
    },ignoreNULL=FALSE,priority=150)
    output$navigation <- shiny::renderUI({
      manifest()$project_id
      if (!isTRUE(config()$enabled)) return(shiny::p("Local views require a dataset abundance source in the project configuration."))
      # Controls remain mounted; one versioned browser event records user intent.
      shiny::isolate(shiny::tagList(
        shiny::div(id=session$ns("browser"),class="gf-atlas-browser", `data-gf-atlas-navigation`=session$ns("nav_choice"),
          lapply(names(gflowui_atlas_navigation_fields()),function(key)
            shiny::div(id=session$ns(paste0("nav_",key,"_row")),
              shiny::selectInput(session$ns(paste0("nav_",key)),gflowui_atlas_navigation_fields()[[key]],c("Choose…"=""),width="100%")))),
        shiny::textOutput(session$ns("navigation_hint")),
        shiny::div(style="display:none",`aria-hidden`="true",
          shiny::selectInput(session$ns("region"), "Data region", region_choices(), selected=region_id())),
        shiny::conditionalPanel(sprintf("input['%s'] !== ''", session$ns("region")),
          shiny::selectInput(session$ns("view"), "Region view", view_choices(), selected=view_id()))))
    })
    output$navigation_hint<-shiny::renderText({
      if(is.null(nav_model()$target))"Choose a region above. The current display stays unchanged until the selection is complete."
    })
    shiny::observe({
      model<-nav_model(); input$nav_mounted
      nav_token <<- nav_token+1L
      nav_sent(list(token=nav_token,project=manifest()$project_id,model=model))
      session$sendCustomMessage("gflowuiAtlasNavigation",list(container=session$ns("browser"),input=session$ns("nav_choice"),
        token=nav_token,project=manifest()$project_id,fields=lapply(names(gflowui_atlas_navigation_fields()),function(key){
          spec<-model$controls[[key]]
          list(id=session$ns(paste0("nav_",key)),key=key,label=spec$label %||% gflowui_atlas_navigation_fields()[[key]],
            selected=spec$selected %||% "",visible=isTRUE(spec$visible),
            options=lapply(seq_along(spec$choices),function(i)list(value=unname(spec$choices[i]),label=names(spec$choices)[i])))
        })))
    })
    shiny::observeEvent(input$nav_choice, {
      e<-input$nav_choice;sent<-nav_sent()
      if(is.null(sent)||!identical(as.character(e$project),sent$project)||!identical(as.integer(e$token),sent$token))return()
      key<-e$key;spec<-sent$model$controls[[key]]
      if(is.null(spec)||!isTRUE(spec$visible)||length(e$value)!=1L||!e$value%in%unname(spec$choices)||identical(e$value,spec$selected))return()
      remember_navigation()
      state<-if(key=="family")nav_memory[[e$value]] %||% list(family=e$value) else gflowui_atlas_navigation_change(sent$model$state,key,e$value)
      model<-gflowui_atlas_navigation_resolve(nav_catalog(),state,length(drafts())>0L)
      nav_state(model$state)
      if(!is.null(model$target))navigate_region(model$target,remember=FALSE)
    },ignoreInit=TRUE,priority=200)
    shiny::observe({
      choices <- region_choices(); selected <- region_id()
      shiny::freezeReactiveValue(input, "region")
      shiny::updateSelectInput(session, "region", choices=choices, selected=selected)
    })
    shiny::observe({
      choices <- view_choices(); selected <- view_id()
      shiny::freezeReactiveValue(input, "view")
      shiny::updateSelectInput(session, "view", choices=choices, selected=selected)
    })
    shiny::observeEvent(input$region, {
      if (!input$region %in% unname(region_choices())) return()
      if (!identical(input$region, region_id())) {
        navigate_region(input$region)
      }
    }, ignoreNULL=TRUE, priority=100)
    shiny::observeEvent(input$view, {
      # A delayed view event from the previous region must not switch this one.
      if (!identical(input$region, region_id()) || !input$view %in% unname(view_choices())) return()
      if (!identical(input$view, view_id())) {
        if(identical(view_id(),"__preview__") && !identical(input$view,"__preview__"))
          parent_set(view_state()$set_id)
        view_id(input$view)
      }
    }, ignoreNULL=TRUE)
    shiny::observeEvent(input$whole, { remember_navigation(); nav_state(list(family="whole")); region_id(""); view_id("__preview__") })
    shiny::observeEvent(input$clear, { remember_navigation(); drafts(list()); nav_state(list(family="whole")); region_id(""); view_id("__preview__") })
    output$context <- shiny::renderText({
      r <- region(); if (is.null(r)) return("Context: whole dataset.")
      st <- view_state(); present <- sum(r$vertex_ids %in% st$vertex_ids)
      if(identical(r$definition$type,"coverage_core")) {
        d<-r$definition
        return(sprintf("%s cells; %s samples before residual filtering; %s retained (%.2f%% of the full reference). %s samples belong to cells left unfiltered. %s",
          d$cells,d$base_n,length(r$vertex_ids),100*d$actual_coverage,d$unfiltered_n,
          if(identical(view_id(),"__preview__")) "Parent embedding preview; existing display filters still apply. Check 'Show only region members' to isolate this core." else "Recomputed Euclidean MDS coordinates. Graph edges are a landmark connection scaffold, not the fitted distance model. Existing display filters still apply."))
      }
      sprintf("%s — %s members; %s present in this graph. %s", r$label, length(r$vertex_ids), present,
        if (identical(view_id(), "__preview__")) "Parent embedding preview: coordinates and distances unchanged. Existing display filters still apply." else if(length(st$graph_set$atlas$excluded_ids)) sprintf("Local chart fit: %d of %d members retained; %d excluded by the explicit chart policy. Saved region membership is unchanged.",present,length(r$vertex_ids),length(st$graph_set$atlas$excluded_ids)) else "Saved local fit: paths and coordinates belong to this region.")
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
      shiny::selectInput(session$ns("groups"), "CSTs in current graph", stats::setNames(names(counts), paste0(names(counts), " (", counts, ")")), multiple=TRUE, selected=shiny::isolate(input$groups))
    })
    shiny::observeEvent(input$table_groups, {
      s <- dcst_selection()
      if(startsWith(s$level %||% "","udcst")) {status("This region builder uses ordered dCSTs. Select dCST under CST type before importing its checked groups.");return()}
      shiny::updateSelectInput(session,"level",selected=s$level)
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
        rs<-gflowui_atlas_update(path(),function(current)gflowui_atlas_merge(current,ds))
        regions(rs); region_id(ds[[1]]$id); drafts(list())
        status("Region membership saved. Existing views and dataset endpoint annotations remain available.")
      },error=function(e)status(conditionMessage(e)))
    })
    output$lifecycle<-shiny::renderUI({
      rs<-regions();if(!length(rs))return(NULL)
      shiny::tagList(shiny::hr(),shiny::h6("Region history"),
        shiny::selectInput(session$ns("revision_parent"),"Revise saved region",stats::setNames(names(rs),vapply(rs,gflowui_atlas_region_label,"")),
          selected=if(region_id()%in%names(rs))region_id() else shiny::isolate(input$revision_parent)),
        shiny::actionButton(session$ns("revise"),"Save single preview as new revision"),
        shiny::textInput(session$ns("rename_label"),"Saved region name",region()$label %||% ""),
        shiny::actionButton(session$ns("rename"),"Rename selected region"),
        shiny::actionButton(session$ns("retire"),"Retire selected region"),
        shiny::actionButton(session$ns("restore"),"Restore selected region"),
        shiny::p("Revisions retain old membership and fitted views. Retirement hides a region without deleting assets; running jobs may finish into its history.",class="gf-hint"))
    })
    shiny::observeEvent(input$revise,{
      tryCatch({
        ds<-drafts();if(length(ds)!=1L)stop("Preview exactly one membership set before creating a revision.")
        new_id<-NULL
        rs<-gflowui_atlas_update(path(),function(current) {
          revised<-gflowui_atlas_revision(current,input$revision_parent,ds[[1]],input$rename_label)
          new_id<<-revised$region$id;revised$regions
        })
        regions(rs);region_id(new_id);drafts(list());view_id("__preview__")
        status("New membership revision saved; earlier views remain with their original revision.")
      },error=function(e)status(conditionMessage(e)))
    })
    for(action in c("rename","retire","restore"))local({
      act<-action
      shiny::observeEvent(input[[act]],{
        tryCatch({
          id<-region_id()
          rs<-gflowui_atlas_update(path(),function(current)gflowui_atlas_lifecycle(current,id,act,input$rename_label))
          regions(rs)
          if(act=="retire"){region_id("");view_id("__preview__")}
          status(paste("Region",act,"saved; existing assets retained."))
        },error=function(e)status(conditionMessage(e)))
      })
    })
    shiny::observeEvent(input$show_retired,{
      if(!isTRUE(input$show_retired)&&isTRUE(region()$retired)){region_id("");view_id("__preview__")}
    })
    output$bundle<-shiny::downloadHandler(filename=function()"local-view-reproducibility.zip",content=function(file) {
      r<-region();i<-match(view_id(),vapply(r$views,`[[`,"","id"))
      if(is.na(i))stop("Choose a completed fitted view before downloading its bundle.")
      gs<-r$views[[i]]
      if(is.null(gs$atlas$bundle_path))stop("This imported legacy view retains its original provenance; replay bundles are available for newly computed atlas views.")
      gflowui_atlas_export_bundle(gs$atlas$bundle_path,file)
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
        rs<-gflowui_atlas_update(path(),function(current)gflowui_atlas_merge(current,imported))
        regions(rs)
        status(sprintf("Imported %d regions with %d shared views. Existing asset files were not copied.",length(imported),sum(vapply(imported,function(r)length(r$views),1L))))
      },error=function(e)status(conditionMessage(e)))
    })
    open_membership <- function(label, ids, definition) {
      if(!isTRUE(config()$enabled))stop("Local regions are not configured for this project.")
      universe<-gflowui_vertex_hover_asset(manifest())$sample_ids
      r<-gflowui_atlas_region(label,ids,universe,definition)
      rs<-gflowui_atlas_update(path(),function(current)gflowui_atlas_merge(current,list(r)))
      regions(rs);region_id(r$id);drafts(list());view_id("__preview__")
      status("Saved state-witness region; membership is fixed to the reference sample IDs.")
      invisible(r$id)
    }
    list(open_membership=open_membership,form=shiny::reactive({v<-shiny::reactiveValuesToList(input);v$calculation<-calculation$form();v}), manifest=shiny::reactive(gflowui_atlas_manifest(manifest(),region(),view_id(),parent_set())),
         region=region, regions=regions, preview=shiny::reactive(if(identical(view_id(),"__preview__")) region() else NULL),
         only_members=shiny::reactive(isTRUE(input$only_members)), context=shiny::reactive({
           r<-region(); if(is.null(r)) "Whole dataset" else paste(r$label, if(identical(view_id(),"__preview__")) "— parent preview" else "— local fitted view")
         }))
  })
}
