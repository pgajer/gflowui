gflowui_ec_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Embedding comparison"),
    shiny::selectInput(ns("graph"), "Graph", choices = character()),
    shiny::div(class="btn-group", role="group", `aria-label`="Graph navigation",
      shiny::actionButton(ns("previous_graph"), "Previous"),
      shiny::actionButton(ns("next_graph"), "Next")),
    shiny::actionButton(ns("toggle_favorite"), "Add to favorites"),
    shiny::textOutput(ns("favorites_status")),
    shiny::actionButton(ns("export_favorites"), "Export favorites"),
    shiny::uiOutput(ns("favorites_export_path")),
    shiny::p(class="gf-hint", "Favorites are saved automatically for this project. Export the list when your selection is ready; no graphs are deleted."),
    shiny::selectInput(ns("run"), "3D embedding", choices = character()),
    shiny::p(class="gf-hint", shiny::textOutput(ns("selected_configuration"))),
    shiny::p(class = "gf-hint", "One saved example per layout configuration, preferring seed 17. All replicates and attempts remain available in the Inspector."),
    shiny::sliderInput(ns("vertex_size"), "Vertex size", min=1, max=8, value=3, step=.5),
    shiny::selectInput(ns("vertex_color"), "Vertex color", choices=c(Blue="#3575B2",Orange="#C7782A",Charcoal="#39434A",Gold="#B79A20", stats::setNames(names(gflowui_ec_property_names()),gflowui_ec_property_names()))),
    shiny::checkboxInput(ns("edges"), "Show graph edges", TRUE),
    shiny::selectInput(ns("edge_coloring"), "Edge colors", choices=c(
      "Uniform gray"="uniform", "Drawn length: short warm, long cool"="length",
      stats::setNames(names(gflowui_ec_property_names("edge")),gflowui_ec_property_names("edge")))),
    shiny::p(class="gf-hint", "Length colors use the current embedding's Euclidean edge lengths, not graph distances. The color scale resets for each layout."),
    shiny::p(class="gf-hint", "Structural colors use the saved unit-length graph and stay fixed across embeddings. Purple to yellow means low to high; red marks infinite detours. Gray marks inapplicable bridge-side sizes. Hover vertices or edge midpoints for values."),
    shiny::textOutput(ns("property_status")),
    shiny::checkboxInput(ns("labels"), "Show vertex labels", TRUE),
    shiny::selectInput(ns("vertex_label"), "Vertex label value", choices=c("Vertex ID"="id", stats::setNames(names(gflowui_ec_property_names()),gflowui_ec_property_names()))),
    shiny::selectInput(ns("vertex_label_scope"), "Label which vertices", choices=c("Selected vertices"="selected","All vertices"="all")),
    shiny::selectInput(ns("edge_label"), "Edge label value", choices=c("No edge labels"="none",stats::setNames(names(gflowui_ec_property_names("edge")),gflowui_ec_property_names("edge")))),
    shiny::selectInput(ns("edge_label_scope"), "Label which edges", choices=c("Touching selected vertices"="selected","All edges"="all")),
    shiny::actionButton(ns("clear_vertices"), "Clear vertex selection", class="btn-light"),
    shiny::p(class="gf-hint", "Click vertices to select them. Selection and color stay fixed when changing embeddings of the same graph."),
    shiny::actionButton(ns("reload"), "Reload saved results", class="btn-light"),
    shiny::textOutput(ns("load_message"))
  )
}

gflowui_ec_workspace_ui <- function(id) {
  ns <- shiny::NS(id)
  choices <- stats::setNames(names(gflowui_ec_metrics()),unname(gflowui_ec_metrics()))
  shiny::div(class="ec-workspace", "data-ec-namespace"=ns(""),
    shiny::div(class="ec-graph-pane",shiny::h4(shiny::textOutput(ns("title"))),
      plotly::plotlyOutput(ns("graph_plot"),height="72vh"),shiny::textOutput(ns("view_status"))),
    shiny::div(class="ec-divider",role="separator",tabindex="0","aria-orientation"="vertical",
      "aria-label"="Resize General Inspector",title="Drag to resize; arrow keys adjust width."),
    shiny::tags$aside(class="ec-inspector","aria-label"="General Inspector",
      shiny::h3("General Inspector"),
      shiny::uiOutput(ns("about_graph")),
      shiny::tags$details(open=NA,shiny::tags$summary("Overview / Graph Data"),shiny::uiOutput(ns("overview"))),
      shiny::tags$details(open=NA,shiny::tags$summary("Embedding quality table"),
        shiny::p(class="gf-hint","Click a heading to sort. Load selects an exact replicate; the active row is highlighted. Blank scores are unavailable, not zero."),
        shiny::uiOutput(ns("quality_table"))),
      shiny::tags$details(open=NA,shiny::tags$summary("Quality comparisons"),
        shiny::textOutput(ns("evaluation_note")),
        shiny::selectInput(ns("metric"),"Metric by method",choices=choices,selected="chord_error"),
        plotly::plotlyOutput(ns("metric_plot"),height="590px"),shiny::textOutput(ns("metric_note")),
        shiny::selectInput(ns("trade_x"),"Trade-off x",choices=choices,selected="chord_error"),
        shiny::selectInput(ns("trade_y"),"Trade-off y",choices=choices,selected="path_error"),
        plotly::plotlyOutput(ns("trade_plot"),height="430px"),
        shiny::selectInput(ns("neighborhood"),"Neighborhood measure",choices=c(Trustworthiness="trustworthiness",Continuity="continuity")),
        plotly::plotlyOutput(ns("neighborhood_plot"),height="380px"),
        shiny::p(class="gf-hint","Gray dashed guides identify the displayed layout. Click a metric/trade-off point to load that exact run. Scores use original graph targets.")),
      shiny::tags$details(shiny::tags$summary("Seed variation and neighborhood ties"),shiny::uiOutput(ns("sensitivity"))),
      shiny::tags$details(shiny::tags$summary("Experimental LGS: locality and resource limits"),
        shiny::uiOutput(ns("lgs_note")),
        shiny::selectInput(ns("lgs_fixture"),"Synthetic validation graph",choices=c(
          "Path (48 vertices)"="validation_path48","Grid (49 vertices)"="validation_grid49",
          "Joined cliques (48 vertices)"="validation_cliques48")),
        shiny::selectInput(ns("lgs_metric"),"Locality comparison measure",choices=choices[names(choices)!="Distance-rank correlation (higher is better)"],selected="chord_error"),
        plotly::plotlyOutput(ns("lgs_plot"),height="400px")),
      shiny::tags$details(shiny::tags$summary("Distance diagnostics"),
        plotly::plotlyOutput(ns("shepard_plot"),height="390px"),
        plotly::plotlyOutput(ns("edge_plot"),height="350px"),shiny::textOutput(ns("diagnostic_note"))),
      shiny::tags$details("data-ec-open-input"=ns("run_details_open"),
        shiny::tags$summary("Active run settings and component diagnostics"),shiny::uiOutput(ns("run_details"))),
      shiny::tags$details(shiny::tags$summary("Metric definitions and interpretation"),
        lapply(names(gflowui_ec_definitions()),function(k) shiny::p(shiny::strong(paste0(k,": ")),gflowui_ec_definitions()[[k]]))),
      shiny::tags$details(shiny::tags$summary("Export comparison bundle"),
        shiny::p("Save every graph and run table, coordinates, metrics, source manifests and figure settings—not just the current selection. PDF/SVG comparisons for the selected graph and a figure-rebuilding R script are included."),
        shiny::textInput(ns("export_dir"),"Bundle directory",value=""),
        shiny::actionButton(ns("save_bundle"),"Save ZIP bundle",class="btn-light"),shiny::textOutput(ns("export_status")))
    )
  )
}

gflowui_ec_html_table <- function(data) {
  if(is.null(data) || !nrow(data)) return(shiny::p("No eligible records."))
  shiny::div(class="ec-table-scroll",shiny::tags$table(class="ec-table",
    shiny::tags$thead(shiny::tags$tr(lapply(names(data),function(n) shiny::tags$th(n,tabindex="0")))),
    shiny::tags$tbody(lapply(seq_len(nrow(data)),function(i) shiny::tags$tr(lapply(data,function(col) {
      v <- col[[i]]
      shiny::tags$td(if(is.na(v)) "unavailable" else if(is.numeric(v)) format(v,digits=6) else as.character(v))
    }))))))
}

gflowui_ec_plot_events <- function(plot,input_id,camera_id=NULL) {
  htmlwidgets::onRender(plot,"function(el,x,data) {
    if(el.ecClick) el.removeListener('plotly_click',el.ecClick);
    el.ecClick=function(e){var p=(e.points || []).find(function(q){return q.customdata;});
      if(p && p.customdata && window.Shiny) Shiny.setInputValue(data.input_id,
        {id:p.customdata,nonce:Date.now()+Math.random()},{priority:'event'});};
    el.on('plotly_click',el.ecClick);
    if(data.camera_id){
      if(el.ecCamera) el.removeListener('plotly_relayout',el.ecCamera);
      el.ecCamera=function(e){if(!Object.keys(e).some(function(k){return k.indexOf('scene.camera')===0;}))return;
        var c=el._fullLayout && el._fullLayout.scene && el._fullLayout.scene.camera;
        if(c && window.Shiny) Shiny.setInputValue(data.camera_id,JSON.parse(JSON.stringify(c)),{priority:'event'});};
      el.on('plotly_relayout',el.ecCamera);
    }
  }",data=list(input_id=input_id,camera_id=camera_id))
}

gflowui_ec_server <- function(id,manifest) {
  shiny::moduleServer(id,function(input,output,session) {
    active <- shiny::reactive(gflowui_ec_active(manifest()))
    counts <- new.env(parent=emptyenv())
    counts$index <- counts$graph <- counts$layout <- counts$graph_plot <- 0L
    counts$run_details <- 0L
    camera <- shiny::reactiveVal(NULL)
    camera_graph <- shiny::reactiveVal(NULL)
    selected_vertices <- shiny::reactiveVal(character())
    export_status <- shiny::reactiveVal("No bundle saved in this session.")
    index_state <- shiny::reactive({
      shiny::req(active()); input$reload
      counts$index <- counts$index+1L
      tryCatch(gflowui_ec_load_index(manifest()$metadata$embedding_comparison$data_root),
        error=function(e) list(error=conditionMessage(e)))
    })
    index <- shiny::reactive({
      state <- index_state()
      shiny::validate(shiny::need(is.null(state$error),state$error));state
    })
    output$load_message <- shiny::renderText({
      state <- index_state()
      if(!is.null(state$error)) state$error else "Saved results loaded; no computations run during viewing."
    })
    shiny::observeEvent(index(),{
      idx <- index();ids <- names(idx$graphs);previous <- shiny::isolate(input$graph)
      if(is.null(previous) || !previous %in% ids)previous <- ids[[1L]]
      shiny::updateSelectInput(session,"graph",choices=stats::setNames(ids,gflowui_ec_graph_labels(ids)),selected=previous)
      if(!nzchar(shiny::isolate(gflowui_ec_text(input$export_dir)))) shiny::updateTextInput(session,"export_dir",value=file.path(idx$root,"exports"))
    })
    graph_id <- shiny::reactive({
      ids <- names(index()$graphs)
      if(!is.null(input$graph) && input$graph %in% ids) input$graph else ids[[1L]]
    })
    step_graph <- function(delta) {
      ids <- names(index()$graphs)
      position <- match(graph_id(),ids)
      target <- ids[((position-1L+delta) %% length(ids))+1L]
      shiny::updateSelectInput(session,"graph",selected=target)
    }
    shiny::observeEvent(input$previous_graph,step_graph(-1L),ignoreInit=TRUE)
    shiny::observeEvent(input$next_graph,step_graph(1L),ignoreInit=TRUE)
    favorite_ids <- shiny::reactiveVal(character())
    favorite_error <- shiny::reactiveVal("")
    shiny::observeEvent(index()$root, {
      tryCatch({
        favorite_ids(gflowui_ec_read_favorites(index()$root))
        favorite_error("")
      }, error=function(e) favorite_error(conditionMessage(e)))
    })
    shiny::observe({
      shiny::updateActionButton(session, "toggle_favorite",
        label=if(graph_id() %in% favorite_ids()) "Remove from favorites" else "Add to favorites")
    })
    shiny::observeEvent(input$toggle_favorite, {
      tryCatch({
        ids <- gflowui_ec_read_favorites(index()$root)
        selected <- graph_id()
        ids <- if(selected %in% ids) setdiff(ids, selected) else c(ids, selected)
        gflowui_ec_save_favorites(index()$root, ids, names(index()$graphs))
        favorite_ids(ids)
        favorite_error("")
      }, error=function(e) favorite_error(conditionMessage(e)))
    }, ignoreInit=TRUE)
    output$favorites_status <- shiny::renderText({
      if(nzchar(favorite_error())) return(paste("Favorites could not be loaded or saved:", favorite_error()))
      paste(length(intersect(favorite_ids(), names(index()$graphs))), "of",
        length(index()$graphs), "graphs favorited. Current graph:",
        if(graph_id() %in% favorite_ids()) "favorite." else "not selected.")
    })
    favorite_export_path <- shiny::reactiveVal("")
    shiny::observeEvent(input$export_favorites, {
      tryCatch({
        path <- gflowui_ec_export_favorites(index()$root, names(index()$graphs))
        favorite_export_path(path)
        favorite_error("")
      }, error=function(e) favorite_error(conditionMessage(e)))
    }, ignoreInit=TRUE)
    output$favorites_export_path <- shiny::renderUI({
      path <- favorite_export_path()
      if(!nzchar(path)) return(NULL)
      shiny::tagList(shiny::tags$label(`for`=session$ns("saved_favorites_path"), "Saved favorites — copy full path"),
        shiny::tags$input(id=session$ns("saved_favorites_path"), type="text",
          class="form-control", value=path, readonly="readonly",
          onclick="this.select();"))
    })
    graph <- shiny::reactive({counts$graph <- counts$graph+1L;gflowui_ec_graph(index(),graph_id())})
    properties <- shiny::reactive(gflowui_ec_properties(index(),graph()))
    output$property_status <- shiny::renderText({
      if(is.null(properties())) "Structural properties are not available for this graph." else
        "Exact structural measures loaded. Betweenness normalization is within each component; vertex endpoints are excluded. Labels can be restricted to selected vertices and their incident edges."
    })
    cohort <- shiny::reactive({tbl <- index()$table;tbl[tbl$graph_id==graph_id(),,drop=FALSE]})
    run_label <- function(row,include_seed=TRUE) {
      if(!nrow(row))return(character())
      # Execution budgets remain in the Inspector/export records, but are not
      # part of the layout name. Keep method parameters and software versions.
      settings <- gsub("; 30 GiB; no time limit; serial","",row$settings,fixed=TRUE)
      settings <- vapply(strsplit(settings,";",fixed=TRUE),function(parts) {
        parts <- trimws(parts)
        paste(parts[nzchar(parts) & !parts %in% c("fixed pilot settings","fixed backend settings")],collapse="; ")
      },"")
      vapply(seq_len(nrow(row)),function(i) {
        parts <- c(row$method[i],settings[i],if(include_seed)paste0("seed ",row$seed[i]))
        paste(parts[nzchar(parts)],collapse=" | ")
      },"")
    }
    menu_labels <- function(rows) {
      labels <- run_label(rows,include_seed=FALSE)
      labels[rows$method == "Metric MDS — SGD (README candidates)"] <- "metric-MDS (SGD)"
      labels
    }
    dropdown_runs <- function(tbl,selected=NULL) {
      available <- tbl[tbl$status=="completed",,drop=FALSE]
      labels <- run_label(available,include_seed=FALSE)
      picks <- vapply(unique(labels),function(label) {
        group <- which(labels==label)
        # Loading an exact replicate from the Inspector replaces its group's
        # example; it must not create a second menu entry or change the run.
        chosen <- group[available$id[group] %in% selected]
        if(length(chosen))return(chosen[[1L]])
        preferred <- group[which(available$seed[group]==17)]
        if(length(preferred))return(preferred[[1L]])
        group[order(available$seed[group],na.last=TRUE)][[1L]]
      },1L,USE.NAMES=FALSE)
      available[picks,,drop=FALSE]
    }
    shiny::observeEvent(cohort(),{
      tbl <- cohort();previous <- shiny::isolate(input$run);available <- dropdown_runs(tbl,previous)
      previous <- gflowui_ec_choose_run(available,index()$table,previous)
      shiny::updateSelectInput(session,"run",choices=stats::setNames(available$id,menu_labels(available)),selected=previous)
    })
    run_id <- shiny::reactive({
      tbl <- cohort();available <- tbl$id[tbl$status=="completed"];if(!length(available))return("")
      if(!is.null(input$run) && input$run %in% available)input$run else gflowui_ec_choose_run(dropdown_runs(tbl),index()$table,input$run)
    })
    output$selected_configuration <- shiny::renderText({
      rows <- cohort(); row <- rows[rows$id==run_id(),,drop=FALSE]
      if (!nrow(row)) return("No completed layout available.")
      paste0("Configuration: ",row$settings[[1L]],"; seed ",row$seed[[1L]],".")
    })
    current <- shiny::reactive({counts$layout <- counts$layout+1L;gflowui_ec_load_run(index(),graph(),run_id())})
    active_label <- shiny::reactive({tbl <- cohort();if(!nzchar(run_id()))"No completed layout" else run_label(tbl[tbl$id==run_id(),,drop=FALSE])})
    shiny::observeEvent(graph_id(),{selected_vertices(character());camera(NULL);camera_graph(NULL)})
    shiny::observeEvent(input$camera,{
      value <- input$camera
      if(is.list(value) && all(c("eye","center","up") %in% names(value))) {
        axes <- value[c("eye","center","up")]
        if(all(vapply(axes,function(a) is.list(a) && identical(sort(names(a)),c("x","y","z")) &&
            all(vapply(a,function(v)is.numeric(v) && length(v)==1 && is.finite(v),TRUE)),TRUE))) {
          camera(value);camera_graph(graph_id())
        }
      }
    },ignoreInit=TRUE)
    shiny::observeEvent(input$vertex_click,{
      vertex <- gflowui_ec_text(input$vertex_click$id)
      if(vertex %in% graph()$ids) {
        old <- selected_vertices()
        selected_vertices(if(vertex %in% old)setdiff(old,vertex) else c(old,vertex))
      }
    },ignoreInit=TRUE)
    shiny::observeEvent(input$clear_vertices,selected_vertices(character()),ignoreInit=TRUE)
    shiny::observeEvent(input$choose_run,{
      candidate <- gflowui_ec_text(input$choose_run$id);tbl <- cohort()
      if(candidate %in% tbl$id[tbl$status=="completed"]) {
        available <- dropdown_runs(tbl,candidate)
        shiny::updateSelectInput(session,"run",choices=stats::setNames(available$id,menu_labels(available)),selected=candidate)
      }
    },ignoreInit=TRUE)
    output$title <- shiny::renderText(paste(gflowui_ec_graph_labels(graph_id()),active_label(),sep=" — "))
    output$view_status <- shiny::renderText({
      g <- graph();s <- current()$result
      text <- sprintf("%s vertices; %s edges; %s components (%s isolates). Components are packed only for display; %s cross-component pairs excluded. %s vertices selected.",
        g$n_vertices,g$n_edges,g$n_components,g$n_isolates,s$cross_component_pairs_excluded,length(selected_vertices()))
      if (identical(s$method,"trimap")) text <- paste(text,
        "Caution: this landmark-feature variant produced repeated feature rows and extreme triplet weights in the pilot. Its large-scale, poor layouts are retained; a separate graph-distance variant is also available.")
      text
    })
    output$about_graph <- shiny::renderUI({
      gflowui_ec_about_ui(index()$annotations[[graph_id()]])
    })
    output$overview <- shiny::renderUI({
      g <- graph()
      main <- data.frame(Characteristic=c("Graph","Vertices","Edges","Components","Isolates","Maximum degree","Mean clustering","Conversion","Edge lengths"),
        Value=c(g$graph_id,g$n_vertices,g$n_edges,g$n_components,g$n_isolates,g$degree_max,format(g$mean_clustering,digits=4),g$recipe,"Unit lengths; coefficients are not distances"))
      shiny::tagList(gflowui_ec_html_table(main),shiny::p("Original metadata, source links and transformation details:"),
        shiny::tags$details(shiny::tags$summary("All available graph metadata"),
          shiny::tags$pre(jsonlite::toJSON(g[setdiff(names(g),c("edges","vertex_ids","component_labels","edge_matrix","ids","labels"))],auto_unbox=TRUE,pretty=TRUE,null="null"))))
    })
    output$quality_table <- shiny::renderUI({
      tbl <- cohort();selected <- run_id()
      columns <- c(method="Method",settings="Settings",seed="Seed",status="Status",termination="Termination / reason",input_type="Input",evaluation="Evaluation",evaluated_pairs="Evaluated pairs",
        chord_error="Euclidean",relative_stress="Relative",path_error="Fixed path",edge_error="Edge",distance_rank_correlation="Correlation",elapsed_seconds="Seconds",memory_mib="MiB")
      shiny::div(class="ec-table-scroll",shiny::tags$table(class="ec-table ec-sortable",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("View"),lapply(columns,function(x)shiny::tags$th(x,tabindex="0")))),
        shiny::tags$tbody(lapply(seq_len(nrow(tbl)),function(i)shiny::tags$tr(class=if(tbl$id[i]==selected)"ec-active-row" else NULL,
          shiny::tags$td(shiny::tags$button(type="button",class="ec-load-run btn btn-light btn-sm","data-ec-run"=tbl$id[i],
            "data-ec-input"=session$ns("choose_run"),disabled=if(tbl$status[i]!="completed")NA else NULL,
            if(tbl$id[i]==selected)"Active" else "Load")),
          lapply(names(columns),function(k) {
            value <- tbl[[k]][i]
            display <- if(is.na(value))"—" else if(is.numeric(value))format(value,digits=5) else value
            if(k %in% names(gflowui_ec_metrics()) && is.finite(tbl[[paste0(k,"_lower")]][i]) && is.finite(tbl[[paste0(k,"_upper")]][i])) {
              display <- paste0(display," [",format(tbl[[paste0(k,"_lower")]][i],digits=4),", ",format(tbl[[paste0(k,"_upper")]][i],digits=4),"]")
            }
            shiny::tags$td("data-sort"=if(is.na(value))"" else as.character(value),
              display)
          }))))))
    })
    output$run_details <- shiny::renderUI({
      # Native <details> visibility is not reliably reported to Shiny. Do not
      # build a potentially large metadata view while this section is closed.
      shiny::req(isTRUE(input$run_details_open))
      counts$run_details <- counts$run_details+1L
      value <- current();record <- value$run
      details <- gflowui_ec_run_details_text(index(),record)
      shiny::tagList(shiny::p("Recorded request, software identity and component-level diagnostics. Scores are computed before display packing."),
        shiny::tags$pre(details))
    })
    output$graph_plot <- plotly::renderPlotly({
      counts$graph_plot <- counts$graph_plot+1L
      g <- graph();z <- current()$display;selected <- selected_vertices()
      size <- input$vertex_size;if(is.null(size))size <- 3
      color <- gflowui_ec_text(input$vertex_color,"#3575B2")
      p <- gflowui_ec_property_plot(g,z,properties(),vertex_color=color,
        edge_color=gflowui_ec_text(input$edge_coloring,"uniform"),size=size,
        show_edges=is.null(input$edges) || isTRUE(input$edges),selected=selected,
        vertex_label=gflowui_ec_text(input$vertex_label,"id"),vertex_labels=is.null(input$labels) || isTRUE(input$labels),
        vertex_label_scope=gflowui_ec_text(input$vertex_label_scope,"selected"),
        edge_label=gflowui_ec_text(input$edge_label,"none"),edge_label_scope=gflowui_ec_text(input$edge_label_scope,"selected"))
      axis <- list(title="",showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE)
      scene <- list(xaxis=axis,yaxis=axis,zaxis=axis,aspectmode="data",uirevision=graph_id(),
        camera=list(eye=list(x=1.8,y=1.8,z=1.8)))
      held_camera <- shiny::isolate(camera())
      if(is.list(held_camera) && identical(shiny::isolate(camera_graph()),graph_id()))scene$camera <- held_camera
      colorbar <- color %in% names(gflowui_ec_property_names()) ||
        ((identical(input$edge_coloring,"length") || gflowui_ec_text(input$edge_coloring,"uniform") %in% names(gflowui_ec_property_names("edge"))) &&
          (is.null(input$edges) || isTRUE(input$edges)) && nrow(g$edge_matrix)>0L)
      p <- plotly::layout(p,scene=scene,uirevision=graph_id(),clickmode="event",showlegend=TRUE,margin=list(l=0,r=if(colorbar)85 else 0,b=35,t=0),
        legend=list(orientation="h",x=0,y=-.02,font=list(size=10)))
      gflowui_ec_plot_events(p,session$ns("vertex_click"),session$ns("camera"))
    })
    metric_key <- gflowui_ec_quality_outputs(input,output,session,index,cohort,current,run_id,active_label,graph_id)
    shiny::observeEvent(input$save_bundle,{
      settings <- list(graph_id=graph_id(),run_id=run_id(),metric=metric_key(),trade_x=input$trade_x,trade_y=input$trade_y,
        neighborhood=input$neighborhood,vertex_color=input$vertex_color,vertex_size=input$vertex_size,
        edges=is.null(input$edges) || isTRUE(input$edges),
        edge_coloring=gflowui_ec_text(input$edge_coloring,"uniform"),
        edge_color_scale="length: short orange/long blue; structural: low purple/high yellow; infinite detour red; inapplicable gray",
        vertex_label=gflowui_ec_text(input$vertex_label,"id"),vertex_label_scope=gflowui_ec_text(input$vertex_label_scope,"selected"),
        edge_label=gflowui_ec_text(input$edge_label,"none"),edge_label_scope=gflowui_ec_text(input$edge_label_scope,"selected"),
        labels=is.null(input$labels) || isTRUE(input$labels),
        lgs_fixture=gflowui_ec_text(input$lgs_fixture,"validation_path48"),
        lgs_metric=gflowui_ec_text(input$lgs_metric,"chord_error"),
        lgs_population="synthetic integration validation; distinct from selected gallery graph",
        lgs_variant="lgs-paper-union-v1",
        lgs_aggregation="mean across available seeds; bars show observed minimum/maximum, not confidence intervals",
        camera=shiny::isolate(camera()),selected_vertices=selected_vertices(),histogram_bins=80,
        guides="gray dashed at active run",figure_source="saved metrics and display-only pair samples")
      tryCatch({
        path <- gflowui_ec_export(index(),settings,gflowui_ec_text(input$export_dir,file.path(index()$root,"exports")))
        export_status(paste("Saved full comparison bundle:",path))
      },error=function(e)export_status(paste("Export failed:",conditionMessage(e))))
    },ignoreInit=TRUE)
    output$export_status <- shiny::renderText(export_status())
    list(active=active,index=index,graph=graph,current=current,run_id=run_id,counts=counts,selected_vertices=selected_vertices,camera=camera)
  })
}
