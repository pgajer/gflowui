gflowui_ec_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Embedding comparison"),
    shiny::selectInput(ns("graph"), "Graph", choices = character()),
    shiny::selectInput(ns("run"), "3D embedding / replicate", choices = character()),
    shiny::p(class = "gf-hint", "Only completed, validated layouts are selectable. All attempts appear in the Inspector."),
    shiny::sliderInput(ns("vertex_size"), "Vertex size", min=1, max=8, value=3, step=.5),
    shiny::selectInput(ns("vertex_color"), "Vertex color", choices=c(Blue="#3575B2",Orange="#C7782A",Charcoal="#39434A",Gold="#B79A20")),
    shiny::checkboxInput(ns("edges"), "Show graph edges", TRUE),
    shiny::checkboxInput(ns("labels"), "Label selected vertices", TRUE),
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
      shiny::tags$details(open=NA,shiny::tags$summary("Overview / Graph Data"),shiny::uiOutput(ns("overview"))),
      shiny::tags$details(open=NA,shiny::tags$summary("Embedding quality table"),
        shiny::p(class="gf-hint","Click a heading to sort. Load selects an exact replicate; the active row is highlighted. Blank scores are unavailable, not zero."),
        shiny::uiOutput(ns("quality_table"))),
      shiny::tags$details(open=NA,shiny::tags$summary("Quality comparisons"),
        shiny::selectInput(ns("metric"),"Metric by method",choices=choices,selected="chord_error"),
        plotly::plotlyOutput(ns("metric_plot"),height="390px"),shiny::textOutput(ns("metric_note")),
        shiny::selectInput(ns("trade_x"),"Trade-off x",choices=choices,selected="chord_error"),
        shiny::selectInput(ns("trade_y"),"Trade-off y",choices=choices,selected="path_error"),
        plotly::plotlyOutput(ns("trade_plot"),height="430px"),
        shiny::selectInput(ns("neighborhood"),"Neighborhood measure",choices=c(Trustworthiness="trustworthiness",Continuity="continuity")),
        plotly::plotlyOutput(ns("neighborhood_plot"),height="380px"),
        shiny::p(class="gf-hint","Gray dashed guides identify the displayed layout. Click a metric/trade-off point to load that exact run. Scores use original graph targets.")),
      shiny::tags$details(shiny::tags$summary("Seed variation and neighborhood ties"),shiny::uiOutput(ns("sensitivity"))),
      shiny::tags$details(shiny::tags$summary("Distance diagnostics"),
        plotly::plotlyOutput(ns("shepard_plot"),height="390px"),
        plotly::plotlyOutput(ns("edge_plot"),height="350px"),shiny::textOutput(ns("diagnostic_note"))),
      shiny::tags$details(shiny::tags$summary("Active run settings and component diagnostics"),shiny::uiOutput(ns("run_details"))),
      shiny::tags$details(shiny::tags$summary("Metric definitions and interpretation"),
        lapply(names(gflowui_ec_definitions()),function(k) shiny::p(shiny::strong(paste0(k,": ")),gflowui_ec_definitions()[[k]]))),
      shiny::tags$details(shiny::tags$summary("Export comparison bundle"),
        shiny::p("Save every graph and run table, coordinates, metrics, source manifests and figure settings—not just the current selection."),
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
    el.ecClick=function(e){var p=e.points && e.points[0];
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
      shiny::updateSelectInput(session,"graph",choices=stats::setNames(ids,ids),selected=previous)
      if(!nzchar(shiny::isolate(gflowui_ec_text(input$export_dir)))) shiny::updateTextInput(session,"export_dir",value=file.path(idx$root,"exports"))
    })
    graph_id <- shiny::reactive({
      ids <- names(index()$graphs)
      if(!is.null(input$graph) && input$graph %in% ids) input$graph else ids[[1L]]
    })
    graph <- shiny::reactive({counts$graph <- counts$graph+1L;gflowui_ec_graph(index(),graph_id())})
    cohort <- shiny::reactive({tbl <- index()$table;tbl[tbl$graph_id==graph_id(),,drop=FALSE]})
    run_label <- function(row) if(!nrow(row))character() else paste(row$method,row$settings,paste0("seed ",row$seed),sep=" | ")
    shiny::observeEvent(cohort(),{
      tbl <- cohort();available <- tbl[tbl$status=="completed",,drop=FALSE];previous <- shiny::isolate(input$run)
      if(is.null(previous) || !previous %in% available$id)previous <- if(nrow(available)) available$id[[1L]] else ""
      shiny::updateSelectInput(session,"run",choices=stats::setNames(available$id,run_label(available)),selected=previous)
    })
    run_id <- shiny::reactive({
      tbl <- cohort();available <- tbl$id[tbl$status=="completed"];if(!length(available))return("")
      if(!is.null(input$run) && input$run %in% available)input$run else available[[1L]]
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
      if(candidate %in% tbl$id[tbl$status=="completed"])shiny::updateSelectInput(session,"run",selected=candidate)
    },ignoreInit=TRUE)
    output$title <- shiny::renderText(paste(graph_id(),active_label(),sep=" — "))
    output$view_status <- shiny::renderText({
      g <- graph();s <- current()$result
      text <- sprintf("%s vertices; %s edges; %s components (%s isolates). Components are packed only for display; %s cross-component pairs excluded. %s vertices selected.",
        g$n_vertices,g$n_edges,g$n_components,g$n_isolates,s$cross_component_pairs_excluded,length(selected_vertices()))
      if (identical(s$method,"trimap")) text <- paste(text,
        "Caution: this landmark-feature variant produced repeated feature rows and extreme triplet weights in the pilot. Its large-scale, poor layouts are retained; a separate graph-distance variant is also available.")
      text
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
      columns <- c(method="Method",settings="Settings",seed="Seed",status="Status",termination="Termination / reason",input_type="Input",
        chord_error="Euclidean",relative_stress="Relative",path_error="Fixed path",edge_error="Edge",distance_rank_correlation="Correlation",elapsed_seconds="Seconds",memory_mib="MiB")
      shiny::div(class="ec-table-scroll",shiny::tags$table(class="ec-table ec-sortable",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("View"),lapply(columns,function(x)shiny::tags$th(x,tabindex="0")))),
        shiny::tags$tbody(lapply(seq_len(nrow(tbl)),function(i)shiny::tags$tr(class=if(tbl$id[i]==selected)"ec-active-row" else NULL,
          shiny::tags$td(shiny::tags$button(type="button",class="ec-load-run btn btn-light btn-sm","data-ec-run"=tbl$id[i],
            "data-ec-input"=session$ns("choose_run"),disabled=if(tbl$status[i]!="completed")NA else NULL,
            if(tbl$id[i]==selected)"Active" else "Load")),
          lapply(names(columns),function(k) {
            value <- tbl[[k]][i]
            shiny::tags$td("data-sort"=if(is.na(value))"" else as.character(value),
              if(is.na(value))"—" else if(is.numeric(value))format(value,digits=5) else value)
          }))))))
    })
    output$run_details <- shiny::renderUI({
      value <- current();record <- value$run
      request <- if(is.list(record$manifest))gflowui_ec_json(gflowui_ec_asset(index()$root,record$manifest)) else record
      shiny::tagList(shiny::p("Recorded request, software identity and component-level diagnostics. Scores are computed before display packing."),
        shiny::tags$pre(jsonlite::toJSON(list(request=request,result=value$result),auto_unbox=TRUE,pretty=TRUE,null="null")))
    })
    output$graph_plot <- plotly::renderPlotly({
      counts$graph_plot <- counts$graph_plot+1L
      g <- graph();z <- current()$display;selected <- selected_vertices()
      size <- input$vertex_size;if(is.null(size))size <- 3
      color <- gflowui_ec_text(input$vertex_color,"#3575B2")
      p <- plotly::plot_ly()
      if(is.null(input$edges) || isTRUE(input$edges)) {
        e <- g$edge_matrix
        edge_coord <- function(j)as.vector(rbind(z[e[,1],j],z[e[,2],j],NA_real_))
        p <- plotly::add_trace(p,x=edge_coord(1),y=edge_coord(2),z=edge_coord(3),type="scatter3d",mode="lines",
          line=list(color="#AAB1B8",width=1),hoverinfo="skip",showlegend=FALSE)
      }
      p <- plotly::add_trace(p,x=z[,1],y=z[,2],z=z[,3],type="scatter3d",mode="markers",
        marker=list(size=size,color=ifelse(g$ids %in% selected,"#C7782A",color)),
        customdata=g$ids,text=g$ids,hoverinfo="text",showlegend=FALSE)
      idx <- which(g$ids %in% selected)
      if(length(idx) && (is.null(input$labels) || isTRUE(input$labels)))p <- plotly::add_trace(p,
        x=z[idx,1],y=z[idx,2],z=z[idx,3],type="scatter3d",mode="text",text=g$ids[idx],customdata=g$ids[idx],
        textposition="top center",textfont=list(size=13,color="#39434A"),showlegend=FALSE,hoverinfo="text")
      axis <- list(title="",showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE)
      scene <- list(xaxis=axis,yaxis=axis,zaxis=axis,aspectmode="data",uirevision=graph_id())
      held_camera <- shiny::isolate(camera())
      if(is.list(held_camera) && identical(shiny::isolate(camera_graph()),graph_id()))scene$camera <- held_camera
      p <- plotly::layout(p,scene=scene,uirevision=graph_id(),margin=list(l=0,r=0,b=0,t=0))
      gflowui_ec_plot_events(p,session$ns("vertex_click"),session$ns("camera"))
    })
    metric_key <- gflowui_ec_quality_outputs(input,output,session,index,cohort,current,run_id,active_label,graph_id)
    shiny::observeEvent(input$save_bundle,{
      settings <- list(graph_id=graph_id(),run_id=run_id(),metric=metric_key(),trade_x=input$trade_x,trade_y=input$trade_y,
        neighborhood=input$neighborhood,vertex_color=input$vertex_color,vertex_size=input$vertex_size,
        edges=is.null(input$edges) || isTRUE(input$edges),
        labels=is.null(input$labels) || isTRUE(input$labels),
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
