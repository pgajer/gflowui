gflowui_embedding_endpoints_ui <- function(id, open=FALSE) {
  ns <- shiny::NS(id)
  shiny::tags$details(open=if(isTRUE(open))"open" else NULL,
    ontoggle=sprintf("Shiny.setInputValue('%s',this.open,{priority:'event'})",ns("open")),
    shiny::tags$summary("Detect endpoints from embedding"),
    shiny::uiOutput(ns("controls")),shiny::textOutput(ns("status")),
    shiny::plotOutput(ns("spacing_plot"),height="180px"),
    shiny::uiOutput(ns("candidates")))
}

gflowui_embedding_endpoints_server <- function(id,frame,label_for_vertex,add_vertices) {
  shiny::moduleServer(id,function(input,output,session) {
    result <- shiny::reactiveVal(NULL); selected <- shiny::reactiveVal(integer())
    message <- shiny::reactiveVal("")
    defaults <- gflowui_embedding_endpoint_defaults()
    settings <- shiny::reactive({
      o <- lapply(names(defaults),function(n) input[[n]] %||% defaults[[n]])
      names(o)<-names(defaults);o
    })
    output$controls <- shiny::renderUI({
      # Keep the rendered values current when the enclosing workflow panel is
      # rebuilt (for example after graph selection or working-set edits).
      ns <- session$ns; o <- settings()
      shiny::tagList(
        shiny::p(class="gf-hint","Uses all vertices in the current 3D embedding, including hidden vertices. Candidates describe this embedding's geometry."),
        shiny::numericInput(ns("k"),"Neighbors for angles and support",o$k,min=2,max=200,step=1),
        shiny::numericInput(ns("angle"),"Maximum angle (degrees)",o$angle,min=0,max=180,step=5),
        shiny::checkboxInput(ns("drop_one"),"Allow exclusion of one angular outlier",o$drop_one),
        shiny::selectInput(ns("spacing"),"Spacing measure",c("Nearest-neighbor distance (d1)"="d1","k-th-neighbor distance (dk)"="dk"),o$spacing),
        shiny::selectInput(ns("rule"),"Spacing cutoff",c("Estimated mode"="mode","Percentile"="percentile","Manual distance"="manual","No spacing filter"="none"),o$rule),
        shiny::conditionalPanel("input.rule == 'mode'",ns=ns,
          shiny::numericInput(ns("bandwidth"),"Mode smoothing multiplier",o$bandwidth,min=.01,max=100,step=.1)),
        shiny::conditionalPanel("input.rule == 'percentile'",ns=ns,
          shiny::numericInput(ns("percentile"),"Spacing percentile",o$percentile,min=0,max=100,step=1)),
        shiny::conditionalPanel("input.rule == 'mode' || input.rule == 'percentile'",ns=ns,
          shiny::numericInput(ns("multiplier"),"Cutoff multiplier",o$multiplier,min=0,step=.1)),
        shiny::conditionalPanel("input.rule == 'manual'",ns=ns,
          shiny::numericInput(ns("cutoff"),"Cutoff in embedding coordinate units",o$cutoff,min=0,step=.01)),
        shiny::checkboxInput(ns("merge"),"Merge nearby candidates",o$merge),
        shiny::conditionalPanel("input.merge",ns=ns,
          shiny::numericInput(ns("merge_radius"),"Merge radius / smaller local dk",o$merge_radius,min=0,max=100,step=.1)),
        shiny::p(class="gf-hint","Merging favors the smallest angle, then the smallest spacing. Coincident points share one representative; zero-length directions are excluded."),
        shiny::actionButton(ns("detect"),"Detect candidates",class="btn-primary"),
        shiny::checkboxInput(ns("show_preview"),"Highlight selected candidates (Plotly)",TRUE))
    })
    valid <- shiny::reactive(gflowui_embedding_endpoint_result_valid(result(),frame(),settings()))
    shiny::observeEvent(input$detect,{
      message(""); f<-frame()
      if(is.null(f)) {message("Load a 3D embedding first.");return()}
      value<-tryCatch(shiny::withProgress(message="Detecting endpoint candidates",value=0,{
        ans<-gflowui_detect_embedding_endpoints(f$coords,settings(),f$vertex_ids)
        shiny::incProgress(1);ans
      }),error=function(e)e)
      if(inherits(value,"error")){message(conditionMessage(value));return()}
      value$key<-f$key;value$project_id<-f$project_id;value$set_id<-f$set_id
      value$created<-format(Sys.time(),tz="UTC",usetz=TRUE)
      rows<-value$rows
      rows$label<-""
      for(v in rows$vertex[rows$candidate]) {
        label<-label_for_vertex(v)
        rows$label[v]<-if(length(label)==1L && !is.na(label) && nzchar(label)) label else rows$sample_id[v]
      }
      value$rows<-rows;result(value);selected(rows$vertex[rows$retained])
      shiny::updateNumericInput(session,"page",value=1)
    },ignoreInit=TRUE)
    candidate_rows <- shiny::reactive({
      if(!valid())return(data.frame())
      r<-result()$rows;r<-r[r$candidate,,drop=FALSE]
      r[order(!r$retained,r$angle,r$vertex),,drop=FALSE]
    })
    shiny::observeEvent(input$selection,{
      if(!valid())return()
      evt<-input$selection;v<-suppressWarnings(as.integer(evt$vertex))
      if(length(v)!=1L || !v%in%candidate_rows()$vertex)return()
      selected(if(isTRUE(evt$checked))union(selected(),v) else setdiff(selected(),v))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$select_all,{if(valid())selected(candidate_rows()$vertex)},ignoreInit=TRUE)
    shiny::observeEvent(input$select_representatives,{if(valid())selected(result()$rows$vertex[result()$rows$retained])},ignoreInit=TRUE)
    shiny::observeEvent(input$select_none,{selected(integer())},ignoreInit=TRUE)
    shiny::observeEvent(input$add,{
      if(!valid()){message("Embedding or settings changed. Detect candidates again before adding them.");return()}
      vertices<-intersect(selected(),candidate_rows()$vertex)
      if(!length(vertices)){message("Select at least one candidate.");return()}
      tryCatch({add_vertices(vertices,result());message(sprintf("Added %d selected candidates to Working Endpoints.",length(vertices)))},
        error=function(e)message(conditionMessage(e)))
    },ignoreInit=TRUE)
    output$status <- shiny::renderText({
      r<-result();msg<-message()
      if(is.null(r))return(if(nzchar(msg))msg else "Choose settings and detect candidates.")
      if(!valid())return(paste("Embedding or settings changed: preview inactive. Detect candidates again.",msg))
      paste(sprintf("%d candidates; %d after merging; %d selected. %d coincident vertices share representatives. Spacing cutoff: %s.",
        sum(r$rows$candidate),sum(r$rows$retained),length(selected()),r$coincident_vertices,
        if(is.finite(r$cutoff))format(r$cutoff,digits=4) else "none"),msg)
    })
    output$spacing_plot <- shiny::renderPlot({
      if(!valid())return(invisible(NULL))
      r<-result();graphics::par(mar=c(4,4,1,1))
      graphics::hist(r$spacings,breaks="FD",main="",xlab=paste(r$options$spacing,"in embedding units"),
        col="#dbeafe",border="white",freq=FALSE)
      if(!is.null(r$density))graphics::lines(r$density$x,r$density$y,col="#334155")
      if(is.finite(r$cutoff))graphics::abline(v=r$cutoff,col="#dc2626",lwd=2)
    })
    output$candidates <- shiny::renderUI({
      ns<-session$ns;r<-candidate_rows()
      if(!nrow(r))return(NULL)
      # Page is read in isolation here; the table below updates independently.
      shiny::tagList(shiny::actionButton(ns("select_all"),"Select all candidates"),
        shiny::actionButton(ns("select_representatives"),"Select representatives"),
        shiny::actionButton(ns("select_none"),"Clear selection"),
        shiny::numericInput(ns("page"),"Candidate page (25 per page)",shiny::isolate(input$page %||% 1L),min=1,max=ceiling(nrow(r)/25),step=1),
        shiny::uiOutput(ns("table")),
        shiny::actionButton(ns("add"),"Add selected to Working Endpoints",class="btn-primary"),
        shiny::downloadButton(ns("download"),"Download scores and settings"))
    })
    output$table <- shiny::renderUI({
      r<-candidate_rows();if(!nrow(r))return(NULL)
      page<-suppressWarnings(as.integer(input$page %||% 1L))
      if(length(page)!=1L || is.na(page))page<-1L
      page<-max(1L,min(ceiling(nrow(r)/25),page));rr<-r[seq.int((page-1L)*25+1L,min(nrow(r),page*25)),,drop=FALSE]
      chosen<-selected();ns<-session$ns
      shiny::div(style="overflow-x:auto;max-height:350px;overflow-y:auto;",
        shiny::tags$table(class="table table-sm",shiny::tags$thead(shiny::tags$tr(lapply(
          c("Select","Vertex","Label","d1","dk","Angle","Raw angle","Merged into"),shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(rr)),function(i){
            x<-rr[i,];event<-sprintf("Shiny.setInputValue('%s',{vertex:%d,checked:this.checked},{priority:'event'})",ns("selection"),x$vertex)
            shiny::tags$tr(shiny::tags$td(shiny::tags$input(type="checkbox",checked=if(x$vertex%in%chosen)"checked" else NULL,
              `aria-label`=paste("Select candidate",x$vertex),onchange=event)),
              shiny::tags$td(x$vertex),shiny::tags$td(x$label),
              lapply(c(x$d1,x$dk,x$angle,x$max_angle),function(v)shiny::tags$td(format(v,digits=4))),
              shiny::tags$td(if(is.na(x$suppressed_by))"" else x$suppressed_by))
          }))))
    })
    output$download <- shiny::downloadHandler(filename=function()"embedding-endpoint-scores.csv",content=function(file){
      shiny::req(valid());r<-result();d<-r$rows
      d$selected<-d$vertex%in%selected();d$embedding_fingerprint<-r$key;d$graph_set<-r$set_id
      d$detected_utc<-r$created;d$spacing_cutoff<-r$cutoff;d$spacing_mode<-r$mode
      for(n in names(r$options))d[[paste0("setting_",n)]]<-r$options[[n]]
      utils::write.csv(d,file,row.names=FALSE)
    })
    preview<-shiny::reactive({
      if(!valid() || identical(input$show_preview,FALSE))return(NULL)
      r<-result()$rows;r[r$vertex%in%selected() & r$candidate,,drop=FALSE]
    })
    list(preview=preview,result=result,valid=valid,selected=selected,settings=settings)
  })
}

gflowui_add_embedding_endpoint_preview <- function(plot,coords,rows,keep_idx=seq_len(nrow(coords))) {
  if(is.null(rows) || !nrow(rows))return(plot)
  rows<-rows[rows$vertex%in%keep_idx,,drop=FALSE];if(!nrow(rows))return(plot)
  v<-rows$vertex
  text<-sprintf("Endpoint candidate %d<br>%s<br>d1: %.5g; dk: %.5g<br>Angle: %.2f degrees; raw: %.2f degrees",
    v,htmltools::htmlEscape(rows$label),rows$d1,rows$dk,rows$angle,rows$max_angle)
  plotly::add_trace(plot,type="scatter3d",mode="markers",x=coords[v,1],y=coords[v,2],z=coords[v,3],
    key=v,customdata=v,hovertext=text,hoverinfo="text",name="Endpoint candidates",
    marker=list(size=7,color="#f97316",symbol="diamond",line=list(color="#111827",width=1)),inherit=FALSE)
}
