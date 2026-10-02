gflowui_atlas_calculation_ui <- function(id,region_input_id=NULL,values=list()) {
  ns<-shiny::NS(id)
  fields<-c("method","coordinates","metric","inner","power","k","mode","landmarks","iterations","seed","memory_mb","chart_anchor","chart_threshold","chart_policy")
  snapshot<-sprintf("var p={}; %s.forEach(function(k){var e=document.getElementById(%s+k); if(e)p[k]=e.type==='number'?Number(e.value):e.value;}); var context=JSON.parse(document.getElementById(%s).textContent); Shiny.setInputValue(%s,{parameters:p,context:context},{priority:'event'});",
    jsonlite::toJSON(fields),jsonlite::toJSON(ns(""),auto_unbox=TRUE),jsonlite::toJSON(ns("request_context"),auto_unbox=TRUE),jsonlite::toJSON(ns("request"),auto_unbox=TRUE))
  if(!is.null(region_input_id))snapshot<-sub("Shiny.setInputValue",paste0("var regionElement=document.getElementById(",jsonlite::toJSON(region_input_id,auto_unbox=TRUE),");if(regionElement)context.region_id=regionElement.value; Shiny.setInputValue"),snapshot,fixed=TRUE)
  coverage_snapshot<-sub(jsonlite::toJSON(ns("request"),auto_unbox=TRUE),jsonlite::toJSON(ns("coverage_request"),auto_unbox=TRUE),snapshot,fixed=TRUE)
  shiny::tagList(shiny::div(style="display:none",shiny::textOutput(ns("request_context"))),shiny::h6("Compute a local view"),
    shiny::p("Distances and paths are recomputed using only the saved region. Membership stays fixed.",class="gf-hint"),
    shiny::selectInput(ns("method"),"Embedding method",c("Metric-MDS (SGD), 3D"="mds","PCA, 3D"="pca"),selected=values$method %||% "mds"),
    shiny::selectInput(ns("coordinates"),"Coordinates",c("Relative abundances"="abundance","Square-root abundances"="sqrt_abundance","Anchor-centered homogeneous chart"="anchor_chart"),selected=values$coordinates %||% "abundance"),
    shiny::conditionalPanel(sprintf("input['%s'] === 'anchor_chart'",ns("coordinates")),
      shiny::textInput(ns("chart_anchor"),"Chart anchor vertex ID",values$chart_anchor %||% ""),
      shiny::numericInput(ns("chart_threshold"),"Minimum chart denominator",values$chart_threshold %||% 1e-6,min=1e-12,max=1,step=1e-6),
      shiny::selectInput(ns("chart_policy"),"Incomplete chart coverage",c("Stop; keep all members"="stop","Explicitly exclude uncovered samples from this fit"="exclude"),selected=values$chart_policy %||% "stop"),
      shiny::actionButton(ns("coverage"),"Preview chart coverage",onclick=coverage_snapshot),shiny::textOutput(ns("coverage_text")),
      shiny::downloadButton(ns("coverage_download"),"Download coverage")),
    shiny::conditionalPanel(sprintf("input['%s'] === 'mds'",ns("method")),
    shiny::selectInput(ns("metric"),"Base metric",c("Hellinger"="hellinger","Euclidean"="euclidean","Jensen–Shannon"="jensen_shannon"),selected=values$metric %||% "hellinger"),
    shiny::selectInput(ns("inner"),"Inner metric",c("Complete Fermat"="fermat","Ambient"="ambient","Symmetric kNN + MST"="sknn_mst"),selected=values$inner %||% "fermat"),
    shiny::conditionalPanel(sprintf("input['%s'] === 'fermat'",ns("inner")),shiny::numericInput(ns("power"),"Fermat power",values$power %||% 2,min=1,max=10,step=.1)),
    shiny::conditionalPanel(sprintf("input['%s'] === 'sknn_mst'",ns("inner")),shiny::numericInput(ns("k"),"Graph neighbors",values$k %||% 5,min=1,step=1)),
    shiny::selectInput(ns("mode"),"Distance fitting",c("Automatic (full up to 1,200 vertices)"="auto","Full distances"="full","Landmark distances"="landmarks"),selected=values$mode %||% "auto"),
    shiny::numericInput(ns("landmarks"),"Landmarks",values$landmarks %||% 200,min=4,step=50),
    shiny::numericInput(ns("iterations"),"SGD iterations",values$iterations %||% 100,min=1,step=25)),
    shiny::numericInput(ns("seed"),"Random seed",values$seed %||% 1,min=1,step=1),
    shiny::numericInput(ns("memory_mb"),"Workspace budget (MiB)",values$memory_mb %||% 1024,min=64,step=256),
    shiny::p("Both MDS modes use inverse-squared target weights. Landmark mode fits landmark-to-all pairs; PCA uses only the coordinate choice. Signed/transformed charts require Euclidean distance.",class="gf-hint"),
    shiny::actionButton(ns("submit"),"Queue local view",onclick=snapshot),
    shiny::textOutput(ns("message")),shiny::uiOutput(ns("jobs")),
    shiny::actionButton(ns("cancel"),"Cancel selected job"))
}

gflowui_atlas_calculation_server <- function(id,manifest,region,path,publish) {
  shiny::moduleServer(id,function(input,output,session) {
    message<-shiny::reactiveVal(""); jobs<-shiny::reactiveVal(list());coverage<-shiny::reactiveVal(NULL)
    root<-shiny::reactive(file.path(dirname(path()),"jobs"))
    shiny::observeEvent(input$coordinates,{
      if(!identical(input$coordinates,"abundance"))shiny::updateSelectInput(session,"metric",selected="euclidean")
    })
    shiny::observeEvent(input$coordinates,{
      if(identical(input$coordinates,"anchor_chart") && !nzchar(input$chart_anchor %||% "")) {
        r<-region();a<-r$definition$anchor
        if(is.null(a)||!a %in% gflowui_vertex_hover_asset(manifest())$sample_ids)a<-r$vertex_ids[1]
        if(length(a))shiny::updateTextInput(session,"chart_anchor",value=a)
      }
    })
    shiny::observeEvent(list(input$chart_anchor,input$chart_threshold,input$chart_policy,region()$id),coverage(NULL))
    shiny::observeEvent(input$coverage_request,{
      tryCatch({
        p<-gflowui_atlas_request_parameters(input$coverage_request,manifest()$project_id,region()$id)
        data<-gflowui_atlas_input(gflowui_vertex_hover_asset(manifest()),region(),p$chart_anchor)
        coverage(gflowui_atlas_chart(data$X,data$anchor$abundances,p$chart_threshold,p$chart_policy))
      },error=function(e){coverage(NULL);message(conditionMessage(e))})
    })
    output$coverage_text<-shiny::renderText({
      c<-coverage();if(is.null(c))return("Preview coverage before queueing a chart fit.")
      sprintf("%d of %d samples covered; %d %s at threshold %.3g. Minimum denominator %.3g. Coordinates are signed; Euclidean distance applies.",nrow(c$coords),nrow(c$coverage),length(c$excluded_ids),if(!length(c$excluded_ids))"uncovered; coverage passes" else if(c$policy=="stop")"uncovered (fit will stop)" else "excluded by explicit policy",c$threshold,min(c$coverage$denominator))
    })
    output$coverage_download<-shiny::downloadHandler(filename=function()"chart-coverage.csv",content=function(file) {
      shiny::req(coverage());utils::write.csv(coverage()$coverage,file,row.names=FALSE)
    })
    output$request_context<-shiny::renderText(jsonlite::toJSON(list(project_id=manifest()$project_id,region_id=region()$id %||% ""),auto_unbox=TRUE))
    shiny::outputOptions(output,"request_context",suspendWhenHidden=FALSE)
    shiny::observeEvent(input$request,{
      tryCatch({
        r<-region(); saved<-if(file.exists(path()))readRDS(path())$regions else list()
        if(is.null(r) || !r$id %in% names(saved))stop("Save the region before starting a calculation.")
        p<-gflowui_atlas_request_parameters(input$request,manifest()$project_id,r$id)
        job<-gflowui_atlas_enqueue(manifest(),r,p,root())
        message(if(job$cached)"Validated cached result found; attaching it to this region." else if(job$reused)"This exact calculation is already queued or running." else "Job queued. You can change views while it runs.")
      },error=function(e)message(conditionMessage(e)))
    })
    shiny::observe({
      shiny::invalidateLater(2000,session)
      current_root<-root()
      roots<-unique(c(current_root,Sys.glob(file.path(gflowui_projects_data_dir(),"projects","*","local_views","jobs"))))
      roots<-roots[dir.exists(roots)]; if(!length(roots))return()
      tryCatch(gflowui_atlas_dispatch(roots),error=function(e)message(conditionMessage(e)))
      folders<-unlist(lapply(roots,function(r)list.dirs(r,recursive=FALSE,full.names=TRUE)))
      winners<-gflowui_atlas_winners(folders)
      rows<-lapply(folders,function(f) {
        s<-gflowui_atlas_job_status(f)
        sf<-file.path(f,"spec.rds");if(!file.exists(sf))return(NULL)
        spec<-readRDS(sf)
        if(f %in% winners) {
          tryCatch(publish(f,spec),error=function(e)message(paste("Registration:",conditionMessage(e))))
        }
        if(!identical(dirname(f),current_root))return(NULL)
        list(folder=f,state=s$state,label=paste(substr(basename(f),1,10),s$state,s$stage,sep=" · "))
      });rows<-Filter(Negate(is.null),rows)
      if(!identical(shiny::isolate(jobs()),rows))jobs(rows)
    })
    output$message<-shiny::renderText(message())
    output$jobs<-shiny::renderUI({
      j<-jobs();if(!length(j))return(shiny::p("No local calculation jobs yet."))
      shiny::selectInput(session$ns("job"),"Calculation jobs",stats::setNames(vapply(j,`[[`,"","folder"),vapply(j,`[[`,"","label")),selected=shiny::isolate(input$job))
    })
    shiny::observeEvent(input$cancel,{
      tryCatch({
        if(!input$job %in% vapply(jobs(),`[[`,"","folder"))stop("Choose a queued or running job.")
        gflowui_atlas_cancel(input$job);message("Cancellation requested. Completed views remain available.")
      },error=function(e)message(conditionMessage(e)))
    })
    list(form=shiny::reactive(shiny::reactiveValuesToList(input)))
  })
}


# Validate the synchronous browser snapshot, never mixing it with debounced inputs.
gflowui_atlas_request_parameters <- function(request,project_id,region_id) {
  if(!identical(request$context$project_id,project_id)||!identical(request$context$region_id,region_id))
    stop("Region or project changed while submitting; review the selected region and queue again.")
  fields<-c("method","coordinates","metric","inner","power","k","mode","landmarks","iterations","seed","memory_mb","chart_anchor","chart_threshold","chart_policy")
  if(!is.list(request$parameters)||!setequal(names(request$parameters),fields))stop("Incomplete calculation settings; review the form and queue again.")
  request$parameters
}
