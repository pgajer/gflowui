# Compare all non-coordinate content before using the coordinate-only route.
# Edge traces and annotation traces are included, so they move with vertices.
gflowui_scene_snapshot <- function(widget) {
  b <- plotly::plotly_build(widget)
  data <- lapply(seq_along(b$x$data), function(i) {
    trace <- b$x$data[[i]]
    trace$uid <- paste0('gf-',digest::digest(list(i,trace$name,trace$legendgroup),algo='xxhash64'))
    trace
  })
  layout <- b$x$layout
  # The camera belongs to the browser once the plot is mounted.
  layout$scene$camera <- NULL
  list(data=data,layout=layout,config=b$x$config)
}

gflowui_scene_delta <- function(old, new) {
  if (identical(old,new)) return(list(kind='unchanged'))
  strip <- function(traces) lapply(traces,function(t)t[setdiff(names(t),c('x','y','z'))])
  if (!is.null(old) && length(old$data)==length(new$data) &&
      identical(old$layout,new$layout) && identical(old$config,new$config) &&
      identical(strip(old$data),strip(new$data))) {
    changed<-which(!vapply(seq_along(new$data),function(i)
      identical(old$data[[i]][c('x','y','z')],new$data[[i]][c('x','y','z')]),logical(1)))
    return(list(kind='coordinates',indices=as.integer(changed-1L),
      coordinates=lapply(new$data[changed],function(t)t[c('x','y','z')])))
  }
  list(kind='full',data=new$data,layout=new$layout,config=new$config)
}

gflowui_scene_server <- function(input,output,session,widget,context) {
  boot <- shiny::reactiveVal(NULL); generation <- 0L; revision <- 0L
  active_scope <- NULL; previous <- NULL; sent_revision <- 0L
  pending <- NULL
  last_context <- NULL
  output$reference_plot <- plotly::renderPlotly({shiny::req(boot());boot()})
  shiny::observe({
    p <- widget();ctx <- context()
    if (is.null(p)) return()
    snap <- gflowui_scene_snapshot(p)
    if(!identical(active_scope,ctx$scope)) {
      generation <<- generation+1L; revision <<- revision+1L
      active_scope <<- ctx$scope;last_context <<- ctx;previous <<- snap;sent_revision <<- revision;pending <<- NULL
      meta <- list(generation=generation,revision=revision,scope=ctx$scope,
        selection_seq=ctx$selection_seq,set_id=ctx$set_id)
      p$x$layout$meta$gflowui_scene <- meta
      p <- htmlwidgets::onRender(p,'function(el,x){window.gflowuiScene.mount(el,x.layout.meta.gflowui_scene);}')
      boot(p)
      return()
    }
    delta <- gflowui_scene_delta(previous,snap)
    if (identical(delta$kind,'unchanged') && identical(last_context,ctx)) return()
    last_context <<- ctx
    revision <<- revision+1L
    # Compare against the last transmitted snapshot. Queued replacements must
    # not depend on a coordinate update that the browser may skip.
    pending <<- list(snapshot=snap,revision=revision,context=ctx)
    mounted <- shiny::isolate(input$gflowui_scene_mounted)
    if(is.list(mounted)&&identical(as.integer(mounted$generation),generation)) {
      # Full messages are independent of earlier messages; coordinate messages
      # declare the exact browser revision on which their trace indices rely.
      session$sendCustomMessage('gflowuiSceneUpdate',c(delta,list(
        generation=generation,revision=revision,base_revision=sent_revision,
        scope=ctx$scope,selection_seq=ctx$selection_seq,set_id=ctx$set_id)))
      previous <<- snap;sent_revision <<- revision;pending <<- NULL
    }
  })
  shiny::observeEvent(input$gflowui_scene_mounted,{
    m<-input$gflowui_scene_mounted
    if(!identical(as.integer(m$generation),generation))return()
    if(is.null(pending) && is.numeric(m$revision) && m$revision < sent_revision)
      pending <<- list(snapshot=previous,revision=sent_revision,context=last_context)
    if(!is.null(pending)) {
      ctx<-pending$context
      session$sendCustomMessage('gflowuiSceneUpdate',c(list(kind='full'),pending$snapshot,
        list(generation=generation,revision=pending$revision,base_revision=sent_revision,
          scope=ctx$scope,selection_seq=ctx$selection_seq,set_id=ctx$set_id)))
      previous<<-pending$snapshot;sent_revision<<-pending$revision;pending<<-NULL
    }
  })
  shiny::observeEvent(input$gflowui_scene_resync,{
    m<-input$gflowui_scene_resync
    if(!identical(as.integer(m$generation),generation)||is.null(previous))return()
    ctx<-last_context;revision<<-revision+1L;sent_revision<<-revision
    session$sendCustomMessage('gflowuiSceneUpdate',c(list(kind='full'),previous,
      list(generation=generation,revision=revision,scope=active_scope,
        selection_seq=ctx$selection_seq,set_id=ctx$set_id)))
  })
}
