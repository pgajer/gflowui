gflowui_state_graphs_server <- function(id, manifest, view, visible, sample_selected,
                                         sample_click, save, open_region) {
  if(!requireNamespace("plotly",quietly=TRUE))return(list(enabled=function()FALSE,mode=function()"samples",
    controls=function()NULL,highlight=function()character(),filter=function(st,idx)idx))
  shiny::moduleServer(id,function(input,output,session) {
    base_asset<-gflowui_distinct_reactive(function()gflowui_state_graph_asset(manifest()))
    enabled<-gflowui_distinct_reactive(function()!is.null(base_asset()))
    state<-shiny::reactiveVal(gflowui_state_graph_default())
    reference_ids<-gflowui_distinct_reactive(function()state()$reference_ids)
    asset<-gflowui_distinct_reactive(function(){a<-base_asset();if(is.null(a))NULL else gflowui_state_graph_reference(a,reference_ids())})
    loaded<-shiny::reactiveVal(NULL); revision<-shiny::reactiveVal(0L)
    selection_ready<-shiny::reactiveVal(FALSE);edge_ready<-shiny::reactiveVal(FALSE)
    job<-NULL; job_spec<-NULL
    message<-shiny::reactiveVal(""); sample_point<-shiny::reactiveVal(character())
    project<-gflowui_distinct_reactive(function()manifest()$project_id)
    change<-function(...) {
      if(!isTRUE(enabled()))return()
      old<-shiny::isolate(state());new<-old
      for(k in names(list(...)))new[k]<-list(list(...)[[k]])
      if(!identical(old,new))state(new)
    }
    shiny::observeEvent(project(),{
      initial<-gflowui_state_graph_default(manifest()$defaults$state_graphs)
      selection_ready(!length(initial$selected));edge_ready(!nzchar(initial$edge))
      state(initial)
      loaded(project());sample_point(character());message("")
    },ignoreInit=FALSE,priority=200)
    mode<-gflowui_distinct_reactive(function()if(isTRUE(enabled()))state()$mode else "samples")
    shiny::observe({session$sendCustomMessage("gflowuiStateMode",list(mode=mode(),enabled=isTRUE(enabled())))})
    pending<-shiny::debounce(shiny::reactive(list(project=project(),state=state())),600)
    shiny::observeEvent(pending(),{
      p<-pending();if(!isTRUE(enabled()) || !identical(p$project,loaded()))return()
      if(!identical(p$state,gflowui_state_graph_default(manifest()$defaults$state_graphs)))
        tryCatch(save(p$state),error=function(e)message(conditionMessage(e)))
    },ignoreInit=FALSE)
    for(k in c("mode","coverage","adjacency","layout","color","width","scope"))local({
      field<-k
      shiny::observeEvent(input[[field]],{
        v<-input[[field]];if(length(v)==1L && nzchar(v))do.call(change,setNames(list(v),field))
      },ignoreInit=TRUE)
    })
    for(k in c("minimum","support"))local({field<-k
      shiny::observeEvent(input[[field]],{
        v<-input[[field]];if(length(v)==1L && is.finite(v) && v>=1 && v==floor(v))
          do.call(change,setNames(list(as.integer(v)),field))
      },ignoreInit=TRUE)
    })
    for(k in c("size","faces"))local({field<-k
      shiny::observeEvent(input[[field]],do.call(change,setNames(list(isTRUE(input[[field]])),field)),ignoreInit=TRUE)
    })
    shiny::observeEvent(input$selected,{
      if(!isTRUE(enabled()))return()
      value<-as.character(input$selected)
      # Ignore the empty HTML control until it has received the saved selection.
      if(!selection_ready()) {
        if(identical(value,state()$selected))selection_ready(TRUE)
        return()
      }
      change(selected=intersect(value,asset()$template$nodes$ID))
    },ignoreNULL=FALSE,ignoreInit=TRUE)
    shiny::observeEvent(input$edge,{
      value<-as.character(input$edge %||% "")
      if(!edge_ready()) {
        if(identical(value,state()$edge))edge_ready(TRUE)
        return()
      }
      change(edge=value)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$component,change(component=input$component),ignoreInit=TRUE)
    spec<-gflowui_distinct_reactive(function(){s<-state();list(project=project(),coverage=s$coverage,
      minimum=s$minimum,adjacency=s$adjacency,support=s$support)})
    graph<-shiny::reactive({shiny::req(enabled());s<-spec()
      gflowui_state_graph_build(asset(),s$coverage,s$minimum,s$adjacency,s$support)})
    cache_dir<-function(){p<-manifest()$metadata$state_graphs$cache_dir
      if(!grepl("^(/|[A-Za-z]:)",p))p<-file.path(manifest()$project_root,p);p}
    fit<-shiny::reactive({
      revision();g<-graph();known<-asset()$fits[[g$identity]]
      if(!is.null(known))return(known)
      p<-file.path(cache_dir(),paste0(g$identity,".rds"))
      if(file.exists(p)){x<-readRDS(p);if(identical(x$identity,g$identity))return(x$fit)}
      NULL
    })
    layout_choice<-gflowui_distinct_reactive(function()state()$layout)
    positions<-shiny::reactive({
      g<-graph();s<-spec();s$layout<-layout_choice()
      if(s$layout=="fit") {f<-fit();if(!is.null(f))return(list(coords=f$coords,
        id=paste0(g$identity,"-fit"),label="grip::metric.mds; unit shortest paths within components; 500 passes, three starts",error=f$error))}
      key<-if(!is.null(reference_ids()))"100" else if(s$coverage %in% names(asset()$references))s$coverage else "100"
      r<-asset()$references[[key]]
      list(coords=r$coords[match(g$graph$nodes$ID,r$ids),,drop=FALSE],
        id=paste0("reference-",key),label=paste("Baseline",key,"% reference:",r$label),error=NA_real_)
    })
    shiny::observeEvent(input$fit,{
      if(!is.null(job)&&job$is_alive()){message("A state layout is already being fitted.");return()}
      if(!is.null(fit())){change(layout="fit");message("Reused the saved fit for this exact graph.");return()}
      if(!requireNamespace("callr",quietly=TRUE)||!requireNamespace("grip",quietly=TRUE)) {
        message("Fitting requires callr and grip.");return()}
      g<-graph();dir.create(cache_dir(),recursive=TRUE,showWarnings=FALSE)
      job_spec<<-list(identity=g$identity,graph=g$graph,file=file.path(cache_dir(),paste0(g$identity,".rds")),project=project())
      fun<-gflowui_state_graph_fit;environment(fun)<-baseenv()
      job<<-callr::r_bg(function(fun,g)fun(g),args=list(fun=fun,g=g$graph),supervise=TRUE)
      message("Fitting in the background with grip::metric.mds; the viewer remains available.")
    },ignoreInit=TRUE)
    shiny::observe({
      shiny::invalidateLater(1000,session)
      if(is.null(job)||job$is_alive())return()
      tryCatch({
        f<-job$get_result();stopifnot(is.matrix(f$coords),all(is.finite(f$coords)))
        x<-list(identity=job_spec$identity,graph=job_spec$graph,fit=f,
          layout_identity=digest::digest(list(job_spec$identity,f),algo="sha256"))
        gflowui_atlas_atomic(x,job_spec$file)
        revision(shiny::isolate(revision())+1L)
        if(identical(job_spec$project,shiny::isolate(project()))) {
          message("Fit saved. Fixed passes do not establish convergence; disconnected component placement is arbitrary.")
          if(identical(job_spec$identity,shiny::isolate(graph())$identity))change(layout="fit")
        }
      },error=function(e)message(paste("State fit failed:",conditionMessage(e))))
      job<<-NULL;job_spec<<-NULL
    })
    session$onSessionEnded(function(){if(!is.null(job)&&job$is_alive())job$kill()})
    edge<-shiny::reactive({g<-graph()$graph;e<-g$edges
      keys<-paste(e$pair.a,e$pair.b,sep="|");i<-match(state()$edge,keys)
      if(is.na(i))NULL else e[i,,drop=FALSE]})
    inspected<-shiny::reactive(gflowui_state_graph_members(graph()$graph,edge=edge(),scope=state()$scope))
    highlighted<-shiny::reactive({if(!isTRUE(enabled())||mode()=="samples")return(character())
      if(!is.null(edge()))inspected() else gflowui_state_graph_members(graph()$graph,state()$selected)})
    shiny::observeEvent(sample_click(),{
      if(!isTRUE(enabled())||mode()!="linked")return()
      i<-suppressWarnings(as.integer(sample_click()));v<-view()$vertex_ids
      if(length(i)==1L&&is.finite(i)&&i>=1&&i<=length(v))sample_point(v[i])
    },ignoreInit=TRUE)
    selected_samples<-shiny::reactive(unique(c(sample_selected(),sample_point())))
    shiny::observeEvent(input$pick,{
      e<-input$pick;if(!identical(e$graph,graph()$identity))return()
      key<-e$key
      if(startsWith(key,"state:")) {
        id<-substring(key,7);if(!id %in% graph()$graph$nodes$ID)return()
        old<-state()$selected
        change(selected=if(isTRUE(e$shift))if(id %in% old)setdiff(old,id) else union(old,id) else id,edge="")
      } else if(startsWith(key,"edge:"))change(edge=substring(key,6))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$camera,{
      e<-input$camera;if(!identical(e$layout,positions()$id)||!is.list(e$camera))return()
      c<-state()$cameras;c[[e$layout]]<-e$camera;change(cameras=c)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$members,{
      ids<-gflowui_state_graph_members(graph()$graph,state()$selected)
      change(sample_filter=ids,mode="linked")
    },ignoreInit=TRUE)
    shiny::observeEvent(input$witness,change(sample_filter=inspected(),mode="linked"),ignoreInit=TRUE)
    shiny::observeEvent(input$clear_filter,change(sample_filter=NULL),ignoreInit=TRUE)
    shiny::observeEvent(input$clear, {change(selected=character(),edge="");sample_point(character())},ignoreInit=TRUE)
    shiny::observeEvent(input$region,{
      tryCatch({ids<-inspected();if(!length(ids))stop("This witness scope has no members.")
        open_region(paste("State witness",state()$scope,edge()$triple),ids,
          list(type="state_witness",graph_id=graph()$identity,scope=state()$scope,
            reference=asset()$reference,pair.a=edge()$pair.a,pair.b=edge()$pair.b,triple=edge()$triple))
        change(mode="samples");message("Region saved and opened as a parent preview; membership uses the full reference.")
      },error=function(e)message(conditionMessage(e)))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$save_variant,{
      label<-trimws(input$variant_name %||% "");if(!nzchar(label)){message("Enter a variant name.");return()}
      g<-graph();s<-state();z<-s[c("coverage","minimum","adjacency","support","layout","reference_ids")]
      z$graph_id<-g$identity;z$layout_id<-positions()$id
      entries<-s$names;entries[[label]]<-z;change(names=entries)
      message("Named definition saved; matching graph/layout assets are reused.")
    },ignoreInit=TRUE)
    shiny::observeEvent(input$load_variant,{
      z<-state()$names[[input$variant]];if(is.null(z))return()
      z["reference_ids"]<-list(z$reference_ids)
      do.call(change,z[c("coverage","minimum","adjacency","support","layout","reference_ids")])
    },ignoreInit=TRUE)
    # Separate selectors from selection/camera updates to avoid rebuilding controls during drags.
    controls_state<-gflowui_distinct_reactive(function(){s<-state();s[c("mode","coverage","minimum","adjacency","support","layout","color","size","width","faces","names")]})
    output$controls<-shiny::renderUI({
      if(!isTRUE(enabled()))return(NULL);project();s<-shiny::isolate(state());ns<-session$ns
      shiny::tagList(shiny::selectInput(ns("mode"),"View",c("Samples"="samples","2-udCST states"="states","Linked samples and states"="linked"),s$mode),
        shiny::uiOutput(ns("filter_status")),
        shiny::conditionalPanel(sprintf("input['%s'] !== 'samples'",ns("mode")),
          shiny::hr(),shiny::h6("Pair-state graph"),
          shiny::actionButton(ns("recompute_reference"),"Recompute on visible sample set",class="btn-sm"),
          shiny::actionButton(ns("full_reference"),"Use full reference",class="btn-sm"),
          shiny::p(class="gf-hint","Recompute freezes the currently visible sample IDs and recalculates frequencies and witnesses. Ordinary display filters leave this reference unchanged."),
          shiny::selectInput(ns("coverage"),"State coverage",c("60%"="60","70%"="70","80%"="80","90%"="90","All observed pair states"="100","Minimum state size"="minimum"),s$coverage),
          shiny::conditionalPanel(sprintf("input['%s'] === 'minimum'",ns("coverage")),shiny::numericInput(ns("minimum"),"Minimum compositions per state",s$minimum,min=1,step=1)),
          shiny::selectInput(ns("adjacency"),"Adjacency",c("Observed triple support"="observed_triple","Shared feature"="shared_feature"),s$adjacency),
          shiny::conditionalPanel(sprintf("input['%s'] === 'observed_triple'",ns("adjacency")),shiny::numericInput(ns("support"),"Minimum per-side support (1, 5, 10, or another integer)",s$support,min=1,step=1)),
          shiny::selectInput(ns("layout"),"State layout",c("Keep baseline SGD positions"="reference","Fit the current graph"="fit"),s$layout),
          shiny::actionButton(ns("fit"),"Fit / reuse current graph",class="btn-sm"),
          shiny::p(class="gf-hint","Baseline positions come from the saved support-1 SGD reference graph (the all-state reference for custom subsets). Changing edges keeps those positions until you choose Fit / reuse current graph. For the 16 sample cores and their three embedding routes, use Local views → Region family → Precomputed cores."),
          shiny::uiOutput(ns("metadata")),
          shiny::selectInput(ns("color"),"State color",c("State palette"="state","Frequency"="frequency"),s$color),
          shiny::checkboxInput(ns("size"),"Size nodes by frequency",s$size),
          shiny::selectInput(ns("width"),"Edge width",c("Constant"="constant","Per-side support"="support"),s$width),
          shiny::checkboxInput(ns("faces"),"Show witnessed faces",s$faces),
          shiny::uiOutput(ns("component_ui")),
          shiny::textInput(ns("variant_name"),"Name this variant"),
          shiny::actionButton(ns("save_variant"),"Save variant",class="btn-sm"),
          shiny::uiOutput(ns("variants")),
          shiny::uiOutput(ns("selection")),shiny::uiOutput(ns("inspector")),shiny::textOutput(ns("status"))))
    })
    output$variants<-shiny::renderUI({
      entries<-controls_state()$names;if(!length(entries))return(NULL)
      shiny::tagList(shiny::selectInput(session$ns("variant"),"Saved variant",names(entries)),
        shiny::actionButton(session$ns("load_variant"),"Load variant",class="btn-sm"))
    })
    shiny::observe({
      shiny::req(enabled());s<-controls_state()
      for(k in c("mode","coverage","adjacency","layout","color","width"))
        if(!identical(shiny::isolate(input[[k]]),s[[k]]))shiny::updateSelectInput(session,k,selected=s[[k]])
      for(k in c("minimum","support"))
        if(!identical(as.numeric(shiny::isolate(input[[k]])),as.numeric(s[[k]])))shiny::updateNumericInput(session,k,value=s[[k]])
      for(k in c("size","faces"))
        if(!identical(shiny::isolate(input[[k]]),s[[k]]))shiny::updateCheckboxInput(session,k,value=s[[k]])
    })
    filter_state<-gflowui_distinct_reactive(function()state()$sample_filter)
    output$filter_status<-shiny::renderUI({
      ids<-filter_state();if(is.null(ids))return(NULL)
      shiny::tagList(shiny::p(class="gf-hint",sprintf("State/witness sample filter active: %d reference IDs, intersected with other sample filters.",length(ids))),
        shiny::actionButton(session$ns("clear_filter"),"Clear state sample filter",class="btn-sm"))
    })
    output$status<-shiny::renderText(message())
    output$metadata<-shiny::renderUI({g<-graph();m<-g$graph$metadata;p<-positions()
      shiny::tagList(shiny::p(class="gf-hint",sprintf("Reference: %s. %d states; %d edges; %d components; %d isolates. %d / %d compositions (%.2f%%).",
        asset()$reference$label,nrow(g$graph$nodes),nrow(g$graph$edges),m$components,m$isolates,m$selected.n,m$reference.n,m$coverage)),
        shiny::p(class="gf-hint",p$label,". All edge lengths = 1; support and colors do not change path distances. Disconnected component placement is arbitrary."),
        if(state()$layout=="fit"&&is.null(fit()))shiny::p("No saved fit for this graph yet. Showing reference positions; use Fit / reuse current graph."),
        shiny::tags$details(shiny::tags$summary("Graph and layout identity"),shiny::p(g$identity),shiny::p(p$id)))})
    output$component_ui<-shiny::renderUI({g<-graph()$graph
      if(g$metadata$components>1)shiny::selectInput(session$ns("component"),"State component",
        c("All components"="all",setNames(as.character(sort(unique(g$nodes$component))),paste("Component",sort(unique(g$nodes$component))))),state()$component)})
    output$selection<-shiny::renderUI({shiny::req(enabled());ns<-session$ns
      shiny::tagList(shiny::hr(),shiny::h6("State/sample selection"),
        shiny::selectizeInput(ns("selected"),"Selected states",choices=NULL,multiple=TRUE),
        shiny::tags$script(shiny::HTML(sprintf("setTimeout(function(){Shiny.setInputValue('%s',Date.now(),{priority:'event'});},0);",ns("options_ready")))),
        shiny::actionButton(ns("members"),"Show member samples",class="btn-sm"),
        shiny::actionButton(ns("clear"),"Clear highlights",class="btn-sm"),
        shiny::textOutput(ns("selection_counts")),
        shiny::selectizeInput(ns("edge"),"Inspect edge / witness",choices=NULL),
        shiny::p(class="gf-hint","Click a node (Shift-click adds/removes); click an edge midpoint to inspect its witness. Cameras are independent. Sample endpoints and arms remain sample objects."))})
    selection_state<-gflowui_distinct_reactive(function()state()[c("selected","edge")])
    shiny::observeEvent(list(if(isTRUE(enabled()))graph()$identity else NULL,input$options_ready),{
      shiny::req(enabled(),input$options_ready);g<-graph()$graph;s<-state()
      shiny::updateSelectizeInput(session,"selected",choices=setNames(asset()$template$nodes$ID,paste(asset()$template$nodes$label,"—",asset()$template$nodes$freq)),selected=s$selected,server=TRUE)
      e<-g$edges;keys<-paste(e$pair.a,e$pair.b,sep="|")
      labs<-if(nrow(e))paste(g$nodes$label[e$from],"↔",g$nodes$label[e$to]) else character()
      shiny::updateSelectizeInput(session,"edge",choices=c("Choose an edge"="",setNames(keys,labs)),selected=if(s$edge %in% keys)s$edge else "",server=TRUE)
    },ignoreInit=FALSE)
    shiny::observeEvent(selection_state(),{
      shiny::req(enabled(),input$options_ready);s<-selection_state()
      if(!identical(as.character(input$selected),s$selected))shiny::updateSelectInput(session,"selected",selected=s$selected)
      if(!identical(input$edge,s$edge))shiny::updateSelectInput(session,"edge",selected=s$edge)
    },ignoreInit=FALSE)
    visible_ids<-shiny::reactive({v<-view();v$vertex_ids[gflowui_visible_indices(visible(),length(v$vertex_ids))]})
    shiny::observeEvent(input$recompute_reference,{
      tryCatch({ids<-visible_ids();gflowui_state_graph_reference(base_asset(),ids)
        change(reference_ids=ids,selected=character(),edge="",component="all",layout="reference")
        selection_ready(TRUE);edge_ready(TRUE)
        message("New reference saved from visible sample IDs. Frequencies and witnesses were recomputed; fixed positions remain the parent reference until a new fit is requested.")
      },error=function(e)message(conditionMessage(e)))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$full_reference,{
      change(reference_ids=NULL,selected=character(),edge="",component="all",layout="reference")
      selection_ready(TRUE);edge_ready(TRUE);message("Full reference restored. Sample filters remain independently clearable.")
    },ignoreInit=TRUE)
    output$selection_counts<-shiny::renderText({
      ids<-gflowui_state_graph_members(graph()$graph,state()$selected)
      ss<-selected_samples();pairs<-graph()$graph$membership$pair[match(ss,graph()$graph$membership$sample.id)]
      sprintf("Selected states: %d reference members, %d currently visible. Selected samples: %d; %d states present in this graph. State sample filter: %s.",
        length(ids),sum(ids %in% visible_ids()),length(ss),length(intersect(unique(pairs),graph()$graph$nodes$ID)),
        if(is.null(state()$sample_filter))"inactive" else paste(length(state()$sample_filter),"reference IDs (intersects other filters)"))})
    output$inspector<-shiny::renderUI({e<-edge();if(is.null(e))return(NULL);ns<-session$ns
      fields<-c("n.a","n.b","n.triple","i.a","i.b","min.support","jaccard.a","jaccard.b","jaccard.mean","jaccard.min","jaccard.geometric","pair.fraction.a","pair.fraction.b","triple.fraction.a","triple.fraction.b","length")
      rows<-lapply(fields,function(k)shiny::tags$tr(shiny::tags$td(k),shiny::tags$td(format(e[[k]],digits=5))))
      ids<-inspected();records<-gflowui_source_asset(manifest())$records
      cohorts<-if(is.null(records))NULL else gflowui_source_summary(records,ids)
      shiny::tagList(shiny::h6("Witness inspector"),shiny::p(paste("Endpoint feature sets:",e$pair.a,"and",e$pair.b,"; triple:",e$triple)),
        shiny::p(class="gf-hint","n.a / n.b: full pair frequencies; n.triple: full triple frequency; i.a / i.b: pair–triple intersections; min.support: smaller intersection. Fractions use the named pair or triple denominator."),
        shiny::tags$table(class="table table-sm",shiny::tags$tbody(rows)),
        shiny::selectInput(ns("scope"),"Witness sample scope",c("Both endpoint intersections"="witnesses","Endpoint A intersection"="side_a","Endpoint B intersection"="side_b","Third pair configuration"="third","Whole triple"="triple","Endpoint union"="endpoints"),state()$scope),
        shiny::p(sprintf("%d full-reference samples; %d currently visible. Whole-triple and endpoint scopes can include samples outside the coverage core.",length(ids),sum(ids %in% visible_ids()))),
        shiny::actionButton(ns("witness"),"Inspect witness samples",class="btn-sm"),
        shiny::actionButton(ns("region"),"Open local region",class="btn-sm"),
        if(!is.null(cohorts)&&nrow(cohorts))shiny::tagList(shiny::h6("Source cohorts for this scope"),
          shiny::tags$table(class="table table-sm",shiny::tags$thead(shiny::tags$tr(lapply(c("Dataset","Compositions","Records"),shiny::tags$th))),
            shiny::tags$tbody(lapply(seq_len(nrow(cohorts)),function(i)shiny::tags$tr(lapply(cohorts[i,],shiny::tags$td)))))))})
    plot_state<-gflowui_distinct_reactive(function(){s<-state();s[c("component","color","size","width","faces","selected")]})
    output$plot<-plotly::renderPlotly({
      shiny::req(enabled(),mode()!="samples");g<-graph()$graph;s<-plot_state();pos<-positions();Z<-pos$coords
      n<-g$nodes;keep<-if(s$component=="all"||!s$component %in% as.character(n$component))seq_len(nrow(n)) else which(as.character(n$component)==s$component)
      e<-g$edges;ee<-which(e$from %in% keep & e$to %in% keep)
      p<-plotly::plot_ly(source=session$ns("state_source"))
      # A few width bins keep the full shared-feature graph usable.
      widths<-if(s$width=="support")pmin(6L,1L+floor(log10(1+e$min.support)*2)) else rep(1L,nrow(e))
      for(w in unique(widths[ee])) {
        ix<-ee[widths[ee]==w];a<-Z[e$from[ix],,drop=FALSE];b<-Z[e$to[ix],,drop=FALSE]
        lines<-function(j)as.vector(rbind(a[,j],b[,j],NA_real_))
        p<-plotly::add_trace(p,x=lines(1),y=lines(2),z=lines(3),type="scatter3d",mode="lines",inherit=FALSE,
          line=list(width=w,color="#adb5bd"),hoverinfo="skip",showlegend=FALSE)
      }
      if(length(ee)) {
        mid<-(Z[e$from[ee],,drop=FALSE]+Z[e$to[ee],,drop=FALSE])/2
        p<-plotly::add_trace(p,x=mid[,1],y=mid[,2],z=mid[,3],type="scatter3d",mode="markers",inherit=FALSE,
          marker=list(size=3,color="#718096",opacity=.5),customdata=paste0("edge:",e$pair.a[ee],"|",e$pair.b[ee]),
          text=paste(n$label[e$from[ee]],"↔",n$label[e$to[ee]],"<br>Per-side support:",e$min.support[ee]),hoverinfo="text",showlegend=FALSE)
      }
      f<-g$faces;f<-f[f$a %in% keep & f$b %in% keep & f$c %in% keep,,drop=FALSE]
      if(s$faces&&nrow(f))p<-plotly::add_trace(p,x=Z[,1],y=Z[,2],z=Z[,3],i=f$a-1L,j=f$b-1L,k=f$c-1L,
        type="mesh3d",inherit=FALSE,color=I("#69b3a2"),opacity=.14,hoverinfo="skip",showlegend=FALSE)
      a<-gflowui_classification_asset(manifest());palette<-a$palettes$udcst_level2
      overrides<-manifest()$metadata$classification_catalogue$palettes$udcst_level2
      if(length(overrides))palette[names(overrides)]<-overrides
      labels<-asset()$palette_keys[match(n$ID,names(asset()$palette_keys))]
      colors<-unname(palette[labels]);colors[is.na(colors)]<-"#438b82"
      if(s$color=="frequency")colors<-grDevices::hcl.colors(100,"Viridis")[pmax(1,ceiling(100*log1p(n$freq)/max(log1p(n$freq),1)))]
      vis<-table(factor(g$membership$pair[g$membership$sample.id %in% visible_ids()],levels=n$ID))
      sel<-table(factor(g$membership$pair[g$membership$sample.id %in% selected_samples()],levels=n$ID))
      sizes<-if(s$size)5+15*sqrt(n$freq/max(n$freq,1)) else rep(8,nrow(n))
      if(!length(keep))p<-plotly::add_trace(p,x=numeric(),y=numeric(),z=numeric(),type="scatter3d",mode="markers",inherit=FALSE,showlegend=FALSE)
      if(length(keep))p<-plotly::add_trace(p,x=Z[keep,1],y=Z[keep,2],z=Z[keep,3],type="scatter3d",mode="markers",inherit=FALSE,
        marker=list(size=sizes[keep],color=colors[keep]),customdata=paste0("state:",n$ID[keep]),
        text=paste(n$label[keep],"<br>State:",n$ID[keep],"<br>Full reference:",n$freq[keep],"; visible:",as.integer(vis)[keep],"; selected samples:",as.integer(sel)[keep]),hoverinfo="text",showlegend=FALSE)
      hi<-intersect(keep,which(n$ID %in% s$selected | as.integer(sel)>0))
      if(length(hi))p<-plotly::add_trace(p,x=Z[hi,1],y=Z[hi,2],z=Z[hi,3],type="scatter3d",mode="markers",inherit=FALSE,
        marker=list(size=sizes[hi]+5,color="#f97316",symbol="circle-open"),customdata=paste0("state:",n$ID[hi]),hoverinfo="skip",showlegend=FALSE)
      sc<-list(xaxis=list(visible=FALSE),yaxis=list(visible=FALSE),zaxis=list(visible=FALSE),aspectmode="data")
      cam<-shiny::isolate(state()$cameras[[pos$id]]);if(!is.null(cam))sc$camera<-cam
      p<-plotly::layout(p,scene=sc,uirevision=paste(project(),pos$id),margin=list(l=0,r=0,b=0,t=35),
        title=if(length(keep))"2-udCST states" else "No states meet this definition")
      p<-htmlwidgets::onRender(p,"function(el,x,d){window.gflowuiStateMount(el,d);}",
        data=list(input=session$ns("pick"),camera=session$ns("camera"),graph=graph()$identity,layout=pos$id,nodes=n$ID[keep],edges=length(ee),visible_members=sum(as.integer(vis)[keep])))
      p
    })
    list(enabled=enabled,mode=mode,controls=function()shiny::uiOutput(session$ns("controls")),
      highlight=highlighted,state=state,graph=graph,
      filter=function(st,idx){if(!isTRUE(enabled())||is.null(state()$sample_filter))idx else intersect(idx,which(st$vertex_ids %in% state()$sample_filter))})
  })
}
