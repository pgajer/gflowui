# Each renderer depends only on its own controls and the selected saved assets.
gflowui_ec_quality_outputs <- function(input,output,session,index,cohort,current,run_id,active_label,graph_id) {
  run_label <- function(row)paste(row$method,row$settings,paste0("seed ",row$seed),row$evaluation,sep=" | ")
  output$evaluation_note <- shiny::renderText({
    rows<-cohort();modes<-unique(rows$evaluation[rows$status=="completed"])
    if("uniform_pair_sample" %in% modes)"Expanded distance scores use shared uniform pairs, up to 20,000 per component. Brackets/bars are approximate 95% conditional sampling intervals, not seed ranges. Edge and neighborhood scores remain exact."
    else "Pilot scores use all within-component pairs. Edge and neighborhood scores are exact. Replicate spread describes optimizer variability, not a confidence interval."
  })
  metric_key <- shiny::reactive({
    key <- input$metric
    if(is.null(key) || !key %in% names(gflowui_ec_metrics()))"chord_error" else key
  })
  output$metric_plot <- plotly::renderPlotly({
    tbl <- cohort();key <- metric_key()
    tbl <- tbl[tbl$status=="completed" & is.finite(tbl[[key]]),,drop=FALSE]
    shiny::validate(shiny::need(nrow(tbl)>0,"This metric is unavailable for the selected graph."))
    active_value <- tbl[[key]][match(run_id(),tbl$id)]
    p <- plotly::plot_ly(tbl,x=tbl[[key]],y=~method,type="scatter",mode="markers",customdata=~id,
      text=run_label(tbl),hovertemplate="%{text}<br>%{x:.6g}<extra></extra>",marker=list(color="#3575B2",size=9),showlegend=FALSE)
    lo<-tbl[[paste0(key,"_lower")]];hi<-tbl[[paste0(key,"_upper")]];valid<-is.finite(lo)&is.finite(hi)
    if(any(valid))p<-plotly::add_trace(p,x=as.vector(rbind(lo[valid],hi[valid],NA_real_)),
      y=rep(tbl$method[valid],each=3),type="scatter",mode="lines",line=list(color="#3575B2",width=1),
      hoverinfo="skip",inherit=FALSE,showlegend=FALSE)
    shapes <- if(is.finite(active_value))list(list(type="line",yref="paper",y0=0,y1=1,x0=active_value,x1=active_value,line=list(color="#737A80",dash="dash"))) else list()
    p <- plotly::layout(p,title=list(text=gflowui_ec_metrics()[key],font=list(size=15)),shapes=shapes,
      xaxis=list(title="Score"),yaxis=list(title="",automargin=TRUE,
        categoryorder="array",categoryarray=rev(unique(tbl$method)),
        tickvals=unique(tbl$method),ticktext=vapply(unique(tbl$method),function(x)paste(strwrap(x,21),collapse="<br>"),"")),
      margin=list(l=150,r=15,b=55,t=50))
    gflowui_ec_plot_events(p,session$ns("choose_run"))
  })
  output$metric_note <- shiny::renderText({
    key <- metric_key();tbl <- cohort();value <- tbl[[key]][match(run_id(),tbl$id)]
    if(is.finite(value))paste("Dashed reference:",active_label(),"=",format(value,digits=6)) else "The active run has no value for this metric; no reference line is shown."
  })
  output$trade_plot <- plotly::renderPlotly({
    x <- gflowui_ec_text(input$trade_x,"chord_error");y <- gflowui_ec_text(input$trade_y,"path_error")
    shiny::req(x %in% names(gflowui_ec_metrics()),y %in% names(gflowui_ec_metrics()))
    tbl <- cohort();tbl <- tbl[tbl$status=="completed" & is.finite(tbl[[x]]) & is.finite(tbl[[y]]),,drop=FALSE]
    shiny::validate(shiny::need(nrow(tbl)>0,"No runs have both selected scores."))
    p <- plotly::plot_ly(tbl,x=tbl[[x]],y=tbl[[y]],customdata=~id,text=run_label(tbl),type="scatter",mode="markers",
      marker=list(color="#3575B2",size=9),hovertemplate="%{text}<br>x=%{x:.6g}<br>y=%{y:.6g}<extra></extra>",showlegend=FALSE)
    i <- match(run_id(),tbl$id);shapes <- list()
    if(!is.na(i)) {
      p <- plotly::add_trace(p,x=tbl[[x]][i],y=tbl[[y]][i],customdata=tbl$id[i],text=run_label(tbl[i,,drop=FALSE]),
        type="scatter",mode="markers",marker=list(color="#39434A",size=16,symbol="circle-open"),inherit=FALSE,showlegend=FALSE)
      shapes <- list(list(type="line",xref="paper",x0=0,x1=1,y0=tbl[[y]][i],y1=tbl[[y]][i],line=list(color="#737A80",dash="dash")),
        list(type="line",yref="paper",y0=0,y1=1,x0=tbl[[x]][i],x1=tbl[[x]][i],line=list(color="#737A80",dash="dash")))
    }
    p <- plotly::layout(p,title=list(text="Embedding quality trade-offs",font=list(size=15)),shapes=shapes,
      xaxis=list(title=gflowui_ec_metrics()[x]),yaxis=list(title=gflowui_ec_metrics()[y]),margin=list(l=85,b=70,t=50,r=15))
    gflowui_ec_plot_events(p,session$ns("choose_run"))
  })
  output$neighborhood_plot <- plotly::renderPlotly({
    kind <- gflowui_ec_text(input$neighborhood,"trustworthiness");shiny::req(kind %in% c("trustworthiness","continuity"))
    tbl <- cohort();tbl <- tbl[tbl$status=="completed",,drop=FALSE]
    shiny::validate(shiny::need(nrow(tbl)>0,"No completed runs have neighborhood diagnostics."))
    p <- plotly::plot_ly()
    order <- c(which(tbl$id!=run_id()),which(tbl$id==run_id()))
    for(i in order) {
      vals <- vapply(c(5,10,20,50),function(k)gflowui_ec_number(index()$results[[tbl$id[i]]]$summary$neighborhood[[paste0(kind,"_",k)]]),0.)
      selected <- tbl$id[i]==run_id()
      p <- plotly::add_trace(p,x=c(5,10,20,50),y=vals,type="scatter",mode="lines+markers",name=paste(strwrap(run_label(tbl[i,,drop=FALSE]),46),collapse="<br>"),
        line=list(color=if(selected)"#39434A" else "#BFC8CF",dash=if(selected)"dash" else "solid",width=if(selected)2 else 1),
        marker=list(size=if(selected)7 else 4),showlegend=selected,connectgaps=FALSE)
    }
    plotly::layout(p,title=list(text=paste(kind,"by neighborhood size"),font=list(size=15)),
      xaxis=list(title="Neighbors (k)"),yaxis=list(title="Preservation (higher is better)",range=c(0,1)),
      legend=list(orientation="h",x=0,y=-.3),margin=list(l=65,r=15,b=110,t=50))
  })
  output$sensitivity <- shiny::renderUI({
    idx <- index();items <- list(shiny::p("Seed ranges are observed minima/maxima, not confidence intervals. Reversed ties change only ID priority, not the layout; close rankings may depend on this choice."))
    table <- gflowui_ec_seed_ranges(idx$table, graph_id())
    items <- c(items,list(shiny::h5("Euclidean error across available seeds"),
      shiny::p("Settings are grouped separately. Available counts include only completed, finite scores. Listed counts include unavailable entries, which are not attempted jobs."),
      gflowui_ec_html_table(table)))
    for (artifact in c("tie_sensitivity.json", "phase03_tie_sensitivity.json", "phase03_trimap_graph_ties.json")) if(!is.null(idx$artifacts[[artifact]])) {
      rows <- gflowui_ec_json(gflowui_ec_asset(idx$root,idx$artifacts[[artifact]]))$runs
      rows <- Filter(function(r)identical(r$graph_id,graph_id()),rows)
      table <- do.call(rbind,lapply(rows,function(r)data.frame(Method=unname(gflowui_ec_methods()[r$method]),Seed=r$seed,
        "Continuity (k=20)"=gflowui_ec_number(r$original$continuity_20),"Reversed ties"=gflowui_ec_number(r$reversed_id_priority$continuity_20),
        "Maximum change"=gflowui_ec_number(r$max_absolute_change),check.names=FALSE)))
      items <- c(items,list(shiny::p("Maximum change is the largest absolute difference across trustworthiness and continuity at all evaluated neighborhood sizes. One-seed results do not establish seed stability.")))
      title <- if (identical(artifact,"tie_sensitivity.json")) "Baseline tie sensitivity: seed 17" else "Additional-method tie sensitivity: seed 17"
      items <- c(items,list(shiny::h5(title),gflowui_ec_html_table(table)))
    }
    shiny::tagList(items)
  })
  lgs_summary <- shiny::reactive({
    idx <- index();spec <- idx$artifacts[["phase04_lgs_summary.json"]]
    shiny::validate(shiny::need(!is.null(spec),"LGS integration results are not available in this saved project."))
    gflowui_ec_json(gflowui_ec_asset(idx$root,spec))
  })
  output$lgs_note <- shiny::renderUI({
    data <- lgs_summary();rows <- Filter(function(r)identical(r$graph_id,graph_id()),data$gallery_rows)
    status <- if(length(rows)) unique(vapply(rows,function(r)gflowui_ec_text(r$reason,r$status),"")) else "No gallery admission record."
    shiny::tagList(shiny::p("The experimental paper-form LGS kernel differs from the original 2D algorithm. Pairwise descent does not guarantee full-objective descent; some attractive-constraint graphs give an unbounded-below objective."),
      shiny::p(shiny::strong("Selected gallery graph: "),paste(status,collapse="; ")),
      shiny::p("The plot below uses separate synthetic validation graphs, not the selected SuiteSparse graph. Points are means across available seeds; bars show observed minima/maxima, not confidence intervals. Locality changes the objective. All scores use original graph targets."))
  })
  output$lgs_plot <- plotly::renderPlotly({
    data <- lgs_summary();fixture <- gflowui_ec_text(input$lgs_fixture,"validation_path48")
    metric <- gflowui_ec_text(input$lgs_metric,"chord_error")
    shiny::req(metric %in% names(gflowui_ec_metrics()))
    rows <- Filter(function(r)identical(r$graph_id,fixture) && identical(r$status,"completed"),data$validation_rows)
    shiny::validate(shiny::need(length(rows)>0,"No completed synthetic locality runs."))
    values <- data.frame(k=vapply(rows,function(r)gflowui_ec_number(r$locality$component_k[[1L]]),0.),
      fraction=vapply(rows,function(r)gflowui_ec_number(r$locality$component_fraction[[1L]]),0.),
      score=vapply(rows,function(r)gflowui_ec_number(r$scores[[metric]]),0.))
    values <- values[is.finite(values$score),,drop=FALSE]
    shiny::validate(shiny::need(nrow(values)>0,"This synthetic metric is unavailable."))
    groups <- split(values,values$k)
    tbl <- do.call(rbind,lapply(groups,function(r)data.frame(k=r$k[1],fraction=r$fraction[1],
      mean=mean(r$score),minimum=min(r$score),maximum=max(r$score),n=nrow(r))))
    tbl <- tbl[order(tbl$fraction),,drop=FALSE]
    p <- plotly::plot_ly(tbl,x=~fraction,y=~mean,type="scatter",mode="lines+markers",showlegend=FALSE,
      text=paste0("k=",tbl$k,"; ",tbl$n," seeds"),
      hovertemplate="%{text}<br>fraction=%{x:.3g}<br>mean=%{y:.6g}<extra></extra>",
      error_y=list(type="data",symmetric=FALSE,array=tbl$maximum-tbl$mean,arrayminus=tbl$mean-tbl$minimum),
      marker=list(color="#3575B2",size=9),line=list(color="#3575B2"))
    plotly::layout(p,title=list(text="Synthetic LGS locality experiment",font=list(size=15)),
      xaxis=list(title="k / (component size − 1)",range=c(0,1.05)),
      yaxis=list(title=gflowui_ec_metrics()[metric]),margin=list(l=80,b=65,t=50,r=15))
  })
  output$shepard_plot <- plotly::renderPlotly({
    rows <- current()$diagnostics$shepard;shiny::validate(shiny::need(length(rows)>0,"No eligible diagnostic pairs."))
    d <- vapply(rows,function(x)x$original_distance,0.);r <- vapply(rows,function(x)x$fitted_chord,0.)
    p <- plotly::plot_ly(x=d,y=r,type="scattergl",mode="markers",marker=list(size=4,color="#3575B2",opacity=.4),showlegend=FALSE)
    plotly::layout(p,title=list(text="Original vs fitted Euclidean distances",font=list(size=15)),
      xaxis=list(title="Original graph distance"),yaxis=list(title="Scale-fitted chord distance"),margin=list(l=70,b=65,t=50,r=15),
      shapes=list(list(type="line",x0=0,y0=0,x1=max(d),y1=max(d),line=list(color="#737A80",dash="dash"))))
  })
  output$edge_plot <- plotly::renderPlotly({
    values <- unlist(current()$diagnostics$edge_residuals,use.names=FALSE)
    shiny::validate(shiny::need(length(values)>0,"No graph edges."))
    p <- plotly::plot_ly(x=values,type="histogram",nbinsx=80,marker=list(color="#3575B2",line=list(color="#39434A",width=.5)),showlegend=FALSE)
    plotly::layout(p,title=list(text="Edge-length residual distribution",font=list(size=15)),
      xaxis=list(title="Embedded length − unit target (identity scale)"),yaxis=list(title="Number of edges"),margin=list(l=65,b=70,t=50,r=15))
  })
  output$diagnostic_note <- shiny::renderText({
    d <- current()$diagnostics
    e<-d$evaluation;mode<-gflowui_ec_text(e$mode,"exact")
    sprintf("%s displayed pairs; %s. Metric evaluation: %s, %s of %s eligible pairs. Edge histogram includes every edge. %s.",
      length(d$shepard),d$sampling,mode,gflowui_ec_text(e$pair_count,as.character(d$exact_pair_count)),d$exact_pair_count,d$scale)
  })
  metric_key
}
