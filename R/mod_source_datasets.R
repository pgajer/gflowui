gflowui_source_datasets_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(
    shiny::p(class="gf-hint","Counts describe the current region before display filters. No checks shows all datasets. Dataset and dCST filters intersect."),
    shiny::actionButton(ns("clear_datasets"),"Show all datasets"),
    shiny::uiOutput(ns("table")),
    shiny::actionButton(ns("cross"),"Dataset × CST"),
    shiny::textOutput(ns("cross_note")),
    shiny::actionButton(ns("clear_cell"),"Clear cross-table cell filter"),
    shiny::p(class="gf-hint","Choose Source dataset under Color by to use these colors. Shared vertices have a Multiple source datasets color. Colors are saved project-wide."))
}

gflowui_within_dcst_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(shiny::checkboxInput(ns("show"),"Show linked 2D view",FALSE),
    shiny::selectInput(ns("coordinate_mode"),"2D coordinates",
      c("Relative abundance (t, r)"="abundance","Homogeneous (xB/xA, distance to axis)"="homogeneous")),
    shiny::selectInput(ns("color_mode"),"2D color by",
      c("CST (same type and level as Graphs)"="dcst","Source dataset"="dataset")),
    shiny::p(class="gf-hint","The CST checkboxes in Graphs control both plots. No checks shows all groups. Ordered level-2 dCSTs use their ordered pair (A, B); unordered pairs use fixed catalogue feature order, even when dominance reverses. Hover identifies A and B."),
    shiny::textOutput(ns("pair_note")),
    shiny::textOutput(ns("coordinate_note")),
    shiny::checkboxInput(ns("add"),"Add to selection / toggle clicked points",FALSE),
    shiny::checkboxInput(ns("only_selected"),"Show only linked selection in 3D",FALSE),
    shiny::actionButton(ns("clear_points"),"Clear linked selection"),
    shiny::textOutput(ns("selection_note")),
    shiny::p(class="gf-hint","Click a point in either display, or box/lasso-select in 2D. Orange rings mark the same composition IDs in both displays. Coordinates always use original relative abundances."),
    shiny::downloadButton(ns("download"),"Download 2D coordinates"))
}

gflowui_source_datasets_server <- function(id, manifest, view, visible, click3d,
    level, save_palette) {
  shiny::moduleServer(id,function(input,output,session) {
    asset <- shiny::reactive(gflowui_source_asset(manifest()))
    groups <- shiny::reactiveVal(character()); selected <- shiny::reactiveVal(character())
    cell <- shiny::reactiveVal(NULL)
    project_key <- gflowui_distinct_reactive(function() manifest()$project_id)
    shiny::observeEvent(project_key(),{
      groups(character());selected(character());cell(NULL)
      shiny::updateCheckboxInput(session,"show",value=FALSE)
      shiny::updateCheckboxInput(session,"only_selected",value=FALSE)
    },ignoreInit=FALSE)
    palette <- shiny::reactive({
      shiny::req(asset())
      gflowui_source_palette(asset()$records,manifest()$metadata$source_datasets$palette)
    })
    summary <- shiny::reactive({
      shiny::req(asset());gflowui_source_summary(asset()$records,view()$vertex_ids)
    })
    shiny::observeEvent(input$datasets,groups(intersect(as.character(input$datasets),unique(asset()$records$dataset))),ignoreNULL=FALSE,ignoreInit=TRUE)
    shiny::observeEvent(input$clear_datasets,groups(character()),ignoreInit=TRUE)
    shiny::observeEvent(input$clear_cell,cell(NULL),ignoreInit=TRUE)
    output$table <- shiny::renderUI({shiny::req(asset());gflowui_source_table_ui(summary(),palette(),groups(),session$ns)})
    shiny::observeEvent(input$color,{
      e<-input$color
      if(length(e$dataset)!=1L || !e$dataset %in% summary()$dataset ||
         length(e$color)!=1L || !grepl("^#[0-9a-fA-F]{6}$",e$color))return()
      p<-palette();p[e$dataset]<-e$color
      tryCatch(save_palette(p),error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$cross,{
      shiny::showModal(shiny::modalDialog(title="Source dataset × CST",size="l",easyClose=TRUE,
        shiny::selectInput(session$ns("cross_level"),"CST level",gflowui_dcst_levels(view()$sources,if(startsWith(level(),"udcst"))"udcst" else "dcst"),selected=level()),
        shiny::selectInput(session$ns("unit"),"Count",c("Source records"="records","Distinct vertices per dataset"="vertices")),
        shiny::selectInput(session$ns("display"),"Display",c("Counts"="count","% within each dataset"="row","% within each dCST"="column")),
        shiny::p(paste(if(is.null(manifest()$metadata$classification_catalogue))"All current-region members, before display filters." else "Currently visible compositions after all display filters.","Shared compositions count in each contributing dataset. Click a cell to filter both displays; this intersects existing filters.")),
        shiny::uiOutput(session$ns("cross_table")),
        shiny::downloadButton(session$ns("cross_download"),"Download table")))
    },ignoreInit=TRUE)
    cross_level <- shiny::reactive({
      valid<-unname(gflowui_dcst_levels(view()$sources,if(startsWith(level(),"udcst"))"udcst" else "dcst"))
      if(length(input$cross_level)==1L && input$cross_level %in% valid)input$cross_level else level()
    })
    cross <- shiny::reactive({
      shiny::req(asset())
      st<-view(); l<-cross_level()
      shiny::validate(shiny::need(length(st$sources[[l]]$values)==length(st$vertex_ids),"This view has no matching dCST annotations."))
      ii<-if(is.null(manifest()$metadata$classification_catalogue))seq_along(st$vertex_ids) else visible()
      gflowui_source_cross(asset()$records,st$vertex_ids[ii],st$sources[[l]]$values[ii],
        input$unit %||% "records",input$display %||% "count")
    })
    output$cross_table <- shiny::renderUI({
      m<-cross()
      js<-paste0("Shiny.setInputValue('",session$ns("cell"),"',{dataset:this.dataset.dataset,group:this.dataset.group,level:this.dataset.level},{priority:'event'});")
      shiny::div(style="max-height:55vh;overflow:auto",shiny::tags$table(class="table table-sm",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Dataset"),lapply(colnames(m),function(g)shiny::tags$th(style="min-width:140px",g)))),
        shiny::tags$tbody(lapply(seq_len(nrow(m)),function(i)shiny::tags$tr(shiny::tags$th(rownames(m)[i]),
          lapply(seq_len(ncol(m)),function(j)shiny::tags$td(shiny::tags$button(type="button",class="btn btn-sm btn-light",
            `data-dataset`=rownames(m)[i],`data-group`=colnames(m)[j],`data-level`=cross_level(),onclick=js,
            if(identical(input$display %||% "count","count"))as.character(m[i,j]) else sprintf("%.1f%%",m[i,j])))))))))
    })
    shiny::observeEvent(input$cell,{
      e<-input$cell;m<-cross()
      if(e$dataset %in% rownames(m) && e$group %in% colnames(m)) {cell(e);shiny::removeModal()}
    },ignoreInit=TRUE)
    output$cross_note<-shiny::renderText({e<-cell();if(is.null(e))return("");paste("Cell filter:",e$dataset,"/",e$group)})
    output$cross_download<-shiny::downloadHandler(
      filename=function()paste("dataset",cross_level(),input$unit %||% "records",paste0(input$display %||% "count",".csv"),sep="-"),
      content=function(path){m<-cross();utils::write.csv(data.frame(dataset=rownames(m),m,check.names=FALSE),path,row.names=FALSE)})
    pairs<-shiny::reactive({
      unordered<-startsWith(level(),"udcst")
      p<-if(unordered)gflowui_classification_asset(manifest())$pairs else asset()$pairs
      if(is.null(p))return(data.frame(group=character(),a=character(),b=character()))
      values<-view()$sources[[if(unordered)"udcst_level2" else "dcst_level2"]]$values
      p<-p[p$group %in% values,,drop=FALSE]
      counts<-vapply(p$group,function(g)sum(values==g,na.rm=TRUE),integer(1))
      p[order(-counts,p$group),,drop=FALSE]
    })
    axes <- shiny::reactive(gflowui_within_axes(input$coordinate_mode))
    coordinates<-shiny::reactive({
      st<-view();a<-gflowui_vertex_hover_asset(manifest());shiny::req(a)
      key<-if(startsWith(level(),"udcst"))"udcst_level2" else "dcst_level2"
      shiny::req(length(st$sources[[key]]$values)==length(st$vertex_ids))
      gflowui_dcst_coordinates(a,st$vertex_ids,st$sources[[key]]$values,pairs())
    })
    plot_rows<-shiny::reactive({
      x<-coordinates();ids<-view()$vertex_ids[visible()]
      x[x$vertex_id %in% ids,,drop=FALSE]
    })
    visible_coords<-shiny::reactive({
      x<-plot_rows();ax<-axes()
      x$plot_x<-x[[ax$x]];x$plot_y<-x[[ax$y]]
      x[is.finite(x$plot_x) & is.finite(x$plot_y),,drop=FALSE]
    })
    output$pair_note<-shiny::renderText({
      x<-plot_rows();shown<-visible_coords()
      undefined<-sum(is.na(x$phylotype_a))
      sprintf("%d compositions in %d level-2 CSTs shown. Of %d currently visible 3D vertices, %d lack an explicit pair; %d more have undefined coordinates.",
        nrow(shown),length(unique(shown$dcst)),nrow(x),undefined,nrow(x)-nrow(shown)-undefined)
    })
    output$coordinate_note<-shiny::renderText({
      if(identical(input$coordinate_mode,"homogeneous")) {
        n<-sum(!visible_coords()$a_dominant,na.rm=TRUE)
        paste0("Horizontal: xB/xA. Vertical: sqrt(sum of (xj/xA)^2 over j other than A and B). The pure pair is the horizontal axis. xA must be positive; no pseudocount is used.",
          if(n) sprintf(" %d plotted points lie outside the A-dominant face, but their ratio chart is defined.",n) else "")
      } else "Horizontal: t = xB/(xA+xB). Vertical: r = 1-xA-xB (other-phylotype abundance). The pair must have positive total abundance."
    })
    plot_colors<-shiny::reactive({
      x<-visible_coords();st<-view()
      if(identical(input$color_mode,"dataset")) {
        cats<-gflowui_source_categories(asset()$records,x$vertex_id);p<-palette()
      } else {
        l<-level();if(!l %in% names(st$sources))l<-"dcst_level2"
        cats<-as.character(st$sources[[l]]$values[match(x$vertex_id,st$vertex_ids)])
        cats[is.na(cats)|!nzchar(cats)]<-"Unclassified"
        p<-st$graph_set$color_assets$categorical_palettes[[l]]
        fixed<-gflowui_explicit_categorical_palette(cats,p)
        if(!is.null(fixed))p<-fixed$colors else {
          lev<-sort(unique(cats));p<-stats::setNames(grDevices::hcl.colors(length(lev),"Dark 3"),lev)
        }
      }
      list(categories=cats,colors=unname(p[cats]))
    })
    select_ids<-function(ids,toggle=FALSE) {
      ids<-unique(intersect(as.character(ids),view()$vertex_ids))
      if(isTRUE(input$add)) {
        old<-selected()
        selected(if(toggle)union(setdiff(old,ids),setdiff(ids,old)) else union(old,ids))
      } else selected(ids)
    }
    shiny::observeEvent(input$clear_points,selected(character()),ignoreInit=TRUE)
    shiny::observeEvent(click3d(),{
      if(!isTRUE(input$show))return()
      i<-suppressWarnings(as.integer(click3d()))
      st<-view();if(length(i)!=1L || !is.finite(i) || i<1 || i>length(st$vertex_ids))return()
      select_ids(st$vertex_ids[i],TRUE)
    },ignoreInit=TRUE)
    for(event in c("plotly_click","plotly_selected")) local({
      ev<-event
      shiny::observeEvent(input[[paste0(ev,"-within_dcst")]],{
        if(!isTRUE(input$show))return()
        raw<-input[[paste0(ev,"-within_dcst")]]
        d<-tryCatch(if(is.character(raw))jsonlite::fromJSON(raw) else raw,error=function(e)NULL)
        if(is.null(d))return()
        ids<-as.character(d$key %||% d$customdata)
        select_ids(intersect(ids,visible_coords()$vertex_id),identical(ev,"plotly_click"))
      },ignoreInit=TRUE)
    })
    output$selection_note<-shiny::renderText({
      ids<-selected();st<-view();present<-sum(ids %in% st$vertex_ids[visible()])
      in2d<-if(isTRUE(input$show))sum(ids %in% visible_coords()$vertex_id) else 0L
      sprintf("%d selected compositions; %d visible in 3D and %d in 2D. Selection follows IDs across embeddings.",length(ids),present,in2d)
    })
    if (requireNamespace("plotly",quietly=TRUE)) output$plot<-plotly::renderPlotly({
      shiny::req(isTRUE(input$show));x<-visible_coords();ax<-axes();pc<-plot_colors()
      cats<-pc$categories;cols<-pc$colors
      hover<-paste0(htmltools::htmlEscape(x$vertex_id),"<br>CST: ",htmltools::htmlEscape(x$dcst),
        "<br>A: ",htmltools::htmlEscape(x$phylotype_a),"<br>B: ",htmltools::htmlEscape(x$phylotype_b),
        "<br>",htmltools::htmlEscape(cats),"<br>",ax$x,"=",signif(x$plot_x,4),"; ",ax$y,"=",signif(x$plot_y,4))
      z<-plotly::plot_ly(source="within_dcst")
      lev<-unique(cats)
      point_type<-if(nrow(x)>3000L)"scattergl" else "scatter"
      if(!length(lev))z<-plotly::add_trace(z,type="scatter",mode="markers",x=numeric(),y=numeric(),showlegend=FALSE)
      if(length(lev)>40L) {
        z<-plotly::add_trace(z,type=point_type,mode="markers",x=x$plot_x,y=x$plot_y,
          key=x$vertex_id,customdata=x$vertex_id,name="CSTs",
          text=hover,hoverinfo="text",marker=list(size=6,color=cols),showlegend=FALSE)
      } else for(g in lev) {
        ii<-which(cats==g)
        z<-plotly::add_trace(z,type=point_type,mode="markers",x=x$plot_x[ii],y=x$plot_y[ii],
          key=x$vertex_id[ii],customdata=x$vertex_id[ii],name=g,
          text=hover[ii],hoverinfo="text",marker=list(size=6,color=cols[ii]),showlegend=length(lev)<=8)
      }
      ss<-x[x$vertex_id %in% selected(),,drop=FALSE]
      if(nrow(ss))z<-plotly::add_trace(z,inherit=FALSE,type="scatter",mode="markers",x=ss$plot_x,y=ss$plot_y,
        key=ss$vertex_id,customdata=ss$vertex_id,marker=list(size=11,color="#f97316",symbol="circle-open",line=list(width=2)),
        hoverinfo="skip",showlegend=FALSE)
      title<-if(identical(input$coordinate_mode,"homogeneous"))"Within-CST homogeneous coordinates" else "Within-CST abundance coordinates"
      z<-plotly::layout(z,dragmode="lasso",uirevision=paste(input$coordinate_mode,view()$project_id,view()$set_id,paste(sort(unique(x$dcst)),collapse="|"),sep="|"),
        title=list(text=title,font=list(size=13)),
        legend=list(orientation="h",y=-.25,font=list(size=10),itemclick=FALSE,itemdoubleclick=FALSE),
        xaxis=list(title=ax$xlabel,range=c(0,min(ax$cap,max(.05,x$plot_x,na.rm=TRUE)*1.04))),
        yaxis=list(title=ax$ylabel,range=c(0,min(ax$cap,max(.05,x$plot_y,na.rm=TRUE)*1.04))))
      if(!nrow(x))z<-plotly::layout(z,annotations=list(list(x=.5,y=.5,xref="paper",yref="paper",
        text="No defined pair coordinates pass the current display filters.",showarrow=FALSE)))
      z<-plotly::event_register(plotly::event_register(z,"plotly_click"),"plotly_selected")
      # Namespace Plotly's event inputs so this module owns the linked selection.
      htmlwidgets::onRender(z,sprintf("function(el){window.gflowuiWithinMount(el,%s);}",jsonlite::toJSON(session$ns(""),auto_unbox=TRUE)))
    })
    output$download<-shiny::downloadHandler(filename=function()paste0("within-dcst-",input$coordinate_mode %||% "abundance",".csv"),content=function(path){
      x<-visible_coords();x$coordinate_mode<-rep(input$coordinate_mode %||% "abundance",nrow(x))
      x$selected<-x$vertex_id %in% selected()
      x$source_category<-gflowui_source_categories(asset()$records,x$vertex_id)
      utils::write.csv(x,path,row.names=FALSE)
    })
    list(augment=function(st){
      if(is.null(asset()) || !is.null(st$error))return(st)
      v<-gflowui_source_categories(asset()$records,st$vertex_ids)
      st$sources$source_dataset<-list(key="source_dataset",label="Source dataset",values=v,type="categorical")
      st$choices<-c(st$choices,"Source dataset"="source_dataset")
      st$graph_set$color_assets$categorical_palettes$source_dataset<-palette()
      st
    },filter=function(st,idx){
      if(is.null(asset()))return(idx)
      idx<-intersect(idx,gflowui_source_filter(asset()$records,st$vertex_ids,groups()))
      e<-cell()
      if(!is.null(e)) {
        idx<-intersect(idx,gflowui_source_filter(asset()$records,st$vertex_ids,e$dataset))
        labels<-as.character(st$sources[[e$level]]$values);labels[is.na(labels)|!nzchar(labels)]<-"Unclassified"
        idx<-intersect(idx,which(labels==e$group))
      }
      idx
    },selected=selected,show=shiny::reactive(isTRUE(input$show)),
    only=shiny::reactive(isTRUE(input$show)&&isTRUE(input$only_selected)),
    coordinates=coordinates,visible_coordinates=visible_coords,groups=groups,cell=cell)
  })
}
