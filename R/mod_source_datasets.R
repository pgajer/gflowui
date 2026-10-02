gflowui_source_datasets_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(
    shiny::p(class="gf-hint","Counts describe the current region before display filters. No checks shows all datasets. Dataset and dCST filters intersect."),
    shiny::actionButton(ns("clear_datasets"),"Show all datasets"),
    shiny::uiOutput(ns("table")),
    shiny::actionButton(ns("cross"),"Dataset × dCST"),
    shiny::textOutput(ns("cross_note")),
    shiny::actionButton(ns("clear_cell"),"Clear cross-table cell filter"),
    shiny::p(class="gf-hint","Choose Source dataset under Color by to use these colors. Shared vertices have a Multiple source datasets color. Colors are saved project-wide."))
}

gflowui_within_dcst_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(shiny::checkboxInput(ns("show"),"Show linked 2D view",FALSE),
    shiny::uiOutput(ns("pair_ui")),
    shiny::checkboxInput(ns("add"),"Add to selection / toggle clicked points",FALSE),
    shiny::checkboxInput(ns("only_selected"),"Show only linked selection in 3D",FALSE),
    shiny::actionButton(ns("clear_points"),"Clear linked selection"),
    shiny::textOutput(ns("selection_note")),
    shiny::p(class="gf-hint","Click a point in either display, or box/lasso-select in 2D. Orange rings mark the same composition IDs in both displays. t = B / (A + B); r = 1 − A − B, using original relative abundances."),
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
      shiny::showModal(shiny::modalDialog(title="Source dataset × dCST",size="l",easyClose=TRUE,
        shiny::selectInput(session$ns("cross_level"),"dCST level",c("Level 1"="dcst_level1","Level 2"="dcst_level2"),selected=level()),
        shiny::selectInput(session$ns("unit"),"Count",c("Source records"="records","Distinct vertices per dataset"="vertices")),
        shiny::selectInput(session$ns("display"),"Display",c("Counts"="count","% within each dataset"="row","% within each dCST"="column")),
        shiny::p("All current-region members, before display filters. Shared compositions count in each contributing dataset. Click a cell to filter both displays; this intersects existing filters."),
        shiny::uiOutput(session$ns("cross_table")),
        shiny::downloadButton(session$ns("cross_download"),"Download table")))
    },ignoreInit=TRUE)
    cross <- shiny::reactive({
      shiny::req(asset())
      st<-view(); l<-input$cross_level %||% level()
      shiny::validate(shiny::need(length(st$sources[[l]]$values)==length(st$vertex_ids),"This view has no matching dCST annotations."))
      gflowui_source_cross(asset()$records,st$vertex_ids,st$sources[[l]]$values,
        input$unit %||% "records",input$display %||% "count")
    })
    output$cross_table <- shiny::renderUI({
      m<-cross()
      js<-paste0("Shiny.setInputValue('",session$ns("cell"),"',{dataset:this.dataset.dataset,group:this.dataset.group,level:this.dataset.level},{priority:'event'});")
      shiny::div(style="max-height:55vh;overflow:auto",shiny::tags$table(class="table table-sm",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Dataset"),lapply(colnames(m),function(g)shiny::tags$th(style="min-width:140px",g)))),
        shiny::tags$tbody(lapply(seq_len(nrow(m)),function(i)shiny::tags$tr(shiny::tags$th(rownames(m)[i]),
          lapply(seq_len(ncol(m)),function(j)shiny::tags$td(shiny::tags$button(type="button",class="btn btn-sm btn-light",
            `data-dataset`=rownames(m)[i],`data-group`=colnames(m)[j],`data-level`=input$cross_level %||% level(),onclick=js,
            if(identical(input$display %||% "count","count"))as.character(m[i,j]) else sprintf("%.1f%%",m[i,j])))))))))
    })
    shiny::observeEvent(input$cell,{
      e<-input$cell;m<-cross()
      if(e$dataset %in% rownames(m) && e$group %in% colnames(m)) {cell(e);shiny::removeModal()}
    },ignoreInit=TRUE)
    output$cross_note<-shiny::renderText({e<-cell();if(is.null(e))return("");paste("Cell filter:",e$dataset,"/",e$group)})
    output$cross_download<-shiny::downloadHandler(
      filename=function()paste("dataset",input$cross_level %||% level(),input$unit %||% "records",paste0(input$display %||% "count",".csv"),sep="-"),
      content=function(path){m<-cross();utils::write.csv(data.frame(dataset=rownames(m),m,check.names=FALSE),path,row.names=FALSE)})
    pairs<-shiny::reactive({
      p<-asset()$pairs
      if(is.null(p))return(data.frame(group=character(),a=character(),b=character()))
      values<-view()$sources$dcst_level2$values
      p<-p[p$group %in% values,,drop=FALSE]
      counts<-vapply(p$group,function(g)sum(values==g,na.rm=TRUE),integer(1))
      p[order(-counts,p$group),,drop=FALSE]
    })
    output$pair_ui<-shiny::renderUI({
      p<-pairs();all<-unique(view()$sources$dcst_level2$values)
      shiny::tagList(shiny::selectInput(session$ns("pair"),"Level-2 dCST / pair",choices=p$group,
        selected=shiny::isolate(input$pair) %||% p$group[1]),
        shiny::p(class="gf-hint",sprintf("%d groups have explicit two-phylotype definitions; %d other groups are unavailable. Only members of the chosen dCST are plotted.",nrow(p),length(setdiff(all,p$group)))))
    })
    pair <- shiny::reactive({
      p<-pairs();at<-match(input$pair,p$group)
      if(length(at)!=1L || is.na(at))return(NULL)
      p[at,,drop=FALSE]
    })
    coordinates<-shiny::reactive({
      p<-pair();shiny::req(!is.null(p))
      st<-view();ids<-st$vertex_ids[which(st$sources$dcst_level2$values==p$group)]
      a<-gflowui_vertex_hover_asset(manifest());shiny::req(a)
      gflowui_pair_coordinates(a,ids,p$a,p$b)
    })
    visible_coords<-shiny::reactive({
      x<-coordinates();ids<-view()$vertex_ids[visible()]
      x[x$vertex_id %in% ids & is.finite(x$t) & is.finite(x$r),,drop=FALSE]
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
      g<-st$sources$dcst_level2$values[i]
      if(g %in% pairs()$group)shiny::updateSelectInput(session,"pair",selected=g)
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
      sprintf("%d selected compositions; %d visible in this 3D view. Selection follows IDs across embeddings.",length(ids),present)
    })
    if (requireNamespace("plotly",quietly=TRUE)) output$plot<-plotly::renderPlotly({
      shiny::req(isTRUE(input$show));x<-visible_coords();p<-pair()
      cats<-gflowui_source_categories(asset()$records,x$vertex_id)
      cols<-unname(palette()[cats])
      z<-plotly::plot_ly(source="within_dcst")
      z<-plotly::add_trace(z,type="scatter",mode="markers",x=x$t,y=x$r,key=x$vertex_id,customdata=x$vertex_id,
        text=paste0(x$vertex_id,"<br>",cats,"<br>t=",signif(x$t,4),"; r=",signif(x$r,4)),hoverinfo="text",
        marker=list(size=6,color=cols),showlegend=FALSE)
      s<-x[x$vertex_id %in% selected(),,drop=FALSE]
      if(nrow(s))z<-plotly::add_trace(z,inherit=FALSE,type="scatter",mode="markers",x=s$t,y=s$r,
        key=s$vertex_id,customdata=s$vertex_id,marker=list(size=11,color="#f97316",symbol="circle-open",line=list(width=2)),
        hoverinfo="skip",showlegend=FALSE)
      z<-plotly::layout(z,dragmode="lasso",uirevision=p$group,
        title=list(text=paste0("Within dCST: ",p$group),font=list(size=13)),
        xaxis=list(title="t: fraction of B within the pair",range=c(0,min(1,max(.05,x$t,na.rm=TRUE)*1.04))),
        yaxis=list(title="r: other-phylotype abundance",range=c(0,min(1,max(.05,x$r,na.rm=TRUE)*1.04))))
      if(!nrow(x))z<-plotly::layout(z,annotations=list(list(x=.5,y=.5,xref="paper",yref="paper",
        text="No members of this pair pass the current display filters.",showarrow=FALSE)))
      z<-plotly::event_register(plotly::event_register(z,"plotly_click"),"plotly_selected")
      # Namespace Plotly's event inputs so this module owns the linked selection.
      htmlwidgets::onRender(z,sprintf("function(el){window.gflowuiWithinMount(el,%s);}",jsonlite::toJSON(session$ns(""),auto_unbox=TRUE)))
    })
    output$download<-shiny::downloadHandler(filename=function()"within-dcst-coordinates.csv",content=function(path){
      x<-visible_coords();p<-pair();x$phylotype_a<-rep(p$a,nrow(x));x$phylotype_b<-rep(p$b,nrow(x))
      x$dcst<-rep(p$group,nrow(x));x$selected<-x$vertex_id %in% selected()
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
    coordinates=coordinates,groups=groups,cell=cell)
  })
}
