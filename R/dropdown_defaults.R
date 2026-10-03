# Resolve saved graph choices before loading the initial layout. The browser
# still owns later user changes and defaults for other kinds of controls.
gflowui_initial_selector_default <- function(entries, id, region, context, choices) {
  for (entry in entries %||% list()) {
    if (!identical(entry$id, id) || !identical(entry$region %||% "", region)) next
    dependencies <- entry$context %||% list()
    if (!all(vapply(names(dependencies), function(key) {
      identical(as.character(context[[key]]), as.character(dependencies[[key]]))
    }, logical(1)))) next
    value <- as.character(entry$value)
    if (length(value) == 1L && !is.na(value) && value %in% choices) return(value)
  }
  ""
}

# User defaults are separate from reference graphs and registered graph inventories.
gflowui_dropdown_default_key <- function(id, region = "", context = list()) {
  if(length(context))context<-context[order(names(context),method="radix")]
  digest::digest(list(id=id,region=region,context=context),algo="sha256")
}

gflowui_dropdown_defaults_config <- function(fields = list(), extra = list()) {
  deps<-list(); previous<-character()
  for(f in fields) {
    id<-f$input_id
    if(length(id)&&nzchar(id)) {deps[[id]]<-previous;previous<-c(previous,id)}
  }
  deps[["graph_k"]]<-unique(c(previous,"graph_data_type"))
  deps[["graph_optimal_method"]]<-previous
  deps[["local_atlas-view"]]<-"local_atlas-region"
  deps[["local_atlas-compute-metric"]]<-"local_atlas-compute-coordinates"
  deps[["local_atlas-compute-inner"]]<-c("local_atlas-compute-method","local_atlas-compute-coordinates")
  deps[["local_atlas-compute-chart_policy"]]<-"local_atlas-compute-coordinates"
  deps<-utils::modifyList(deps,extra$dependencies %||% list(),keep.null=TRUE)
  list(dependencies=deps,exclude=unique(c("local_atlas-compute-job","local_atlas-revision_parent",
    "local_atlas-endpoint",as.character(extra$exclude %||% character()))))
}

gflowui_dropdown_default_store <- function(manifest, entry, catalog, region = "", region_label = region) {
  if(!is.list(entry)||length(entry$id)!=1L||!entry$id %in% names(catalog))stop("This dropdown is no longer available.")
  control<-catalog[[entry$id]]
  if(!identical(as.character(entry$region %||% ""),region))stop("The region changed. Open the label menu again.")
  value<-as.character(entry$value %||% character())
  if((!length(value)&&!isTRUE(control$multiple))||anyNA(value)||!all(value %in% as.character(control$choices)))stop("Choose an available dropdown value.")
  context<-entry$context %||% list()
  if(!is.list(context)||any(!names(context)%in%names(catalog)))stop("Dropdown dependencies changed. Open the label menu again.")
  context<-lapply(context,as.character)
  for(id in names(context))if(!all(context[[id]]%in%as.character(catalog[[id]]$choices)))stop("A dependency is unavailable.")
  saved<-list(id=entry$id,label=as.character(control$label),value=value,
    value_label=as.character(entry$value_label %||% value),region=region,region_label=region_label,context=context,
    updated_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  key<-gflowui_dropdown_default_key(saved$id,region,context)
  manifest$defaults$dropdowns[[key]]<-saved
  manifest
}

gflowui_dropdown_defaults_server <- function(input,output,session,manifest,region,fields,save,region_label=function()region()) {
  catalog<-shiny::reactiveVal(list())
  shiny::observe({
    m<-manifest();cfg<-gflowui_dropdown_defaults_config(fields(),m$metadata$dropdown_defaults %||% list())
    session$sendCustomMessage("gflowuiDropdownDefaults",list(project_id=m$project_id %||% "",
      region=region() %||% "",entries=unname(m$defaults$dropdowns %||% list()),config=cfg))
  })
  shiny::observeEvent(input$dropdown_defaults_catalog,{
    request<-input$dropdown_defaults_catalog
    if(!identical(request$project_id,manifest()$project_id))return()
    controls<-request$controls %||% list()
    valid<-Filter(function(x)is.list(x)&&length(x$id)==1L&&nzchar(x$id)&&length(x$label)==1L,controls)
    catalog(stats::setNames(valid,vapply(valid,`[[`,"","id")))
  },ignoreInit=TRUE)
  shiny::observeEvent(input$dropdown_default_save,{
    tryCatch({
      request<-input$dropdown_default_save;m<-manifest()
      if(!identical(request$project_id,m$project_id))stop("The project changed. Open the label menu again.")
      entry<-request$entry
      expected<-if(identical(entry$id,"local_atlas-region"))"" else region() %||% ""
      next_manifest<-gflowui_dropdown_default_store(m,entry,catalog(),expected,if(nzchar(expected))region_label() else "Whole dataset")
      # Pass only defaults to the writer; never persist a local effective manifest.
      save(next_manifest$defaults$dropdowns)
      shiny::showNotification(paste0("Default saved: ",catalog()[[entry$id]]$label," — ",paste(entry$value_label,collapse=", ")),type="message")
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  },ignoreInit=TRUE)
  output$dropdown_defaults_overview<-shiny::renderUI({
    entries<-manifest()$defaults$dropdowns %||% list()
    if(!length(entries))return(shiny::p("No label-menu defaults saved. Click a dropdown label to save its current value."))
    shiny::tagList(shiny::p("Saved immediately for this project. Resetting removes an override for future use; it does not change the current selection or reference graph."),
      shiny::tags$table(class="table table-sm",shiny::tags$thead(shiny::tags$tr(lapply(c("Dropdown","Default","Context"),shiny::tags$th))),
        shiny::tags$tbody(lapply(entries,function(e)shiny::tags$tr(shiny::tags$td(e$label),shiny::tags$td(paste(e$value_label,collapse=", ")),
          shiny::tags$td(paste(c(if(nzchar(e$region))paste("Region",e$region_label %||% e$region) else "Whole dataset",vapply(e$context,function(v)paste(v,collapse=", "),"")),collapse=" · ")))))),
      shiny::selectInput("dropdown_default_reset_key","Saved default to reset",stats::setNames(names(entries),vapply(entries,function(e)paste(e$label,paste(e$value_label,collapse=", "),if(nzchar(e$region))paste0("(",e$region_label %||% e$region,")") else "(whole dataset)",sep=" — "),""))),
      shiny::actionButton("dropdown_default_reset","Reset selected default"),shiny::actionButton("dropdown_defaults_reset_all","Reset all dropdown defaults"))
  })
  reset<-function(entries)tryCatch(save(entries),error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  shiny::observeEvent(input$dropdown_default_reset,{
    entries<-manifest()$defaults$dropdowns %||% list();key<-input$dropdown_default_reset_key
    if(length(key)==1L&&key%in%names(entries)){entries[[key]]<-NULL;reset(entries)}
  },ignoreInit=TRUE)
  shiny::observeEvent(input$dropdown_defaults_reset_all,{reset(list())},ignoreInit=TRUE)
}
