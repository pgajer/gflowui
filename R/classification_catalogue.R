# Optional project-declared classifications and fixed-reference coverage subsets.
# All joins use immutable sample IDs; layouts and graph objects are never changed.
gflowui_classification_asset <- local({
  cache <- new.env(parent=emptyenv())
  function(manifest) {
    path <- manifest$metadata$classification_catalogue$file
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (!grepl("^(/|[A-Za-z]:)",path))path<-file.path(manifest$project_root,path)
    key <- gflowui_file_version(path)
    if (!identical(cache$key,key)) {
      a <- readRDS(path)
      if (!is.data.frame(a$samples) || !"sample_id" %in% names(a$samples) ||
          anyNA(a$samples$sample_id) || anyDuplicated(a$samples$sample_id))
        stop("Classification catalogue needs unique, nonmissing sample IDs.")
      if (!all(names(a$levels) %in% names(a$samples)) || !"All" %in% names(a$subsets))
        stop("Classification catalogue levels or All subset are missing.")
      for (s in a$subsets) if (anyNA(s$ids) || anyDuplicated(s$ids) ||
          !all(s$ids %in% a$samples$sample_id)) stop("Invalid subset sample IDs.")
      retention <- a$within_cell_retention
      if (!is.null(retention)) {
        if (!"100" %in% names(retention$presets)) stop("Retention catalogue needs an All preset.")
        for (r in retention$presets) if (anyNA(r$ids) || anyDuplicated(r$ids) ||
            !all(r$ids %in% a$samples$sample_id)) stop("Invalid retention sample IDs.")
        if (!setequal(retention$presets[["100"]]$ids,a$samples$sample_id))
          stop("All retention must include the complete reference.")
      }
      cache$key <- key; cache$asset <- a
    }
    cache$asset
  }
})

gflowui_classification_augment <- function(st, manifest, asset=gflowui_classification_asset(manifest)) {
  if (is.null(asset) || !is.null(st$error) || is.null(st$vertex_ids)) return(st)
  at <- match(st$vertex_ids,asset$samples$sample_id)
  for (key in names(asset$levels)) {
    st$sources[[key]] <- list(key=key,label=asset$levels[[key]]$label,
      type="categorical",values=asset$samples[[key]][at])
    st$choices <- c(st$choices,stats::setNames(key,asset$levels[[key]]$label))
    palette <- asset$palettes[[key]]
    overrides <- manifest$metadata$classification_catalogue$palettes[[key]]
    if (length(overrides)) palette[names(overrides)] <- overrides
    st$graph_set$color_assets$categorical_palettes[[key]] <- palette
  }
  st
}

gflowui_classification_subset <- function(asset, subset, ids, retention="100") {
  if (is.null(asset)) return(seq_along(ids))
  keep <- seq_along(ids)
  if (!identical(subset,"All")) {
    s <- asset$subsets[[subset]]
    if (is.null(s)) stop("Unknown sample subset.")
    keep <- which(ids %in% s$ids)
  }
  if (!identical(retention,"100")) {
    r <- asset$within_cell_retention$presets[[retention]]
    if (is.null(r)) stop("Unknown within-cell retention preset.")
    keep <- intersect(keep,which(ids %in% r$ids))
  }
  keep
}

gflowui_classification_state <- function(saved=list(), asset) {
  state <- utils::modifyList(list(type="udcst",levels=list(udcst="udcst_level2",dcst="dcst_level1"),
    subset="All",retention="100",groups=list(),color="udcst"),saved %||% list())
  if (!state$type %in% c("udcst","dcst")) state$type <- "udcst"
  for (type in c("udcst","dcst")) {
    valid <- paste0(type,"_level",seq_len(if(type=="udcst")2L else 3L))
    if (!state$levels[[type]] %in% valid) state$levels[[type]] <- valid[1]
  }
  if (!state$subset %in% names(asset$subsets)) state$subset <- "All"
  if (!state$retention %in% c("100",names(asset$within_cell_retention$presets))) state$retention <- "100"
  state
}

# Root-input controller: independent of active graph/region, with a single
# authoritative type/level/color and per-level selections. Old projects opt out.
gflowui_classification_server <- function(input, session, manifest, save, subset_override=function()NULL) {
  asset <- shiny::reactive(gflowui_classification_asset(manifest()))
  state <- shiny::reactiveVal(NULL); loaded <- shiny::reactiveVal(NULL)
  project <- gflowui_distinct_reactive(function()manifest()$project_id)
  shiny::observeEvent(project(),{
    m <- manifest(); a <- asset()
    state(if(is.null(a))NULL else gflowui_classification_state(m$defaults$classification_state,a))
    loaded(m$project_id)
  },ignoreInit=FALSE,priority=200)
  enabled <- shiny::reactive(!is.null(asset()) && identical(loaded(),project()) && !is.null(state()))
  change <- function(f) {if(!enabled())return(); old<-state(); new<-f(old);if(!identical(old,new))state(new)}
  shiny::observeEvent(input$graph_cst_type,change(function(s){
    x<-input$graph_cst_type
    if(x %in% c("udcst","dcst")){s$type<-x;if(s$color %in% c("udcst","dcst"))s$color<-x};s
  }),ignoreInit=TRUE)
  shiny::observeEvent(input$graph_dcst_level,change(function(s){
    x<-input$graph_dcst_level
    valid<-paste0(s$type,"_level",seq_len(if(s$type=="udcst")2L else 3L))
    if(x %in% valid)s$levels[[s$type]]<-x;s
  }),ignoreInit=TRUE)
  shiny::observeEvent(input$graph_sample_subset,change(function(s){
    if(is.null(subset_override()) && input$graph_sample_subset %in% names(asset()$subsets))s$subset<-input$graph_sample_subset;s
  }),ignoreInit=TRUE)
  shiny::observeEvent(input$graph_within_cell_retention,change(function(s){
    x<-input$graph_within_cell_retention
    if(is.null(subset_override()) && x %in% names(asset()$within_cell_retention$presets))s$retention<-x;s
  }),ignoreInit=TRUE)
  shiny::observeEvent(input$graph_layout_color_by,change(function(s){
    x<-input$graph_layout_color_by
    if(length(x)==1L && nzchar(x)){s$color<-x;if(x %in% c("udcst","dcst"))s$type<-x};s
  }),ignoreInit=TRUE)
  shiny::observeEvent(input$graph_dcst_table_selection,change(function(s){
    e<-input$graph_dcst_table_selection;l<-s$levels[[s$type]]
    if(identical(e$project,project()) && identical(e$level,l))s$groups[[l]]<-unique(as.character(e$groups));s
  }),ignoreInit=TRUE)
  # Save only after a user change; project/manifest reloads must not dirty data.
  pending <- shiny::debounce(shiny::reactive(list(project=project(),state=state())),400)
  shiny::observeEvent(pending(),{
    p<-pending();if(!enabled() || !identical(p$project,project()) || is.null(p$state))return()
    baseline<-gflowui_classification_state(manifest()$defaults$classification_state,asset())
    if(!identical(p$state,baseline))tryCatch(save(p$state),error=function(e)
      shiny::showNotification(paste("Unable to save CST controls:",conditionMessage(e)),type="error"))
  },ignoreInit=FALSE)
  level <- shiny::reactive(if(enabled())state()$levels[[state()$type]] else input$graph_dcst_level %||% "dcst_level1")
  selection <- shiny::reactive(if(enabled())list(project=project(),level=level(),groups=state()$groups[[level()]] %||% character()) else input$graph_dcst_table_selection)
  effective_subset<-shiny::reactive(subset_override()$subset %||% state()$subset)
  effective_retention<-shiny::reactive(if(!is.null(subset_override()))"100" else state()$retention)
  list(asset=asset,state=state,enabled=enabled,level=level,selection=selection,
    color=function(fallback)if(enabled())state()$color else fallback,
    filter=function(st,idx)if(enabled())intersect(idx,gflowui_classification_subset(asset(),effective_subset(),st$vertex_ids,effective_retention())) else idx,
    controls=function(st){
      if(!enabled())return(NULL)
      override<-subset_override()
      if(!is.null(override))return(shiny::tagList(
        shiny::p(class="gf-hint",paste("Sample subset:",override$label)),
        shiny::p(class="gf-hint","The global coverage preset is paused while this saved core is active and is restored on return. Within-cell retention is also paused because this core already has fixed membership. CST and source-dataset filters still apply.")))
      a<-asset();s<-a$subsets[[state()$subset]]
      retained<-length(gflowui_classification_subset(a,state()$subset,st$vertex_ids,state()$retention))
      retention<-a$within_cell_retention
      kept_reference<-gflowui_classification_subset(a,state()$subset,a$samples$sample_id,state()$retention)
      unmodeled<-sum(a$samples$sample_id[kept_reference] %in% retention$unmodeled_ids)
      shiny::tagList(shiny::selectInput("graph_sample_subset","Sample subset",
        choices=stats::setNames(names(a$subsets),vapply(a$subsets,`[[`,"","label")),selected=state()$subset,width="100%"),
        if(!is.null(retention))shiny::selectInput("graph_within_cell_retention","Within-cell retention:",
          choices=stats::setNames(names(retention$presets),vapply(retention$presets,`[[`,"","label")),
          selected=state()$retention,width="100%"),
        shiny::p(class="gf-hint",sprintf("%s / %s reference compositions (%.2f%%); %s retained states. %s / %s members of this view pass this preset. %s",
          format(length(s$ids),big.mark=","),format(nrow(a$samples),big.mark=","),100*length(s$ids)/nrow(a$samples),
          s$states %||% "all",format(retained,big.mark=","),format(length(st$vertex_ids),big.mark=","),s$policy)),
        if(!is.null(retention) && !identical(state()$retention,"100"))shiny::p(class="gf-hint",sprintf(
          "After within-cell retention: %s reference compositions (%.2f%% of the full dataset); %s in cells without a usable model remain unfiltered. %s",
          format(length(kept_reference),big.mark=","),100*length(kept_reference)/nrow(a$samples),format(unmodeled,big.mark=","),retention$policy)),
        shiny::p(class="gf-hint","Display filter only: saved coordinates and graph paths are unchanged. Graph counts above describe the complete current graph, before display filters."))
    })
}

# Keep path gaps: removing a hidden vertex must never create a shortcut edge.
gflowui_visible_path <- function(path, visible) {
  path <- as.integer(path)
  path[!path %in% visible] <- NA_integer_
  path
}

# Empty is a valid mask. Only an absent mask means all vertices.
gflowui_visible_indices <- function(keep, n) {
  if(is.null(keep))seq_len(n) else as.integer(keep)
}
