# Optional project annotations. One row per source record, not per graph vertex.
gflowui_validate_source_records <- function(x) {
  cols <- c("vertex_id", "record_id", "dataset")
  if (!is.data.frame(x) || !all(cols %in% names(x))) stop("Source records need vertex_id, record_id, and dataset columns.")
  x <- x[, cols, drop=FALSE]
  x[] <- lapply(x, as.character)
  if (anyNA(x) || any(!nzchar(as.matrix(x))) || anyDuplicated(x$record_id))
    stop("Source records must have nonempty IDs and unique record IDs.")
  x
}

gflowui_source_asset <- local({
  cache <- new.env(parent=emptyenv())
  function(manifest) {
    path <- manifest$metadata$source_datasets$file
    if (is.null(path)) return(NULL)
    if (!grepl("^(/|[A-Za-z]:)", path)) path <- file.path(manifest$project_root, path)
    key <- gflowui_file_version(path)
    if (!identical(cache$key, key)) {
      a <- readRDS(path)
      a$records <- gflowui_validate_source_records(a$records)
      if (!is.null(a$pairs)) {
        if (!all(c("group", "a", "b") %in% names(a$pairs)) || anyNA(a$pairs) ||
            anyDuplicated(a$pairs$group) || any(a$pairs$a == a$pairs$b)) stop("Invalid dCST pair definitions.")
      }
      cache$key <- key; cache$value <- a
    }
    cache$value
  }
})

gflowui_source_summary <- function(records, ids) {
  x <- records[records$vertex_id %in% ids, , drop=FALSE]
  groups <- sort(unique(x$dataset))
  n <- vapply(groups, function(g) length(unique(x$vertex_id[x$dataset == g])), integer(1))
  nr <- vapply(groups, function(g) sum(x$dataset == g), integer(1))
  out <- data.frame(dataset=groups, vertices=n, records=nr)
  out[order(-out$vertices, out$dataset), , drop=FALSE]
}

gflowui_source_categories <- function(records, ids) {
  memberships <- split(records$dataset, records$vertex_id)
  vapply(ids, function(id) {
    x <- unique(memberships[[id]])
    if (!length(x)) "Unknown source" else if (length(x)>1L) "Multiple source datasets" else x
  }, character(1), USE.NAMES=FALSE)
}

gflowui_source_palette <- function(records, saved=NULL) {
  groups <- sort(unique(records$dataset))
  p <- stats::setNames(grDevices::hcl.colors(length(groups), "Dark 3"), groups)
  p <- c(p, "Multiple source datasets"="#777777", "Unknown source"="#bbbbbb")
  saved <- unlist(saved)
  good <- intersect(names(saved), names(p))
  p[good] <- saved[good]
  p
}

gflowui_source_filter <- function(records, ids, groups) {
  if (!length(groups)) return(seq_along(ids))
  which(ids %in% records$vertex_id[records$dataset %in% groups])
}

# Counts use all region members, before dataset/dCST display filters. Shared
# vertices count once per dataset in vertex mode, once per record in record mode.
gflowui_source_cross <- function(records, ids, labels, unit="records", display="count") {
  stopifnot(length(ids)==length(labels), !anyDuplicated(ids))
  x <- records[records$vertex_id %in% ids, , drop=FALSE]
  x$group <- labels[match(x$vertex_id, ids)]
  x$group[is.na(x$group) | !nzchar(x$group)] <- "Unclassified"
  if (identical(unit,"vertices")) x <- unique(x[,c("vertex_id","dataset","group")])
  m <- as.matrix(table(x$dataset,x$group))
  m <- m[order(-rowSums(m),rownames(m)),order(-colSums(m),colnames(m)),drop=FALSE]
  if (identical(display,"row")) m <- 100*sweep(m,1,pmax(1,rowSums(m)),"/")
  if (identical(display,"column")) m <- 100*sweep(m,2,pmax(1,colSums(m)),"/")
  m
}

# These are original-composition summaries, independent of graph and embedding.
gflowui_pair_coordinates <- function(asset, ids, a, b) {
  j <- match(c(a,b), asset$taxon_names)
  if (anyNA(j) || j[1]==j[2]) stop("The pair must identify two distinct abundance features.")
  at <- match(ids,asset$sample_ids)
  abundance <- function(k) vapply(at,function(i) {
    if (is.na(i)) return(NA_real_)
    pos <- match(k,asset$indices[[i]])
    if (is.na(pos)) 0 else asset$abundances[[i]][pos]
  },numeric(1))
  xa <- abundance(j[1]); xb <- abundance(j[2]); mass <- xa+xb
  data.frame(vertex_id=ids,t=ifelse(mass>0,xb/mass,NA_real_),r=pmax(0,1-mass),a=xa,b=xb)
}

gflowui_source_table_ui <- function(summary,palette,selected,ns) {
  js <- paste0("var t=this.closest('table');Shiny.setInputValue('", ns("datasets"),
    "',Array.from(t.querySelectorAll('input[type=checkbox]:checked')).map(x=>x.value),{priority:'event'});")
  color <- paste0("Shiny.setInputValue('",ns("color"),
    "',{dataset:this.dataset.group,color:this.value},{priority:'event'});")
  shiny::div(style="max-height:350px;overflow:auto",
    shiny::tags$table(class="table table-sm",style="font-size:12px",
      shiny::tags$thead(shiny::tags$tr(lapply(c("Show","Dataset","Vertices","Records","Color"),shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(summary)),function(i) {
        g<-summary$dataset[i]
        shiny::tags$tr(shiny::tags$td(shiny::tags$input(type="checkbox",value=g,
          checked=if(g %in% selected) "checked" else NULL,onchange=js,`aria-label`=paste("Show dataset",g))),
          shiny::tags$td(g),shiny::tags$td(summary$vertices[i]),shiny::tags$td(summary$records[i]),
          shiny::tags$td(shiny::tags$input(type="color",value=palette[[g]],`data-group`=g,
            `aria-label`=paste("Dataset color",g),onchange=color,style="width:36px;height:26px;padding:1px")))
      }))))
}
