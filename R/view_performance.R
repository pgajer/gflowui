# Bounded, session-owned caches. File keys change on replacement or modification.
gflowui_file_version <- function(path) {
  info <- file.info(path)
  if (nrow(info) != 1L || is.na(info$size)) stop("Asset is missing: ", path)
  paste(normalizePath(path, mustWork = TRUE), info$size,
    as.numeric(info$mtime), as.numeric(info$ctime), sep = "|")
}

gflowui_lru_cache <- function(max_entries = 6L, max_bytes = 64 * 1024^2) {
  entries <- list(); order <- character(); sizes <- numeric()
  function(key, compute) {
    if (key %in% order) {
      order <<- c(setdiff(order, key), key)
      return(entries[[key]])
    }
    value <- compute(); size <- as.numeric(object.size(value))
    if (size > max_bytes) return(value)
    while (length(order) && (length(order) >= max_entries || sum(sizes) + size > max_bytes)) {
      oldest <- order[[1L]]; entries[[oldest]] <<- NULL
      sizes <<- sizes[names(sizes) != oldest]; order <<- order[-1L]
    }
    entries[key] <<- list(value); sizes[key] <<- size; order <<- c(order, key)
    value
  }
}

gflowui_hover_cache <- function() {
  cache <- gflowui_lru_cache(3L, 32 * 1024^2)
  function(ids, asset, n) {
    if (is.null(asset)) return(NULL)
    n <- gflowui_hover_top_n(n, length(asset$taxon_names))
    key <- digest::digest(list(ids, asset, n), algo = "xxhash64")
    cache(key, function() gflowui_vertex_hover_text(ids, asset, n))
  }
}

# Keep the accordion and each independent body fragment mounted while other
# fragments change. This also avoids destroying inputs in collapsed panels.
gflowui_workflow_parts <- function(panels, open) {
  flatten <- function(x) {
    if (is.null(x)) return(list(NULL))
    if (is.list(x) && !inherits(x, c("shiny.tag", "html_dependency", "shiny.tag.function")))
      return(unlist(lapply(x, flatten), recursive = FALSE))
    list(x)
  }
  chunks <- list(); shell <- list(); signature <- list()
  for (panel in panels) {
    if (is.null(panel)) next
    id <- panel$attribs[['data-value']]
    body <- panel$children[[2L]]$children[[1L]]$children
    items <- flatten(body)
    ids <- paste0('stable_', id, '_', seq_along(items))
    chunks <- c(chunks, stats::setNames(items, ids))
    # Preserve the original heading and icon; discard only the body.
    title <- panel$children[[1L]]$children[[1L]]$children
    signature[[id]] <- list(title = as.character(htmltools::tagList(title)), ids = ids)
    panel$children[[2L]]$children[[1L]]$children <- lapply(ids, shiny::uiOutput)
    shell[[id]] <- panel
  }
  list(panels = shell, chunks = chunks, signature = signature, open = open)
}

gflowui_distinct_reactive <- function(compute) {
  value <- shiny::reactiveVal(NULL)
  shiny::observe({
    next_value <- compute()
    if(!identical(shiny::isolate(value()),next_value))value(next_value)
  },priority=100)
  value
}

gflowui_stable_workflow_server <- function(output, model, scope) {
  fragments <- shiny::reactiveValues()
  shell <- shiny::reactiveVal(NULL)
  registered <- character(); keys <- list(); shell_key <- NULL
  shiny::observe({
    x <- model(); context <- scope()
    if(!inherits(x,'gflowui_workflow_model')) {
      key<-list(context,as.character(htmltools::tagList(x)))
      if(!identical(key,shell_key)){shell_key<<-key;shell(x)}
      return()
    }
    p<-gflowui_workflow_parts(x$panels,x$open)
    for(id in names(p$chunks)) {
      if(!id %in% registered) local({
        slot<-id
        output[[slot]]<-shiny::renderUI(fragments[[slot]])
        shiny::outputOptions(output,slot,suspendWhenHidden=FALSE)
      })
      key<-list(context,as.character(htmltools::tagList(p$chunks[[id]])))
      if(!identical(key,keys[[id]])) {
        keys[[id]]<<-key
        fragments[[id]]<-p$chunks[[id]]
      }
    }
    registered<<-union(registered,names(p$chunks))
    key<-list(context,p$signature)
    if(!identical(key,shell_key)) {
      shell_key<<-key
      shell(shiny::div(class='gf-sidebar-panel gf-accordion-wrap',
        do.call(bslib::accordion,c(list(id='workflow_accordion',
          open=if(length(p$open))p$open else FALSE,multiple=TRUE),unname(p$panels)))))
    }
  },priority=100)
  output$workflow_controls<-shiny::renderUI(shell())
}

# A user selection is a patch to the last resolved tuple, never a snapshot of
# independently rebound input widgets. Rebinding cannot change this state.
gflowui_selection_patch <- function(request, scope, fields, previous_seq) {
  if (!is.list(request) || !identical(request$scope, scope) ||
      length(request$seq) != 1L || !is.numeric(request$seq) ||
      !is.finite(request$seq) || request$seq <= previous_seq) return(NULL)
  ids <- vapply(fields, function(x) x$input_id, '')
  hit <- match(request$id, ids)
  if (length(hit) != 1L || is.na(hit) || length(request$value) != 1L ||
      !as.character(request$value) %in% unname(fields[[hit]]$choices)) return(NULL)
  values <- stats::setNames(lapply(fields, function(x) x$selected), ids)
  values[[request$id]] <- as.character(request$value)
  list(scope=scope, seq=request$seq, values=values)
}
