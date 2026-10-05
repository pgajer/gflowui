# Offer only the dCST levels actually supplied by the current view.
gflowui_dcst_levels <- function(sources, type = "dcst") {
  keys <- intersect(paste0(type, "_level", if(type == "udcst")1:2 else 1:3), names(sources))
  if (!length(keys)) return(character())
  stats::setNames(keys, paste("Level", sub("^.*_level", "", keys)))
}
gflowui_dcst_options <- function(sources, level = "dcst_level1", group = "") {
  type <- if(length(level)==1L && !is.na(level) && startsWith(level,"udcst_"))"udcst" else "dcst"
  keys <- unname(gflowui_dcst_levels(sources,type))
  if (!length(keys)) return(NULL)
  if (length(level) != 1L || !level %in% keys) level <- keys[[1L]]
  values <- as.character(sources[[level]]$values)
  groups <- sort(unique(values[!is.na(values) & nzchar(values)]))
  counts <- vapply(groups, function(x) sum(values == x, na.rm = TRUE), integer(1))
  rank <- order(-counts, groups)
  groups <- groups[rank]
  counts <- counts[rank]
  # Encode the level in each choice so a level change resets the selection.
  ids <- paste(level, groups, sep = ":")
  choices <- c("All dCSTs" = "__all__", stats::setNames(ids,
    sprintf("%s (%s vertices)", groups, format(counts, big.mark = ",", trim = TRUE))))
  if (length(group) != 1L || !group %in% ids) group <- "__all__"
  list(level = level, type = type, levels = gflowui_dcst_levels(sources,type), group = group, choices = choices, groups = groups, counts = counts,
       selected_label = if (group %in% ids) groups[match(group, ids)] else NULL)
}

gflowui_dcst_focus <- function(st, keep_idx, level, group = "",
                               mode = "gray", background = "#b3b3b3") {
  opt <- gflowui_dcst_options(st$sources, level, group)
  if (is.null(opt) || is.null(opt$selected_label))
    return(list(st = st, keep_idx = keep_idx, note = ""))
  values <- as.character(st$sources[[opt$level]]$values)
  selected <- !is.na(values) & values == opt$selected_label
  if (identical(mode, "hide")) {
    keep_idx <- intersect(keep_idx, which(selected))
  } else {
    tryCatch(grDevices::col2rgb(background), error = function(e) {
      stop("Invalid nonselected vertex color.")
    })
    palette <- st$graph_set$color_assets$categorical_palettes[[opt$level]]
    if (is.null(palette)) {
      groups <- sort(unique(values[!is.na(values)]))
      palette <- stats::setNames(grDevices::hcl.colors(length(groups), "Dark 3"), groups)
    }
    other <- "Other dCSTs"
    while (other %in% values) other <- paste0(other, " ")
    values[!selected] <- other
    palette[other] <- background
    st$sources[[opt$level]]$values <- values
    st$graph_set$color_assets$categorical_palettes[[opt$level]] <- palette
  }
  list(st = st, keep_idx = keep_idx,
       note = sprintf("Selected %s: %s vertices in this view. Other vertices %s. Coordinates unchanged.",
         opt$selected_label, format(sum(selected[keep_idx]), big.mark = ","),
         if (identical(mode, "hide")) "hidden" else "recolored"))
}

# Table selection is scoped to a project and level; no selection shows all groups.
gflowui_dcst_table_groups <- function(selection, project, level) {
  if (!is.list(selection) || !identical(selection$project, project) ||
      !identical(selection$level, level)) return(character())
  unique(as.character(selection$groups))
}

gflowui_dcst_table_focus <- function(st, keep_idx, level, groups = character()) {
  if (!length(groups)) return(list(st = st, keep_idx = keep_idx, note = ""))
  values <- as.character(st$sources[[level]]$values)
  keep_idx <- intersect(keep_idx, which(!is.na(values) & values %in% groups))
  list(st = st, keep_idx = keep_idx,
       note = sprintf("Showing %d selected dCSTs: %s vertices. Coordinates unchanged.",
         length(groups), format(length(keep_idx), big.mark = ",")))
}

gflowui_dcst_table_ui <- function(options, palettes, project, selection = NULL) {
  level <- options$level
  groups <- options$groups
  selected <- gflowui_dcst_table_groups(selection, project, level)
  palette <- palettes[[level]]
  fallback <- stats::setNames(grDevices::hcl.colors(length(groups), "Dark 3"), groups)
  colors <- vapply(groups, function(group) {
    col <- palette[[group]]
    if (is.null(col)) col <- fallback[[group]]
    rgb <- grDevices::col2rgb(col)
    grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }, character(1))
  selection_js <- paste0(
    "var t=this.closest('.gf-dcst-table');",
    "Shiny.setInputValue('graph_dcst_table_selection',",
    "{project:t.dataset.project,level:t.dataset.level,groups:",
    "Array.from(t.querySelectorAll('input[type=checkbox]:checked')).map(x=>x.value)},",
    "{priority:'event'});")
  color_js <- paste0(
    "var t=this.closest('.gf-dcst-table');",
    "Shiny.setInputValue('graph_dcst_table_color',",
    "{project:t.dataset.project,level:t.dataset.level,group:this.dataset.group,color:this.value},",
    "{priority:'event'});")
  shiny::div(class = "gf-dcst-table", `data-level` = level, `data-project` = project,
    if(length(setdiff(selected,groups)))shiny::p(class="gf-hint",sprintf("%d selected CSTs are absent from this view. Clear the selection to show the available groups.",length(setdiff(selected,groups)))),
    shiny::p("Check one or more CSTs to show only those groups. No checks shows all. Sizes count vertices in this layout before filters. Colors are saved across this project's layouts.",
      style = "font-size:12px; margin:8px 0;"),
    shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
      onclick = paste0("this.closest('.gf-dcst-table').querySelectorAll('input[type=checkbox]').forEach(x=>x.checked=false);", selection_js),
      "Show all / clear selection"),
    shiny::div(style = "max-height:420px;overflow:auto;margin-top:8px;",
      shiny::tags$table(class = "table table-sm", style = "width:100%;font-size:12px;",
        shiny::tags$thead(shiny::tags$tr(lapply(c("Show", paste(if(identical(options$type,"udcst"))"udCST" else "dCST", "name"), "Size", "Color"), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_along(groups), function(i) shiny::tags$tr(
          shiny::tags$td(shiny::tags$input(type = "checkbox", value = groups[i],
            checked = if(groups[i] %in% selected) "checked" else NULL,
            `aria-label` = paste("Show", if(identical(options$type,"udcst"))"udCST" else "dCST", groups[i]), onchange = selection_js)),
          shiny::tags$td(style = "overflow-wrap:anywhere;", groups[i]),
          shiny::tags$td(style = "white-space:nowrap;", format(options$counts[i], big.mark = ",")),
          shiny::tags$td(shiny::tags$input(type = "color", value = colors[i],
            `data-group` = groups[i], `aria-label` = paste("Color for", groups[i]),
            style = "width:36px;height:26px;padding:1px;", onchange = color_js))
        )))
      )
    )
  )
}

# Match inherited metadata strictly by sample identity, never by row position.
gflowui_metadata_match_vertices <- function(metadata, vertex_ids, id_column = NULL) {
  ids <- gflowui_endpoint_ids(vertex_ids)
  if (!is.data.frame(metadata) || is.null(ids)) return(NULL)
  if (is.null(id_column)) {
    candidate <- intersect(c("sample_id", "vertex_id"), names(metadata))
    if (length(candidate)) id_column <- candidate[1]
  }
  if (is.null(id_column) && identical(rownames(metadata), as.character(seq_len(nrow(metadata))))) return(NULL)
  source_ids <- if (!is.null(id_column)) metadata[[id_column]] else rownames(metadata)
  source_ids <- gflowui_endpoint_ids(source_ids)
  if (is.null(source_ids)) return(NULL)
  at <- match(ids, source_ids)
  if (anyNA(at)) return(NULL)
  metadata[at, , drop=FALSE]
}

# One dCST color choice uses the level control; numeric alternatives stay available.
gflowui_vertex_color_options <- function(st, requested = NULL, fallback = NULL) {
  choices <- st$choices %||% c("Vertex Degree"="vertex_degree")
  has_dcst <- !is.null(gflowui_dcst_options(st$sources))
  if (has_dcst) choices <- c("dCST"="dcst", choices[!unname(choices) %in% unname(gflowui_dcst_levels(st$sources))])
  has_udcst <- length(gflowui_dcst_levels(st$sources,"udcst")) > 0L
  if (has_udcst) choices <- c("udCST"="udcst",choices[!unname(choices) %in% unname(gflowui_dcst_levels(st$sources,"udcst"))])
  choices <- c("Solid color..."="solid_color", choices)
  if (has_dcst && (is.null(requested) || requested %in% unname(gflowui_dcst_levels(st$sources)))) requested <- "dcst"
  if (length(requested) != 1L || is.na(requested) || !requested %in% unname(choices))
    requested <- if (has_udcst) "udcst" else if (has_dcst) "dcst" else fallback %||% st$default_key %||% "vertex_degree"
  if (!requested %in% unname(choices)) requested <- unname(choices)[1]
  list(choices=choices, selected=requested)
}
