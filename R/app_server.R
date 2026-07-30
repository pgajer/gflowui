app_server <- function(input, output, session) {
  data_state <- mod_data_server("data")
  graph_state <- mod_graph_server("graph", data_state = data_state)
  condexp_state <- mod_condexp_server(
    "condexp",
    data_state = data_state,
    graph_state = graph_state
  )
  viz_state <- mod_visualize_server(
    "viz",
    data_state = data_state,
    graph_state = graph_state,
    condexp_state = condexp_state
  )

  project_helpers <- gflowui_make_server_project_helpers(
    session = session,
    data_state = data_state,
    graph_state = graph_state,
    condexp_state = condexp_state,
    viz_state = viz_state
  )
  list2env(project_helpers, envir = environment())

  graph_helpers <- gflowui_make_server_graph_helpers(rv = rv)
  list2env(graph_helpers, envir = environment())

  # Prefer off-screen/null rgl device in Shiny; avoids noisy init warnings
  # and brittle native OpenGL paths on some macOS/XQuartz setups.
  old_rgl_use_null <- getOption("rgl.useNULL")
  if (!isTRUE(old_rgl_use_null)) {
    options(rgl.useNULL = TRUE)
    session$onSessionEnded(function() {
      options(rgl.useNULL = old_rgl_use_null)
    })
  }

  graph_solid_color_key <- "solid_color"
  graph_solid_color_default <- "#111827"
  reference_plotly_source <- "reference_plot_source"
  reference_plot_camera_input_id <- "reference_plot_camera_state"
  reference_plot_camera_state <- shiny::reactiveVal(NULL)
  graph_selection_state <- shiny::reactiveValues(
    set_id = "",
    k = NA_integer_
  )
  graph_layout_state <- shiny::reactiveValues(
    renderer = NA_character_,
    vertex_layout = "point",
    size_label = NA_character_,
    color_by = NA_character_,
    vertex_color = NA_character_,
    component = NA_character_
  )
  occupation_density_result <- shiny::reactiveVal(NULL)
  occupation_density_status <- shiny::reactiveVal(
    "Choose an estimate, then show it on the graph."
  )
  basin_result <- shiny::reactiveVal(NULL)
  basin_inspector_open <- shiny::reactiveVal(FALSE)
  basin_selected_keys <- shiny::reactiveVal(character())
  basin_color_map <- shiny::reactiveVal(
    structure(character(), names = character())
  )
  basin_plot_specs <- shiny::reactiveVal(list())
  basin_plot_next_id <- shiny::reactiveVal(0L)
  basin_plot_remove_observers <- new.env(parent = emptyenv())
  basin_status <- shiny::reactiveVal(
    "Apply an occupation density or choose a conditional-expectation estimate."
  )
  basin_export_directory <- shiny::reactiveVal(
    normalizePath(path.expand("~"), winslash = "/", mustWork = TRUE)
  )
  basin_export_status <- shiny::reactiveVal(
    "No bundle saved for the active basin complex."
  )
  basin_export_last_path <- shiny::reactiveVal("")
  basin_export_last_fingerprint <- shiny::reactiveVal("")
  density_display_settings <- shiny::reactiveValues(
    low = "yellow",
    midpoint = "none",
    high = "red",
    low_alpha = 0.2,
    midpoint_alpha = 1,
    high_alpha = 1
  )
  basin_display_settings <- shiny::reactiveValues(
    show_maxima = FALSE,
    label_maxima = FALSE,
    show_minima = FALSE,
    label_minima = FALSE,
    maxima_scope = "none",
    minima_scope = "none",
    top_k_max = 6L,
    top_k_min = 6L,
    rank_by = "auto",
    display_mode = "both",
    opacity = 0.85,
    unselected_color = "#D1D5DB",
    unselected_opacity = 0.28,
    inspector_filter = "all",
    inspector_columns = "compact",
    inspector_show_extremum_vertex = FALSE,
    inspector_width = 620
  )
  graph_vertex_color_choices <- function() {
    c(
      "Black" = "#111827",
      "Dark Gray" = "#374151",
      "Gray" = "#6b7280",
      "Light Gray" = "#9ca3af",
      "Blue" = "#2563eb",
      "Red" = "#dc2626",
      "Green" = "#16a34a",
      "Gold" = "#ca8a04"
    )
  }
  density_color_choice_names <- function(include_none = FALSE) {
    colors <- gflowui_density_colors(include_none = include_none)
    lapply(names(colors), function(key) {
      color <- unname(colors[[key]])
      label <- paste0(toupper(substr(key, 1L, 1L)), substr(key, 2L, nchar(key)))
      shiny::span(
        class = "gf-density-color-option",
        shiny::span(
          class = paste(
            "gf-density-color-swatch",
            if (identical(key, "none")) "gf-density-color-none" else ""
          ),
          style = if (nzchar(color)) sprintf("background:%s;", color) else NULL
        ),
        label
      )
    })
  }
  normalize_density_color_key <- function(
      value,
      default,
      include_none = FALSE) {
    choices <- names(gflowui_density_colors(include_none = include_none))
    key <- tolower(trimws(as.character(value %||% default)))
    if (length(key) < 1L || !(key[[1L]] %in% choices)) {
      default
    } else {
      key[[1L]]
    }
  }
  density_display_snapshot <- function() {
    list(
      low = as.character(density_display_settings$low %||% "yellow"),
      midpoint = as.character(
        density_display_settings$midpoint %||% "none"
      ),
      high = as.character(density_display_settings$high %||% "red"),
      low_alpha = suppressWarnings(as.numeric(
        density_display_settings$low_alpha %||% 0.2
      )),
      midpoint_alpha = suppressWarnings(as.numeric(
        density_display_settings$midpoint_alpha %||% 1
      )),
      high_alpha = suppressWarnings(as.numeric(
        density_display_settings$high_alpha %||% 1
      ))
    )
  }
  apply_density_display_settings <- function(values) {
    if (!is.list(values)) {
      return(invisible(NULL))
    }
    low <- values$low
    if (!is.null(low)) {
      density_display_settings$low <- normalize_density_color_key(
        low,
        default = "yellow"
      )
    }
    midpoint <- values$midpoint
    if (!is.null(midpoint)) {
      density_display_settings$midpoint <- normalize_density_color_key(
        midpoint,
        default = "none",
        include_none = TRUE
      )
    }
    high <- values$high
    if (!is.null(high)) {
      density_display_settings$high <- normalize_density_color_key(
        high,
        default = "red"
      )
    }
    sync_alpha <- function(value, field) {
      if (is.null(value)) {
        return(invisible(NULL))
      }
      alpha <- suppressWarnings(as.numeric(value))
      if (length(alpha) > 0L && is.finite(alpha[[1L]])) {
        density_display_settings[[field]] <- max(0, min(1, alpha[[1L]]))
      }
      invisible(NULL)
    }
    sync_alpha(values$low_alpha, "low_alpha")
    sync_alpha(values$midpoint_alpha, "midpoint_alpha")
    sync_alpha(values$high_alpha, "high_alpha")
    invisible(NULL)
  }
  sync_density_display_settings_from_inputs <- function() {
    apply_density_display_settings(list(
      low = input$occupation_density_low_color,
      midpoint = input$occupation_density_mid_color,
      high = input$occupation_density_high_color,
      low_alpha = input$occupation_density_low_alpha,
      midpoint_alpha = input$occupation_density_mid_alpha,
      high_alpha = input$occupation_density_high_alpha
    ))
    invisible(NULL)
  }
  basin_display_snapshot <- function() {
    list(
      show_maxima = isTRUE(basin_display_settings$show_maxima),
      label_maxima = isTRUE(basin_display_settings$label_maxima),
      show_minima = isTRUE(basin_display_settings$show_minima),
      label_minima = isTRUE(basin_display_settings$label_minima),
      maxima_scope = as.character(
        basin_display_settings$maxima_scope %||% "none"
      ),
      minima_scope = as.character(
        basin_display_settings$minima_scope %||% "none"
      ),
      top_k_max = suppressWarnings(as.integer(
        basin_display_settings$top_k_max %||% 6L
      )),
      top_k_min = suppressWarnings(as.integer(
        basin_display_settings$top_k_min %||% 6L
      )),
      rank_by = as.character(
        basin_display_settings$rank_by %||% "auto"
      ),
      display_mode = as.character(
        basin_display_settings$display_mode %||% "both"
      ),
      opacity = suppressWarnings(as.numeric(
        basin_display_settings$opacity %||% 0.85
      )),
      unselected_color = as.character(
        basin_display_settings$unselected_color %||% "#D1D5DB"
      ),
      unselected_opacity = suppressWarnings(as.numeric(
        basin_display_settings$unselected_opacity %||% 0.28
      ))
    )
  }
  apply_basin_extrema_source_defaults <- function(source_type) {
    defaults <- gflowui_basin_extrema_defaults(source_type)
    basin_display_settings$maxima_scope <- defaults$maxima_scope
    basin_display_settings$show_maxima <- !identical(
      defaults$maxima_scope,
      "none"
    )
    basin_display_settings$label_maxima <- defaults$label_maxima
    basin_display_settings$minima_scope <- defaults$minima_scope
    basin_display_settings$show_minima <- !identical(
      defaults$minima_scope,
      "none"
    )
    basin_display_settings$label_minima <- defaults$label_minima
    shiny::updateSelectInput(
      session,
      "basin_extrema_max_scope",
      selected = defaults$maxima_scope
    )
    shiny::updateCheckboxInput(
      session,
      "basin_label_maxima",
      value = defaults$label_maxima
    )
    shiny::updateSelectInput(
      session,
      "basin_extrema_min_scope",
      selected = defaults$minima_scope
    )
    shiny::updateCheckboxInput(
      session,
      "basin_label_minima",
      value = defaults$label_minima
    )
    invisible(defaults)
  }
  normalize_palette_choice <- function(x, choices, default = NULL) {
    vals <- tolower(unname(as.character(choices %||% character(0))))
    default_use <- as.character(default %||% "")
    if (length(default_use) < 1L || !nzchar(default_use[[1]])) {
      default_use <- if (length(vals) > 0L) vals[[1]] else ""
    }
    cand <- tolower(trimws(as.character(x %||% default_use)))
    cand <- cand[nzchar(cand)]
    if (length(cand) < 1L) {
      return(default_use[[1]])
    }
    if (cand[[1]] %in% vals) {
      return(cand[[1]])
    }
    default_use[[1]]
  }
  set_reactive_val_if_changed <- function(rv, value) {
    current <- shiny::isolate(rv())
    if (!identical(current, value)) {
      rv(value)
    }
    invisible(value)
  }
  set_reactive_field_if_changed <- function(rv, field, value) {
    current <- shiny::isolate(rv[[field]])
    if (!identical(current, value)) {
      rv[[field]] <- value
    }
    invisible(value)
  }
  normalize_plotly_camera <- function(cam) {
    if (!is.list(cam)) {
      return(NULL)
    }
    normalize_xyz <- function(node, default = NULL) {
      if (!is.list(node)) {
        return(default)
      }
      x <- suppressWarnings(as.numeric(node$x %||% NA_real_))
      y <- suppressWarnings(as.numeric(node$y %||% NA_real_))
      z <- suppressWarnings(as.numeric(node$z %||% NA_real_))
      if (!all(is.finite(c(x, y, z)))) {
        return(default)
      }
      list(x = x, y = y, z = z)
    }
    out <- list()
    eye <- normalize_xyz(cam$eye)
    center <- normalize_xyz(cam$center, default = list(x = 0, y = 0, z = 0))
    up <- normalize_xyz(cam$up, default = list(x = 0, y = 0, z = 1))
    if (is.null(eye)) {
      return(NULL)
    }
    out$eye <- eye
    if (!is.null(center)) {
      out$center <- center
    }
    if (!is.null(up)) {
      out$up <- up
    }
    projection_type <- as.character(cam$projection$type %||% "")
    if (nzchar(projection_type)) {
      out$projection <- list(type = projection_type)
    }
    out
  }
  capture_reference_plot_camera_js <- function() {
    sprintf(
      "(function(){var gd=document.getElementById('reference_plot'); if(gd && gd._fullLayout && gd._fullLayout.scene && gd._fullLayout.scene.camera && window.Shiny && typeof window.Shiny.setInputValue==='function'){ try { window.Shiny.setInputValue('%s', JSON.parse(JSON.stringify(gd._fullLayout.scene.camera)), {priority:'event'}); } catch(e) {} }})();",
      reference_plot_camera_input_id
    )
  }
  arm_preview_build_request_js <- function() {
    "(function(){var cam=null; var gd=document.getElementById('reference_plot'); if(gd && gd._fullLayout && gd._fullLayout.scene && gd._fullLayout.scene.camera){ try { cam=JSON.parse(JSON.stringify(gd._fullLayout.scene.camera)); } catch(e) { cam=null; } } if(window.Shiny && typeof window.Shiny.setInputValue==='function'){ window.Shiny.setInputValue('arm_preview_build_request', {ts: Date.now(), camera: cam}, {priority:'event'}); }})();"
  }
  arm_builder_camera_hook_script <- function() {
    sprintf(
      "(function(){var cloneCamera=function(cam){try{return JSON.parse(JSON.stringify(cam));}catch(e){return cam||null;}}; var currentCamera=function(){var gd=document.getElementById('reference_plot'); if(!(gd && gd._fullLayout && gd._fullLayout.scene && gd._fullLayout.scene.camera)) return null; return cloneCamera(gd._fullLayout.scene.camera);}; var remember=function(){var cam=currentCamera(); if(!cam) return; window.__gflowuiReferenceCamera=cam; if(window.Shiny && typeof window.Shiny.setInputValue==='function'){ try { window.Shiny.setInputValue('%s', cam, {priority:'event'}); } catch(e) {} } }; var restore=function(){var cam=window.__gflowuiReferenceCamera||currentCamera(); if(!(cam && window.Plotly)) return; var apply=function(){var gd=document.getElementById('reference_plot'); if(!gd) return; try { window.Plotly.relayout(gd, {'scene.camera': cam}); } catch(e) {} }; if(window.requestAnimationFrame){ window.requestAnimationFrame(apply); window.requestAnimationFrame(function(){ window.requestAnimationFrame(apply); }); } setTimeout(apply, 40); setTimeout(apply, 140); setTimeout(apply, 320); }; var hook=function(id){var el=document.getElementById(id); if(el && !el.dataset.gfCameraHooked){ el.addEventListener('mousedown', remember, true); el.addEventListener('focus', remember, true); el.addEventListener('change', function(){ restore(); }, true); el.dataset.gfCameraHooked='1'; } var sel=document.getElementById(id + '-selectized'); if(sel && !sel.dataset.gfCameraHooked){ sel.addEventListener('mousedown', remember, true); sel.addEventListener('focus', remember, true); sel.dataset.gfCameraHooked='1'; } }; hook('arm_endpoint_a'); hook('arm_endpoint_b');})();",
      reference_plot_camera_input_id
    )
  }
  normalize_live_renderer_choice <- function(x, default = "plotly") {
    val <- tolower(trimws(as.character(x %||% default)))
    if (identical(val, "rgl")) {
      val <- "rglwidget"
    }
    if (identical(val, "html")) {
      val <- "plotly"
    }
    if (!(val %in% c("rglwidget", "plotly"))) {
      val <- as.character(default %||% "plotly")
    }
    val
  }
  restore_reference_plot_camera_proxy <- function() {
    rr <- tryCatch(reference_renderer_state(), error = function(e) NULL)
    cam <- isolate(reference_plot_camera_state())
    if (!is.list(rr) || !identical(as.character(rr$effective %||% ""), "plotly") || !is.list(cam) || !requireNamespace("plotly", quietly = TRUE)) {
      return(invisible(FALSE))
    }
    session$onFlushed(function() {
      proxy <- plotly::plotlyProxy("reference_plot", session = session)
      try(
        plotly::plotlyProxyInvoke(proxy, "relayout", list(`scene.camera` = cam)),
        silent = TRUE
      )
    }, once = TRUE)
    invisible(TRUE)
  }
  resolve_gflow_plot3d_fn <- function(base_name) {
    if (!requireNamespace("gflow", quietly = TRUE)) {
      stop("Package 'gflow' is required for 3D graph rendering.", call. = FALSE)
    }
    ns <- asNamespace("gflow")
    preferred <- sprintf("%s.widget", base_name)
    legacy <- sprintf("%s.html", base_name)
    if (exists(preferred, envir = ns, inherits = FALSE)) {
      return(get(preferred, envir = ns, inherits = FALSE))
    }
    if (exists(legacy, envir = ns, inherits = FALSE)) {
      return(get(legacy, envir = ns, inherits = FALSE))
    }
    stop(sprintf("Neither '%s' nor '%s' is available in gflow.", preferred, legacy), call. = FALSE)
  }
  endpoint_session_id <- paste(session$token %||% "session", as.integer(Sys.time()), sep = "-")
  quadform_layout_revision <- shiny::reactiveVal(0L)

  shiny::observeEvent(list(rv$project.active, rv$project.id), {
    graph_selection_state$set_id <- ""
    graph_selection_state$k <- NA_integer_
    graph_layout_state$renderer <- "plotly"
    graph_layout_state$vertex_layout <- "point"
    graph_layout_state$size_label <- NA_character_
    graph_layout_state$color_by <- NA_character_
    graph_layout_state$vertex_color <- NA_character_
    graph_layout_state$component <- NA_character_
    occupation_density_result(NULL)
    occupation_density_status(
      "Choose an estimate, then show it on the graph."
    )
    basin_result(NULL)
    basin_inspector_open(FALSE)
    basin_selected_keys(character())
    basin_color_map(structure(character(), names = character()))
    basin_plot_specs(list())
    basin_plot_next_id(0L)
    basin_status(
      "Apply an occupation density or choose a conditional-expectation estimate."
    )
    quadform_layout_revision(0L)
  }, ignoreInit = FALSE)

  shiny::observe({
    sync_density_display_settings_from_inputs()
  })
  shiny::observeEvent(
    input$density_display_client_snapshot,
    {
      apply_density_display_settings(input$density_display_client_snapshot)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    priority = 100
  )
  shiny::observeEvent(input$basin_extrema_max_scope, {
    scope <- as.character(input$basin_extrema_max_scope %||% "none")
    if (scope %in% c("none", "selected", "listed", "all")) {
      basin_display_settings$maxima_scope <- scope
      basin_display_settings$show_maxima <- !identical(scope, "none")
    }
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  shiny::observeEvent(input$basin_label_maxima, {
    basin_display_settings$label_maxima <- isTRUE(
      input$basin_label_maxima
    )
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  shiny::observeEvent(input$basin_extrema_min_scope, {
    scope <- as.character(input$basin_extrema_min_scope %||% "none")
    if (scope %in% c("none", "selected", "listed", "all")) {
      basin_display_settings$minima_scope <- scope
      basin_display_settings$show_minima <- !identical(scope, "none")
    }
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  shiny::observeEvent(input$basin_label_minima, {
    basin_display_settings$label_minima <- isTRUE(
      input$basin_label_minima
    )
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  shiny::observe({
    renderer_val <- normalize_live_renderer_choice(input$graph_layout_renderer, default = "")
    if (renderer_val %in% c("rglwidget", "plotly")) {
      set_reactive_field_if_changed(graph_layout_state, "renderer", renderer_val)
    }

    vertex_val <- tolower(trimws(as.character(input$graph_layout_vertex %||% "")))
    if (vertex_val %in% c("sphere", "point")) {
      set_reactive_field_if_changed(graph_layout_state, "vertex_layout", vertex_val)
    }

    size_val <- as.character(input$graph_layout_size %||% "")
    if (length(size_val) > 0L && nzchar(size_val[[1]])) {
      set_reactive_field_if_changed(graph_layout_state, "size_label", size_val[[1]])
    }

    color_by_val <- as.character(input$graph_layout_color_by %||% "")
    if (length(color_by_val) > 0L && nzchar(color_by_val[[1]])) {
      set_reactive_field_if_changed(graph_layout_state, "color_by", color_by_val[[1]])
    }

    vertex_color_val <- as.character(input$graph_layout_vertex_color %||% "")
    if (length(vertex_color_val) > 0L && nzchar(vertex_color_val[[1]])) {
      set_reactive_field_if_changed(
        graph_layout_state,
        "vertex_color",
        normalize_palette_choice(
          vertex_color_val[[1]],
          graph_vertex_color_choices(),
          default = graph_solid_color_default
        )
      )
    }

    component_val <- tolower(trimws(as.character(input$graph_layout_component %||% "")))
    if (component_val %in% c("all", "lcc")) {
      set_reactive_field_if_changed(graph_layout_state, "component", component_val)
    }
  })

  project_open_selection_defaults <- function(project_id, manifest = NULL, graph_sets = list()) {
    pid <- tolower(trimws(as.character(project_id %||% "")))
    if (identical(pid, "agp")) {
      return(list(
        set_id = "shared_all_asv",
        k = 6L,
        open_panels = c("workflow_endpoint_structure")
      ))
    }
    list(
      set_id = "",
      k = NA_integer_,
      open_panels = NULL
    )
  }

  current_graph_selection <- shiny::reactive({
    if (!isTRUE(rv$project.active)) {
      return(list(
        error = "No project selected.",
        manifest = NULL,
        graph_sets = list(),
        set_id = "",
        k_selected = NA_integer_,
        data_type_choices = c(),
        k_choices = c()
      ))
    }

    manifest <- active_manifest()
    if (!is.list(manifest)) {
      return(list(
        error = "Project manifest not found.",
        manifest = NULL,
        graph_sets = list(),
        set_id = "",
        k_selected = NA_integer_,
        data_type_choices = c(),
        k_choices = c()
      ))
    }

    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    if (length(graph_sets) < 1L) {
      return(list(
        error = "No graph sets are available.",
        manifest = manifest,
        graph_sets = graph_sets,
        set_id = "",
        k_selected = NA_integer_,
        data_type_choices = c(),
        k_choices = c()
      ))
    }

    project_defaults <- project_open_selection_defaults(
      project_id = rv$project.id,
      manifest = manifest,
      graph_sets = graph_sets
    )
    selector_schema <- graph_selector_schema(manifest, graph_sets)
    selector_input_values <- list()
    selector_fields <- selector_schema$fields %||% list()
    if (length(selector_fields) > 0L) {
      for (field_spec in selector_fields) {
        input_id <- scalar_chr(field_spec$input_id %||% "", default = "")
        if (!nzchar(input_id)) {
          next
        }
        ## Only depend on the grouped selector inputs for the active project.
        selector_input_values[[input_id]] <- input[[input_id]]
      }
    }
    resolved <- resolve_graph_selection(
      manifest = manifest,
      graph_sets = graph_sets,
      input_set_id = input$graph_data_type,
      input_selector_values = selector_input_values,
      input_k = input$graph_k,
      preferred_default_set_id = project_defaults$set_id,
      preferred_default_k = project_defaults$k,
      sticky_set_id = isolate(graph_selection_state$set_id),
      sticky_k = isolate(graph_selection_state$k)
    )
    resolved$error <- NULL
    resolved$manifest <- manifest
    resolved$graph_sets <- graph_sets
    resolved
  })

  shiny::observeEvent(current_graph_selection(), {
    sel <- current_graph_selection()
    if (!is.list(sel) || !is.null(sel$error)) {
      return()
    }

    next_set <- scalar_chr(sel$set_id %||% "", default = "")
    next_k <- scalar_int(sel$k_selected, default = NA_integer_)
    prev_set <- isolate(scalar_chr(graph_selection_state$set_id %||% "", default = ""))
    prev_k <- isolate(scalar_int(graph_selection_state$k, default = NA_integer_))
    if (!identical(prev_set, next_set)) {
      graph_selection_state$set_id <- next_set
    }
    if (!identical(prev_k, next_k)) {
      graph_selection_state$k <- next_k
    }
  }, ignoreInit = FALSE, priority = -100)

  shiny::observeEvent(project_registry(), {
    reg <- project_registry()
    selected <- input$project_select %||% ""
    if (!selected %in% reg$id) {
      selected <- ""
    }
    populate_project_select(selected = selected)
  }, ignoreInit = FALSE)

  shiny::observeEvent(project_registry(), {
    reg <- gflowui_sanitize_registry(project_registry())
    gflowui_save_registry(reg)
  }, ignoreInit = TRUE)

  shiny::observe({
    if (!isTRUE(rv$project.active) || is.null(rv$project.baseline.signature)) {
      return()
    }
    set_reactive_field_if_changed(
      rv,
      "project.dirty",
      !identical(current_state_signature(), rv$project.baseline.signature)
    )
  })

  shiny::observeEvent(
    list(
      input$`graph-build`,
      input$`condexp-fit`,
      input$`viz-detect_endpoints`,
      input$`viz-render`
    ),
    {
      if (isTRUE(rv$project.active)) {
        rv$run.monitor.visible <- TRUE
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$hide_run_monitor, {
    rv$run.monitor.visible <- FALSE
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_select, {
    project_id <- input$project_select %||% ""
    if (!nzchar(project_id)) {
      return()
    }
    open_project(project_id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_new, {
    reg <- project_registry()

    template_choices <- if (nrow(template_catalog) > 0) {
      stats::setNames(template_catalog$id, template_catalog$label)
    } else {
      c("No templates available" = "")
    }

    clone_choices <- if (nrow(reg) > 0) {
      stats::setNames(reg$id, reg$label)
    } else {
      c("No projects available" = "")
    }

    shiny::showModal(
      shiny::modalDialog(
        title = "New Project",
        easyClose = TRUE,
        shiny::radioButtons(
          "new_project_type",
          label = NULL,
          choices = c(
            "New project from scratch" = "scratch",
            "New project from template" = "template",
            "Clone existing project" = "clone"
          ),
          selected = "scratch"
        ),
        shiny::conditionalPanel(
          condition = "input.new_project_type == 'template'",
          shiny::selectInput(
            "new_project_template",
            "Template",
            choices = template_choices,
            selected = unname(template_choices[[1]])
          )
        ),
        shiny::conditionalPanel(
          condition = "input.new_project_type == 'clone'",
          shiny::selectInput(
            "new_project_clone_source",
            "Project to clone",
            choices = clone_choices,
            selected = unname(clone_choices[[1]])
          )
        ),
        shiny::textInput("new_project_name", "Project name", value = ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_new_project", "Create Project", class = "btn-primary")
        )
      )
    )
  })

  shiny::observeEvent(input$confirm_new_project, {
    kind <- input$new_project_type %||% "scratch"
    reg <- project_registry()

    project_name <- trimws(input$new_project_name %||% "")
    has_graphs <- FALSE
    source_id <- NA_character_
    source_manifest <- NULL

    if (identical(kind, "scratch")) {
      if (!nzchar(project_name)) {
        project_name <- "Untitled Project"
      }
      has_graphs <- FALSE
    }

    if (identical(kind, "template")) {
      template_id <- input$new_project_template %||% ""
      template_idx <- match(template_id, template_catalog$id)
      if (!is.na(template_idx)) {
        has_graphs <- isTRUE(template_catalog$has_graphs[[template_idx]])
        if (!nzchar(project_name)) {
          project_name <- sprintf("%s Project", template_catalog$label[[template_idx]])
        }
      }
      if (!nzchar(project_name)) {
        project_name <- "Template Project"
      }
    }

    if (identical(kind, "clone")) {
      source_id <- input$new_project_clone_source %||% ""
      source_idx <- match(source_id, reg$id)
      if (is.na(source_idx)) {
        shiny::showNotification(
          "No clone source selected.",
          type = "error"
        )
        return()
      }

      has_graphs <- isTRUE(reg$has_graphs[[source_idx]])
      if (!nzchar(project_name)) {
        project_name <- sprintf("%s Copy", reg$label[[source_idx]])
      }

      source_manifest <- gflowui_read_manifest(reg$manifest_file[[source_idx]])
    }

    project_id <- make_project_id(project_name, reg$id)

    manifest <- build_new_project_manifest(
      project_id = project_id,
      project_name = project_name,
      kind = kind,
      has_graphs = has_graphs,
      source_id = source_id,
      source_manifest = source_manifest
    )
    manifest_file <- gflowui_manifest_path(project_id)
    gflowui_write_manifest(manifest, manifest_file)

    entry <- gflowui_registry_entry(
      id = project_id,
      label = project_name,
      origin = kind,
      has_graphs = isTRUE(has_graphs),
      has_condexp = length(manifest$condexp_sets %||% list()) > 0L,
      has_endpoints = length(manifest$endpoint_runs %||% list()) > 0L,
      project_root = as.character(manifest$project_root %||% NA_character_),
      manifest_file = normalizePath(manifest_file, mustWork = FALSE),
      created_at = as.character(manifest$created_at %||% .gflowui_now()),
      updated_at = .gflowui_now()
    )
    updated_registry <- gflowui_upsert_registry_row(reg, entry, overwrite = FALSE)

    project_registry(updated_registry)
    shiny::removeModal()
    populate_project_select(selected = project_id)
    open_project(project_id)
  })

  shiny::observeEvent(input$add_data_section, {
    rv$project.show.data <- TRUE
  })

  shiny::observeEvent(input$hide_data_section, {
    rv$project.show.data <- FALSE
  })

  write_csv_safely <- function(df, path) {
    out <- tryCatch(
      {
        utils::write.csv(df, path, row.names = FALSE)
        normalizePath(path, mustWork = FALSE)
      },
      error = function(e) ""
    )
    as.character(out %||% "")
  }

  merge_named_artifact_map <- function(existing, generated) {
    out <- if (is.list(existing)) existing else list()
    gen <- if (is.list(generated)) generated else list()
    if (length(gen) < 1L) {
      return(out)
    }
    for (nm in names(gen)) {
      key <- as.character(nm %||% "")
      if (!nzchar(key)) {
        key <- sprintf("criterion_%d", length(out) + 1L)
      }
      out[[key]] <- gen[[nm]]
    }
    out
  }

  build_optimal_k_artifacts <- function(graph_obj, artifact_dir, set_id) {
    if (!is.list(graph_obj)) {
      return(list())
    }

    build_res <- graph_obj$build.result
    if (!is.list(build_res)) {
      build_res <- graph_obj$build_result
    }
    if (!is.list(build_res) && is.list(graph_obj$X.graphs)) {
      build_res <- graph_obj
    }
    if (!is.list(build_res)) {
      return(list())
    }

    k_vals <- suppressWarnings(as.integer(build_res$k.values %||% build_res$k_values %||% integer(0)))
    k_vals <- k_vals[is.finite(k_vals)]
    if (length(k_vals) < 1L) {
      return(list())
    }

    sel_k <- scalar_int(graph_obj$selected.k %||% build_res$k.opt.edit %||% build_res$k.opt.mixing, default = NA_integer_)
    sel_src <- scalar_chr(graph_obj$selected.k.source %||% "", default = "")
    artifacts <- list()

    mk_path <- function(tag) {
      file.path(artifact_dir, sprintf("%s.optimal_k.%s.csv", set_id, tag))
    }
    as_metric <- function(x) {
      vals <- suppressWarnings(as.numeric(x))
      if (length(vals) == length(k_vals)) {
        vals
      } else {
        numeric(0)
      }
    }

    edit_vals <- as_metric(build_res$edit %||% build_res$edit_distance)
    if (length(edit_vals) == length(k_vals)) {
      df_edit <- data.frame(
        k = k_vals,
        edit_distance = edit_vals,
        selected = if (is.finite(sel_k)) k_vals == sel_k else rep(FALSE, length(k_vals)),
        stringsAsFactors = FALSE
      )
      pp <- write_csv_safely(df_edit, mk_path("edit_distance"))
      if (nzchar(pp)) {
        artifacts$edit_distance <- pp
      }
    }

    mixing_vals <- as_metric(build_res$mixing)
    if (length(mixing_vals) == length(k_vals)) {
      df_mix <- data.frame(
        k = k_vals,
        mixing = mixing_vals,
        selected = if (is.finite(sel_k)) k_vals == sel_k else rep(FALSE, length(k_vals)),
        stringsAsFactors = FALSE
      )
      pp <- write_csv_safely(df_mix, mk_path("mixing"))
      if (nzchar(pp)) {
        artifacts$mixing <- pp
      }
    }

    conn_tbl <- build_res$connectivity
    if (is.data.frame(conn_tbl) && nrow(conn_tbl) > 0L) {
      keep <- intersect(c("k", "n.components", "lcc.frac"), names(conn_tbl))
      df_conn <- conn_tbl[, keep, drop = FALSE]
      if (!("k" %in% names(df_conn)) && nrow(df_conn) == length(k_vals)) {
        df_conn$k <- k_vals
      }
      if ("k" %in% names(df_conn)) {
        df_conn$k <- suppressWarnings(as.integer(df_conn$k))
        df_conn <- df_conn[is.finite(df_conn$k), , drop = FALSE]
      }
      if (nrow(df_conn) > 0L) {
        pp <- write_csv_safely(df_conn, mk_path("connectivity"))
        if (nzchar(pp)) {
          artifacts$connectivity <- pp
        }
      }
    }

    df_summary <- data.frame(
      k = k_vals,
      selected = if (is.finite(sel_k)) k_vals == sel_k else rep(FALSE, length(k_vals)),
      stringsAsFactors = FALSE
    )
    if (length(edit_vals) == length(k_vals)) {
      df_summary$edit_distance <- edit_vals
    }
    if (length(mixing_vals) == length(k_vals)) {
      df_summary$mixing <- mixing_vals
    }
    if (is.data.frame(conn_tbl) && nrow(conn_tbl) > 0L && "k" %in% names(conn_tbl)) {
      conn_k <- suppressWarnings(as.integer(conn_tbl$k))
      pos <- match(df_summary$k, conn_k)
      if ("n.components" %in% names(conn_tbl)) {
        df_summary$n_components <- suppressWarnings(as.integer(conn_tbl$n.components[pos]))
      }
      if ("lcc.frac" %in% names(conn_tbl)) {
        df_summary$lcc_frac <- suppressWarnings(as.numeric(conn_tbl$lcc.frac[pos]))
      }
    }
    if (nzchar(sel_src)) {
      df_summary$selected_source <- sel_src
    }
    pp_sum <- write_csv_safely(df_summary, mk_path("criteria"))
    if (nzchar(pp_sum)) {
      artifacts$criterion <- pp_sum
    }

    artifacts
  }

  default_grip_layout_params <- function() {
    list(
      dim = 3L,
      rounds = 200L,
      final_rounds = 200L,
      num_init = 10L,
      num_nbrs = 30L,
      r = 0.1,
      s = 1.0,
      tinit_factor = 6,
      seed = 6L
    )
  }

  build_grip_layout_assets_for_graph <- function(graph_obj, set_id, output_dir, k_values_hint = integer(0)) {
    out <- list(
      layouts = list(),
      params = default_grip_layout_params(),
      generated = FALSE,
      message = NULL
    )

    if (!requireNamespace("grip", quietly = TRUE)) {
      out$message <- "Package `grip` is unavailable; skipped grip.layout generation."
      return(out)
    }

    collection <- extract_graph_collection(graph_obj)
    if (is.null(collection) || !is.list(collection$graphs) || length(collection$graphs) < 1L) {
      out$message <- "Could not extract graph collection for grip.layout generation."
      return(out)
    }

    graphs <- collection$graphs
    k_vals <- suppressWarnings(as.integer(collection$k_values))
    if (length(k_vals) != length(graphs)) {
      k_hint <- suppressWarnings(as.integer(k_values_hint))
      k_hint <- k_hint[is.finite(k_hint)]
      if (length(k_hint) == length(graphs)) {
        k_vals <- k_hint
      } else {
        k_vals <- seq_along(graphs)
      }
    }

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    params <- out$params

    for (ii in seq_along(graphs)) {
      one <- graphs[[ii]]
      if (!is.list(one) || is.null(one$adj_list)) {
        next
      }

      adj_list <- one$adj_list
      weight_list <- one$weight_list
      if (!is.list(weight_list) || length(weight_list) != length(adj_list)) {
        weight_list <- lapply(adj_list, function(nb) {
          nn <- suppressWarnings(as.integer(nb %||% integer(0)))
          rep(1, length(nn))
        })
      }

      layout_res <- tryCatch(
        do.call(
          grip::grip.layout,
          c(list(adj_list = adj_list, weight_list = weight_list), params)
        ),
        error = function(e) e
      )
      if (inherits(layout_res, "error")) {
        next
      }

      coords <- suppressWarnings(as.matrix(layout_res))
      if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
        next
      }
      coords <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
      if (!is.matrix(coords) || ncol(coords) < 3L) {
        next
      }
      coords <- coords[, seq_len(3L), drop = FALSE]
      coords[!is.finite(coords)] <- 0

      k_use <- suppressWarnings(as.integer(k_vals[[ii]]))
      if (!is.finite(k_use) || k_use < 1L) {
        next
      }
      file_name <- sprintf("%s_k%02d_layout3d.rds", set_id, as.integer(k_use))
      file_path <- file.path(output_dir, file_name)
      saveRDS(coords, file_path)

      key <- sprintf("k%02d", as.integer(k_use))
      out$layouts[[key]] <- list(
        k = as.integer(k_use),
        path = normalizePath(file_path, mustWork = FALSE),
        source = "grip.layout"
      )
    }

    out$generated <- length(out$layouts) > 0L
    if (!isTRUE(out$generated)) {
      out$message <- "No grip.layout files were generated from the graph object."
    }
    out
  }

  shiny::observeEvent(input$graph_update_placeholder, {
    if (!isTRUE(rv$project.active)) {
      return()
    }

    dat <- data_state()
    manifest <- active_manifest()
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    existing_ids <- if (length(graph_sets) > 0L) {
      vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    } else {
      character(0)
    }

    seed_id <- sanitize_token_id(sprintf("graph_%s", format(Sys.time(), "%H%M%S")), fallback = "graph_set")
    set_id_default <- if (seed_id %in% existing_ids) {
      gflowui_make_project_id(seed_id, existing_ids = existing_ids)
    } else {
      seed_id
    }
    set_label_default <- sprintf("%s Graph Set", rv$project.name %||% "Project")

    feature_cols <- if (is.null(dat$data)) character(0) else names(dat$data)
    mode_default <- if (is.null(dat$data)) "register" else "build"

    shiny::showModal(
      shiny::modalDialog(
        title = sprintf("Update / Expand Graphs (%s)", rv$project.name %||% "Project"),
        size = "l",
        easyClose = TRUE,
        shiny::radioButtons(
          "graph_update_mode",
          "Action",
          choices = c(
            "Build graph set from current data" = "build",
            "Register existing graph object (.rds)" = "register"
          ),
          selected = mode_default
        ),
        shiny::textInput(
          "graph_update_set_id",
          "Graph set id",
          value = set_id_default
        ),
        shiny::textInput(
          "graph_update_set_label",
          "Graph set label",
          value = set_label_default
        ),
        shiny::checkboxInput(
          "graph_update_make_default",
          "Set as default graph set",
          value = TRUE
        ),
        shiny::conditionalPanel(
          condition = "input.graph_update_mode == 'build'",
          shiny::p(
            class = "gf-hint",
            "Build uses currently loaded data from the Data section."
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::numericInput("graph_update_kmin", "k min", value = 5, min = 1, step = 1),
            shiny::numericInput("graph_update_kmax", "k max", value = 25, min = 2, step = 1),
            shiny::selectInput(
              "graph_update_method",
              "Selection method",
              choices = c("both", "edit", "mixing", "none"),
              selected = "edit"
            )
          ),
          shiny::selectInput(
            "graph_update_label_col",
            "Label column (required for mixing/both)",
            choices = c("None" = "", feature_cols),
            selected = ""
          )
        ),
        shiny::conditionalPanel(
          condition = "input.graph_update_mode == 'register'",
          shiny::textInput(
            "graph_update_register_path",
            "Path to existing graph .rds",
            value = ""
          ),
          shiny::textInput(
            "graph_update_register_k_values",
            "k values (optional; comma/space separated)",
            value = ""
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_graph_update", "Save Graph Asset", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$confirm_graph_update, {
    if (!isTRUE(rv$project.active)) {
      return()
    }

    mode <- as.character(input$graph_update_mode %||% "build")
    set_id <- sanitize_token_id(
      trimws(as.character(input$graph_update_set_id %||% "")),
      fallback = "graph_set"
    )
    set_label <- trimws(as.character(input$graph_update_set_label %||% ""))
    if (!nzchar(set_label)) {
      set_label <- set_id
    }
    make_default <- isTRUE(input$graph_update_make_default)
    layout_presets <- list(
      renderer = normalize_live_renderer_choice(input$graph_layout_renderer, default = "plotly"),
      vertex_layout = tolower(as.character(input$graph_layout_vertex %||% "point")),
      vertex_size = as.character(input$graph_layout_size %||% "1.0x"),
      color_by = as.character(input$graph_layout_color_by %||% "vertex_degree"),
      vertex_color = normalize_palette_choice(
        input$graph_layout_vertex_color %||% graph_solid_color_default,
        graph_vertex_color_choices(),
        default = graph_solid_color_default
      ),
      component = tolower(as.character(input$graph_layout_component %||% "all"))
    )
    graph_asset_dir <- file.path(gflowui_projects_data_dir(), "graphs", rv$project.id)
    dir.create(graph_asset_dir, recursive = TRUE, showWarnings = FALSE)

    set_run_monitor_note(sprintf("Graph update started: %s (%s).", set_label, mode))

    graph_set <- NULL
    if (identical(mode, "build")) {
      dat <- data_state()
      if (is.null(dat$data)) {
        set_run_monitor_note("Graph update failed: no data loaded.")
        shiny::showNotification("Load data before building graph assets.", type = "error")
        return()
      }

      kmin <- suppressWarnings(as.integer(input$graph_update_kmin))
      kmax <- suppressWarnings(as.integer(input$graph_update_kmax))
      if (!is.finite(kmin) || !is.finite(kmax) || kmin < 1L || kmax < kmin) {
        set_run_monitor_note("Graph update failed: invalid k range.")
        shiny::showNotification("Invalid k range.", type = "error")
        return()
      }

      label_col <- as.character(input$graph_update_label_col %||% "")
      method <- as.character(input$graph_update_method %||% "edit")
      x_df <- dat$data
      labels <- NULL

      if (nzchar(label_col)) {
        if (!(label_col %in% colnames(x_df))) {
          set_run_monitor_note("Graph update failed: label column not found.")
          shiny::showNotification(sprintf("Label column '%s' not found.", label_col), type = "error")
          return()
        }
        labels <- x_df[[label_col]]
        x_df <- x_df[, setdiff(colnames(x_df), label_col), drop = FALSE]
      }

      if (method %in% c("mixing", "both") && is.null(labels)) {
        set_run_monitor_note("Graph update failed: method requires label column.")
        shiny::showNotification("Method 'mixing'/'both' requires a label column.", type = "error")
        return()
      }

      if (ncol(x_df) < 1L) {
        set_run_monitor_note("Graph update failed: no numeric feature columns.")
        shiny::showNotification("No feature columns available for graph construction.", type = "error")
        return()
      }

      non_numeric <- names(x_df)[!vapply(x_df, is.numeric, logical(1))]
      if (length(non_numeric) > 0L) {
        set_run_monitor_note("Graph update failed: non-numeric feature columns.")
        shiny::showNotification(
          sprintf("Feature columns must be numeric. Non-numeric: %s", paste(non_numeric, collapse = ", ")),
          type = "error"
        )
        return()
      }

      set_run_monitor_note(sprintf("Graph update running: %s (%s) k=%d..%d.", set_label, method, kmin, kmax))
      res <- tryCatch(
        gflow_build_graph(
          X = as.matrix(x_df),
          kmin = kmin,
          kmax = kmax,
          method = method,
          labels = labels
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        set_run_monitor_note(sprintf("Graph update failed: %s", conditionMessage(res)))
        shiny::showNotification(
          sprintf("Graph build failed: %s", conditionMessage(res)),
          type = "error"
        )
        return()
      }

      graph_file <- file.path(graph_asset_dir, sprintf("%s.rds", set_id))
      saveRDS(res, graph_file)

      k_vals <- sort(unique(c(seq.int(kmin, kmax), suppressWarnings(as.integer(res$selected.k)))))
      optimal_artifacts <- build_optimal_k_artifacts(
        graph_obj = res,
        artifact_dir = graph_asset_dir,
        set_id = set_id
      )
      layout_variants <- discover_layout_variants_for_graph(
        graph_path = graph_file,
        set_id = set_id,
        k_values = k_vals
      )
      grip_layout_dir <- file.path(graph_asset_dir, sprintf("%s_layouts_3d_rds", set_id))
      grip_assets <- build_grip_layout_assets_for_graph(
        graph_obj = res,
        set_id = set_id,
        output_dir = grip_layout_dir,
        k_values_hint = k_vals
      )
      graph_set <- list(
        id = set_id,
        label = set_label,
        data_type_id = set_id,
        data_type_label = set_label,
        graph_file = normalizePath(graph_file, mustWork = FALSE),
        k_values = k_vals,
        n_samples = nrow(x_df),
        n_features = ncol(x_df),
        optimal_k_artifacts = optimal_artifacts,
        layout_assets = list(
          presets = layout_presets,
          variants = layout_variants,
          grip_layouts = grip_assets$layouts,
          grip_layout_params = grip_assets$params
        ),
        selected_k = suppressWarnings(as.integer(res$selected.k)),
        selection_method = method,
        source = "gflowui_build",
        updated_at = .gflowui_now()
      )
      if (!isTRUE(grip_assets$generated) && nzchar(as.character(grip_assets$message %||% ""))) {
        set_run_monitor_note(as.character(grip_assets$message))
      }
    } else {
      path_raw <- trimws(as.character(input$graph_update_register_path %||% ""))
      if (!nzchar(path_raw)) {
        set_run_monitor_note("Graph update failed: missing graph path.")
        shiny::showNotification("Provide a path to an existing .rds graph file.", type = "error")
        return()
      }
      graph_path <- tryCatch(
        normalizePath(path.expand(path_raw), mustWork = TRUE),
        error = function(e) NA_character_
      )
      if (!is.character(graph_path) || !nzchar(graph_path) || identical(graph_path, "NA") || !file.exists(graph_path)) {
        set_run_monitor_note("Graph update failed: graph path not found.")
        shiny::showNotification("Graph file path does not exist.", type = "error")
        return()
      }

      graph_obj <- tryCatch(readRDS(graph_path), error = function(e) e)
      if (inherits(graph_obj, "error")) {
        set_run_monitor_note(sprintf("Graph update failed: unreadable RDS (%s).", conditionMessage(graph_obj)))
        shiny::showNotification(
          sprintf("Could not read graph file: %s", conditionMessage(graph_obj)),
          type = "error"
        )
        return()
      }

      pull_from_graph_obj <- function(obj, name) {
        if (is.list(obj) && !is.null(obj[[name]])) {
          return(obj[[name]])
        }
        NULL
      }

      k_vals <- parse_k_values_text(input$graph_update_register_k_values)
      if (length(k_vals) < 1L) {
        candidates <- c(
          pull_from_graph_obj(graph_obj, "k.values"),
          pull_from_graph_obj(graph_obj, "k_values"),
          pull_from_graph_obj(graph_obj, "k.seq"),
          pull_from_graph_obj(graph_obj, "k.sequence"),
          pull_from_graph_obj(graph_obj, "k")
        )
        k_vals <- suppressWarnings(as.integer(candidates))
        k_vals <- sort(unique(k_vals[is.finite(k_vals)]))
      }

      existing_optimal <- if (is.list(graph_obj$optimal_k_artifacts)) graph_obj$optimal_k_artifacts else list()
      generated_optimal <- build_optimal_k_artifacts(
        graph_obj = graph_obj,
        artifact_dir = graph_asset_dir,
        set_id = set_id
      )
      optimal_artifacts <- merge_named_artifact_map(existing_optimal, generated_optimal)

      layout_assets <- if (is.list(graph_obj$layout_assets)) graph_obj$layout_assets else list()
      layout_assets$presets <- layout_presets
      layout_assets$variants <- list()

      graph_set <- list(
        id = set_id,
        label = set_label,
        data_type_id = set_id,
        data_type_label = set_label,
        graph_file = graph_path,
        k_values = k_vals,
        n_samples = if (is.null(data_state()$data)) NA_integer_ else nrow(data_state()$data),
        n_features = if (is.null(data_state()$data)) NA_integer_ else ncol(data_state()$data),
        optimal_k_artifacts = optimal_artifacts,
        layout_assets = layout_assets,
        selected_k = suppressWarnings(as.integer(pull_from_graph_obj(graph_obj, "selected.k") %||% NA_integer_)),
        selection_method = as.character(pull_from_graph_obj(graph_obj, "selected.k.source") %||% "external"),
        source = "external_rds",
        updated_at = .gflowui_now()
      )
    }

    ok <- tryCatch(
      upsert_active_graph_set(graph_set, make_default = make_default),
      error = function(e) e
    )
    if (inherits(ok, "error") || !isTRUE(ok)) {
      msg <- if (inherits(ok, "error")) conditionMessage(ok) else "unknown error"
      set_run_monitor_note(sprintf("Graph update failed: %s", msg))
      shiny::showNotification(
        sprintf("Failed to save graph asset: %s", msg),
        type = "error"
      )
      return()
    }

    shiny::removeModal()
    set_run_monitor_note(sprintf("Graph asset saved: %s (%s).", set_label, set_id))
    shiny::showNotification(
      sprintf("Graph asset '%s' saved.", set_label),
      type = "message"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$condexp_update_placeholder, {
    shiny::showNotification(
      "Conditional expectation update/refit workflow is not wired yet.",
      type = "message"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_update_placeholder, {
    shiny::showNotification(
      "Endpoint recomputation workflow is not wired yet.",
      type = "message"
    )
  }, ignoreInit = TRUE)

  endpoint_overlay_selection <- shiny::reactiveVal(character(0))
  endpoint_autoselect_done <- shiny::reactiveVal(FALSE)
  endpoint_show_working_set <- shiny::reactiveVal(NA)
  subject_state <- shiny::reactiveValues(
    selected_ids = character(0),
    show_overlay = FALSE,
    dim_background = FALSE,
    background_opacity = 0.22,
    vertex_color = "#dc2626",
    vertex_size = 1.0,
    edge_mode = "none",
    edge_color = "#dc2626",
    edge_width = 2,
    label_mode = "none",
    label_size = 1.0
  )
  arm_session_id <- paste(session$token %||% "session", "arm", as.integer(Sys.time()), sep = "-")
  arm_workspace_revision <- shiny::reactiveVal(0L)
  arm_overlay_selection <- shiny::reactiveVal(character(0))
  arm_show_working_set <- shiny::reactiveVal(NA)
  arm_datasets_open <- shiny::reactiveVal(FALSE)
  arm_preview_layout_open <- shiny::reactiveVal(FALSE)
  arm_preview_variant <- shiny::reactiveVal(NULL)
  arm_preview_revision <- shiny::reactiveVal(0L)
  arm_builder_virtual_markers <- shiny::reactiveVal(list())
  arm_pending_load_dataset_id <- shiny::reactiveVal("")
  arm_selected_id <- shiny::reactiveVal("")
  arm_draft_banner_dismissed <- shiny::reactiveVal(FALSE)
  workflow_open_panels <- shiny::reactiveVal(NULL)
  endpoint_working_hide_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_working_restore_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_working_delete_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_working_label_event_values <- shiny::reactiveVal(structure(character(0), names = character(0)))
  endpoint_dataset_load_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_dataset_rename_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_dataset_delete_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_dataset_default_counts <- shiny::reactiveVal(structure(integer(0), names = character(0)))
  endpoint_datasets_open <- shiny::reactiveVal(FALSE)
  endpoint_working_scroll_top <- shiny::reactiveVal(0L)
  endpoint_draft_banner_dismissed <- shiny::reactiveVal(FALSE)
  endpoint_pending_load_dataset_id <- shiny::reactiveVal("")
  endpoint_pending_project_action <- shiny::reactiveVal("")
  ## Generation counter: incremented whenever an endpoint-label
  ## parameter changes so the renderUI emits a *new* output ID for
  ## the rglwidget, forcing the browser to destroy the old WebGL
  ## context and create a fresh one (avoids stale-texture black
  ## rectangles on in-place widget updates).
  rgl_gen <- shiny::reactiveVal(0L)
  rgl_last_output_id <- shiny::reactiveVal(NULL)
  shiny::observeEvent(rv$project.id, {
    endpoint_overlay_selection(character(0))
    endpoint_autoselect_done(FALSE)
    endpoint_show_working_set(NA)
    subject_state$selected_ids <- character(0)
    subject_state$show_overlay <- FALSE
    subject_state$dim_background <- FALSE
    subject_state$background_opacity <- 0.22
    subject_state$vertex_color <- "#dc2626"
    subject_state$vertex_size <- 1.0
    subject_state$edge_mode <- "none"
    subject_state$edge_color <- "#dc2626"
    subject_state$edge_width <- 2
    subject_state$label_mode <- "none"
    subject_state$label_size <- 1.0
    endpoint_working_hide_counts(structure(integer(0), names = character(0)))
    endpoint_working_restore_counts(structure(integer(0), names = character(0)))
    endpoint_working_delete_counts(structure(integer(0), names = character(0)))
    endpoint_working_label_event_values(structure(character(0), names = character(0)))
    endpoint_dataset_load_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_rename_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_delete_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_default_counts(structure(integer(0), names = character(0)))
    endpoint_datasets_open(FALSE)
    endpoint_draft_banner_dismissed(FALSE)
    endpoint_pending_load_dataset_id("")
    endpoint_pending_project_action("")
    arm_workspace_revision(0L)
    arm_overlay_selection(character(0))
    arm_show_working_set(NA)
    arm_datasets_open(FALSE)
    arm_preview_layout_open(FALSE)
    arm_preview_variant(NULL)
    arm_preview_revision(0L)
    arm_builder_virtual_markers(list())
    arm_pending_load_dataset_id("")
    arm_selected_id("")
    arm_draft_banner_dismissed(FALSE)
    workflow_open_panels(NULL)
    rgl_last_output_id(NULL)
    rgl_gen(0L)
  }, ignoreInit = TRUE)
  shiny::observeEvent(
    list(input$endpoint_label_size, input$endpoint_label_offset,
         input$endpoint_marker_size, input$endpoint_marker_color,
         input$subject_ids, input$subject_show_overlay,
         input$subject_dim_background, input$subject_background_opacity,
         input$subject_vertex_color, input$subject_vertex_size,
         input$subject_edge_mode, input$subject_edge_color,
         input$subject_edge_width, input$subject_label_mode,
         input$subject_label_size,
         input$arm_label_size, input$arm_tube_opacity, input$arm_path_width,
         input$arm_vertex_size, input$arm_color,
         input$arm_preview_path_color, input$arm_preview_body_color,
         input$arm_preview_body_color_mode,
         input$arm_preview_body_opacity, input$arm_preview_path_width,
         input$arm_preview_body_size, input$arm_center_marker_color,
         input$arm_center_marker_size),
    {
      rr <- shiny::isolate(reference_renderer_state())
      if (!is.list(rr) || !identical(as.character(rr$effective %||% ""), "rglwidget")) {
        return()
      }
      rgl_gen(shiny::isolate(rgl_gen()) + 1L)
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(endpoint_overlay_selection(), {
    rgl_gen(shiny::isolate(rgl_gen()) + 1L)
  }, ignoreInit = TRUE)
  shiny::observeEvent(list(arm_overlay_selection(), arm_show_working_set(), arm_preview_revision()), {
    rgl_gen(shiny::isolate(rgl_gen()) + 1L)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$workflow_accordion, {
    if (!isTRUE(rv$project.active) || is.null(input$workflow_accordion)) {
      return()
    }
    vals <- as.character(input$workflow_accordion %||% character(0))
    vals <- unique(vals[nzchar(vals)])
    set_reactive_val_if_changed(workflow_open_panels, vals)
  }, ignoreInit = TRUE)

  read_csv_safely <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp) || !file.exists(pp)) {
      return(NULL)
    }
    tryCatch(utils::read.csv(pp, stringsAsFactors = FALSE), error = function(e) NULL)
  }

  quadform_project_active <- shiny::reactive({
    manifest <- active_manifest()
    quadform_is_benchmark_manifest(manifest)
  })

  quadform_index_state <- shiny::reactive({
    manifest <- active_manifest()
    qb <- quadform_benchmark_metadata(manifest)
    if (!is.list(qb)) {
      return(list(error = "Not a quadform benchmark project."))
    }
    quadform_index_from_metadata(qb)
  })

  quadform_selector_input_values <- shiny::reactive({
    fields <- c(
      "selection_mode", "metric_target",
      "surface", "n", "seed", "graph_family", "k", "radius_rank",
      "k_scale", "radius_rule", "radius_factor", "prune_method", "stage"
    )
    out <- list()
    for (field in fields) {
      out[[field]] <- input[[paste0("quadform_", field)]]
    }
    out
  })

  quadform_selection_state <- shiny::reactive({
    idx <- quadform_index_state()
    if (!is.list(idx) || !is.null(idx$error)) {
      return(list(error = as.character(idx$error %||% "Quadform benchmark index unavailable.")))
    }
    quadform_benchmark_selector_state(idx$index, idx$metrics, quadform_selector_input_values())
  })

  quadform_dataset_coords <- function(index_state, dataset_id) {
    ds_assets <- if (is.list(index_state) && is.data.frame(index_state$dataset_assets)) index_state$dataset_assets else data.frame()
    if (!is.data.frame(ds_assets) || nrow(ds_assets) < 1L || !("dataset_id" %in% names(ds_assets))) {
      return(list(status = "missing_dataset", message = "Dataset asset index is missing."))
    }
    hit <- which(as.character(ds_assets$dataset_id) == as.character(dataset_id))
    if (length(hit) != 1L) {
      return(list(
        status = if (length(hit) < 1L) "missing_dataset" else "ambiguous_dataset",
        message = "Could not resolve one dataset asset row."
      ))
    }
    path_col <- quadform_first_col(ds_assets, c("dataset_asset_file", "path", "file"))
    pp <- if (nzchar(path_col)) as.character(ds_assets[[path_col]][[hit]]) else ""
    if (!nzchar(pp) || !file.exists(pp)) {
      return(list(status = "missing_dataset", message = "Dataset asset file is missing.", path = pp))
    }
    obj <- tryCatch(readRDS(pp), error = function(e) e)
    if (inherits(obj, "error")) {
      return(list(status = "error", message = conditionMessage(obj), path = pp))
    }
    coords <- obj$X_embed %||% obj$X %||% obj$coords
    if (is.data.frame(coords)) {
      coords <- as.matrix(coords)
    } else {
      coords <- suppressWarnings(as.matrix(coords))
    }
    if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
      return(list(status = "error", message = "Dataset asset does not contain X_embed/X 3D coordinates.", path = pp))
    }
    coords <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
    if (!is.matrix(coords) || ncol(coords) < 3L || !any(is.finite(coords))) {
      return(list(status = "error", message = "Dataset coordinates are not numeric.", path = pp))
    }
    coords <- coords[, seq_len(3L), drop = FALSE]
    coords[!is.finite(coords)] <- 0
    list(status = "ok", coords = coords, obj = obj, path = pp)
  }

  quadform_view_state <- shiny::reactive({
    quadform_layout_revision()
    idx <- quadform_index_state()
    if (!is.list(idx) || !is.null(idx$error)) {
      return(list(status = "error", message = as.character(idx$error %||% "Quadform benchmark index unavailable.")))
    }
    sel <- quadform_selection_state()
    if (!is.list(sel) || !is.null(sel$error)) {
      return(list(status = "error", message = as.character(sel$error %||% "Quadform selection unavailable.")))
    }
    if (!identical(sel$status, "ok")) {
      return(list(
        status = sel$status,
        message = sprintf("Benchmark selector matched %s graph-stage rows.", as.integer(sel$n_matches %||% 0L)),
        selection = sel
      ))
    }
    key <- as.character(sel$key %||% "")
    selected_row <- sel$row[1, , drop = FALSE]
    graph_hit <- quadform_exact_graph_row(idx, key)
    if (!identical(graph_hit$status, "ok")) {
      return(list(
        status = "missing_graph",
        message = sprintf("Could not resolve graph asset row for key %s.", key),
        key = key,
        selection = sel,
        selected_row = selected_row
      ))
    }
    graph_row <- graph_hit$row
    graph_path <- as.character(graph_row$graph_asset_file[[1]] %||% "")
    graph <- quadform_parse_graph_asset(graph_path)
    if (!identical(graph$status, "ok")) {
      return(list(
        status = "missing_graph",
        message = as.character(graph$message %||% "Graph asset unavailable."),
        key = key,
        selection = sel,
        selected_row = selected_row,
        graph_row = graph_row,
        graph_asset_file = graph_path
      ))
    }

    dataset <- quadform_dataset_coords(idx, selected_row$dataset_id[[1]])
    metrics <- if (is.data.frame(idx$metrics) && nrow(idx$metrics) > 0L) {
      idx$metrics[
        as.character(idx$metrics$dataset_id) == as.character(selected_row$dataset_id[[1]]) &
          as.character(idx$metrics$setting_id) == as.character(selected_row$setting_id[[1]]),
        ,
        drop = FALSE
      ]
    } else {
      data.frame()
    }
    diagnostics <- if (is.data.frame(idx$diagnostics) && nrow(idx$diagnostics) > 0L) {
      idx$diagnostics[
        as.character(idx$diagnostics$dataset_id) == as.character(selected_row$dataset_id[[1]]) &
          as.character(idx$diagnostics$setting_id) == as.character(selected_row$setting_id[[1]]),
        ,
        drop = FALSE
      ]
    } else {
      data.frame()
    }

    cache_path <- quadform_generated_layout_cache_path(rv$project.id %||% "project", key)
    layout_source <- "missing"
    layout_path <- ""
    layout <- NULL
    if (file.exists(cache_path)) {
      parsed <- quadform_parse_layout_asset(cache_path)
      if (identical(parsed$status, "ok")) {
        layout <- parsed
        layout_source <- "gflowui_cache"
        layout_path <- cache_path
      } else {
        return(list(
          status = "layout_error",
          message = as.character(parsed$message %||% "Cached layout could not be read."),
          key = key,
          selection = sel,
          selected_row = selected_row,
          graph_row = graph_row,
          graph = graph,
          graph_asset_file = graph_path,
          layout_asset_file = cache_path,
          layout_source = "gflowui_cache",
          dataset = dataset,
          metrics = metrics,
          diagnostics = diagnostics
        ))
      }
    } else {
      layout_hit <- quadform_exact_layout_row(idx, key, method = "weighted_grip")
      if (identical(layout_hit$status, "ok")) {
        layout_row <- layout_hit$row
        benchmark_layout_path <- as.character(layout_row$layout_asset_file[[1]] %||% "")
        parsed <- quadform_parse_layout_asset(benchmark_layout_path)
        if (identical(parsed$status, "ok")) {
          layout <- parsed
          layout_source <- "benchmark"
          layout_path <- benchmark_layout_path
        } else {
          return(list(
            status = "missing_layout",
            message = as.character(parsed$message %||% "Benchmark layout asset unavailable."),
            key = key,
            selection = sel,
            selected_row = selected_row,
            graph_row = graph_row,
            graph = graph,
            graph_asset_file = graph_path,
            layout_row = layout_row,
            layout_asset_file = benchmark_layout_path,
            layout_source = "benchmark",
            cache_path = cache_path,
            dataset = dataset,
            metrics = metrics,
            diagnostics = diagnostics
          ))
        }
      }
    }

    list(
      status = "ok",
      key = key,
      selection = sel,
      selected_row = selected_row,
      graph_row = graph_row,
      graph = graph,
      graph_asset_file = graph_path,
      layout = layout,
      layout_coords = layout$coords,
      layout_asset_file = layout_path,
      layout_source = layout_source,
      cache_path = cache_path,
      dataset = dataset,
      metrics = metrics,
      diagnostics = diagnostics
    )
  })

  shiny::observeEvent(input$quadform_generate_layout, {
    st <- quadform_view_state()
    if (!is.list(st) || !(st$status %in% c("missing_layout", "layout_error"))) {
      shiny::showNotification("No missing weighted layout is selected.", type = "message")
      return()
    }
    graph_path <- as.character(st$graph_asset_file %||% "")
    cache_path <- as.character(st$cache_path %||% quadform_generated_layout_cache_path(rv$project.id %||% "project", st$key %||% ""))
    set_run_monitor_note("Weighted GRIP layout generation started.")
    out <- quadform_generate_weighted_layout(
      graph_asset_path = graph_path,
      output_path = cache_path,
      params = list(seed = 6L)
    )
    if (!identical(out$status, "ok")) {
      msg <- as.character(out$message %||% "Weighted layout generation failed.")
      set_run_monitor_note(msg)
      shiny::showNotification(msg, type = if (identical(out$status, "unavailable")) "warning" else "error")
      return()
    }
    quadform_layout_revision(shiny::isolate(quadform_layout_revision()) + 1L)
    set_run_monitor_note("Weighted GRIP layout cached.")
    shiny::showNotification("Weighted GRIP layout cached.", type = "message")
  }, ignoreInit = TRUE)

  first_existing_col <- function(df, candidates) {
    if (!is.data.frame(df) || length(candidates) < 1L) {
      return("")
    }
    cn <- names(df)
    low <- tolower(cn)
    for (cand in as.character(candidates)) {
      idx <- match(tolower(cand), low)
      if (is.finite(idx)) {
        return(cn[[idx]])
      }
    }
    ""
  }

  parse_k_from_token <- function(x) {
    txt <- tolower(as.character(x %||% ""))
    mm <- regexec("k0*([0-9]+)", txt, perl = TRUE)
    rr <- regmatches(txt, mm)[[1]]
    if (length(rr) >= 2L && nzchar(rr[[2]])) {
      vv <- suppressWarnings(as.integer(rr[[2]]))
      if (is.finite(vv) && vv > 0L) {
        return(vv)
      }
    }
    NA_integer_
  }

  parse_scale_multiplier <- function(x, default = 1) {
    txt <- tolower(trimws(as.character(x %||% "")))
    if (!nzchar(txt)) {
      return(as.numeric(default))
    }
    val <- suppressWarnings(as.numeric(gsub("[^0-9.]+", "", txt)))
    if (!is.finite(val) || val < 0) {
      return(as.numeric(default))
    }
    val
  }

  normalize_scale_label <- function(x, default = "1.0x") {
    val <- parse_scale_multiplier(x, default = NA_real_)
    if (!is.finite(val) || val <= 0) {
      return(as.character(default))
    }

    if (val < 1 || isTRUE(all.equal(val, round(val), tolerance = 1e-10))) {
      return(sprintf("%.1fx", val))
    }
    if (isTRUE(all.equal(val, 1.25, tolerance = 1e-10))) {
      return("1.25x")
    }
    if (isTRUE(all.equal(val, 1.5, tolerance = 1e-10))) {
      return("1.50x")
    }

    sprintf("%sx", format(val, scientific = FALSE, trim = TRUE))
  }

  endpoint_label_positions <- function(coords, endpoint_idx, offset_mult = 1) {
    if (!is.matrix(coords) || nrow(coords) < 1L || ncol(coords) < 3L) {
      return(matrix(numeric(0), ncol = 3))
    }
    idx <- suppressWarnings(as.integer(endpoint_idx))
    idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(coords)]
    if (length(idx) < 1L) {
      return(matrix(numeric(0), ncol = 3))
    }

    base <- coords[idx, 1:3, drop = FALSE]
    center <- colMeans(coords[, 1:3, drop = FALSE], na.rm = TRUE)
    dir <- sweep(base, 2, center, "-")
    norm <- sqrt(rowSums(dir^2))
    unit <- dir
    good <- is.finite(norm) & norm > 1e-12
    if (any(good)) {
      unit[good, ] <- unit[good, , drop = FALSE] / norm[good]
    }
    if (any(!good)) {
      unit[!good, ] <- c(0, 0, 1)
    }

    span <- apply(coords[, 1:3, drop = FALSE], 2, function(vv) diff(range(vv, na.rm = TRUE)))
    span <- span[is.finite(span)]
    span_ref <- if (length(span) > 0L) mean(span) else 1
    shift <- max(1e-8, span_ref * 0.018 * as.numeric(offset_mult))
    base + unit * shift
  }

  normalize_endpoint_method <- function(ep) {
    methods <- unique(tolower(c(
      as.character(ep$method %||% character(0)),
      as.character(ep$methods %||% character(0))
    )))
    methods <- methods[nzchar(methods) & methods != "na"]
    if (length(methods) < 1L) {
      hint <- tolower(sprintf(
        "%s %s",
        as.character(ep$id %||% ""),
        as.character(ep$label %||% "")
      ))
      if (grepl("evenness", hint, fixed = TRUE)) {
        return("evenness")
      }
      return("endpoint")
    }
    if (any(grepl("evenness", methods, fixed = TRUE))) {
      return("evenness")
    }
    out <- gsub("[^a-z0-9]+", "_", methods[[1]])
    out <- gsub("^_+|_+$", "", out)
    if (!nzchar(out)) {
      out <- "endpoint"
    }
    out
  }

  resolve_endpoint_run <- function(manifest, preferred_k = NA_integer_) {
    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    if (length(endpoint_runs) < 1L) {
      return(NULL)
    }
    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    default_id <- as.character(defaults$endpoint_run_id %||% "")
    ids <- vapply(endpoint_runs, function(ep) as.character(ep$id %||% ""), character(1))
    idx <- match(default_id, ids)
    if (!is.finite(idx)) {
      idx <- 1L
    }

    k_pref <- suppressWarnings(as.integer(preferred_k))
    if (is.finite(k_pref)) {
      has_k <- vapply(endpoint_runs, function(ep) {
        kvals <- suppressWarnings(as.integer(ep$k_values %||% integer(0)))
        kvals <- kvals[is.finite(kvals)]
        if (length(kvals) > 0L) {
          return(k_pref %in% kvals)
        }
        sf <- read_csv_safely(ep$summary_csv %||% "")
        if (is.data.frame(sf) && "k" %in% names(sf)) {
          kk <- suppressWarnings(as.integer(sf$k))
          return(any(is.finite(kk) & kk == k_pref))
        }
        lf <- read_csv_safely(ep$labels_csv %||% "")
        if (is.data.frame(lf) && "k" %in% names(lf)) {
          kk <- suppressWarnings(as.integer(lf$k))
          return(any(is.finite(kk) & kk == k_pref))
        }
        FALSE
      }, logical(1))

      if ((length(has_k) == length(endpoint_runs)) && any(has_k)) {
        if (!isTRUE(has_k[[idx]])) {
          idx <- which(has_k)[[1]]
        }
      }
    }

    endpoint_runs[[idx]]
  }

  endpoint_rows_for_run <- function(ep_run) {
    if (!is.list(ep_run) || length(ep_run) < 1L) {
      return(data.frame())
    }

    run_id <- as.character(ep_run$id %||% "endpoint_run")
    method <- normalize_endpoint_method(ep_run)
    labels_csv <- as.character(ep_run$labels_csv %||% "")
    summary_csv <- as.character(ep_run$summary_csv %||% "")
    bundle_file <- as.character(ep_run$bundle_file %||% "")
    per_k_files <- normalize_paths(ep_run$per_k_bundles %||% character(0))
    per_k_files <- per_k_files[file.exists(per_k_files)]

    kvals <- suppressWarnings(as.integer(ep_run$k_values %||% integer(0)))
    kvals <- kvals[is.finite(kvals) & kvals > 0L]

    sf <- read_csv_safely(summary_csv)
    if (is.data.frame(sf) && "k" %in% names(sf)) {
      ks <- suppressWarnings(as.integer(sf$k))
      ks <- ks[is.finite(ks) & ks > 0L]
      kvals <- c(kvals, ks)
    }

    lf <- read_csv_safely(labels_csv)
    if (is.data.frame(lf) && "k" %in% names(lf)) {
      ks <- suppressWarnings(as.integer(lf$k))
      ks <- ks[is.finite(ks) & ks > 0L]
      kvals <- c(kvals, ks)
    }

    if (length(per_k_files) > 0L) {
      ks <- suppressWarnings(as.integer(vapply(per_k_files, parse_k_from_token, integer(1))))
      ks <- ks[is.finite(ks) & ks > 0L]
      kvals <- c(kvals, ks)
    }

    if (file.exists(bundle_file)) {
      kk <- suppressWarnings(as.integer(tryCatch(readRDS(bundle_file)$k, error = function(e) NA_integer_)))
      if (is.finite(kk) && kk > 0L) {
        kvals <- c(kvals, kk)
      }
    }

    kvals <- sort(unique(kvals))
    if (length(kvals) < 1L) {
      kk <- parse_k_from_token(run_id)
      if (is.finite(kk) && kk > 0L) {
        kvals <- kk
      }
    }
    if (length(kvals) < 1L) {
      kvals <- NA_integer_
    }

    rows <- lapply(seq_along(kvals), function(ii) {
      kk <- suppressWarnings(as.integer(kvals[[ii]]))
      per_file <- ""
      if (length(per_k_files) > 0L && is.finite(kk)) {
        hit <- per_k_files[vapply(
          per_k_files,
          function(pp) {
            kf <- parse_k_from_token(basename(pp))
            is.finite(kf) && identical(as.integer(kf), as.integer(kk))
          },
          logical(1)
        )]
        if (length(hit) > 0L) {
          per_file <- hit[[1]]
        }
      }

      key <- sanitize_token_id(
        sprintf(
          "%s_%s_k%s",
          run_id,
          method,
          if (is.finite(kk)) sprintf("%03d", kk) else "na"
        ),
        fallback = sprintf("endpoint_row_%d", ii)
      )

      data.frame(
        key = key,
        input_id = sprintf("endpoint_pick_%s", key),
        run_id = run_id,
        method = method,
        k = kk,
        k_display = if (is.finite(kk)) as.character(kk) else "-",
        labels_csv = labels_csv,
        bundle_file = bundle_file,
        per_k_file = per_file,
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  read_endpoint_labels_from_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    row <- row_df[1, , drop = FALSE]
    k_use <- suppressWarnings(as.integer(row$k[[1]]))

    extract_from_labels_csv <- function(path) {
      tbl <- read_csv_safely(path)
      if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
        return(NULL)
      }
      if ("k" %in% names(tbl) && is.finite(k_use)) {
        kk <- suppressWarnings(as.integer(tbl$k))
        tbl <- tbl[is.finite(kk) & kk == k_use, , drop = FALSE]
      }
      if (nrow(tbl) < 1L) {
        return(NULL)
      }

      vcol <- first_existing_col(
        tbl,
        c(
          "vertex.global", "vertex_global", "vertex",
          "vertex.id", "vertex_id",
          "vertex.local", "vertex_local",
          "endpoint.vertex", "endpoint_vertex"
        )
      )
      if (!nzchar(vcol)) {
        return(NULL)
      }

      vv <- suppressWarnings(as.integer(tbl[[vcol]]))
      keep <- is.finite(vv) & vv > 0L
      vv <- vv[keep]
      if (length(vv) < 1L) {
        return(NULL)
      }

      lcol <- first_existing_col(
        tbl,
        c("label", "endpoint.label", "endpoint_label", "name", "end.label", "end_label")
      )
      labs <- if (nzchar(lcol)) as.character(tbl[[lcol]]) else rep("", nrow(tbl))
      labs <- labs[keep]
      labs[is.na(labs)] <- ""
      if (!any(nzchar(labs))) {
        labs <- sprintf("v%d", vv)
      }

      list(vertices = as.integer(vv), labels = as.character(labs))
    }

    extract_from_rds <- function(path) {
      pp <- as.character(path %||% "")
      if (!nzchar(pp) || !file.exists(pp)) {
        return(NULL)
      }
      obj <- tryCatch(readRDS(pp), error = function(e) NULL)
      if (!is.list(obj)) {
        return(NULL)
      }
      if (is.finite(k_use) && "k" %in% names(obj)) {
        kk <- suppressWarnings(as.integer(obj$k))
        if (is.finite(kk) && !identical(as.integer(kk), as.integer(k_use))) {
          return(NULL)
        }
      }

      vv <- suppressWarnings(as.integer(
        obj$`end.vertices.global` %||%
          obj$end_vertices_global %||%
          obj$`end.vertices` %||%
          obj$end_vertices %||%
          obj$endpoints %||%
          obj$`end.vertices.local` %||%
          integer(0)
      ))
      vv <- vv[is.finite(vv) & vv > 0L]
      if (length(vv) < 1L) {
        return(NULL)
      }

      labs_raw <- obj$`end.labels` %||% obj$end_labels %||% character(0)
      labs <- rep("", length(vv))
      if (is.character(labs_raw) || is.factor(labs_raw)) {
        lr <- as.character(labs_raw)
        if (length(lr) == length(vv)) {
          labs <- lr
        } else if (!is.null(names(labs_raw)) && length(names(labs_raw)) > 0L) {
          nm_int <- suppressWarnings(as.integer(names(labs_raw)))
          mm <- match(vv, nm_int)
          ok <- is.finite(mm)
          labs[ok] <- lr[mm[ok]]
        }
      }
      labs[is.na(labs)] <- ""
      if (!any(nzchar(labs))) {
        labs <- sprintf("v%d", vv)
      }

      list(vertices = as.integer(vv), labels = as.character(labs))
    }

    from_csv <- extract_from_labels_csv(as.character(row$labels_csv[[1]] %||% ""))
    if (is.list(from_csv) && length(from_csv$vertices) > 0L) {
      return(from_csv)
    }

    from_per_k <- extract_from_rds(as.character(row$per_k_file[[1]] %||% ""))
    if (is.list(from_per_k) && length(from_per_k$vertices) > 0L) {
      return(from_per_k)
    }

    from_bundle <- extract_from_rds(as.character(row$bundle_file[[1]] %||% ""))
    if (is.list(from_bundle) && length(from_bundle$vertices) > 0L) {
      return(from_bundle)
    }

    list(vertices = integer(0), labels = character(0))
  }

  empty_working_endpoint_rows <- function() {
    data.frame(
      vertex = integer(0),
      accepted = logical(0),
      visible = logical(0),
      label = character(0),
      auto_label = character(0),
      source_type = character(0),
      source_dataset_id = character(0),
      manually_added = logical(0),
      manually_removed = logical(0),
      notes = character(0),
      updated_at = character(0),
      stringsAsFactors = FALSE
    )
  }

  normalize_endpoint_labels <- function(vertices, labels = character(0)) {
    vv <- suppressWarnings(as.integer(vertices %||% integer(0)))
    vv <- vv[is.finite(vv) & vv > 0L]
    if (length(vv) < 1L) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    labs <- as.character(labels %||% character(0))
    if (length(labs) != length(vv)) {
      labs <- rep("", length(vv))
    }
    labs[is.na(labs)] <- ""
    labs[!nzchar(labs)] <- sprintf("v%d", vv[!nzchar(labs)])
    list(vertices = as.integer(vv), labels = as.character(labs))
  }

  empty_working_endpoint_state <- function(ctx = NULL) {
    list(
      version = "1",
      project_id = as.character(ctx$project_id %||% rv$project.id %||% ""),
      graph_set_id = as.character(ctx$graph_set_id %||% ""),
      k = suppressWarnings(as.integer(ctx$k %||% NA_integer_)),
      base_dataset_id = NA_character_,
      base_dataset_label = NA_character_,
      base_source_k = suppressWarnings(as.integer(NA_integer_)),
      is_modified = FALSE,
      last_snapshot_id = NA_character_,
      last_snapshot_label = NA_character_,
      last_session_id = NA_character_,
      rows = empty_working_endpoint_rows(),
      updated_at = .gflowui_now()
    )
  }

  sanitize_working_endpoint_state <- function(x, ctx = NULL) {
    out <- if (is.list(x)) x else empty_working_endpoint_state(ctx = ctx)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_endpoint_rows()
    template <- empty_working_endpoint_rows()
    missing_cols <- setdiff(names(template), names(rows))
    if (length(missing_cols) > 0L) {
      for (cc in missing_cols) {
        rows[[cc]] <- template[[cc]]
      }
    }
    rows <- rows[, names(template), drop = FALSE]
    rows$vertex <- suppressWarnings(as.integer(rows$vertex))
    rows <- rows[is.finite(rows$vertex) & rows$vertex > 0L, , drop = FALSE]
    rows$accepted <- as.logical(rows$accepted)
    rows$visible <- as.logical(rows$visible)
    rows$label <- as.character(rows$label)
    rows$auto_label <- as.character(rows$auto_label)
    rows$source_type <- as.character(rows$source_type)
    rows$source_dataset_id <- as.character(rows$source_dataset_id)
    rows$manually_added <- as.logical(rows$manually_added)
    rows$manually_removed <- as.logical(rows$manually_removed)
    rows$notes <- as.character(rows$notes)
    rows$updated_at <- as.character(rows$updated_at)
    rows$label[is.na(rows$label)] <- ""
    rows$auto_label[is.na(rows$auto_label)] <- ""
    rows$source_type[is.na(rows$source_type)] <- ""
    rows$source_dataset_id[is.na(rows$source_dataset_id)] <- ""
    rows$notes[is.na(rows$notes)] <- ""
    rows$updated_at[is.na(rows$updated_at)] <- ""
    rows$accepted[is.na(rows$accepted)] <- FALSE
    rows$visible[is.na(rows$visible)] <- FALSE
    rows$manually_added[is.na(rows$manually_added)] <- FALSE
    rows$manually_removed[is.na(rows$manually_removed)] <- FALSE
    if (nrow(rows) > 0L) {
      rows <- rows[!duplicated(rows$vertex), , drop = FALSE]
      missing_label <- !nzchar(rows$label)
      rows$label[missing_label] <- ifelse(
        nzchar(rows$auto_label[missing_label]),
        rows$auto_label[missing_label],
        sprintf("v%d", rows$vertex[missing_label])
      )
      missing_auto <- !nzchar(rows$auto_label)
      rows$auto_label[missing_auto] <- sprintf("v%d", rows$vertex[missing_auto])
    }
    out$project_id <- as.character(out$project_id %||% ctx$project_id %||% rv$project.id %||% "")
    out$graph_set_id <- as.character(out$graph_set_id %||% ctx$graph_set_id %||% "")
    out$k <- suppressWarnings(as.integer(out$k %||% ctx$k %||% NA_integer_))
    out$base_dataset_id <- as.character(out$base_dataset_id %||% NA_character_)
    out$base_dataset_label <- as.character(out$base_dataset_label %||% NA_character_)
    out$base_source_k <- suppressWarnings(as.integer(out$base_source_k %||% NA_integer_))
    out$is_modified <- isTRUE(out$is_modified)
    out$last_snapshot_id <- as.character(out$last_snapshot_id %||% NA_character_)
    out$last_snapshot_label <- as.character(out$last_snapshot_label %||% NA_character_)
    out$last_session_id <- as.character(out$last_session_id %||% NA_character_)
    out$rows <- rows
    out$updated_at <- as.character(out$updated_at %||% .gflowui_now())
    out
  }

  working_endpoint_is_modified <- function(state) {
    isTRUE(state$is_modified)
  }

  working_endpoint_mark_clean <- function(state, base_dataset_id = NULL, base_dataset_label = NULL, base_source_k = NULL) {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    if (!is.null(base_dataset_id)) {
      out$base_dataset_id <- as.character(base_dataset_id %||% NA_character_)
    }
    if (!is.null(base_dataset_label)) {
      out$base_dataset_label <- as.character(base_dataset_label %||% NA_character_)
    }
    if (!is.null(base_source_k)) {
      out$base_source_k <- suppressWarnings(as.integer(base_source_k %||% NA_integer_))
    }
    out$is_modified <- FALSE
    out$last_session_id <- endpoint_session_id
    out$updated_at <- .gflowui_now()
    sanitize_working_endpoint_state(out, ctx = NULL)
  }

  working_endpoint_mark_modified <- function(state) {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    out$is_modified <- TRUE
    out$last_session_id <- endpoint_session_id
    out$updated_at <- .gflowui_now()
    sanitize_working_endpoint_state(out, ctx = NULL)
  }

  working_endpoint_is_recovered <- function(state) {
    st <- sanitize_working_endpoint_state(state, ctx = NULL)
    working_endpoint_is_modified(st) &&
      nzchar(as.character(st$last_session_id %||% "")) &&
      !identical(as.character(st$last_session_id %||% ""), endpoint_session_id)
  }

  read_workspace_endpoint_dataset <- function(path) {
    obj <- read_rds_if_exists(path, default = NULL)
    if (!is.list(obj)) {
      return(NULL)
    }
    payload <- normalize_endpoint_labels(
      vertices = obj$vertices %||% integer(0),
      labels = obj$labels %||% character(0)
    )
    list(
      dataset_id = as.character(obj$dataset_id %||% tools::file_path_sans_ext(basename(path))),
      label = as.character(obj$label %||% obj$dataset_id %||% basename(path)),
      method = as.character(obj$method %||% "workspace"),
      origin = as.character(obj$origin %||% "workspace"),
      graph_set_id = as.character(obj$graph_set_id %||% ""),
      k = suppressWarnings(as.integer(obj$source_k %||% obj$k %||% NA_integer_)),
      created_at = as.character(obj$created_at %||% ""),
      parameter_summary = as.character(obj$parameter_summary %||% obj$summary_label %||% "workspace candidate"),
      source_dataset_id = as.character(obj$source_dataset_id %||% ""),
      vertices = payload$vertices,
      labels = payload$labels,
      path = as.character(path %||% "")
    )
  }

  empty_endpoint_dataset_meta <- function(ctx = NULL) {
    list(
      version = "1",
      project_id = as.character(ctx$project_id %||% rv$project.id %||% ""),
      graph_set_id = as.character(ctx$graph_set_id %||% ""),
      default_dataset_id = NA_character_,
      updated_at = .gflowui_now()
    )
  }

  sanitize_endpoint_dataset_meta <- function(x, ctx = NULL) {
    out <- if (is.list(x)) x else empty_endpoint_dataset_meta(ctx = ctx)
    out$project_id <- as.character(out$project_id %||% ctx$project_id %||% rv$project.id %||% "")
    out$graph_set_id <- as.character(out$graph_set_id %||% ctx$graph_set_id %||% "")
    out$default_dataset_id <- as.character(out$default_dataset_id %||% NA_character_)
    out$updated_at <- as.character(out$updated_at %||% .gflowui_now())
    out
  }

  read_endpoint_dataset_meta <- function(ctx) {
    if (!is.list(ctx)) {
      return(empty_endpoint_dataset_meta(ctx = ctx))
    }
    meta <- read_rds_if_exists(
      endpoint_dataset_meta_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      default = NULL
    )
    sanitize_endpoint_dataset_meta(meta, ctx = ctx)
  }

  save_endpoint_dataset_meta <- function(meta, ctx) {
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    out <- sanitize_endpoint_dataset_meta(meta, ctx = ctx)
    out$updated_at <- .gflowui_now()
    save_rds_safely(
      out,
      endpoint_dataset_meta_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      )
    )
    endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  read_external_endpoint_dataset <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    row <- row_df[1, , drop = FALSE]
    path <- as.character(row$external_rds_file[[1]] %||% "")
    if (!nzchar(path) || !file.exists(path)) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.list(obj)) {
      return(list(vertices = integer(0), labels = character(0)))
    }

    summary_df <- NULL
    if (is.data.frame(obj$summary)) {
      summary_df <- obj$summary
    } else if (is.list(obj$result) && is.data.frame(obj$result$summary)) {
      summary_df <- obj$result$summary
    }

    min_scale_stability <- suppressWarnings(as.numeric(row$filter_min_scale_stability[[1]] %||% NA_real_))
    if (is.data.frame(summary_df) && nrow(summary_df) > 0L) {
      vcol <- first_existing_col(summary_df, c("vertex", "vertex.global", "vertex_global"))
      ecol <- first_existing_col(summary_df, c("is.endpoint", "is_endpoint", "endpoint"))
      if (nzchar(vcol) && nzchar(ecol)) {
        vv <- suppressWarnings(as.integer(summary_df[[vcol]]))
        keep <- as.logical(summary_df[[ecol]])
        keep[is.na(keep)] <- FALSE
        if (is.finite(min_scale_stability)) {
          scol <- first_existing_col(summary_df, c("scale.stability", "scale_stability"))
          if (nzchar(scol)) {
            ss <- suppressWarnings(as.numeric(summary_df[[scol]]))
            keep <- keep & is.finite(ss) & ss >= min_scale_stability
          }
        }
        vv <- vv[keep]
        vv <- vv[is.finite(vv) & vv > 0L]
        if (length(vv) > 0L) {
          labs <- sprintf("v%d", vv)
          return(list(vertices = as.integer(vv), labels = as.character(labs)))
        }
      }
    }

    result_obj <- if (is.list(obj$result)) obj$result else obj
    vv <- suppressWarnings(as.integer(
      result_obj$endpoints %||%
        result_obj$`end.vertices.global` %||%
        result_obj$end_vertices_global %||%
        integer(0)
    ))
    vv <- vv[is.finite(vv) & vv > 0L]
    if (length(vv) < 1L) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    list(vertices = as.integer(vv), labels = sprintf("v%d", vv))
  }

  endpoint_summary_cache <- new.env(parent = emptyenv())

  read_endpoint_summary_from_rds <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp) || !file.exists(pp)) {
      return(NULL)
    }
    if (exists(pp, envir = endpoint_summary_cache, inherits = FALSE)) {
      return(get(pp, envir = endpoint_summary_cache, inherits = FALSE))
    }

    obj <- tryCatch(readRDS(pp), error = function(e) NULL)
    summary_df <- NULL
    if (is.data.frame(obj$summary)) {
      summary_df <- obj$summary
    } else if (is.list(obj$result) && is.data.frame(obj$result$summary)) {
      summary_df <- obj$result$summary
    }

    if (is.data.frame(summary_df) && nrow(summary_df) > 0L) {
      rownames(summary_df) <- NULL
      for (cc in c("vertex", "vertex.global", "vertex_global")) {
        if (cc %in% names(summary_df)) {
          summary_df[[cc]] <- suppressWarnings(as.integer(summary_df[[cc]]))
        }
      }
    } else {
      summary_df <- NULL
    }

    assign(pp, summary_df, envir = endpoint_summary_cache)
    summary_df
  }

  read_endpoint_summary_from_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(NULL)
    }
    row <- row_df[1, , drop = FALSE]
    candidates <- c(
      as.character(row$external_rds_file[[1]] %||% ""),
      as.character(row$per_k_file[[1]] %||% ""),
      as.character(row$bundle_file[[1]] %||% ""),
      as.character(row$workspace_file[[1]] %||% "")
    )
    candidates <- unique(candidates[nzchar(candidates)])
    if (length(candidates) < 1L) {
      return(NULL)
    }
    for (pp in candidates) {
      summary_df <- read_endpoint_summary_from_rds(pp)
      if (is.data.frame(summary_df) && nrow(summary_df) > 0L) {
        return(summary_df)
      }
    }
    NULL
  }

  endpoint_metrics_for_vertex <- function(vertex_id, rows_df) {
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L || !is.data.frame(rows_df) || nrow(rows_df) < 1L) {
      return(data.frame())
    }

    metric_rows <- list()
    idx_out <- 1L
    preferred_cols <- c(
      "s.min",
      "s.q",
      "m",
      "score",
      "detection.score",
      "scale.stability",
      "is.local.max",
      "is.endpoint",
      "s.min.smooth",
      "s.q.smooth",
      "m.smooth",
      "score.smooth"
    )

    for (ii in seq_len(nrow(rows_df))) {
      rr <- rows_df[ii, , drop = FALSE]
      summary_df <- read_endpoint_summary_from_row(rr)
      if (!is.data.frame(summary_df) || nrow(summary_df) < 1L) {
        next
      }
      vcol <- first_existing_col(summary_df, c("vertex", "vertex.global", "vertex_global"))
      if (!nzchar(vcol)) {
        next
      }
      vv <- suppressWarnings(as.integer(summary_df[[vcol]]))
      hit <- which(is.finite(vv) & vv == as.integer(vid))
      if (length(hit) < 1L) {
        next
      }
      one <- summary_df[hit[[1]], , drop = FALSE]
      keep_cols <- intersect(preferred_cols, names(one))
      metric_rows[[idx_out]] <- data.frame(
        dataset = as.character(rr$label[[1]] %||% rr$dataset_id[[1]] %||% ""),
        method = as.character(rr$method[[1]] %||% ""),
        k = suppressWarnings(as.integer(rr$k[[1]] %||% NA_integer_)),
        one[, keep_cols, drop = FALSE],
        stringsAsFactors = FALSE
      )
      idx_out <- idx_out + 1L
    }

    if (length(metric_rows) < 1L) {
      return(data.frame())
    }
    out <- do.call(rbind, metric_rows)
    rownames(out) <- NULL
    out
  }

  format_endpoint_metric_value <- function(x) {
    if (length(x) < 1L) {
      return("")
    }
    if (is.logical(x)) {
      return(ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE")))
    }
    if (is.numeric(x)) {
      out <- rep("", length(x))
      ok <- is.finite(x)
      out[ok] <- formatC(x[ok], digits = 4, format = "fg", flag = "#")
      return(out)
    }
    as.character(x)
  }

  empty_endpoint_feature_profile <- function() {
    data.frame(
      rank = integer(0),
      feature = character(0),
      taxonomy = character(0),
      abundance = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  empty_endpoint_label_profile_suggestion <- function(vertex_id = NA_integer_) {
    list(
      vertex = suppressWarnings(as.integer(vertex_id)),
      label = NA_character_,
      sample_id = NA_character_,
      profile = empty_endpoint_feature_profile(),
      source_kind = "",
      source_detail = ""
    )
  }

  normalize_endpoint_feature_profile <- function(tbl) {
    template <- empty_endpoint_feature_profile()
    if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
      return(template)
    }
    missing_cols <- setdiff(names(template), names(tbl))
    if (length(missing_cols) > 0L) {
      for (cc in missing_cols) {
        tbl[[cc]] <- template[[cc]]
      }
    }
    tbl <- tbl[, names(template), drop = FALSE]
    tbl$rank <- suppressWarnings(as.integer(tbl$rank))
    tbl$feature <- as.character(tbl$feature)
    tbl$taxonomy <- as.character(tbl$taxonomy)
    tbl$abundance <- suppressWarnings(as.numeric(tbl$abundance))
    tbl$feature[is.na(tbl$feature)] <- ""
    tbl$taxonomy[is.na(tbl$taxonomy)] <- ""
    tbl$rank[is.na(tbl$rank)] <- seq_len(sum(is.na(tbl$rank)))
    tbl$abundance[is.na(tbl$abundance)] <- NA_real_
    rownames(tbl) <- NULL
    tbl
  }

  clean_taxonomy_label_for_ui <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return("NA")
    }
    y <- as.character(x)
    y <- gsub("^[a-z]_", "", y)
    y <- gsub("_+", " ", y)
    y <- gsub("\\s+", " ", y)
    y <- gsub("(?i)\\bgasseri\\s+johnsonii\\b", "gasseri", y, perl = TRUE)
    y <- gsub("(?i)\\bcrispatus\\s+helveticus\\b", "crispatus", y, perl = TRUE)
    y <- gsub("(?i)^lactobacillus\\b", "L", y, perl = TRUE)
    trimws(y)
  }

  abbrev_taxon_for_ui <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return("NA")
    }
    y <- as.character(x)
    y <- sub("^.*__", "", y)
    y <- gsub("[|;]", "_", y)
    y <- gsub("[^A-Za-z0-9_]", "_", y)
    y <- gsub("_+", "_", y)
    y <- gsub("^_|_$", "", y)
    if (!nzchar(y)) {
      return("NA")
    }
    parts <- strsplit(y, "_", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) < 1L) {
      return("NA")
    }
    if (length(parts) == 1L) {
      p1 <- parts[[1]]
      if (nchar(p1) >= 2L) {
        return(substr(p1, 1L, 2L))
      }
      return(toupper(p1))
    }
    paste0(substr(parts[[1]], 1L, 1L), substr(parts[[2]], 1L, 1L))
  }

  endpoint_profile_csv_cache <- new.env(parent = emptyenv())
  endpoint_live_label_provider_cache <- new.env(parent = emptyenv())
  subject_live_provider_cache <- new.env(parent = emptyenv())

  resolve_manifest_project_path <- function(path, project_root) {
    pp <- trimws(as.character(path %||% ""))
    if (!nzchar(pp)) {
      return("")
    }
    pp <- path.expand(pp)
    if (!grepl("^(/|~)", pp) && nzchar(as.character(project_root %||% ""))) {
      pp <- file.path(project_root, pp)
    }
    normalizePath(pp, mustWork = FALSE)
  }

  read_manifest_table <- function(path) {
    if (!nzchar(as.character(path %||% "")) || !file.exists(path)) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(path))
    if (identical(ext, "rds")) {
      obj <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.data.frame(obj)) {
        return(obj)
      }
      if (is.list(obj) && is.data.frame(obj$rows)) {
        return(obj$rows)
      }
      return(NULL)
    }
    sep <- if (ext %in% c("tsv", "tab")) "\t" else ","
    tryCatch(
      utils::read.table(
        path,
        sep = sep,
        header = TRUE,
        quote = "\"",
        comment.char = "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  }

  read_endpoint_profile_csv <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp) || !file.exists(pp)) {
      return(NULL)
    }
    if (exists(pp, envir = endpoint_profile_csv_cache, inherits = FALSE)) {
      return(get(pp, envir = endpoint_profile_csv_cache, inherits = FALSE))
    }

    tbl <- read_csv_safely(pp)
    out <- NULL
    if (is.data.frame(tbl) && nrow(tbl) > 0L) {
      vg_col <- first_existing_col(tbl, c("vertex.global", "vertex_global"))
      vl_col <- first_existing_col(tbl, c("vertex.local", "vertex_local", "vertex"))
      label_col <- first_existing_col(tbl, c("label", "endpoint.label", "endpoint_label"))
      sample_col <- first_existing_col(tbl, c("sample.id", "sample_id"))
      rank_col <- first_existing_col(tbl, c("rank", "profile.rank", "profile_rank"))
      feature_col <- first_existing_col(tbl, c("asv.id", "asv_id", "species", "feature", "feature_id"))
      taxonomy_col <- first_existing_col(tbl, c("taxonomy", "taxon", "species", "feature"))
      abundance_col <- first_existing_col(tbl, c("abundance", "relative.abundance", "rel_abundance", "value"))

      out <- data.frame(
        vertex_global = if (nzchar(vg_col)) suppressWarnings(as.integer(tbl[[vg_col]])) else rep(NA_integer_, nrow(tbl)),
        vertex_local = if (nzchar(vl_col)) suppressWarnings(as.integer(tbl[[vl_col]])) else rep(NA_integer_, nrow(tbl)),
        sample_id = if (nzchar(sample_col)) as.character(tbl[[sample_col]]) else rep(NA_character_, nrow(tbl)),
        label = if (nzchar(label_col)) as.character(tbl[[label_col]]) else rep(NA_character_, nrow(tbl)),
        rank = if (nzchar(rank_col)) suppressWarnings(as.integer(tbl[[rank_col]])) else seq_len(nrow(tbl)),
        feature = if (nzchar(feature_col)) as.character(tbl[[feature_col]]) else rep("", nrow(tbl)),
        taxonomy = if (nzchar(taxonomy_col)) as.character(tbl[[taxonomy_col]]) else rep("", nrow(tbl)),
        abundance = if (nzchar(abundance_col)) suppressWarnings(as.numeric(tbl[[abundance_col]])) else rep(NA_real_, nrow(tbl)),
        stringsAsFactors = FALSE
      )
      out$feature[is.na(out$feature)] <- ""
      out$taxonomy[is.na(out$taxonomy)] <- ""
      needs_taxonomy <- !nzchar(out$taxonomy) & nzchar(out$feature)
      out$taxonomy[needs_taxonomy] <- out$feature[needs_taxonomy]
      needs_feature <- !nzchar(out$feature) & nzchar(out$taxonomy)
      out$feature[needs_feature] <- out$taxonomy[needs_feature]
      out$label[is.na(out$label)] <- ""
      out$sample_id[is.na(out$sample_id)] <- ""
      out$rank[!is.finite(out$rank)] <- seq_len(sum(!is.finite(out$rank)))
    }

    assign(pp, out, envir = endpoint_profile_csv_cache)
    out
  }

  endpoint_profile_csv_candidates_for_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(character(0))
    }
    row <- row_df[1, , drop = FALSE]
    direct_paths <- character(0)
    if ("profiles_csv" %in% names(row)) {
      direct_paths <- c(direct_paths, as.character(row$profiles_csv[[1]] %||% ""))
    }
    parent_dirs <- unique(dirname(c(
      as.character(row$labels_csv[[1]] %||% ""),
      as.character(row$bundle_file[[1]] %||% ""),
      as.character(row$per_k_file[[1]] %||% ""),
      as.character(row$workspace_file[[1]] %||% ""),
      as.character(row$external_rds_file[[1]] %||% "")
    )))
    parent_dirs <- unique(parent_dirs[nzchar(parent_dirs) & dir.exists(parent_dirs)])
    guessed_paths <- direct_paths
    known_names <- c(
      "evenness.endpoint.top_asv_profiles.k05.csv",
      "evenness.endpoint.top_asv_profiles.csv",
      "evenness_endpoint_top_asv_profiles.csv"
    )
    for (dd in parent_dirs) {
      guessed_paths <- c(guessed_paths, file.path(dd, known_names))
      extra <- list.files(
        dd,
        pattern = "top.*profile.*\\.csv$|top_asv_profiles.*\\.csv$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(extra) > 0L) {
        guessed_paths <- c(guessed_paths, extra)
      }
    }
    guessed_paths <- unique(as.character(guessed_paths))
    guessed_paths[nzchar(guessed_paths) & file.exists(guessed_paths)]
  }

  label_from_taxonomy_profile <- function(taxonomy, abundance, separator = " / ") {
    tax <- as.character(taxonomy %||% character(0))
    abund <- suppressWarnings(as.numeric(abundance %||% numeric(0)))
    if (length(tax) < 1L) {
      return(NA_character_)
    }
    if (length(abund) != length(tax)) {
      abund <- rep(NA_real_, length(tax))
    }
    keep <- which(is.finite(abund) & abund >= 0.05)
    if (length(keep) < 1L) {
      keep <- 1L
    }
    pick <- head(keep, 2L)
    out <- paste(tax[pick], collapse = separator)
    if (!nzchar(out)) {
      return(NA_character_)
    }
    out
  }

  precomputed_endpoint_label_profile_suggestion <- function(vertex_id, rows_df) {
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L || !is.data.frame(rows_df) || nrow(rows_df) < 1L) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }

    candidate_rows <- rows_df
    if ("selected" %in% names(candidate_rows)) {
      selected_flag <- as.logical(candidate_rows$selected)
      selected_flag[is.na(selected_flag)] <- FALSE
      current_k <- suppressWarnings(as.integer(current_endpoint_graph_context()$k %||% NA_integer_))
      current_match <- if ("k" %in% names(candidate_rows) && is.finite(current_k)) {
        kk <- suppressWarnings(as.integer(candidate_rows$k))
        is.finite(kk) & kk == as.integer(current_k)
      } else {
        rep(FALSE, nrow(candidate_rows))
      }
      ord <- order(!selected_flag, !current_match, as.character(candidate_rows$label), na.last = TRUE)
      candidate_rows <- candidate_rows[ord, , drop = FALSE]
    }

    for (ii in seq_len(nrow(candidate_rows))) {
      rr <- candidate_rows[ii, , drop = FALSE]
      profile_paths <- endpoint_profile_csv_candidates_for_row(rr)
      if (length(profile_paths) < 1L) {
        next
      }
      for (pp in profile_paths) {
        tbl <- read_endpoint_profile_csv(pp)
        if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
          next
        }
        hit <- which(
          (is.finite(tbl$vertex_global) & tbl$vertex_global == as.integer(vid)) |
            (is.finite(tbl$vertex_local) & tbl$vertex_local == as.integer(vid))
        )
        if (length(hit) < 1L) {
          next
        }
        one <- tbl[hit, , drop = FALSE]
        one <- one[order(one$rank, na.last = TRUE), , drop = FALSE]
        profile_tbl <- normalize_endpoint_feature_profile(data.frame(
          rank = one$rank,
          feature = one$feature,
          taxonomy = one$taxonomy,
          abundance = one$abundance,
          stringsAsFactors = FALSE
        ))
        label_val <- unique(one$label[nzchar(one$label)])
        label_val <- if (length(label_val) > 0L) label_val[[1]] else label_from_taxonomy_profile(profile_tbl$taxonomy, profile_tbl$abundance)
        sample_id <- unique(one$sample_id[nzchar(one$sample_id)])
        sample_id <- if (length(sample_id) > 0L) sample_id[[1]] else NA_character_
        return(list(
          vertex = as.integer(vid),
          label = as.character(label_val %||% NA_character_),
          sample_id = as.character(sample_id %||% NA_character_),
          profile = profile_tbl,
          source_kind = "precomputed",
          source_detail = sprintf(
            "%s (%s)",
            as.character(rr$label[[1]] %||% rr$dataset_id[[1]] %||% "endpoint dataset"),
            basename(pp)
          )
        ))
      }
    }

    empty_endpoint_label_profile_suggestion(vertex_id)
  }

  load_symptoms_taxonomy_map_for_ui <- function(project_root) {
    candidates <- c(
      file.path(dirname(project_root), "Pdata", "data", "asv_Sys.rda"),
      file.path(path.expand("~/current_projects/Pdata"), "data", "asv_Sys.rda"),
      file.path("/Users/pgajer/current_projects/Pdata", "data", "asv_Sys.rda")
    )
    candidates <- unique(normalizePath(path.expand(candidates), mustWork = FALSE))
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) < 1L) {
      return(NULL)
    }
    env <- new.env(parent = emptyenv())
    load(candidates[[1]], envir = env)
    if (!exists("asv_Sys", envir = env, inherits = FALSE)) {
      return(NULL)
    }
    asv.sys <- get("asv_Sys", envir = env, inherits = FALSE)
    tx <- as.character(asv.sys$asv.tx %||% NULL)
    if (length(tx) < 1L) {
      return(NULL)
    }
    names(tx) <- names(asv.sys$asv.tx)
    tx
  }

  coerce_endpoint_feature_matrix <- function(x, feature_cols = NULL) {
    if (is.data.frame(x)) {
      rn <- rownames(x)
      x <- as.matrix(x)
      rownames(x) <- rn
    }
    if (!is.matrix(x)) {
      return(NULL)
    }
    storage.mode(x) <- "numeric"
    if (!is.numeric(x) || nrow(x) < 1L || ncol(x) < 1L) {
      return(NULL)
    }
    if ((is.null(colnames(x)) || any(!nzchar(as.character(colnames(x))))) &&
        length(feature_cols) == ncol(x)) {
      colnames(x) <- as.character(feature_cols)
    }
    if (is.null(colnames(x))) {
      colnames(x) <- sprintf("feature_%d", seq_len(ncol(x)))
    }
    x
  }

  endpoint_matrix_from_candidate <- function(obj) {
    if (is.matrix(obj) || is.data.frame(obj)) {
      return(obj)
    }
    if (!is.list(obj)) {
      return(NULL)
    }
    for (nm in c("matrix", "X", "graph_matrix", "feature_matrix", "data")) {
      if (!is.null(obj[[nm]]) && (is.matrix(obj[[nm]]) || is.data.frame(obj[[nm]]))) {
        return(obj[[nm]])
      }
    }
    NULL
  }

  extract_manifest_endpoint_matrices <- function(obj, provider_spec) {
    if (!is.list(provider_spec)) {
      provider_spec <- list()
    }
    feature_cols <- if (is.list(obj)) as.character(obj$feature_cols %||% character(0)) else character(0)
    container_key <- as.character(
      provider_spec$representations_object %||%
        provider_spec$matrix_list_object %||%
        provider_spec$matrices_object %||%
        ""
    )
    container <- obj
    if (nzchar(container_key) && is.list(obj) && !is.null(obj[[container_key]])) {
      container <- obj[[container_key]]
    } else if (is.list(obj) && is.list(obj$graph_representations)) {
      container <- obj$graph_representations
    } else if (is.list(obj) && is.list(obj$matrices)) {
      container <- obj$matrices
    } else if (is.list(obj) && is.list(obj$representations)) {
      container <- obj$representations
    }

    direct <- endpoint_matrix_from_candidate(container)
    if (!is.null(direct)) {
      x <- coerce_endpoint_feature_matrix(direct, feature_cols = feature_cols)
      if (is.matrix(x)) {
        return(list(default = x))
      }
      return(list())
    }

    if (!is.list(container) || length(container) < 1L) {
      return(list())
    }
    out <- list()
    nms <- names(container)
    if (is.null(nms)) {
      nms <- sprintf("matrix_%d", seq_along(container))
    }
    for (ii in seq_along(container)) {
      x_raw <- endpoint_matrix_from_candidate(container[[ii]])
      x <- coerce_endpoint_feature_matrix(x_raw, feature_cols = feature_cols)
      if (is.matrix(x)) {
        out[[as.character(nms[[ii]])]] <- x
      }
    }
    out
  }

  sample_ids_for_manifest_endpoint_matrix <- function(meta, graph_set, x, provider_spec) {
    if (!is.data.frame(meta) || nrow(meta) < 1L || !is.matrix(x)) {
      ids <- rownames(x)
      if (length(ids) == nrow(x) && all(nzchar(as.character(ids)))) {
        return(as.character(ids))
      }
      return(sprintf("v%d", seq_len(nrow(x))))
    }

    representation_col <- as.character(provider_spec$representation_col %||% provider_spec$representation_column %||% "")
    if (!nzchar(representation_col)) {
      representation_col <- first_existing_col(meta, c("representation_id", "representation", "data_type_id", "graph_set_id"))
    }
    vertex_col <- as.character(provider_spec$vertex_col %||% provider_spec$vertex_column %||% "")
    if (!nzchar(vertex_col)) {
      vertex_col <- first_existing_col(meta, c("graph_vertex_id", "vertex", "vertex_id"))
    }
    sample_col <- as.character(provider_spec$sample_col %||% provider_spec$sample_column %||% "")
    if (!nzchar(sample_col)) {
      sample_col <- first_existing_col(meta, c("first_UID", "UID", "sample_id", "sample"))
    }
    if (!nzchar(vertex_col) || !nzchar(sample_col)) {
      ids <- rownames(x)
      if (length(ids) == nrow(x) && all(nzchar(as.character(ids)))) {
        return(as.character(ids))
      }
      return(sprintf("v%d", seq_len(nrow(x))))
    }

    rows <- meta
    if (nzchar(representation_col)) {
      candidates <- unique(as.character(c(
        graph_set$representation_id,
        graph_set$representation,
        graph_set$data_type_id,
        graph_set$id
      )))
      candidates <- candidates[nzchar(candidates)]
      vals <- as.character(rows[[representation_col]])
      vals[is.na(vals)] <- ""
      if (length(candidates) > 0L && any(vals %in% candidates)) {
        rows <- rows[vals %in% candidates, , drop = FALSE]
      }
    }
    if (nrow(rows) < 1L) {
      return(sprintf("v%d", seq_len(nrow(x))))
    }
    rows <- rows[order(suppressWarnings(as.integer(rows[[vertex_col]])), na.last = TRUE), , drop = FALSE]
    ids <- as.character(rows[[sample_col]])
    ids[is.na(ids)] <- ""
    if (length(ids) >= nrow(x)) {
      ids <- ids[seq_len(nrow(x))]
      ids[!nzchar(ids)] <- sprintf("v%d", which(!nzchar(ids)))
      return(ids)
    }
    fallback <- rownames(x)
    if (length(fallback) != nrow(x) || any(!nzchar(as.character(fallback)))) {
      fallback <- sprintf("v%d", seq_len(nrow(x)))
    }
    as.character(fallback)
  }

  build_manifest_endpoint_label_provider <- function(project_id, manifest) {
    provider_spec <- NULL
    if (is.list(manifest$metadata) && is.list(manifest$metadata$endpoint_label_provider)) {
      provider_spec <- manifest$metadata$endpoint_label_provider
    } else if (is.list(manifest$endpoint_label_provider)) {
      provider_spec <- manifest$endpoint_label_provider
    }
    if (!is.list(provider_spec)) {
      return(NULL)
    }

    project_root <- as.character(manifest$project_root %||% "")
    matrix_file <- resolve_manifest_project_path(
      provider_spec$matrix_file %||%
        provider_spec$feature_matrix_file %||%
        provider_spec$input_matrices_file %||%
        provider_spec$file %||%
        provider_spec$path %||%
        "",
      project_root = project_root
    )
    if (!nzchar(matrix_file) || !file.exists(matrix_file)) {
      return(NULL)
    }
    matrix_obj <- tryCatch(readRDS(matrix_file), error = function(e) NULL)
    if (is.null(matrix_obj)) {
      return(NULL)
    }
    matrices <- extract_manifest_endpoint_matrices(matrix_obj, provider_spec)
    if (!is.list(matrices) || length(matrices) < 1L) {
      return(NULL)
    }

    metadata_file <- resolve_manifest_project_path(
      provider_spec$vertex_metadata_file %||%
        provider_spec$vertices_file %||%
        provider_spec$sample_metadata_file %||%
        "",
      project_root = project_root
    )
    vertex_meta <- read_manifest_table(metadata_file)

    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    graph_set_matrix_map <- if (is.list(provider_spec$graph_set_matrix_map)) provider_spec$graph_set_matrix_map else list()
    X_by_graph_set <- list()
    sample_ids_by_graph_set <- list()

    if (length(graph_sets) > 0L) {
      for (gs in graph_sets) {
        set_id <- as.character(gs$id %||% "")
        if (!nzchar(set_id)) {
          next
        }
        mapped_key <- as.character(graph_set_matrix_map[[set_id]] %||% "")
        candidates <- unique(as.character(c(
          mapped_key,
          gs$representation_id,
          gs$representation,
          gs$data_type_id,
          set_id,
          "default"
        )))
        candidates <- candidates[nzchar(candidates)]
        hit <- candidates[candidates %in% names(matrices)]
        if (length(hit) < 1L && length(matrices) == 1L) {
          hit <- names(matrices)[[1L]]
        }
        if (length(hit) < 1L) {
          next
        }
        x <- matrices[[hit[[1L]]]]
        X_by_graph_set[[set_id]] <- x
        sample_ids_by_graph_set[[set_id]] <- sample_ids_for_manifest_endpoint_matrix(
          meta = vertex_meta,
          graph_set = gs,
          x = x,
          provider_spec = provider_spec
        )
      }
    }

    if (length(X_by_graph_set) < 1L && length(matrices) > 0L) {
      X_by_graph_set[["default"]] <- matrices[[1L]]
      sample_ids_by_graph_set[["default"]] <- sample_ids_for_manifest_endpoint_matrix(
        meta = vertex_meta,
        graph_set = list(id = "default"),
        x = matrices[[1L]],
        provider_spec = provider_spec
      )
    }
    if (length(X_by_graph_set) < 1L) {
      return(NULL)
    }

    list(
      project_id = tolower(trimws(as.character(project_id %||% ""))),
      project_root = project_root,
      mode = as.character(provider_spec$mode %||% "manifest"),
      matrix_file = matrix_file,
      X_by_graph_set = X_by_graph_set,
      sample_ids_by_graph_set = sample_ids_by_graph_set,
      taxonomy_map = NULL,
      label_style = as.character(provider_spec$label_style %||% "taxonomy_profile"),
      source_detail = as.character(provider_spec$source_detail %||% "Manifest feature profile")
    )
  }

  build_live_endpoint_label_provider <- function(project_id, manifest) {
    pid <- tolower(trimws(as.character(project_id %||% "")))
    project_root <- as.character(manifest$project_root %||% "")
    if (!nzchar(project_root) || identical(project_root, "NA") || !dir.exists(project_root)) {
      return(NULL)
    }

    generic_provider <- build_manifest_endpoint_label_provider(project_id = pid, manifest = manifest)
    if (is.list(generic_provider) && is.list(generic_provider$X_by_graph_set)) {
      return(generic_provider)
    }

    if (identical(pid, "symptoms")) {
      data_file <- file.path(project_root, "data", "S_asv.rda")
      if (!file.exists(data_file)) {
        return(NULL)
      }
      env <- new.env(parent = emptyenv())
      load(data_file, envir = env)
      S.asv <- env$S.asv %||% NULL
      S.asv.3d <- env$S.asv.3d %||% NULL
      if (is.null(S.asv) || is.null(S.asv.3d)) {
        return(NULL)
      }
      sample_ids <- rownames(S.asv.3d)
      if (length(sample_ids) < 1L) {
        return(NULL)
      }
      X <- as.matrix(S.asv[sample_ids, , drop = FALSE])
      if (!is.numeric(X) || nrow(X) < 1L || ncol(X) < 1L) {
        return(NULL)
      }
      taxonomy_map <- load_symptoms_taxonomy_map_for_ui(project_root)
      list(
        project_id = pid,
        project_root = project_root,
        mode = "symptoms",
        sample_ids = as.character(sample_ids),
        X = X,
        taxonomy_map = taxonomy_map
      )
    } else if (identical(pid, "agp")) {
      data_file <- file.path(project_root, "data", "AGP_gg2_tx_relAb_tbl.rda")
      sample_set_file <- file.path(project_root, "results", "frozen_inputs", "sample_sets.rds")
      if (!file.exists(data_file)) {
        return(NULL)
      }
      env <- new.env(parent = emptyenv())
      load(data_file, envir = env)
      S.agp <- env$S.agp %||% NULL
      if (is.null(S.agp)) {
        return(NULL)
      }
      sample_ids <- rownames(S.agp)
      if (file.exists(sample_set_file)) {
        ss <- tryCatch(readRDS(sample_set_file), error = function(e) NULL)
        use_ids <- as.character(ss$asv_mt %||% character(0))
        use_ids <- intersect(use_ids, rownames(S.agp))
        if (length(use_ids) > 0L) {
          sample_ids <- use_ids
        }
      }
      if (length(sample_ids) < 1L) {
        return(NULL)
      }
      X <- as.matrix(S.agp[sample_ids, , drop = FALSE])
      if (!is.numeric(X) || nrow(X) < 1L || ncol(X) < 1L) {
        return(NULL)
      }
      list(
        project_id = pid,
        project_root = project_root,
        mode = "agp",
        sample_ids = as.character(sample_ids),
        X = X,
        taxonomy_map = NULL
      )
    } else {
      NULL
    }
  }

  resolve_live_endpoint_label_provider <- function(project_id, manifest) {
    pid <- tolower(trimws(as.character(project_id %||% "")))
    project_root <- as.character(manifest$project_root %||% "")
    cache_key <- paste(pid, normalizePath(path.expand(project_root), mustWork = FALSE), sep = "|")
    if (exists(cache_key, envir = endpoint_live_label_provider_cache, inherits = FALSE)) {
      return(get(cache_key, envir = endpoint_live_label_provider_cache, inherits = FALSE))
    }
    provider <- build_live_endpoint_label_provider(project_id = pid, manifest = manifest)
    assign(cache_key, provider, envir = endpoint_live_label_provider_cache)
    provider
  }

  empty_subject_sample_rows <- function() {
    data.frame(
      vertex = integer(0),
      subject_id = character(0),
      sample_id = character(0),
      week = integer(0),
      day = integer(0),
      time_order = numeric(0),
      visit_label = character(0),
      graph_set_id = character(0),
      representation_id = character(0),
      stringsAsFactors = FALSE
    )
  }

  resolve_manifest_subject_provider_path <- function(path, project_root) {
    resolve_manifest_project_path(path = path, project_root = project_root)
  }

  read_manifest_subject_provider_rows <- function(path) {
    read_manifest_table(path)
  }

  build_manifest_subject_provider <- function(project_id, manifest) {
    provider_spec <- NULL
    if (is.list(manifest$metadata) && is.list(manifest$metadata$subject_provider)) {
      provider_spec <- manifest$metadata$subject_provider
    } else if (is.list(manifest$subject_provider)) {
      provider_spec <- manifest$subject_provider
    }
    if (!is.list(provider_spec)) {
      return(NULL)
    }

    project_root <- as.character(manifest$project_root %||% "")
    rows_file <- resolve_manifest_subject_provider_path(
      provider_spec$rows_file %||% provider_spec$file %||% provider_spec$path %||% "",
      project_root = project_root
    )
    raw <- read_manifest_subject_provider_rows(rows_file)
    if (!is.data.frame(raw) || nrow(raw) < 1L) {
      return(NULL)
    }

    resolve_col <- function(keys, defaults = character(0), required = FALSE) {
      candidates <- unique(c(
        unlist(provider_spec[keys], recursive = TRUE, use.names = FALSE),
        defaults
      ))
      candidates <- as.character(candidates)
      candidates <- candidates[nzchar(candidates)]
      hit <- candidates[candidates %in% names(raw)]
      if (length(hit) > 0L) {
        return(hit[[1]])
      }
      if (isTRUE(required)) {
        return(NA_character_)
      }
      ""
    }

    vertex_col <- resolve_col(
      c("vertex_col", "vertex_column"),
      defaults = c("vertex", "graph_vertex_id", "vertex_id"),
      required = TRUE
    )
    subject_col <- resolve_col(
      c("subject_col", "subject_column"),
      defaults = c("subject_id", "subjID", "subject"),
      required = TRUE
    )
    if (is.na(vertex_col) || is.na(subject_col)) {
      return(NULL)
    }

    sample_col <- resolve_col(c("sample_col", "sample_column"), defaults = c("sample_id", "UID", "sample"))
    week_col <- resolve_col(c("week_col", "week_column"), defaults = c("week", "week_num", "WEEK"))
    day_col <- resolve_col(c("day_col", "day_column"), defaults = c("day", "day_num", "DAY"))
    order_col <- resolve_col(c("order_col", "time_col", "time_order_col"), defaults = c("time_order", "time_idx", "visit_order"))
    visit_col <- resolve_col(c("visit_label_col", "visit_col"), defaults = c("visit_label", "visit"))
    graph_set_col <- resolve_col(c("graph_set_col", "graph_set_column"), defaults = c("graph_set_id", "graph_set"))
    representation_col <- resolve_col(c("representation_col", "representation_column"), defaults = c("representation_id", "representation"))

    vertex <- suppressWarnings(as.integer(raw[[vertex_col]]))
    subject_id <- trimws(as.character(raw[[subject_col]]))
    sample_id <- if (nzchar(sample_col)) as.character(raw[[sample_col]]) else sprintf("v%d", vertex)
    week <- if (nzchar(week_col)) suppressWarnings(as.integer(raw[[week_col]])) else rep(NA_integer_, nrow(raw))
    day <- if (nzchar(day_col)) suppressWarnings(as.integer(raw[[day_col]])) else rep(NA_integer_, nrow(raw))
    time_order <- if (nzchar(order_col)) suppressWarnings(as.numeric(raw[[order_col]])) else rep(NA_real_, nrow(raw))
    visit_label <- if (nzchar(visit_col)) as.character(raw[[visit_col]]) else rep("", nrow(raw))
    graph_set_id <- if (nzchar(graph_set_col)) as.character(raw[[graph_set_col]]) else rep("", nrow(raw))
    representation_id <- if (nzchar(representation_col)) as.character(raw[[representation_col]]) else rep("", nrow(raw))

    subject_id[is.na(subject_id)] <- ""
    sample_id[is.na(sample_id)] <- ""
    visit_label[is.na(visit_label)] <- ""
    graph_set_id[is.na(graph_set_id)] <- ""
    representation_id[is.na(representation_id)] <- ""

    missing_visit <- !nzchar(visit_label) & (is.finite(week) | is.finite(day))
    if (any(missing_visit)) {
      visit_label[missing_visit] <- sprintf(
        "W%sD%s",
        ifelse(is.finite(week[missing_visit]), as.character(week[missing_visit]), "?"),
        ifelse(is.finite(day[missing_visit]), as.character(day[missing_visit]), "?")
      )
    }

    rows <- data.frame(
      vertex = vertex,
      subject_id = subject_id,
      sample_id = sample_id,
      week = week,
      day = day,
      time_order = time_order,
      visit_label = visit_label,
      graph_set_id = graph_set_id,
      representation_id = representation_id,
      stringsAsFactors = FALSE
    )
    rows <- rows[is.finite(rows$vertex) & rows$vertex >= 1L & nzchar(rows$subject_id), , drop = FALSE]
    if (nrow(rows) < 1L) {
      return(NULL)
    }

    list(
      project_id = tolower(trimws(as.character(project_id %||% ""))),
      project_root = project_root,
      mode = as.character(provider_spec$mode %||% "manifest"),
      rows_file = rows_file,
      rows = rows
    )
  }

  build_live_subject_provider <- function(project_id, manifest) {
    pid <- tolower(trimws(as.character(project_id %||% "")))
    project_root <- as.character(manifest$project_root %||% "")
    if (!nzchar(project_root) || identical(project_root, "NA") || !dir.exists(project_root)) {
      return(NULL)
    }

    generic_provider <- build_manifest_subject_provider(project_id = pid, manifest = manifest)
    if (is.list(generic_provider) && is.data.frame(generic_provider$rows)) {
      return(generic_provider)
    }

    if (!identical(pid, "symptoms")) {
      return(NULL)
    }

    data_file <- file.path(project_root, "data", "S_asv.rda")
    if (!file.exists(data_file)) {
      return(NULL)
    }

    env <- new.env(parent = emptyenv())
    load(data_file, envir = env)
    S.asv.3d <- env$S.asv.3d %||% NULL
    mt.asv <- env$mt.asv %||% NULL
    if (is.null(S.asv.3d) || !is.data.frame(mt.asv)) {
      return(NULL)
    }

    sample_ids <- rownames(S.asv.3d)
    if (length(sample_ids) < 1L || !all(sample_ids %in% rownames(mt.asv))) {
      return(NULL)
    }

    meta <- mt.asv[sample_ids, , drop = FALSE]
    subject_id <- trimws(as.character(meta$subjID %||% rep("", nrow(meta))))
    subject_id[is.na(subject_id)] <- ""
    keep <- nzchar(subject_id)
    if (!any(keep)) {
      return(NULL)
    }

    week <- suppressWarnings(as.integer(meta$WEEK %||% rep(NA_integer_, nrow(meta))))
    day <- suppressWarnings(as.integer(meta$DAY %||% rep(NA_integer_, nrow(meta))))
    visit_label <- rep("", length(sample_ids))
    have_visit <- is.finite(week) | is.finite(day)
    visit_label[have_visit] <- sprintf(
      "W%sD%s",
      ifelse(is.finite(week[have_visit]), as.character(week[have_visit]), "?"),
      ifelse(is.finite(day[have_visit]), as.character(day[have_visit]), "?")
    )

    rows <- data.frame(
      vertex = seq_along(sample_ids),
      subject_id = subject_id,
      sample_id = as.character(sample_ids),
      week = week,
      day = day,
      time_order = seq_along(sample_ids),
      visit_label = visit_label,
      graph_set_id = "",
      representation_id = "",
      stringsAsFactors = FALSE
    )
    rows <- rows[keep, , drop = FALSE]
    if (nrow(rows) < 1L) {
      return(NULL)
    }

    list(
      project_id = pid,
      project_root = project_root,
      mode = "symptoms",
      rows = rows
    )
  }

  resolve_live_subject_provider <- function(project_id, manifest) {
    pid <- tolower(trimws(as.character(project_id %||% "")))
    project_root <- as.character(manifest$project_root %||% "")
    cache_key <- paste(pid, normalizePath(path.expand(project_root), mustWork = FALSE), sep = "|")
    if (exists(cache_key, envir = subject_live_provider_cache, inherits = FALSE)) {
      return(get(cache_key, envir = subject_live_provider_cache, inherits = FALSE))
    }
    provider <- build_live_subject_provider(project_id = pid, manifest = manifest)
    assign(cache_key, provider, envir = subject_live_provider_cache)
    provider
  }

  endpoint_provider_active_view <- function(provider) {
    if (!is.list(provider)) {
      return(NULL)
    }
    if (is.matrix(provider$X)) {
      return(list(
        X = provider$X,
        sample_ids = as.character(provider$sample_ids %||% rownames(provider$X) %||% character(0)),
        graph_set_id = ""
      ))
    }
    if (!is.list(provider$X_by_graph_set) || length(provider$X_by_graph_set) < 1L) {
      return(NULL)
    }
    ctx <- current_endpoint_graph_context()
    set_id <- if (is.list(ctx)) as.character(ctx$graph_set_id %||% "") else ""
    if (!nzchar(set_id) || is.null(provider$X_by_graph_set[[set_id]])) {
      set_id <- names(provider$X_by_graph_set)[[1L]]
    }
    x <- provider$X_by_graph_set[[set_id]]
    if (!is.matrix(x)) {
      return(NULL)
    }
    sample_ids <- if (is.list(provider$sample_ids_by_graph_set)) {
      as.character(provider$sample_ids_by_graph_set[[set_id]] %||% character(0))
    } else {
      character(0)
    }
    if (length(sample_ids) != nrow(x)) {
      sample_ids <- rownames(x)
    }
    if (length(sample_ids) != nrow(x) || any(!nzchar(as.character(sample_ids)))) {
      sample_ids <- sprintf("v%d", seq_len(nrow(x)))
    }
    list(
      X = x,
      sample_ids = as.character(sample_ids),
      graph_set_id = set_id
    )
  }

  live_endpoint_label_profile_suggestion <- function(vertex_id, manifest) {
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L || !is.list(manifest)) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }
    provider <- resolve_live_endpoint_label_provider(rv$project.id, manifest)
    provider_view <- endpoint_provider_active_view(provider)
    if (!is.list(provider) || !is.list(provider_view) || !is.matrix(provider_view$X)) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }
    X_use <- provider_view$X
    sample_ids_use <- as.character(provider_view$sample_ids %||% character(0))
    if (as.integer(vid) > nrow(X_use)) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }

    x <- as.numeric(X_use[as.integer(vid), , drop = TRUE])
    if (length(x) < 1L || all(!is.finite(x))) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }
    ord <- order(x, decreasing = TRUE, na.last = NA)
    keep_idx <- head(ord, 5L)
    keep_idx <- keep_idx[is.finite(keep_idx) & keep_idx >= 1L & keep_idx <= ncol(X_use)]
    if (length(keep_idx) < 1L) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }

    feature_ids <- as.character(colnames(X_use)[keep_idx])
    abund <- x[keep_idx]
    if (identical(provider$mode, "symptoms")) {
      taxonomy <- as.character(provider$taxonomy_map[feature_ids] %||% feature_ids)
      taxonomy[is.na(taxonomy) | !nzchar(taxonomy)] <- feature_ids[is.na(taxonomy) | !nzchar(taxonomy)]
      taxonomy <- vapply(taxonomy, clean_taxonomy_label_for_ui, FUN.VALUE = character(1))
      label_val <- label_from_taxonomy_profile(taxonomy, abund, separator = " / ")
      profile_tbl <- normalize_endpoint_feature_profile(data.frame(
        rank = seq_along(feature_ids),
        feature = feature_ids,
        taxonomy = taxonomy,
        abundance = abund,
        stringsAsFactors = FALSE
      ))
      return(list(
        vertex = as.integer(vid),
        label = as.character(label_val %||% NA_character_),
        sample_id = as.character(sample_ids_use[[as.integer(vid)]] %||% NA_character_),
        profile = profile_tbl,
        source_kind = "live",
        source_detail = "Symptoms project ASV profile"
      ))
    }

    taxonomy <- as.character(provider$taxonomy_map[feature_ids] %||% feature_ids)
    taxonomy[is.na(taxonomy) | !nzchar(taxonomy)] <- feature_ids[is.na(taxonomy) | !nzchar(taxonomy)]
    taxonomy <- vapply(taxonomy, clean_taxonomy_label_for_ui, FUN.VALUE = character(1))
    if (identical(as.character(provider$label_style %||% ""), "abbrev")) {
      above <- which(is.finite(abund) & abund >= 0.05)
      if (length(above) < 1L) {
        above <- 1L
      }
      pick <- head(above, 2L)
      label_val <- paste(vapply(feature_ids[pick], abbrev_taxon_for_ui, FUN.VALUE = character(1)), collapse = "")
    } else {
      label_val <- label_from_taxonomy_profile(taxonomy, abund, separator = " / ")
    }
    profile_tbl <- normalize_endpoint_feature_profile(data.frame(
      rank = seq_along(feature_ids),
      feature = feature_ids,
      taxonomy = taxonomy,
      abundance = abund,
      stringsAsFactors = FALSE
    ))
    list(
      vertex = as.integer(vid),
      label = as.character(label_val %||% NA_character_),
      sample_id = as.character(sample_ids_use[[as.integer(vid)]] %||% NA_character_),
      profile = profile_tbl,
      source_kind = "live",
      source_detail = as.character(provider$source_detail %||% "Live feature profile")
    )
  }

  endpoint_label_profile_suggestion <- function(vertex_id, panel_state = NULL) {
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L) {
      return(empty_endpoint_label_profile_suggestion(vertex_id))
    }
    state_use <- if (is.list(panel_state)) panel_state else endpoint_panel_state()
    rows_df <- if (is.list(state_use) && is.data.frame(state_use$rows)) state_use$rows else data.frame()
    precomputed <- precomputed_endpoint_label_profile_suggestion(vid, rows_df)
    if (is.list(precomputed) &&
        is.data.frame(precomputed$profile) &&
        nrow(precomputed$profile) > 0L &&
        nzchar(as.character(precomputed$label %||% ""))) {
      return(precomputed)
    }
    manifest <- active_manifest()
    live_endpoint_label_profile_suggestion(vid, manifest = manifest)
  }

  upsert_working_endpoint_vertex_state <- function(
      state,
      vertex_id,
      label = NULL,
      source_type = "manual",
      source_dataset_id = "") {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_endpoint_rows()
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L) {
      return(out)
    }

    label_use <- as.character(label %||% sprintf("v%d", as.integer(vid)))
    if (length(label_use) < 1L || !nzchar(label_use[[1]])) {
      label_use <- sprintf("v%d", as.integer(vid))
    } else {
      label_use <- label_use[[1]]
    }
    ts <- .gflowui_now()
    hit <- which(rows$vertex == as.integer(vid))
    if (length(hit) < 1L) {
      rows <- rbind(
        rows,
        data.frame(
          vertex = as.integer(vid),
          accepted = TRUE,
          visible = TRUE,
          label = label_use,
          auto_label = label_use,
          source_type = as.character(source_type %||% "manual"),
          source_dataset_id = as.character(source_dataset_id %||% ""),
          manually_added = TRUE,
          manually_removed = FALSE,
          notes = "",
          updated_at = ts,
          stringsAsFactors = FALSE
        )
      )
    } else {
      ii <- hit[[1]]
      rows$accepted[[ii]] <- TRUE
      rows$visible[[ii]] <- TRUE
      rows$manually_removed[[ii]] <- FALSE
      if (!nzchar(as.character(rows$label[[ii]] %||% ""))) {
        rows$label[[ii]] <- label_use
      }
      if (!nzchar(as.character(rows$auto_label[[ii]] %||% ""))) {
        rows$auto_label[[ii]] <- label_use
      }
      rows$updated_at[[ii]] <- ts
    }

    out$rows <- rows
    out$updated_at <- ts
    working_endpoint_mark_modified(out)
  }

  normalize_working_endpoint_label <- function(label, vertex_id, auto_label = NULL) {
    vid <- suppressWarnings(as.integer(vertex_id))
    fallback <- as.character(auto_label %||% "")
    if (!nzchar(fallback)) {
      fallback <- sprintf("v%d", as.integer(vid))
    }
    lbl <- trimws(as.character(label %||% ""))
    if (length(lbl) < 1L || !nzchar(lbl[[1]])) {
      return(fallback)
    }
    lbl[[1]]
  }

  update_working_endpoint_vertex_label_state <- function(state, vertex_id, label) {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_endpoint_rows()
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L || nrow(rows) < 1L) {
      return(out)
    }
    hit <- which(rows$vertex == as.integer(vid))
    if (length(hit) < 1L) {
      return(out)
    }
    ii <- hit[[1]]
    rows$label[[ii]] <- normalize_working_endpoint_label(
      label = label,
      vertex_id = vid,
      auto_label = rows$auto_label[[ii]] %||% sprintf("v%d", as.integer(vid))
    )
    rows$updated_at[[ii]] <- .gflowui_now()
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_endpoint_mark_modified(out)
  }

  set_working_endpoint_visibility_state <- function(state, vertex_id, visible = TRUE) {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_endpoint_rows()
    vid <- suppressWarnings(as.integer(vertex_id))
    visible_flag <- isTRUE(visible)
    if (!is.finite(vid) || vid < 1L || nrow(rows) < 1L) {
      return(out)
    }
    hit <- which(rows$vertex == as.integer(vid))
    if (length(hit) < 1L) {
      return(out)
    }
    ii <- hit[[1]]
    rows$visible[[ii]] <- visible_flag
    rows$updated_at[[ii]] <- .gflowui_now()
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_endpoint_mark_modified(out)
  }

  hide_working_endpoint_vertex_state <- function(state, vertex_id) {
    set_working_endpoint_visibility_state(state = state, vertex_id = vertex_id, visible = FALSE)
  }

  restore_working_endpoint_vertex_state <- function(state, vertex_id) {
    set_working_endpoint_visibility_state(state = state, vertex_id = vertex_id, visible = TRUE)
  }

  remove_working_endpoint_vertex_state <- function(state, vertex_id) {
    out <- sanitize_working_endpoint_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_endpoint_rows()
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L || nrow(rows) < 1L) {
      return(out)
    }
    rows <- rows[rows$vertex != as.integer(vid), , drop = FALSE]
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_endpoint_mark_modified(out)
  }

  read_endpoint_dataset_from_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(list(vertices = integer(0), labels = character(0)))
    }
    row <- row_df[1, , drop = FALSE]
    source_type <- tolower(as.character(row$source_type[[1]] %||% "manifest"))
    if (identical(source_type, "workspace")) {
      ds <- read_workspace_endpoint_dataset(as.character(row$workspace_file[[1]] %||% ""))
      if (is.list(ds)) {
        return(list(vertices = ds$vertices, labels = ds$labels))
      }
      return(list(vertices = integer(0), labels = character(0)))
    }
    if (identical(source_type, "external_rds")) {
      return(read_external_endpoint_dataset(row_df))
    }
    read_endpoint_labels_from_row(row_df)
  }

  current_endpoint_graph_context <- shiny::reactive({
    if (!isTRUE(rv$project.active) || !nzchar(rv$project.id %||% "")) {
      return(NULL)
    }
    gs <- graph_structure_state()
    if (!is.list(gs) || !is.null(gs$error)) {
      return(NULL)
    }
    graph_set_id <- as.character(gs$set_id %||% "")
    k_val <- suppressWarnings(as.integer(gs$k_selected %||% input$graph_k %||% NA_integer_))
    if (!nzchar(graph_set_id) || !is.finite(k_val) || k_val < 1L) {
      return(NULL)
    }
    list(
      project_id = as.character(rv$project.id %||% ""),
      graph_set_id = graph_set_id,
      k = as.integer(k_val)
    )
  })

  subject_vertex_color_choices <- function() {
    c(
      "Red" = "#dc2626",
      "Orange" = "#f97316",
      "Blue" = "#2563eb",
      "Green" = "#16a34a",
      "Purple" = "#8b5cf6",
      "Black" = "#111827"
    )
  }

  subject_overlay_palette <- function(n) {
    rep_len(
      c("#dc2626", "#2563eb", "#16a34a", "#f97316", "#8b5cf6", "#0891b2", "#a16207", "#db2777"),
      max(1L, suppressWarnings(as.integer(n %||% 1L)))
    )
  }

  subject_edge_mode_choices <- c(
    "None" = "none",
    "Graph edges among subject vertices" = "graph",
    "Temporal trajectory" = "temporal"
  )

  shiny::observe({
    subject_ids_val <- input$subject_ids
    if (!is.null(subject_ids_val)) {
      set_reactive_field_if_changed(
        subject_state,
        "selected_ids",
        unique(as.character(subject_ids_val %||% character(0)))
      )
    }
    show_val <- input$subject_show_overlay
    if (!is.null(show_val)) {
      set_reactive_field_if_changed(subject_state, "show_overlay", isTRUE(show_val))
    }
    dim_val <- input$subject_dim_background
    if (!is.null(dim_val)) {
      set_reactive_field_if_changed(subject_state, "dim_background", isTRUE(dim_val))
    }
    bg_opacity_val <- suppressWarnings(as.numeric(input$subject_background_opacity %||% NA_real_))
    if (is.finite(bg_opacity_val) && bg_opacity_val > 0 && bg_opacity_val <= 1) {
      set_reactive_field_if_changed(subject_state, "background_opacity", as.numeric(bg_opacity_val))
    }
    color_val <- as.character(input$subject_vertex_color %||% "")
    if (length(color_val) > 0L && nzchar(color_val[[1]])) {
      set_reactive_field_if_changed(
        subject_state,
        "vertex_color",
        normalize_palette_choice(
          color_val[[1]],
          subject_vertex_color_choices(),
          default = "#dc2626"
        )
      )
    }
    size_val <- suppressWarnings(as.numeric(input$subject_vertex_size %||% NA_real_))
    if (is.finite(size_val) && size_val > 0) {
      set_reactive_field_if_changed(subject_state, "vertex_size", as.numeric(size_val))
    }
    edge_mode_val <- as.character(input$subject_edge_mode %||% "")
    if (edge_mode_val %in% unname(subject_edge_mode_choices)) {
      set_reactive_field_if_changed(subject_state, "edge_mode", edge_mode_val)
    }
    edge_color_val <- as.character(input$subject_edge_color %||% "")
    if (length(edge_color_val) > 0L && nzchar(edge_color_val[[1]])) {
      set_reactive_field_if_changed(
        subject_state,
        "edge_color",
        normalize_palette_choice(
          edge_color_val[[1]],
          subject_vertex_color_choices(),
          default = "#dc2626"
        )
      )
    }
    edge_width_val <- suppressWarnings(as.numeric(input$subject_edge_width %||% NA_real_))
    if (is.finite(edge_width_val) && edge_width_val > 0) {
      set_reactive_field_if_changed(subject_state, "edge_width", as.numeric(edge_width_val))
    }
    label_mode_val <- as.character(input$subject_label_mode %||% "")
    if (nzchar(label_mode_val)) {
      set_reactive_field_if_changed(subject_state, "label_mode", label_mode_val)
    }
    label_size_val <- suppressWarnings(as.numeric(input$subject_label_size %||% NA_real_))
    if (is.finite(label_size_val) && label_size_val > 0) {
      set_reactive_field_if_changed(subject_state, "label_size", as.numeric(label_size_val))
    }
  })

  subject_panel_state <- shiny::reactive({
    manifest <- active_manifest()
    provider <- if (is.list(manifest)) resolve_live_subject_provider(rv$project.id, manifest) else NULL
    rows <- if (is.list(provider) && is.data.frame(provider$rows)) provider$rows else empty_subject_sample_rows()
    sel <- current_graph_selection()
    active_set_id <- if (is.list(sel) && is.null(sel$error)) as.character(sel$set_id %||% "") else ""
    if (is.data.frame(rows) && nrow(rows) > 0L && nzchar(active_set_id) && "graph_set_id" %in% names(rows)) {
      row_set <- as.character(rows$graph_set_id %||% rep("", nrow(rows)))
      row_set[is.na(row_set)] <- ""
      if (any(nzchar(row_set))) {
        rows <- rows[!nzchar(row_set) | row_set == active_set_id, , drop = FALSE]
      }
    }
    n_vertices <- reference_vertex_count()
    if (is.data.frame(rows) && nrow(rows) > 0L && is.finite(n_vertices) && n_vertices > 0L) {
      rows <- rows[rows$vertex >= 1L & rows$vertex <= as.integer(n_vertices), , drop = FALSE]
    }
    if (!is.data.frame(rows) || nrow(rows) < 1L) {
      return(list(
        available = FALSE,
        provider = provider,
        rows = empty_subject_sample_rows(),
        subject_choices = c("Choose subject..." = ""),
        selected_ids = character(0),
        selected_id = "",
        selected_rows = empty_subject_sample_rows(),
        show_overlay = isTRUE(subject_state$show_overlay),
        dim_background = isTRUE(subject_state$dim_background),
        background_opacity = as.numeric(subject_state$background_opacity %||% 0.22),
        vertex_color = as.character(subject_state$vertex_color %||% "#dc2626"),
        vertex_size = as.numeric(subject_state$vertex_size %||% 1.0),
        edge_mode = as.character(subject_state$edge_mode %||% "none"),
        edge_color = as.character(subject_state$edge_color %||% "#dc2626"),
        edge_width = as.numeric(subject_state$edge_width %||% 2),
        label_mode = "none",
        label_choices = c("None" = "none", "Vertex ID" = "vertex"),
        label_size = as.numeric(subject_state$label_size %||% 1.0)
      ))
    }

    subject_ids <- sort(unique(as.character(rows$subject_id)))
    subject_ids <- subject_ids[nzchar(subject_ids)]
    counts <- vapply(subject_ids, function(ss) sum(as.character(rows$subject_id) == ss, na.rm = TRUE), integer(1))
    choices <- c("Choose subject..." = "", stats::setNames(subject_ids, sprintf("%s (%d)", subject_ids, counts)))
    selected_ids <- unique(as.character(subject_state$selected_ids %||% character(0)))
    selected_ids <- selected_ids[nzchar(selected_ids) & selected_ids %in% subject_ids]
    selected_rows <- if (length(selected_ids) > 0L) {
      rows[as.character(rows$subject_id) %in% selected_ids, , drop = FALSE]
    } else {
      empty_subject_sample_rows()
    }
    if (nrow(selected_rows) > 0L) {
      time_order <- if ("time_order" %in% names(selected_rows)) {
        suppressWarnings(as.numeric(selected_rows$time_order))
      } else {
        rep(NA_real_, nrow(selected_rows))
      }
      ord <- order(
        as.character(selected_rows$subject_id),
        time_order,
        suppressWarnings(as.integer(selected_rows$week)),
        suppressWarnings(as.integer(selected_rows$day)),
        as.character(selected_rows$sample_id),
        na.last = TRUE
      )
      selected_rows <- selected_rows[ord, , drop = FALSE]
    }

    label_choices <- c("None" = "none", "Vertex ID" = "vertex", "Sample Order" = "order")
    if (nrow(selected_rows) > 0L && any(nzchar(as.character(selected_rows$sample_id %||% character(0))))) {
      label_choices <- c(label_choices, "Sample ID" = "sample")
    }
    visit_labels <- as.character(selected_rows$visit_label %||% character(0))
    has_visit_info <- nrow(selected_rows) > 0L && (
      any(nzchar(visit_labels)) ||
        any(is.finite(suppressWarnings(as.integer(selected_rows$week)))) ||
        any(is.finite(suppressWarnings(as.integer(selected_rows$day))))
    )
    if (isTRUE(has_visit_info)) {
      label_choices <- c(label_choices, "Visit" = "visit")
    }
    label_mode_use <- as.character(subject_state$label_mode %||% "none")
    if (!(label_mode_use %in% unname(label_choices))) {
      label_mode_use <- "none"
    }
    edge_mode_use <- as.character(subject_state$edge_mode %||% "none")
    if (!(edge_mode_use %in% unname(subject_edge_mode_choices))) {
      edge_mode_use <- "none"
    }

    list(
      available = TRUE,
      provider = provider,
      rows = rows,
      subject_choices = choices,
      selected_ids = selected_ids,
      selected_id = if (length(selected_ids) > 0L) selected_ids[[1]] else "",
      selected_rows = selected_rows,
      show_overlay = isTRUE(subject_state$show_overlay),
      dim_background = isTRUE(subject_state$dim_background),
      background_opacity = as.numeric(subject_state$background_opacity %||% 0.22),
      vertex_color = normalize_palette_choice(
        subject_state$vertex_color %||% "#dc2626",
        subject_vertex_color_choices(),
        default = "#dc2626"
      ),
      vertex_size = as.numeric(subject_state$vertex_size %||% 1.0),
      edge_mode = edge_mode_use,
      edge_color = normalize_palette_choice(
        subject_state$edge_color %||% "#dc2626",
        subject_vertex_color_choices(),
        default = "#dc2626"
      ),
      edge_width = as.numeric(subject_state$edge_width %||% 2),
      label_mode = label_mode_use,
      label_choices = label_choices,
      label_size = as.numeric(subject_state$label_size %||% 1.0)
    )
  })

  subject_overlay_active <- shiny::reactive({
    build_subject_label_text <- function(rows_df, mode = "none") {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
        return(character(0))
      }
      mode_use <- as.character(mode %||% "none")
      if (identical(mode_use, "vertex")) {
        return(sprintf("v%d", suppressWarnings(as.integer(rows_df$vertex))))
      }
      if (identical(mode_use, "sample")) {
        labs <- as.character(rows_df$sample_id %||% rep("", nrow(rows_df)))
        labs[is.na(labs)] <- ""
        return(labs)
      }
      if (identical(mode_use, "order")) {
        return(as.character(seq_len(nrow(rows_df))))
      }
      if (identical(mode_use, "visit")) {
        visit_label <- as.character(rows_df$visit_label %||% rep("", nrow(rows_df)))
        visit_label[is.na(visit_label)] <- ""
        need_fallback <- !nzchar(visit_label)
        if (any(need_fallback)) {
          week <- suppressWarnings(as.integer(rows_df$week %||% rep(NA_integer_, nrow(rows_df))))
          day <- suppressWarnings(as.integer(rows_df$day %||% rep(NA_integer_, nrow(rows_df))))
          visit_label[need_fallback] <- sprintf(
            "W%sD%s",
            ifelse(is.finite(week[need_fallback]), as.character(week[need_fallback]), "?"),
            ifelse(is.finite(day[need_fallback]), as.character(day[need_fallback]), "?")
          )
        }
        return(visit_label)
      }
      rep("", nrow(rows_df))
    }

    compute_subject_edges <- function(vertices, adj_list) {
      verts <- suppressWarnings(as.integer(vertices %||% integer(0)))
      verts <- sort(unique(verts[is.finite(verts) & verts >= 1L]))
      if (length(verts) < 2L || !is.list(adj_list) || length(adj_list) < max(verts)) {
        return(matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))))
      }
      in_set <- rep.int(FALSE, length(adj_list))
      in_set[verts] <- TRUE
      edge_rows <- lapply(verts, function(vv) {
        nb <- suppressWarnings(as.integer(adj_list[[vv]] %||% integer(0)))
        nb <- nb[is.finite(nb) & nb >= 1L & nb <= length(adj_list)]
        nb <- nb[in_set[nb] & nb > vv]
        if (length(nb) < 1L) {
          return(NULL)
        }
        cbind(from = rep.int(vv, length(nb)), to = nb)
      })
      edge_rows <- Filter(Negate(is.null), edge_rows)
      if (length(edge_rows) < 1L) {
        return(matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))))
      }
      out <- do.call(rbind, edge_rows)
      if (!is.matrix(out)) {
        out <- matrix(as.integer(out), ncol = 2L, byrow = TRUE)
      }
      storage.mode(out) <- "integer"
      colnames(out) <- c("from", "to")
      out
    }

    compute_subject_temporal_edges <- function(rows_df) {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 2L) {
        return(matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))))
      }
      rows_use <- rows_df
      time_order <- if ("time_order" %in% names(rows_use)) {
        suppressWarnings(as.numeric(rows_use$time_order))
      } else {
        rep(NA_real_, nrow(rows_use))
      }
      ord <- order(
        time_order,
        suppressWarnings(as.integer(rows_use$week)),
        suppressWarnings(as.integer(rows_use$day)),
        as.character(rows_use$sample_id),
        na.last = TRUE
      )
      rows_use <- rows_use[ord, , drop = FALSE]
      verts <- suppressWarnings(as.integer(rows_use$vertex))
      if (length(verts) < 2L) {
        return(matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))))
      }
      edges <- cbind(from = verts[-length(verts)], to = verts[-1L])
      keep <- is.finite(edges[, 1]) & is.finite(edges[, 2]) &
        edges[, 1] >= 1L & edges[, 2] >= 1L &
        edges[, 1] != edges[, 2]
      edges <- edges[keep, , drop = FALSE]
      if (nrow(edges) < 1L) {
        return(matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))))
      }
      storage.mode(edges) <- "integer"
      colnames(edges) <- c("from", "to")
      edges
    }

    build_subject_color_map <- function(subject_ids, single_color) {
      ids <- unique(as.character(subject_ids %||% character(0)))
      ids <- ids[nzchar(ids)]
      if (length(ids) < 1L) {
        return(structure(character(0), names = character(0)))
      }
      if (length(ids) == 1L) {
        cols <- normalize_palette_choice(
          single_color %||% "#dc2626",
          subject_vertex_color_choices(),
          default = "#dc2626"
        )
        return(stats::setNames(cols, ids))
      }
      stats::setNames(subject_overlay_palette(length(ids)), ids)
    }

    if (!isTRUE(subject_state$show_overlay)) {
      return(list(
        vertices = integer(0),
        rows = empty_subject_sample_rows(),
        edges = matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))),
        edge_groups = list(),
        hover_text = character(0),
        label_text = character(0),
        vertex_subject_ids = character(0),
        vertex_colors = character(0),
        color = normalize_palette_choice(
          subject_state$vertex_color %||% "#dc2626",
          subject_vertex_color_choices(),
          default = "#dc2626"
        ),
        size = as.numeric(subject_state$vertex_size %||% 1.0),
        dim_background = isTRUE(subject_state$dim_background),
        background_opacity = as.numeric(subject_state$background_opacity %||% 0.22),
        edge_color = normalize_palette_choice(
          subject_state$edge_color %||% "#dc2626",
          subject_vertex_color_choices(),
          default = "#dc2626"
        ),
        edge_width = as.numeric(subject_state$edge_width %||% 2),
        label_size = as.numeric(subject_state$label_size %||% 1.0),
        subject_id = ""
      ))
    }
    st <- subject_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$selected_rows)) st$selected_rows else empty_subject_sample_rows()
    if (nrow(rows) < 1L) {
      return(list(
        vertices = integer(0),
        rows = empty_subject_sample_rows(),
        edges = matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to"))),
        edge_groups = list(),
        hover_text = character(0),
        label_text = character(0),
        vertex_subject_ids = character(0),
        vertex_colors = character(0),
        color = as.character(st$vertex_color %||% "#dc2626"),
        size = as.numeric(st$vertex_size %||% 1.0),
        dim_background = isTRUE(st$dim_background),
        background_opacity = as.numeric(st$background_opacity %||% 0.22),
        edge_color = as.character(st$edge_color %||% "#dc2626"),
        edge_width = as.numeric(st$edge_width %||% 2),
        label_size = as.numeric(st$label_size %||% 1.0),
        subject_id = as.character(st$selected_id %||% "")
      ))
    }
    hover_text <- vapply(seq_len(nrow(rows)), function(ii) {
      rr <- rows[ii, , drop = FALSE]
      extra <- character(0)
      if (is.finite(suppressWarnings(as.integer(rr$week[[1]])))) {
        extra <- c(extra, sprintf("week=%d", suppressWarnings(as.integer(rr$week[[1]]))))
      }
      if (is.finite(suppressWarnings(as.integer(rr$day[[1]])))) {
        extra <- c(extra, sprintf("day=%d", suppressWarnings(as.integer(rr$day[[1]]))))
      }
      paste(
        c(
          sprintf("subject=%s", as.character(rr$subject_id[[1]] %||% "")),
          sprintf("sample=%s", as.character(rr$sample_id[[1]] %||% "")),
          sprintf("vertex=%d", suppressWarnings(as.integer(rr$vertex[[1]])))
        ),
        collapse = "<br>"
      ) |>
        paste(collapse = "") |>
        (\(base) if (length(extra) > 0L) paste(base, paste(extra, collapse = "<br>"), sep = "<br>") else base)()
    }, character(1))
    vertices <- suppressWarnings(as.integer(rows$vertex))
    label_text <- build_subject_label_text(rows, st$label_mode %||% "none")
    subject_ids_use <- as.character(rows$subject_id %||% rep("", nrow(rows)))
    color_map <- build_subject_color_map(subject_ids_use, st$vertex_color %||% "#dc2626")
    vertex_colors <- unname(color_map[subject_ids_use])
    vertex_colors[is.na(vertex_colors)] <- as.character(st$vertex_color %||% "#dc2626")
    edge_mode_use <- as.character(st$edge_mode %||% "none")
    edge_groups <- if (identical(edge_mode_use, "graph")) {
      view_state <- reference_view_state()
      lapply(unique(subject_ids_use[nzchar(subject_ids_use)]), function(ss) {
        edge_mat <- compute_subject_edges(
          vertices = suppressWarnings(as.integer(rows$vertex[as.character(rows$subject_id) == ss])),
          adj_list = view_state$adj_list %||% NULL
        )
        list(
          subject_id = ss,
          edges = edge_mat,
          color = as.character(color_map[[ss]] %||% st$edge_color %||% "#dc2626")
        )
      })
    } else if (identical(edge_mode_use, "temporal")) {
      lapply(unique(subject_ids_use[nzchar(subject_ids_use)]), function(ss) {
        rows_one <- rows[as.character(rows$subject_id) == ss, , drop = FALSE]
        edge_mat <- compute_subject_temporal_edges(rows_one)
        list(
          subject_id = ss,
          edges = edge_mat,
          color = as.character(color_map[[ss]] %||% st$edge_color %||% "#dc2626")
        )
      })
    } else {
      list()
    }
    edge_groups <- Filter(function(one) is.list(one) && is.matrix(one$edges) && nrow(one$edges) > 0L, edge_groups)
    edge_mat <- if (length(edge_groups) > 0L) {
      do.call(rbind, lapply(edge_groups, function(one) one$edges))
    } else {
      matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("from", "to")))
    }
    overlay_rows <- rows
    overlay_rows$hover_text <- hover_text
    overlay_rows$label_text <- label_text
    overlay_rows$color <- vertex_colors
    list(
      vertices = vertices,
      rows = overlay_rows,
      edges = edge_mat,
      edge_groups = edge_groups,
      hover_text = hover_text,
      label_text = label_text,
      vertex_subject_ids = subject_ids_use,
      vertex_colors = vertex_colors,
      color = as.character(st$vertex_color %||% "#dc2626"),
      size = as.numeric(st$vertex_size %||% 1.0),
      dim_background = isTRUE(st$dim_background),
      background_opacity = as.numeric(st$background_opacity %||% 0.22),
      edge_color = as.character(st$edge_color %||% "#dc2626"),
      edge_width = as.numeric(st$edge_width %||% 2),
      label_size = as.numeric(st$label_size %||% 1.0),
      subject_id = as.character(st$selected_id %||% "")
    )
  })

  endpoint_workspace_revision <- shiny::reactiveVal(0L)
  endpoint_vertex_state <- shiny::reactiveValues(
    vertex = NA_integer_,
    source = ""
  )
  endpoint_vertex_input_source_override <- shiny::reactiveVal("")

  endpoint_context_key <- shiny::reactive({
    ctx <- current_endpoint_graph_context()
    if (!is.list(ctx)) {
      return("")
    }
    sprintf("%s|%s", ctx$project_id, ctx$graph_set_id)
  })

  shiny::observeEvent(endpoint_context_key(), {
    if (!nzchar(endpoint_context_key())) {
      return()
    }
    endpoint_overlay_selection(character(0))
    endpoint_autoselect_done(FALSE)
    endpoint_dataset_load_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_rename_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_delete_counts(structure(integer(0), names = character(0)))
    endpoint_dataset_default_counts(structure(integer(0), names = character(0)))
    endpoint_show_working_set(NA)
    endpoint_draft_banner_dismissed(FALSE)
    endpoint_pending_load_dataset_id("")
    endpoint_pending_project_action("")
    endpoint_working_hide_counts(structure(integer(0), names = character(0)))
    endpoint_working_restore_counts(structure(integer(0), names = character(0)))
    endpoint_working_delete_counts(structure(integer(0), names = character(0)))
    endpoint_working_label_event_values(structure(character(0), names = character(0)))
    endpoint_vertex_state$vertex <- NA_integer_
    endpoint_vertex_state$source <- ""
    endpoint_vertex_input_source_override("")
    shiny::updateNumericInput(session, "endpoint_vertex_id", value = NA)
  }, ignoreInit = TRUE)

  accepted_visible_working_rows <- function(working_state) {
    rows_df <- if (is.list(working_state) && is.data.frame(working_state$rows)) {
      working_state$rows
    } else {
      empty_working_endpoint_rows()
    }
    keep <- rows_df$accepted & rows_df$visible
    keep[is.na(keep)] <- FALSE
    rows_df[keep, , drop = FALSE]
  }

  accepted_hidden_working_rows <- function(working_state) {
    rows_df <- if (is.list(working_state) && is.data.frame(working_state$rows)) {
      working_state$rows
    } else {
      empty_working_endpoint_rows()
    }
    keep <- rows_df$accepted & !rows_df$visible
    keep[is.na(keep)] <- FALSE
    rows_df[keep, , drop = FALSE]
  }

  endpoint_show_working_set_effective <- function(working_state) {
    working_rows <- accepted_visible_working_rows(working_state)
    if (nrow(working_rows) < 1L) {
      return(FALSE)
    }
    pref <- endpoint_show_working_set()
    if (isFALSE(pref)) {
      return(FALSE)
    }
    TRUE
  }

  endpoint_working_label_dom_id <- function(vertex_id) {
    sprintf("endpoint_working_label_input_%d", suppressWarnings(as.integer(vertex_id)))
  }

  endpoint_working_label_event_id <- function(vertex_id) {
    sprintf("endpoint_working_label_edit_%d", suppressWarnings(as.integer(vertex_id)))
  }

  endpoint_working_hide_input_id <- function(vertex_id) {
    sprintf("endpoint_working_hide_%d", suppressWarnings(as.integer(vertex_id)))
  }

  endpoint_working_restore_input_id <- function(vertex_id) {
    sprintf("endpoint_working_restore_%d", suppressWarnings(as.integer(vertex_id)))
  }

  endpoint_working_delete_input_id <- function(vertex_id) {
    sprintf("endpoint_working_delete_%d", suppressWarnings(as.integer(vertex_id)))
  }

  endpoint_working_select_input_id <- function(vertex_id) {
    sprintf("endpoint_working_select_%d", suppressWarnings(as.integer(vertex_id)))
  }

  shiny::observe({
    vv <- input$endpoint_show_working_set
    if (!is.null(vv)) {
      set_reactive_val_if_changed(endpoint_show_working_set, isTRUE(vv))
    }
  })

  extract_plotly_clicked_vertex_id <- function(event_data) {
    if (is.null(event_data)) {
      return(NA_integer_)
    }

    extract_candidate <- function(x) {
      if (is.null(x) || length(x) < 1L) {
        return(NA_integer_)
      }
      cand <- suppressWarnings(as.integer(x[[1]]))
      if (is.finite(cand) && cand >= 1L) {
        return(as.integer(cand))
      }

      txt <- as.character(x[[1]] %||% "")
      if (!nzchar(txt)) {
        return(NA_integer_)
      }
      mm <- regexpr("([0-9]+)", txt, perl = TRUE)
      if (!isTRUE(mm[[1]] > 0L)) {
        return(NA_integer_)
      }
      match_txt <- regmatches(txt, mm)
      cand <- suppressWarnings(as.integer(match_txt))
      if (is.finite(cand) && cand >= 1L) {
        return(as.integer(cand))
      }
      NA_integer_
    }

    if (is.data.frame(event_data) && nrow(event_data) > 0L) {
      for (cc in c("key", "customdata", "text")) {
        if (cc %in% names(event_data)) {
          cand <- extract_candidate(event_data[[cc]])
          if (is.finite(cand)) {
            return(as.integer(cand))
          }
        }
      }
    }

    if (is.list(event_data)) {
      for (cc in c("key", "customdata", "text")) {
        if (cc %in% names(event_data)) {
          cand <- extract_candidate(event_data[[cc]])
          if (is.finite(cand)) {
            return(as.integer(cand))
          }
        }
      }
    }

    NA_integer_
  }

  reference_vertex_count <- shiny::reactive({
    rr <- reference_renderer_state()
    st <- rr$st
    if (!is.list(st) || !is.null(st$error) || !is.matrix(st$coords)) {
      return(NA_integer_)
    }
    nn <- suppressWarnings(as.integer(nrow(st$coords)))
    if (!is.finite(nn) || nn < 1L) {
      return(NA_integer_)
    }
    as.integer(nn)
  })

  normalize_selected_endpoint_vertex <- function(vertex_id) {
    vid <- suppressWarnings(as.integer(vertex_id))
    if (!is.finite(vid) || vid < 1L) {
      return(NA_integer_)
    }
    max_vertex <- reference_vertex_count()
    if (is.finite(max_vertex) && vid > max_vertex) {
      return(NA_integer_)
    }
    as.integer(vid)
  }

  if (requireNamespace("plotly", quietly = TRUE)) {
    attach_reference_plotly_camera_preserver <- function(widget) {
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        return(widget)
      }
      htmlwidgets::onRender(
        widget,
        sprintf("function(el, x) {
          var gd = document.getElementById(el.id) || el;
          if (!gd) return;

          var cameraToRestore = window.__gflowuiReferenceCamera
            ? JSON.parse(JSON.stringify(window.__gflowuiReferenceCamera))
            : null;

          function cloneCamera(cam) {
            try {
              return JSON.parse(JSON.stringify(cam));
            } catch (e) {
              return cam || null;
            }
          }

          function currentCamera() {
            try {
              if (gd._fullLayout && gd._fullLayout.scene && gd._fullLayout.scene.camera) {
                return cloneCamera(gd._fullLayout.scene.camera);
              }
            } catch (e) {}
            return null;
          }

          function rememberCamera(ev) {
            if (gd.__gflowuiSuppressRemember) return;
            var cam = ev && ev['scene.camera'] ? ev['scene.camera'] : currentCamera();
            if (cam) {
              window.__gflowuiReferenceCamera = cloneCamera(cam);
            }
            if (cam && window.Shiny && typeof window.Shiny.setInputValue === 'function') {
              window.Shiny.setInputValue('%s', cloneCamera(cam), {priority: 'event'});
            }
          }

          gd.on('plotly_relayout', rememberCamera);
          gd.on('plotly_afterplot', function() {
            if (gd.__gflowuiSuppressRemember) return;
            var cam = currentCamera();
            if (cam) {
              window.__gflowuiReferenceCamera = cloneCamera(cam);
            }
            if (cam && window.Shiny && typeof window.Shiny.setInputValue === 'function') {
              window.Shiny.setInputValue('%s', cloneCamera(cam), {priority: 'event'});
            }
          });

          if (cameraToRestore) {
            gd.__gflowuiSuppressRemember = true;
            setTimeout(function() {
              try {
                Plotly.relayout(gd, {'scene.camera': cameraToRestore}).then(function() {
                  window.__gflowuiReferenceCamera = JSON.parse(JSON.stringify(cameraToRestore));
                  gd.__gflowuiSuppressRemember = false;
                }).catch(function() {
                  gd.__gflowuiSuppressRemember = false;
                });
              } catch (e) {
                gd.__gflowuiSuppressRemember = false;
              }
            }, 80);
          }
        }", reference_plot_camera_input_id, reference_plot_camera_input_id)
      )
    }

    parse_plotly_event_input <- function(event_id) {
      raw_val <- input[[event_id]]
      if (is.null(raw_val)) {
        return(NULL)
      }
      if (is.list(raw_val) || is.data.frame(raw_val)) {
        return(raw_val)
      }
      txt <- as.character(raw_val[[1]] %||% "")
      if (!nzchar(txt)) {
        return(NULL)
      }
      tryCatch(
        jsonlite::parse_json(txt, simplifyVector = TRUE),
        error = function(e) NULL
      )
    }

    reference_plotly_click_event <- shiny::reactive({
      rr <- reference_renderer_state()
      if (!is.list(rr) || !identical(as.character(rr$effective %||% ""), "plotly")) {
        return(NULL)
      }
      parse_plotly_event_input(
        sprintf("plotly_click-%s", reference_plotly_source)
      )
    })

    shiny::observeEvent(
      reference_plotly_click_event(),
      {
        vid <- normalize_selected_endpoint_vertex(
          extract_plotly_clicked_vertex_id(reference_plotly_click_event())
        )
        if (!is.finite(vid)) {
          return()
        }
        endpoint_vertex_state$vertex <- as.integer(vid)
        endpoint_vertex_state$source <- "plotly"
        endpoint_vertex_input_source_override("plotly")
        shiny::updateNumericInput(session, "endpoint_vertex_id", value = as.integer(vid))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input[[reference_plot_camera_input_id]], {
      cam <- input[[reference_plot_camera_input_id]]
      if (is.null(cam) || !is.list(cam)) {
        return()
      }
      cam_norm <- normalize_plotly_camera(cam)
      if (is.null(cam_norm)) {
        return()
      }
      set_reactive_val_if_changed(reference_plot_camera_state, cam_norm)
    }, ignoreInit = TRUE)
  }

  arm_preview_layout_inputs <- shiny::debounce(
    shiny::reactive({
      list(
        path_color = as.character(input$arm_preview_path_color %||% "#f97316"),
        body_color_mode = as.character(input$arm_preview_body_color_mode %||% "solid"),
        body_color = as.character(input$arm_preview_body_color %||% "#eab308"),
        body_opacity = suppressWarnings(as.numeric(input$arm_preview_body_opacity %||% 0.75)),
        path_width = suppressWarnings(as.numeric(input$arm_preview_path_width %||% 5)),
        body_size = suppressWarnings(as.numeric(input$arm_preview_body_size %||% 1.8)),
        center_marker_color = as.character(input$arm_center_marker_color %||% "#111827"),
        center_marker_size = suppressWarnings(as.numeric(input$arm_center_marker_size %||% 1.7))
      )
    }),
    millis = 180
  )

  arm_builder_preview_inputs <- shiny::debounce(
    shiny::reactive({
      list(
        endpoint_a = as.character(input$arm_endpoint_a %||% ""),
        endpoint_b = as.character(input$arm_endpoint_b %||% ""),
        thickening_method = as.character(input$arm_thickening_method %||% "path_only"),
        path_relative_radius = suppressWarnings(as.numeric(input$arm_path_relative_radius %||% 0.10)),
        excess_tolerance = suppressWarnings(as.numeric(input$arm_excess_tolerance %||% NA_real_))
      )
    }),
    millis = 220
  )

  shiny::observeEvent(input$endpoint_vertex_id, {
    raw_val <- input$endpoint_vertex_id
    if (is.null(raw_val) || (length(raw_val) > 0L && all(is.na(raw_val)))) {
      endpoint_vertex_state$vertex <- NA_integer_
      endpoint_vertex_state$source <- ""
      endpoint_vertex_input_source_override("")
      return()
    }

    vid <- normalize_selected_endpoint_vertex(raw_val)
    source_override <- as.character(endpoint_vertex_input_source_override() %||% "")
    if (nzchar(source_override)) {
      endpoint_vertex_input_source_override("")
    }
    if (is.finite(vid)) {
      endpoint_vertex_state$vertex <- as.integer(vid)
      endpoint_vertex_state$source <- if (nzchar(source_override)) source_override else "manual"
    } else {
      endpoint_vertex_state$vertex <- NA_integer_
      endpoint_vertex_state$source <- if (nzchar(source_override)) source_override else "manual"
    }
  }, ignoreInit = FALSE)

  shiny::observe({
    vv <- input$endpoint_datasets_open
    if (!is.null(vv)) {
      set_reactive_val_if_changed(endpoint_datasets_open, isTRUE(vv))
    }
  })

  selected_endpoint_vertex <- shiny::reactive({
    normalize_selected_endpoint_vertex(endpoint_vertex_state$vertex)
  })

  shiny::observeEvent(input$endpoint_working_select_vertex, {
    event_val <- input$endpoint_working_select_vertex
    scroll_top <- 0L
    if (is.list(event_val)) {
      scroll_top <- suppressWarnings(as.integer(event_val$scrollTop %||% 0L))
      event_val <- event_val$vertex %||% NA_integer_
    }
    if (is.finite(scroll_top) && scroll_top >= 0L) {
      endpoint_working_scroll_top(as.integer(scroll_top))
    }
    vid <- normalize_selected_endpoint_vertex(event_val)
    if (is.finite(vid)) {
      endpoint_vertex_state$vertex <- as.integer(vid)
      endpoint_vertex_state$source <- "working_table"
      endpoint_vertex_input_source_override("working_table")
      shiny::updateNumericInput(session, "endpoint_vertex_id", value = as.integer(vid))
    }
  }, ignoreInit = TRUE)

  add_selected_vertex_to_working_set <- function() {
    ctx <- current_endpoint_graph_context()
    if (!is.list(ctx)) {
      shiny::showNotification("No active endpoint graph context.", type = "error")
      return(invisible(FALSE))
    }

    vid <- selected_endpoint_vertex()
    if (!is.finite(vid)) {
      shiny::showNotification("Select a valid vertex first.", type = "warning")
      return(invisible(FALSE))
    }

    st <- endpoint_panel_state()
    suggestion <- endpoint_label_profile_suggestion(vid, panel_state = st)
    suggested_label <- as.character(suggestion$label %||% "")
    if (!nzchar(suggested_label)) {
      suggested_label <- sprintf("v%d", as.integer(vid))
    }
    working <- if (is.list(st) && is.list(st$working)) st$working else empty_working_endpoint_state(ctx = ctx)
    working_rows <- if (is.data.frame(working$rows)) working$rows else empty_working_endpoint_rows()
    already_present <- any(as.integer(working_rows$vertex) == as.integer(vid))
    source_dataset_id <- as.character(working$base_dataset_id %||% "")
    updated <- upsert_working_endpoint_vertex_state(
      state = working,
      vertex_id = vid,
      label = suggested_label,
      source_type = "manual",
      source_dataset_id = source_dataset_id
    )
    save_working_endpoint_state(updated, ctx = ctx)
    endpoint_overlay_selection(character(0))
    endpoint_show_working_set(TRUE)

    note <- if (isTRUE(already_present)) {
      sprintf("Restored v%d in the working endpoint set.", as.integer(vid))
    } else {
      sprintf("Added v%d to the working endpoint set.", as.integer(vid))
    }
    shiny::showNotification(note, type = "message")
    invisible(TRUE)
  }

  shiny::observeEvent(input$endpoint_add_selected_vertex, {
    add_selected_vertex_to_working_set()
  }, ignoreInit = TRUE)

  endpoint_candidate_workspace_files <- function(ctx) {
    if (!is.list(ctx)) {
      return(character(0))
    }
    files <- character(0)
    candidate_dir <- endpoint_candidates_dir(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    if (nzchar(candidate_dir) && dir.exists(candidate_dir)) {
      files <- c(files, list.files(candidate_dir, pattern = "\\.rds$", full.names = TRUE))
    }
    legacy_dirs <- endpoint_state_legacy_k_dirs(
      graph_set_id = ctx$graph_set_id,
      project_id = ctx$project_id
    )
    if (length(legacy_dirs) > 0L) {
      for (dd in legacy_dirs) {
        cand_dir <- file.path(dd, "candidates")
        if (dir.exists(cand_dir)) {
          files <- c(files, list.files(cand_dir, pattern = "\\.rds$", full.names = TRUE))
        }
      }
    }
    unique(normalizePath(files[file.exists(files)], mustWork = FALSE))
  }

  load_workspace_endpoint_candidates <- function(ctx) {
    if (!is.list(ctx)) {
      return(data.frame())
    }
    files <- endpoint_candidate_workspace_files(ctx)
    if (length(files) < 1L) {
      return(data.frame())
    }

    rows <- lapply(seq_along(files), function(ii) {
      ds <- read_workspace_endpoint_dataset(files[[ii]])
      if (!is.list(ds) ||
          !identical(as.character(ds$graph_set_id %||% ""), as.character(ctx$graph_set_id))) {
        return(NULL)
      }
      source_k <- suppressWarnings(as.integer(ds$k %||% NA_integer_))
      key <- sanitize_token_id(ds$dataset_id, fallback = sprintf("workspace_dataset_%d", ii))
      data.frame(
        dataset_id = key,
        key = key,
        input_id = sprintf("endpoint_dataset_%s", key),
        load_input_id = sprintf("endpoint_load_%s", key),
        rename_input_id = sprintf("endpoint_rename_%s", key),
        delete_input_id = sprintf("endpoint_delete_%s", key),
        default_input_id = sprintf("endpoint_default_%s", key),
        source_type = "workspace",
        origin = as.character(ds$origin %||% "workspace"),
        label = as.character(ds$label %||% key),
        method = as.character(ds$method %||% "workspace"),
        k = source_k,
        k_display = if (is.finite(source_k)) as.character(source_k) else "-",
        n_endpoints = length(ds$vertices %||% integer(0)),
        parameter_summary = as.character(ds$parameter_summary %||% "workspace candidate"),
        run_id = key,
        labels_csv = "",
        bundle_file = "",
        per_k_file = "",
        workspace_file = as.character(ds$path %||% files[[ii]]),
        created_at = as.character(ds$created_at %||% ""),
        can_load = TRUE,
        can_rename = TRUE,
        can_delete = TRUE,
        can_set_default = TRUE,
        is_default = FALSE,
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) < 1L) {
      return(data.frame())
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  empty_endpoint_candidate_rows <- function() {
    data.frame(
      dataset_id = character(0),
      key = character(0),
      input_id = character(0),
      load_input_id = character(0),
      rename_input_id = character(0),
      delete_input_id = character(0),
      default_input_id = character(0),
      source_type = character(0),
      origin = character(0),
      label = character(0),
      method = character(0),
      k = integer(0),
      k_display = character(0),
      n_endpoints = integer(0),
      parameter_summary = character(0),
      run_id = character(0),
      labels_csv = character(0),
      bundle_file = character(0),
      per_k_file = character(0),
      workspace_file = character(0),
      external_rds_file = character(0),
      filter_min_scale_stability = numeric(0),
      created_at = character(0),
      autoselect = logical(0),
      sort_quantile = numeric(0),
      can_load = logical(0),
      can_rename = logical(0),
      can_delete = logical(0),
      can_set_default = logical(0),
      is_default = logical(0),
      stringsAsFactors = FALSE
    )
  }

  normalize_endpoint_candidate_rows <- function(x) {
    template <- empty_endpoint_candidate_rows()
    if (!is.data.frame(x) || nrow(x) < 1L) {
      return(template[0, , drop = FALSE])
    }
    typed_missing_column <- function(example, n) {
      if (is.integer(example)) {
        return(rep(NA_integer_, n))
      }
      if (is.numeric(example)) {
        return(rep(NA_real_, n))
      }
      if (is.logical(example)) {
        return(rep(NA, n))
      }
      rep("", n)
    }
    missing_cols <- setdiff(names(template), names(x))
    if (length(missing_cols) > 0L) {
      for (cc in missing_cols) {
        x[[cc]] <- typed_missing_column(template[[cc]], nrow(x))
      }
    }
    x <- x[, names(template), drop = FALSE]
    x$dataset_id <- as.character(x$dataset_id)
    x$key <- as.character(x$key)
    x$input_id <- as.character(x$input_id)
    x$load_input_id <- as.character(x$load_input_id)
    x$rename_input_id <- as.character(x$rename_input_id)
    x$delete_input_id <- as.character(x$delete_input_id)
    x$default_input_id <- as.character(x$default_input_id)
    x$source_type <- as.character(x$source_type)
    x$origin <- as.character(x$origin)
    x$label <- as.character(x$label)
    x$method <- as.character(x$method)
    x$k <- suppressWarnings(as.integer(x$k))
    x$k_display <- as.character(x$k_display)
    x$n_endpoints <- suppressWarnings(as.integer(x$n_endpoints))
    x$parameter_summary <- as.character(x$parameter_summary)
    x$run_id <- as.character(x$run_id)
    x$labels_csv <- as.character(x$labels_csv)
    x$bundle_file <- as.character(x$bundle_file)
    x$per_k_file <- as.character(x$per_k_file)
    x$workspace_file <- as.character(x$workspace_file)
    x$external_rds_file <- as.character(x$external_rds_file)
    x$filter_min_scale_stability <- suppressWarnings(as.numeric(x$filter_min_scale_stability))
    x$created_at <- as.character(x$created_at)
    x$autoselect <- as.logical(x$autoselect)
    x$sort_quantile <- suppressWarnings(as.numeric(x$sort_quantile))
    x$can_load <- as.logical(x$can_load)
    x$can_rename <- as.logical(x$can_rename)
    x$can_delete <- as.logical(x$can_delete)
    x$can_set_default <- as.logical(x$can_set_default)
    x$is_default <- as.logical(x$is_default)
    rownames(x) <- NULL
    x
  }

  load_external_endpoint_candidates <- function(manifest, ctx) {
    if (!is.list(manifest) || !is.list(ctx)) {
      return(data.frame())
    }
    project_root <- as.character(manifest$project_root %||% "")
    if (!nzchar(project_root) || identical(project_root, "NA")) {
      return(data.frame())
    }
    if (!identical(as.character(ctx$graph_set_id %||% ""), "shared_all_asv")) {
      return(data.frame())
    }

    results_root <- file.path(project_root, "results", "asv_hv_k_gcv_sweep")
    sweep_dirs <- Sys.glob(file.path(results_root, "embedding_geometry_k*_threshold_sweep_focus*"))
    sweep_dirs <- sweep_dirs[dir.exists(sweep_dirs)]
    if (length(sweep_dirs) < 1L) {
      return(data.frame())
    }

    quantile_token <- function(x) {
      gsub("\\.", "p", sprintf("%0.2f", as.numeric(x)))
    }

    build_row <- function(one_row, base_detect_dir, bundle_file, created_at, current_k) {
      source_k <- suppressWarnings(as.integer(one_row$k[[1]]))
      qv <- as.numeric(one_row$min.score.quantile[[1]])
      rds_pattern <- sprintf(
        "^k%02d_msq%s_.*dmr2p00_.*dmns02_.*ssr1p00_.*metricscore_.*smooth1\\.rds$",
        as.integer(source_k),
        quantile_token(qv)
      )
      rds_files <- list.files(base_detect_dir, pattern = rds_pattern, full.names = TRUE)
      if (length(rds_files) < 1L) {
        return(NULL)
      }
      rds_path <- rds_files[[1]]
      q_label <- format(qv, nsmall = 2, trim = TRUE)
      key <- sanitize_token_id(
        sprintf("embedding_geometry_k%02d_msq%s_mss1_ssr1", as.integer(source_k), quantile_token(qv)),
        fallback = sprintf("embedding_geometry_k%02d_%s", as.integer(source_k), quantile_token(qv))
      )
      data.frame(
        dataset_id = key,
        key = key,
        input_id = sprintf("endpoint_dataset_%s", key),
        load_input_id = sprintf("endpoint_load_%s", key),
        rename_input_id = sprintf("endpoint_rename_%s", key),
        delete_input_id = sprintf("endpoint_delete_%s", key),
        default_input_id = sprintf("endpoint_default_%s", key),
        source_type = "external_rds",
        origin = "sweep",
        label = sprintf("Embedding Geometry (%s / 1 / ssr=1)", q_label),
        method = "embedding_geometry",
        k = as.integer(source_k),
        k_display = as.character(source_k),
        n_endpoints = suppressWarnings(as.integer(one_row$n.endpoints[[1]])),
        parameter_summary = sprintf("embedding_geometry | %s / 1 / ssr=1", q_label),
        run_id = sprintf("embedding_geometry_k%02d_threshold_sweep_focus", as.integer(source_k)),
        labels_csv = "",
        bundle_file = bundle_file,
        per_k_file = "",
        workspace_file = "",
        external_rds_file = rds_path,
        filter_min_scale_stability = 1,
        created_at = as.character(created_at %||% ""),
        autoselect = identical(qv, 0.98) && is.finite(current_k) && identical(as.integer(source_k), as.integer(current_k)),
        sort_quantile = qv,
        can_load = TRUE,
        can_rename = FALSE,
        can_delete = FALSE,
        can_set_default = TRUE,
        is_default = FALSE,
        stringsAsFactors = FALSE
      )
    }

    dir_k <- suppressWarnings(as.integer(vapply(sweep_dirs, function(dd) parse_k_from_token(basename(dd)), integer(1))))
    keep_dirs <- is.finite(dir_k) & dir_k > 0L
    sweep_dirs <- sweep_dirs[keep_dirs]
    dir_k <- dir_k[keep_dirs]
    if (length(sweep_dirs) < 1L) {
      return(data.frame())
    }

    dir_info <- file.info(sweep_dirs)
    best_idx <- tapply(
      seq_along(sweep_dirs),
      dir_k,
      function(ii) ii[[order(dir_info$mtime[ii], decreasing = TRUE)[[1]]]]
    )
    sweep_dirs <- sweep_dirs[unlist(best_idx, use.names = FALSE)]
    dir_k <- suppressWarnings(as.integer(vapply(sweep_dirs, function(dd) parse_k_from_token(basename(dd)), integer(1))))

    current_k <- suppressWarnings(as.integer(ctx$k %||% NA_integer_))
    all_rows <- list()
    idx_out <- 1L
    preferred_quantiles <- c(0.97, 0.98, 0.99)

    for (jj in seq_along(sweep_dirs)) {
      sweep_dir <- sweep_dirs[[jj]]
      k_val <- dir_k[[jj]]
      bundle_file <- file.path(sweep_dir, sprintf("k%02d_threshold_sweep_bundle.rds", as.integer(k_val)))
      summary_file <- file.path(sweep_dir, sprintf("k%02d_threshold_sweep_summary.csv", as.integer(k_val)))
      if (!file.exists(bundle_file) || !file.exists(summary_file)) {
        next
      }
      bundle <- tryCatch(readRDS(bundle_file), error = function(e) NULL)
      summary_tbl <- read_csv_safely(summary_file)
      if (!is.list(bundle) || !is.data.frame(summary_tbl) || nrow(summary_tbl) < 1L) {
        next
      }
      cache_dir <- as.character(bundle$options$cache.dir %||% "")
      if (!nzchar(cache_dir) || !dir.exists(cache_dir)) {
        next
      }
      base_detect_dir <- file.path(cache_dir, "base_detect")
      if (!dir.exists(base_detect_dir)) {
        next
      }

      summary_tbl$k <- suppressWarnings(as.integer(summary_tbl$k))
      summary_tbl$min.score.quantile <- suppressWarnings(as.numeric(summary_tbl$min.score.quantile))
      summary_tbl$min.scale.stability <- suppressWarnings(as.numeric(summary_tbl$min.scale.stability))
      summary_tbl$scale.stability.radius <- suppressWarnings(as.numeric(summary_tbl$scale.stability.radius))
      summary_tbl$detect.max.radius <- suppressWarnings(as.numeric(summary_tbl$detect.max.radius))
      summary_tbl$detect.min.neighborhood.size <- suppressWarnings(as.integer(summary_tbl$detect.min.neighborhood.size))

      summary_tbl <- summary_tbl[
        is.finite(summary_tbl$k) &
          summary_tbl$k == as.integer(k_val) &
          is.finite(summary_tbl$min.score.quantile) &
          summary_tbl$min.score.quantile %in% preferred_quantiles &
          is.finite(summary_tbl$min.scale.stability) &
          summary_tbl$min.scale.stability >= 1 &
          is.finite(summary_tbl$scale.stability.radius) &
          summary_tbl$scale.stability.radius == 1 &
          is.finite(summary_tbl$detect.max.radius) &
          summary_tbl$detect.max.radius == 2 &
          is.finite(summary_tbl$detect.min.neighborhood.size) &
          summary_tbl$detect.min.neighborhood.size == 2,
        ,
        drop = FALSE
      ]
      if (nrow(summary_tbl) < 1L) {
        next
      }
      rows_one <- lapply(
        seq_len(nrow(summary_tbl)),
        function(ii) build_row(
          summary_tbl[ii, , drop = FALSE],
          base_detect_dir = base_detect_dir,
          bundle_file = bundle_file,
          created_at = as.character(bundle$generated.at %||% ""),
          current_k = current_k
        )
      )
      rows_one <- rows_one[!vapply(rows_one, is.null, logical(1))]
      if (length(rows_one) < 1L) {
        next
      }
      for (rr in rows_one) {
        all_rows[[idx_out]] <- rr
        idx_out <- idx_out + 1L
      }
    }

    if (length(all_rows) < 1L) {
      return(data.frame())
    }
    out <- do.call(rbind, all_rows)
    out <- out[order(out$k, out$sort_quantile, decreasing = FALSE), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  manifest_endpoint_dataset_rows <- function(manifest, ctx) {
    if (!is.list(manifest) || !is.list(manifest$endpoint_runs) || length(manifest$endpoint_runs) < 1L) {
      return(data.frame())
    }
    rows <- list()
    idx <- 1L
    for (ep_run in manifest$endpoint_runs) {
      run_rows <- endpoint_rows_for_run(ep_run)
      if (!is.data.frame(run_rows) || nrow(run_rows) < 1L) {
        next
      }
      if (nrow(run_rows) < 1L) {
        next
      }
      run_label <- as.character(ep_run$label %||% ep_run$id %||% "endpoint")
      for (ii in seq_len(nrow(run_rows))) {
        rr <- run_rows[ii, , drop = FALSE]
        payload <- read_endpoint_labels_from_row(rr)
        kk <- suppressWarnings(as.integer(rr$k[[1]]))
        key <- as.character(rr$key[[1]] %||% sprintf("manifest_endpoint_%d", idx))
        label <- run_label
        if (is.finite(kk) && sum(is.finite(run_rows$k)) > 1L) {
          label <- sprintf("%s (k=%d)", run_label, as.integer(kk))
        }
        rows[[idx]] <- data.frame(
          dataset_id = key,
          key = key,
          input_id = as.character(rr$input_id[[1]] %||% sprintf("endpoint_dataset_%s", key)),
          load_input_id = sprintf("endpoint_load_%s", key),
          rename_input_id = sprintf("endpoint_rename_%s", key),
          delete_input_id = sprintf("endpoint_delete_%s", key),
          default_input_id = sprintf("endpoint_default_%s", key),
          source_type = "manifest",
          origin = "manifest",
          label = label,
          method = as.character(rr$method[[1]] %||% normalize_endpoint_method(ep_run)),
          k = kk,
          k_display = as.character(rr$k_display[[1]] %||% if (is.finite(kk)) kk else "-"),
          n_endpoints = length(payload$vertices %||% integer(0)),
          parameter_summary = sprintf(
            "%s | k=%s",
            as.character(rr$method[[1]] %||% "endpoint"),
            if (is.finite(kk)) as.character(kk) else "?"
          ),
          run_id = as.character(rr$run_id[[1]] %||% ""),
          labels_csv = as.character(rr$labels_csv[[1]] %||% ""),
          bundle_file = as.character(rr$bundle_file[[1]] %||% ""),
          per_k_file = as.character(rr$per_k_file[[1]] %||% ""),
          workspace_file = "",
          created_at = as.character(ep_run$created_at %||% ""),
          can_load = TRUE,
          can_rename = FALSE,
          can_delete = FALSE,
          can_set_default = TRUE,
          is_default = FALSE,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
    if (length(rows) < 1L) {
      return(data.frame())
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  working_endpoint_state_from_snapshot_record <- function(obj, ctx = NULL) {
    if (!is.list(obj)) {
      return(empty_working_endpoint_state(ctx = ctx))
    }
    norm <- normalize_endpoint_labels(
      vertices = obj$vertices %||% integer(0),
      labels = obj$labels %||% character(0)
    )
    rows <- empty_working_endpoint_rows()
    if (length(norm$vertices) > 0L) {
      rows <- data.frame(
        vertex = as.integer(norm$vertices),
        accepted = rep(TRUE, length(norm$vertices)),
        visible = rep(TRUE, length(norm$vertices)),
        label = as.character(norm$labels),
        auto_label = as.character(norm$labels),
        source_type = rep("manual", length(norm$vertices)),
        source_dataset_id = rep(as.character(obj$source_dataset_id %||% ""), length(norm$vertices)),
        manually_added = rep(TRUE, length(norm$vertices)),
        manually_removed = rep(FALSE, length(norm$vertices)),
        notes = rep("", length(norm$vertices)),
        updated_at = rep(as.character(obj$created_at %||% .gflowui_now()), length(norm$vertices)),
        stringsAsFactors = FALSE
      )
    }
    sanitize_working_endpoint_state(
      list(
        version = "1",
        project_id = as.character(obj$project_id %||% ctx$project_id %||% rv$project.id %||% ""),
        graph_set_id = as.character(obj$graph_set_id %||% ctx$graph_set_id %||% ""),
        k = suppressWarnings(as.integer(obj$source_k %||% obj$k %||% ctx$k %||% NA_integer_)),
        base_dataset_id = as.character(obj$source_dataset_id %||% NA_character_),
        base_dataset_label = as.character(obj$label %||% obj$dataset_id %||% NA_character_),
        base_source_k = suppressWarnings(as.integer(obj$source_k %||% obj$k %||% NA_integer_)),
        is_modified = FALSE,
        last_snapshot_id = as.character(obj$dataset_id %||% NA_character_),
        last_snapshot_label = as.character(obj$label %||% obj$dataset_id %||% NA_character_),
        last_session_id = as.character(obj$last_session_id %||% NA_character_),
        rows = rows,
        updated_at = as.character(obj$created_at %||% .gflowui_now())
      ),
      ctx = ctx
    )
  }

  legacy_working_endpoint_state_candidates <- function(ctx) {
    if (!is.list(ctx)) {
      return(list())
    }
    states <- list()
    idx_out <- 1L
    legacy_dirs <- endpoint_state_legacy_k_dirs(
      graph_set_id = ctx$graph_set_id,
      project_id = ctx$project_id
    )
    if (length(legacy_dirs) < 1L) {
      return(states)
    }

    for (dd in legacy_dirs) {
      kk <- suppressWarnings(as.integer(sub("^k=", "", basename(dd))))
      current_file <- file.path(dd, "working", "current.rds")
      current_obj <- read_rds_if_exists(current_file, default = NULL)
      if (is.list(current_obj)) {
        states[[idx_out]] <- list(
          state = sanitize_working_endpoint_state(current_obj, ctx = ctx),
          source_kind = "current",
          source_k = kk,
          source_path = current_file
        )
        idx_out <- idx_out + 1L
      }
      snap_dir <- file.path(dd, "working", "snapshots")
      if (!dir.exists(snap_dir)) {
        next
      }
      snap_files <- sort(list.files(snap_dir, pattern = "\\.rds$", full.names = TRUE))
      if (length(snap_files) < 1L) {
        next
      }
      for (pp in snap_files) {
        snap_obj <- read_rds_if_exists(pp, default = NULL)
        if (!is.list(snap_obj)) {
          next
        }
        states[[idx_out]] <- list(
          state = working_endpoint_state_from_snapshot_record(snap_obj, ctx = ctx),
          source_kind = "snapshot",
          source_k = kk,
          source_path = pp
        )
        idx_out <- idx_out + 1L
      }
    }
    states
  }

  pick_best_working_endpoint_state <- function(states, ctx = NULL) {
    if (!is.list(states) || length(states) < 1L) {
      return(empty_working_endpoint_state(ctx = ctx))
    }
    score_one <- function(st) {
      rec <- if (is.list(st) && is.list(st$state)) st else list(
        state = st,
        source_kind = "current",
        source_k = suppressWarnings(as.integer(st$k %||% NA_integer_))
      )
      keep_rows <- accepted_visible_working_rows(rec$state)
      n_keep <- nrow(keep_rows)
      n_all <- if (is.list(rec$state) && is.data.frame(rec$state$rows)) nrow(rec$state$rows) else 0L
      source_rank <- if (identical(rec$source_kind, "current")) 2 else 1
      source_k <- suppressWarnings(as.integer(rec$source_k %||% rec$state$k %||% NA_integer_))
      prefer_k <- suppressWarnings(as.integer(ctx$k %||% NA_integer_))
      k_rank <- if (is.finite(prefer_k) && is.finite(source_k) && source_k == prefer_k) 1 else 0
      ts_val <- suppressWarnings(as.numeric(as.POSIXct(as.character(rec$state$updated_at %||% ""), tz = "UTC")))
      if (!is.finite(ts_val)) {
        ts_val <- -Inf
      }
      c(source_rank, k_rank, ts_val, n_keep, n_all)
    }
    scores <- do.call(rbind, lapply(states, score_one))
    if (!is.matrix(scores) || nrow(scores) < 1L) {
      return(empty_working_endpoint_state(ctx = ctx))
    }
    ord <- do.call(order, c(lapply(seq_len(ncol(scores)), function(jj) -scores[, jj]), list(na.last = TRUE)))
    best <- states[[ord[[1]]]]
    best_state <- if (is.list(best) && is.list(best$state)) best$state else best
    sanitize_working_endpoint_state(best_state, ctx = ctx)
  }

  load_working_endpoint_state <- function(ctx) {
    if (!is.list(ctx)) {
      st <- empty_working_endpoint_state(ctx = ctx)
      attr(st, "state_exists") <- FALSE
      return(st)
    }
    path <- endpoint_working_file(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    obj <- read_rds_if_exists(path, default = NULL)
    if (is.list(obj)) {
      st <- sanitize_working_endpoint_state(obj, ctx = ctx)
      attr(st, "state_exists") <- TRUE
      return(st)
    }
    legacy_states <- legacy_working_endpoint_state_candidates(ctx)
    st <- pick_best_working_endpoint_state(legacy_states, ctx = ctx)
    attr(st, "state_exists") <- length(legacy_states) > 0L
    st
  }

  save_working_endpoint_state <- function(state, ctx) {
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    path <- endpoint_working_file(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    cleaned <- sanitize_working_endpoint_state(state, ctx = ctx)
    cleaned$updated_at <- .gflowui_now()
    cleaned$last_session_id <- endpoint_session_id
    save_rds_safely(cleaned, path)
    endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  working_endpoint_state_from_dataset <- function(row_df) {
    ctx <- current_endpoint_graph_context()
    if (!is.list(ctx) || !is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(empty_working_endpoint_state(ctx = ctx))
    }
    row <- row_df[1, , drop = FALSE]
    payload <- read_endpoint_dataset_from_row(row)
    norm <- normalize_endpoint_labels(payload$vertices, payload$labels)
    rows <- empty_working_endpoint_rows()
    if (length(norm$vertices) > 0L) {
      rows <- data.frame(
        vertex = as.integer(norm$vertices),
        accepted = rep(TRUE, length(norm$vertices)),
        visible = rep(TRUE, length(norm$vertices)),
        label = as.character(norm$labels),
        auto_label = as.character(norm$labels),
        source_type = rep(as.character(row$method[[1]] %||% row$source_type[[1]] %||% "endpoint"), length(norm$vertices)),
        source_dataset_id = rep(as.character(row$dataset_id[[1]] %||% ""), length(norm$vertices)),
        manually_added = rep(FALSE, length(norm$vertices)),
        manually_removed = rep(FALSE, length(norm$vertices)),
        notes = rep("", length(norm$vertices)),
        updated_at = rep(.gflowui_now(), length(norm$vertices)),
        stringsAsFactors = FALSE
      )
    }
    sanitize_working_endpoint_state(
      list(
        version = "1",
        project_id = ctx$project_id,
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        base_dataset_id = as.character(row$dataset_id[[1]] %||% ""),
        base_dataset_label = as.character(row$label[[1]] %||% row$dataset_id[[1]] %||% ""),
        base_source_k = suppressWarnings(as.integer(row$k[[1]] %||% NA_integer_)),
        is_modified = FALSE,
        last_snapshot_id = as.character(row$dataset_id[[1]] %||% ""),
        last_snapshot_label = as.character(row$label[[1]] %||% row$dataset_id[[1]] %||% ""),
        last_session_id = endpoint_session_id,
        rows = rows,
        updated_at = .gflowui_now()
      ),
      ctx = ctx
    )
  }

  use_endpoint_dataset_as_working_set <- function(dataset_id) {
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    if (nrow(rows) < 1L) {
      return(invisible(FALSE))
    }
    hit <- which(as.character(rows$dataset_id) == as.character(dataset_id))
    if (length(hit) < 1L) {
      return(invisible(FALSE))
    }
    state <- working_endpoint_state_from_dataset(rows[hit[[1]], , drop = FALSE])
    save_working_endpoint_state(state, ctx = current_endpoint_graph_context())
    endpoint_overlay_selection(character(0))
    endpoint_show_working_set(TRUE)
    endpoint_draft_banner_dismissed(FALSE)
    shiny::showNotification(
      sprintf("Working endpoints loaded from '%s'.", as.character(rows$label[[hit[[1]]]] %||% dataset_id)),
      type = "message"
    )
    invisible(TRUE)
  }

  save_working_endpoint_snapshot <- function() {
    st <- endpoint_panel_state()
    working <- if (is.list(st)) st$working else NULL
    ctx <- current_endpoint_graph_context()
    if (!is.list(ctx) || !is.list(working)) {
      return(invisible(list(ok = FALSE)))
    }
    rows <- if (is.data.frame(working$rows)) working$rows else empty_working_endpoint_rows()
    keep <- rows$accepted & rows$visible
    rows <- rows[keep, , drop = FALSE]
    if (nrow(rows) < 1L) {
      shiny::showNotification("Working endpoint set is empty.", type = "warning")
      return(invisible(list(ok = FALSE)))
    }
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    dataset_id <- sanitize_token_id(
      sprintf("working_%s_k%03d_%s", ctx$graph_set_id, as.integer(ctx$k), stamp),
      fallback = sprintf("working_snapshot_%s", stamp)
    )
    label <- sprintf("Working snapshot %s", format(Sys.time(), "%Y-%m-%d %H:%M"))
    out <- list(
      version = "1",
      dataset_id = dataset_id,
      label = label,
      method = "working_snapshot",
      origin = "workspace",
      graph_set_id = as.character(ctx$graph_set_id),
      k = as.integer(ctx$k),
      source_k = as.integer(ctx$k),
      created_at = .gflowui_now(),
      parameter_summary = sprintf(
        "snapshot from %s | source k=%s",
        as.character(working$base_dataset_label %||% working$base_dataset_id %||% "working set"),
        as.character(ctx$k)
      ),
      source_dataset_id = as.character(working$base_dataset_id %||% ""),
      last_session_id = endpoint_session_id,
      vertices = as.integer(rows$vertex),
      labels = as.character(rows$label)
    )
    candidate_path <- file.path(
      endpoint_candidates_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      sprintf("%s.rds", dataset_id)
    )
    snapshot_path <- file.path(
      endpoint_snapshot_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      sprintf("%s.rds", dataset_id)
    )
    save_rds_safely(out, candidate_path)
    save_rds_safely(out, snapshot_path)
    cleaned_working <- working_endpoint_mark_clean(
      working,
      base_dataset_id = dataset_id,
      base_dataset_label = label,
      base_source_k = ctx$k
    )
    cleaned_working$last_snapshot_id <- dataset_id
    cleaned_working$last_snapshot_label <- label
    save_working_endpoint_state(cleaned_working, ctx = ctx)
    endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    shiny::showNotification(sprintf("Saved snapshot '%s'.", label), type = "message")
    invisible(list(ok = TRUE, dataset_id = dataset_id, label = label, state = cleaned_working))
  }

  endpoint_panel_state <- shiny::reactive({
    endpoint_workspace_revision()
    ctx <- current_endpoint_graph_context()
    manifest <- active_manifest()
    if (!is.list(ctx)) {
      return(list(
        rows = data.frame(),
        working = empty_working_endpoint_state(ctx = ctx),
        context = NULL,
        meta = empty_endpoint_dataset_meta(ctx = ctx),
        draft_banner = NULL
      ))
    }

    focused_rows <- load_external_endpoint_candidates(manifest = manifest, ctx = ctx)
    manifest_rows <- if (is.data.frame(focused_rows) && nrow(focused_rows) > 0L) {
      data.frame()
    } else {
      manifest_endpoint_dataset_rows(manifest = manifest, ctx = ctx)
    }
    workspace_rows <- load_workspace_endpoint_candidates(ctx = ctx)
    row_parts <- Filter(
      f = function(x) is.data.frame(x) && nrow(x) > 0L,
      x = lapply(
        list(focused_rows, manifest_rows, workspace_rows),
        normalize_endpoint_candidate_rows
      )
    )
    rows <- if (length(row_parts) > 0L) {
      do.call(rbind, row_parts)
    } else {
      data.frame()
    }
    if (is.data.frame(rows) && nrow(rows) > 0L) {
      rows <- rows[!duplicated(as.character(rows$dataset_id)), , drop = FALSE]
    }

    meta <- read_endpoint_dataset_meta(ctx)
    working <- load_working_endpoint_state(ctx = ctx)
    working_state_exists <- isTRUE(attr(working, "state_exists", exact = TRUE))

    if (!working_state_exists && is.data.frame(rows) && nrow(rows) > 0L) {
      default_id <- as.character(meta$default_dataset_id %||% "")
      default_hit <- which(as.character(rows$dataset_id) == default_id)
      if (length(default_hit) > 0L) {
        working <- working_endpoint_state_from_dataset(rows[default_hit[[1]], , drop = FALSE])
      }
    }

    if (!is.data.frame(rows) || nrow(rows) < 1L) {
      draft_banner <- if (working_endpoint_is_recovered(working) && !isTRUE(endpoint_draft_banner_dismissed())) {
        list(kind = "recovered")
      } else {
        NULL
      }
      return(list(rows = data.frame(), working = working, context = ctx, meta = meta, draft_banner = draft_banner))
    }

    default_id <- as.character(meta$default_dataset_id %||% "")
    rows$is_default <- as.character(rows$dataset_id) == default_id

    current_k <- suppressWarnings(as.integer(ctx$k %||% NA_integer_))
    k_distance_rank <- if (is.finite(current_k) && "k" %in% names(rows)) {
      out <- rep(Inf, nrow(rows))
      kk <- suppressWarnings(as.integer(rows$k))
      keep <- is.finite(kk)
      out[keep] <- abs(kk[keep] - as.integer(current_k))
      out
    } else {
      rep(Inf, nrow(rows))
    }
    current_k_match <- if (is.finite(current_k) && "k" %in% names(rows)) {
      kk <- suppressWarnings(as.integer(rows$k))
      is.finite(kk) & kk == as.integer(current_k)
    } else {
      rep(FALSE, nrow(rows))
    }
    autoselect_rank <- if ("autoselect" %in% names(rows)) {
      !as.logical(rows$autoselect)
    } else {
      rep(TRUE, nrow(rows))
    }
    sort_quantile_rank <- if ("sort_quantile" %in% names(rows)) {
      suppressWarnings(as.numeric(rows$sort_quantile))
    } else {
      rep(NA_real_, nrow(rows))
    }
    ord <- order(
      !(as.character(rows$dataset_id) == as.character(working$base_dataset_id %||% "")),
      !as.logical(rows$is_default),
      !current_k_match,
      k_distance_rank,
      autoselect_rank,
      sort_quantile_rank,
      as.character(rows$origin),
      as.character(rows$method),
      as.character(rows$label),
      na.last = TRUE
    )
    rows <- rows[ord, , drop = FALSE]
    rows$selected <- as.character(rows$dataset_id) %in% endpoint_overlay_selection()
    rows$is_working_source <- as.character(rows$dataset_id) == as.character(working$base_dataset_id %||% "")
    draft_banner <- if (working_endpoint_is_recovered(working) && !isTRUE(endpoint_draft_banner_dismissed())) {
      list(kind = "recovered")
    } else {
      NULL
    }

    list(rows = rows, working = working, context = ctx, meta = meta, draft_banner = draft_banner)
  })

  endpoint_dataset_row_by_id <- function(dataset_id, panel_state = NULL) {
    st <- if (is.list(panel_state)) panel_state else endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    if (nrow(rows) < 1L) {
      return(NULL)
    }
    hit <- which(as.character(rows$dataset_id) == as.character(dataset_id))
    if (length(hit) < 1L) {
      return(NULL)
    }
    rows[hit[[1]], , drop = FALSE]
  }

  working_endpoint_has_content <- function(state) {
    is.list(state) && is.data.frame(state$rows) && nrow(state$rows) > 0L
  }

  working_endpoint_needs_replace_prompt <- function(state) {
    working_endpoint_is_modified(state)
  }

  save_workspace_endpoint_dataset_object <- function(row_df, updater) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L || !is.function(updater)) {
      return(invisible(FALSE))
    }
    workspace_file <- as.character(row_df$workspace_file[[1]] %||% "")
    if (!nzchar(workspace_file) || !file.exists(workspace_file)) {
      return(invisible(FALSE))
    }
    obj <- read_rds_if_exists(workspace_file, default = NULL)
    if (!is.list(obj)) {
      return(invisible(FALSE))
    }
    obj <- updater(obj)
    save_rds_safely(obj, workspace_file)
    ctx <- current_endpoint_graph_context()
    if (is.list(ctx)) {
      snap_file <- file.path(
        endpoint_snapshot_dir(
          graph_set_id = ctx$graph_set_id,
          k = ctx$k,
          project_id = ctx$project_id
        ),
        basename(workspace_file)
      )
      if (file.exists(snap_file)) {
        save_rds_safely(obj, snap_file)
      }
    }
    endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  rename_workspace_endpoint_dataset <- function(dataset_id, label) {
    row <- endpoint_dataset_row_by_id(dataset_id)
    if (!is.data.frame(row) || nrow(row) < 1L || !isTRUE(row$can_rename[[1]])) {
      return(invisible(FALSE))
    }
    label_use <- trimws(as.character(label %||% ""))
    if (!nzchar(label_use)) {
      return(invisible(FALSE))
    }
    save_workspace_endpoint_dataset_object(row, function(obj) {
      obj$label <- label_use
      obj
    })
    st <- endpoint_panel_state()
    working <- if (is.list(st)) st$working else NULL
    ctx <- if (is.list(st)) st$context else NULL
    if (is.list(ctx) &&
        is.list(working) &&
        identical(as.character(working$base_dataset_id %||% ""), as.character(dataset_id))) {
      working$base_dataset_label <- label_use
      save_working_endpoint_state(working, ctx = ctx)
    }
    meta <- if (is.list(st)) st$meta else NULL
    if (is.list(ctx) &&
        is.list(meta) &&
        identical(as.character(meta$default_dataset_id %||% ""), as.character(dataset_id))) {
      save_endpoint_dataset_meta(meta, ctx = ctx)
    }
    shiny::showNotification(sprintf("Renamed endpoint dataset to '%s'.", label_use), type = "message")
    invisible(TRUE)
  }

  delete_workspace_endpoint_dataset <- function(dataset_id) {
    st <- endpoint_panel_state()
    row <- endpoint_dataset_row_by_id(dataset_id, panel_state = st)
    ctx <- if (is.list(st)) st$context else NULL
    if (!is.data.frame(row) || nrow(row) < 1L || !isTRUE(row$can_delete[[1]]) || !is.list(ctx)) {
      return(invisible(FALSE))
    }
    workspace_file <- as.character(row$workspace_file[[1]] %||% "")
    if (nzchar(workspace_file) && file.exists(workspace_file)) {
      unlink(workspace_file, force = TRUE)
    }
    snap_file <- file.path(
      endpoint_snapshot_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      basename(workspace_file)
    )
    if (nzchar(snap_file) && file.exists(snap_file)) {
      unlink(snap_file, force = TRUE)
    }
    meta <- if (is.list(st)) st$meta else empty_endpoint_dataset_meta(ctx = ctx)
    if (identical(as.character(meta$default_dataset_id %||% ""), as.character(dataset_id))) {
      meta$default_dataset_id <- NA_character_
      save_endpoint_dataset_meta(meta, ctx = ctx)
    }
    working <- if (is.list(st)) st$working else NULL
    if (is.list(working) &&
        identical(as.character(working$base_dataset_id %||% ""), as.character(dataset_id))) {
      working$base_dataset_id <- NA_character_
      working$base_dataset_label <- NA_character_
      working$base_source_k <- NA_integer_
      save_working_endpoint_state(working, ctx = ctx)
    } else {
      endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    }
    shiny::showNotification(sprintf("Deleted endpoint dataset '%s'.", as.character(row$label[[1]] %||% dataset_id)), type = "message")
    invisible(TRUE)
  }

  set_default_endpoint_dataset <- function(dataset_id) {
    st <- endpoint_panel_state()
    ctx <- if (is.list(st)) st$context else NULL
    row <- endpoint_dataset_row_by_id(dataset_id, panel_state = st)
    if (!is.list(ctx) || !is.data.frame(row) || nrow(row) < 1L) {
      return(invisible(FALSE))
    }
    meta <- if (is.list(st)) st$meta else empty_endpoint_dataset_meta(ctx = ctx)
    meta$default_dataset_id <- as.character(dataset_id)
    save_endpoint_dataset_meta(meta, ctx = ctx)
    shiny::showNotification(sprintf("Set '%s' as the default endpoint dataset.", as.character(row$label[[1]] %||% dataset_id)), type = "message")
    invisible(TRUE)
  }

  show_endpoint_dataset_load_modal <- function(dataset_id) {
    row <- endpoint_dataset_row_by_id(dataset_id)
    if (!is.data.frame(row) || nrow(row) < 1L) {
      return(invisible(FALSE))
    }
    endpoint_pending_load_dataset_id(as.character(dataset_id))
    shiny::showModal(
      shiny::modalDialog(
        title = "Replace Working Endpoints",
        easyClose = FALSE,
        shiny::p(sprintf(
          "The current working endpoint draft has unsaved modifications. What do you want to do before loading '%s'?",
          as.character(row$label[[1]] %||% dataset_id)
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("endpoint_replace_working_set", "Replace Working Set", class = "btn-secondary"),
          shiny::actionButton("endpoint_snapshot_replace_working_set", "Save Snapshot And Replace", class = "btn-primary")
        )
      )
    )
    invisible(TRUE)
  }

  maybe_load_endpoint_dataset <- function(dataset_id) {
    st <- endpoint_panel_state()
    working <- if (is.list(st)) st$working else NULL
    if (working_endpoint_needs_replace_prompt(working)) {
      show_endpoint_dataset_load_modal(dataset_id)
      return(invisible(FALSE))
    }
    use_endpoint_dataset_as_working_set(dataset_id)
  }

  discard_working_endpoint_draft <- function() {
    st <- endpoint_panel_state()
    ctx <- if (is.list(st)) st$context else NULL
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    working <- if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx)
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    target <- NULL
    base_id <- as.character(working$base_dataset_id %||% "")
    if (nzchar(base_id) && nrow(rows) > 0L) {
      hit <- which(as.character(rows$dataset_id) == base_id)
      if (length(hit) > 0L) {
        target <- rows[hit[[1]], , drop = FALSE]
      }
    }
    if (is.null(target) && nrow(rows) > 0L) {
      meta <- if (is.list(st)) st$meta else empty_endpoint_dataset_meta(ctx = ctx)
      default_id <- as.character(meta$default_dataset_id %||% "")
      if (nzchar(default_id)) {
        hit <- which(as.character(rows$dataset_id) == default_id)
        if (length(hit) > 0L) {
          target <- rows[hit[[1]], , drop = FALSE]
        }
      }
    }
    next_state <- if (is.data.frame(target) && nrow(target) > 0L) {
      working_endpoint_state_from_dataset(target)
    } else {
      working_endpoint_mark_clean(empty_working_endpoint_state(ctx = ctx))
    }
    save_working_endpoint_state(next_state, ctx = ctx)
    endpoint_show_working_set(nrow(accepted_visible_working_rows(next_state)) > 0L)
    endpoint_draft_banner_dismissed(TRUE)
    shiny::showNotification("Discarded the recovered working draft.", type = "message")
    invisible(TRUE)
  }

  show_endpoint_project_action_modal <- function(action = c("save_project", "exit_project")) {
    action <- match.arg(action)
    endpoint_pending_project_action(action)
    title <- if (identical(action, "exit_project")) "Unsaved Working Endpoints" else "Save Working Draft"
    keep_label <- if (identical(action, "exit_project")) "Keep Draft And Exit" else "Keep Draft And Save Project"
    snapshot_label <- if (identical(action, "exit_project")) "Save Snapshot And Exit" else "Save Snapshot And Save Project"
    shiny::showModal(
      shiny::modalDialog(
        title = title,
        easyClose = FALSE,
        shiny::p("The current working endpoints draft has unsaved modifications."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("endpoint_project_keep_draft", keep_label, class = "btn-secondary"),
          shiny::actionButton("endpoint_project_snapshot_then_continue", snapshot_label, class = "btn-primary")
        )
      )
    )
    invisible(TRUE)
  }

  shiny::observeEvent(endpoint_panel_state(), {
    if (!isTRUE(rv$project.active) || isTRUE(endpoint_autoselect_done())) {
      return()
    }
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    working_rows <- accepted_visible_working_rows(if (is.list(st)) st$working else NULL)
    if (nrow(working_rows) > 0L) {
      endpoint_overlay_selection(character(0))
      if (isTRUE(endpoint_show_working_set_effective(st$working))) {
        endpoint_show_working_set(TRUE)
      }
      endpoint_autoselect_done(TRUE)
      return()
    }
    if (nrow(rows) < 1L) {
      return()
    }

    prev <- endpoint_overlay_selection()
    if (length(prev) > 0L) {
      endpoint_autoselect_done(TRUE)
      return()
    }

    auto_keys <- character(0)
    if ("autoselect" %in% names(rows)) {
      auto_flag <- as.logical(rows$autoselect)
      auto_flag[is.na(auto_flag)] <- FALSE
      current_k <- suppressWarnings(as.integer(st$context$k %||% NA_integer_))
      current_auto <- rep(FALSE, nrow(rows))
      if (is.finite(current_k) && "k" %in% names(rows)) {
        kk <- suppressWarnings(as.integer(rows$k))
        current_auto <- auto_flag & is.finite(kk) & kk == as.integer(current_k)
      }
      auto_keys <- if (any(current_auto)) {
        as.character(rows$dataset_id[current_auto])
      } else {
        as.character(rows$dataset_id[auto_flag])
      }
      auto_keys <- unique(auto_keys[nzchar(auto_keys)])
    }
    keys <- if (length(auto_keys) > 0L) {
      auto_keys
    } else {
      current_k <- suppressWarnings(as.integer(st$context$k %||% NA_integer_))
      if (is.finite(current_k) && "k" %in% names(rows)) {
        kk <- suppressWarnings(as.integer(rows$k))
        current_keys <- as.character(rows$dataset_id[is.finite(kk) & kk == as.integer(current_k)])
        current_keys <- unique(current_keys[nzchar(current_keys)])
        if (length(current_keys) > 0L) current_keys else as.character(rows$dataset_id %||% character(0))
      } else {
        as.character(rows$dataset_id %||% character(0))
      }
    }
    keys <- unique(keys[nzchar(keys)])
    if (length(keys) > 0L) {
      endpoint_overlay_selection(keys)
    }
    endpoint_autoselect_done(TRUE)
  }, ignoreInit = FALSE, priority = 100)

  shiny::observe({
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    if (nrow(rows) < 1L) {
      empty_counts <- structure(integer(0), names = character(0))
      set_reactive_val_if_changed(endpoint_overlay_selection, character(0))
      set_reactive_val_if_changed(endpoint_dataset_load_counts, empty_counts)
      set_reactive_val_if_changed(endpoint_dataset_rename_counts, empty_counts)
      set_reactive_val_if_changed(endpoint_dataset_delete_counts, empty_counts)
      set_reactive_val_if_changed(endpoint_dataset_default_counts, empty_counts)
      return()
    }

    prev <- shiny::isolate(endpoint_overlay_selection())
    sel <- character(0)
    load_counts_prev <- shiny::isolate(endpoint_dataset_load_counts())
    load_counts_next <- structure(integer(0), names = character(0))
    rename_counts_prev <- shiny::isolate(endpoint_dataset_rename_counts())
    rename_counts_next <- structure(integer(0), names = character(0))
    delete_counts_prev <- shiny::isolate(endpoint_dataset_delete_counts())
    delete_counts_next <- structure(integer(0), names = character(0))
    default_counts_prev <- shiny::isolate(endpoint_dataset_default_counts())
    default_counts_next <- structure(integer(0), names = character(0))

    for (ii in seq_len(nrow(rows))) {
      in_id <- as.character(rows$input_id[[ii]] %||% "")
      key <- as.character(rows$dataset_id[[ii]] %||% "")
      if (nzchar(in_id) && nzchar(key)) {
        vv <- input[[in_id]]
        if (isTRUE(vv)) {
          sel <- c(sel, key)
        } else if (is.null(vv) && key %in% prev) {
          sel <- c(sel, key)
        }
      }

      load_id <- as.character(rows$load_input_id[[ii]] %||% "")
      if (nzchar(load_id) && nzchar(key)) {
        cur_count <- scalar_int(input[[load_id]], default = 0L)
        has_prev <- key %in% names(load_counts_prev)
        prev_raw <- if (has_prev) load_counts_prev[[key]] else cur_count
        prev_count <- scalar_int(prev_raw, default = 0L)
        if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
          maybe_load_endpoint_dataset(key)
        }
        load_counts_next[[key]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
      }

      rename_id <- as.character(rows$rename_input_id[[ii]] %||% "")
      if (nzchar(rename_id) && nzchar(key)) {
        cur_count <- scalar_int(input[[rename_id]], default = 0L)
        has_prev <- key %in% names(rename_counts_prev)
        prev_raw <- if (has_prev) rename_counts_prev[[key]] else cur_count
        prev_count <- scalar_int(prev_raw, default = 0L)
        if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
          endpoint_pending_load_dataset_id(key)
          shiny::showModal(
            shiny::modalDialog(
              title = "Rename Endpoint Dataset",
              easyClose = FALSE,
              shiny::textInput(
                "endpoint_dataset_rename_value",
                "Dataset name",
                value = as.character(rows$label[[ii]] %||% key)
              ),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("endpoint_dataset_rename_confirm", "Rename", class = "btn-primary")
              )
            )
          )
        }
        rename_counts_next[[key]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
      }

      delete_id <- as.character(rows$delete_input_id[[ii]] %||% "")
      if (nzchar(delete_id) && nzchar(key)) {
        cur_count <- scalar_int(input[[delete_id]], default = 0L)
        has_prev <- key %in% names(delete_counts_prev)
        prev_raw <- if (has_prev) delete_counts_prev[[key]] else cur_count
        prev_count <- scalar_int(prev_raw, default = 0L)
        if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
          endpoint_pending_load_dataset_id(key)
          shiny::showModal(
            shiny::modalDialog(
              title = "Delete Endpoint Dataset",
              easyClose = FALSE,
              shiny::p(sprintf("Delete '%s'?", as.character(rows$label[[ii]] %||% key))),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("endpoint_dataset_delete_confirm", "Delete", class = "btn-danger")
              )
            )
          )
        }
        delete_counts_next[[key]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
      }

      default_id <- as.character(rows$default_input_id[[ii]] %||% "")
      if (nzchar(default_id) && nzchar(key)) {
        cur_count <- scalar_int(input[[default_id]], default = 0L)
        has_prev <- key %in% names(default_counts_prev)
        prev_raw <- if (has_prev) default_counts_prev[[key]] else cur_count
        prev_count <- scalar_int(prev_raw, default = 0L)
        if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
          set_default_endpoint_dataset(key)
        }
        default_counts_next[[key]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
      }
    }

    set_reactive_val_if_changed(endpoint_overlay_selection, unique(sel))
    set_reactive_val_if_changed(endpoint_dataset_load_counts, load_counts_next)
    set_reactive_val_if_changed(endpoint_dataset_rename_counts, rename_counts_next)
    set_reactive_val_if_changed(endpoint_dataset_delete_counts, delete_counts_next)
    set_reactive_val_if_changed(endpoint_dataset_default_counts, default_counts_next)
  })

  shiny::observeEvent(input$endpoint_replace_working_set, {
    dataset_id <- as.character(endpoint_pending_load_dataset_id() %||% "")
    shiny::removeModal()
    if (nzchar(dataset_id)) {
      use_endpoint_dataset_as_working_set(dataset_id)
    }
    endpoint_pending_load_dataset_id("")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_snapshot_replace_working_set, {
    dataset_id <- as.character(endpoint_pending_load_dataset_id() %||% "")
    shiny::removeModal()
    snap <- save_working_endpoint_snapshot()
    if (is.list(snap) && isTRUE(snap$ok) && nzchar(dataset_id)) {
      use_endpoint_dataset_as_working_set(dataset_id)
    }
    endpoint_pending_load_dataset_id("")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_dataset_rename_confirm, {
    dataset_id <- as.character(endpoint_pending_load_dataset_id() %||% "")
    new_label <- as.character(input$endpoint_dataset_rename_value %||% "")
    shiny::removeModal()
    if (nzchar(dataset_id)) {
      rename_workspace_endpoint_dataset(dataset_id, new_label)
    }
    endpoint_pending_load_dataset_id("")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_dataset_delete_confirm, {
    dataset_id <- as.character(endpoint_pending_load_dataset_id() %||% "")
    shiny::removeModal()
    if (nzchar(dataset_id)) {
      delete_workspace_endpoint_dataset(dataset_id)
    }
    endpoint_pending_load_dataset_id("")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_recovered_continue, {
    endpoint_draft_banner_dismissed(TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_recovered_save_snapshot, {
    endpoint_draft_banner_dismissed(TRUE)
    save_working_endpoint_snapshot()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_recovered_discard, {
    discard_working_endpoint_draft()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_project_keep_draft, {
    action <- as.character(endpoint_pending_project_action() %||% "")
    shiny::removeModal()
    endpoint_pending_project_action("")
    if (identical(action, "save_project")) {
      ok <- save_current_project()
      if (isTRUE(ok)) {
        shiny::showNotification(
          sprintf("Project '%s' saved.", rv$project.name %||% "Untitled Project"),
          type = "message"
        )
      } else {
        shiny::showNotification("Unable to save current project.", type = "error")
      }
    } else if (identical(action, "exit_project")) {
      close_project()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_project_snapshot_then_continue, {
    action <- as.character(endpoint_pending_project_action() %||% "")
    shiny::removeModal()
    snap <- save_working_endpoint_snapshot()
    endpoint_pending_project_action("")
    if (!is.list(snap) || !isTRUE(snap$ok)) {
      return()
    }
    if (identical(action, "save_project")) {
      ok <- save_current_project()
      if (isTRUE(ok)) {
        shiny::showNotification(
          sprintf("Project '%s' saved.", rv$project.name %||% "Untitled Project"),
          type = "message"
        )
      } else {
        shiny::showNotification("Unable to save current project.", type = "error")
      }
    } else if (identical(action, "exit_project")) {
      close_project()
    }
  }, ignoreInit = TRUE)

  shiny::observe({
    st <- endpoint_panel_state()
    working_rows <- accepted_visible_working_rows(if (is.list(st)) st$working else NULL)
    ctx <- if (is.list(st)) st$context else NULL
    prev_values <- shiny::isolate(endpoint_working_label_event_values())
    next_values <- structure(character(0), names = character(0))

    if (nrow(working_rows) < 1L || !is.list(ctx)) {
      set_reactive_val_if_changed(endpoint_working_label_event_values, next_values)
      return()
    }

    for (ii in seq_len(nrow(working_rows))) {
      rr <- working_rows[ii, , drop = FALSE]
      vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
      if (!is.finite(vid) || vid < 1L) {
        next
      }
      input_id <- endpoint_working_label_event_id(vid)
      evt_val <- input[[input_id]]
      if (is.null(evt_val)) {
        next
      }
      normalized_evt <- normalize_working_endpoint_label(
        label = evt_val,
        vertex_id = vid,
        auto_label = rr$auto_label[[1]] %||% sprintf("v%d", vid)
      )
      current_label <- normalize_working_endpoint_label(
        label = rr$label[[1]] %||% "",
        vertex_id = vid,
        auto_label = rr$auto_label[[1]] %||% sprintf("v%d", vid)
      )
      prev_val <- if (input_id %in% names(prev_values)) prev_values[[input_id]] else current_label
      if (!identical(normalized_evt, prev_val)) {
        updated <- update_working_endpoint_vertex_label_state(
          state = if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx),
          vertex_id = vid,
          label = normalized_evt
        )
        save_working_endpoint_state(updated, ctx = ctx)
        next_values[[input_id]] <- normalize_working_endpoint_label(
          label = normalized_evt,
          vertex_id = vid,
          auto_label = rr$auto_label[[1]] %||% sprintf("v%d", vid)
        )
      } else {
        next_values[[input_id]] <- normalized_evt
      }
    }

    set_reactive_val_if_changed(endpoint_working_label_event_values, next_values)
  })

  shiny::observe({
    st <- endpoint_panel_state()
    working_rows <- accepted_visible_working_rows(if (is.list(st)) st$working else NULL)
    ctx <- if (is.list(st)) st$context else NULL
    prev_counts <- shiny::isolate(endpoint_working_hide_counts())
    next_counts <- structure(integer(0), names = character(0))

    if (nrow(working_rows) < 1L || !is.list(ctx)) {
      set_reactive_val_if_changed(endpoint_working_hide_counts, next_counts)
      return()
    }

    for (ii in seq_len(nrow(working_rows))) {
      rr <- working_rows[ii, , drop = FALSE]
      vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
      if (!is.finite(vid) || vid < 1L) {
        next
      }
      input_id <- endpoint_working_hide_input_id(vid)
      cur_count <- scalar_int(input[[input_id]], default = 0L)
      has_prev <- input_id %in% names(prev_counts)
      prev_raw <- if (has_prev) prev_counts[[input_id]] else cur_count
      prev_count <- scalar_int(prev_raw, default = 0L)
      if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
        updated <- hide_working_endpoint_vertex_state(
          state = if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx),
          vertex_id = vid
        )
        save_working_endpoint_state(updated, ctx = ctx)
        shiny::showNotification(sprintf("Hid working endpoint v%d.", vid), type = "message")
      }
      next_counts[[input_id]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
    }

    set_reactive_val_if_changed(endpoint_working_hide_counts, next_counts)
  })

  shiny::observe({
    st <- endpoint_panel_state()
    hidden_rows <- accepted_hidden_working_rows(if (is.list(st)) st$working else NULL)
    ctx <- if (is.list(st)) st$context else NULL
    prev_counts <- shiny::isolate(endpoint_working_restore_counts())
    next_counts <- structure(integer(0), names = character(0))

    if (nrow(hidden_rows) < 1L || !is.list(ctx)) {
      set_reactive_val_if_changed(endpoint_working_restore_counts, next_counts)
      return()
    }

    for (ii in seq_len(nrow(hidden_rows))) {
      rr <- hidden_rows[ii, , drop = FALSE]
      vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
      if (!is.finite(vid) || vid < 1L) {
        next
      }
      input_id <- endpoint_working_restore_input_id(vid)
      cur_count <- scalar_int(input[[input_id]], default = 0L)
      has_prev <- input_id %in% names(prev_counts)
      prev_raw <- if (has_prev) prev_counts[[input_id]] else cur_count
      prev_count <- scalar_int(prev_raw, default = 0L)
      if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
        updated <- restore_working_endpoint_vertex_state(
          state = if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx),
          vertex_id = vid
        )
        save_working_endpoint_state(updated, ctx = ctx)
        shiny::showNotification(sprintf("Restored working endpoint v%d.", vid), type = "message")
      }
      next_counts[[input_id]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
    }

    set_reactive_val_if_changed(endpoint_working_restore_counts, next_counts)
  })

  shiny::observe({
    st <- endpoint_panel_state()
    hidden_rows <- accepted_hidden_working_rows(if (is.list(st)) st$working else NULL)
    ctx <- if (is.list(st)) st$context else NULL
    prev_counts <- shiny::isolate(endpoint_working_delete_counts())
    next_counts <- structure(integer(0), names = character(0))

    if (nrow(hidden_rows) < 1L || !is.list(ctx)) {
      set_reactive_val_if_changed(endpoint_working_delete_counts, next_counts)
      return()
    }

    for (ii in seq_len(nrow(hidden_rows))) {
      rr <- hidden_rows[ii, , drop = FALSE]
      vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
      if (!is.finite(vid) || vid < 1L) {
        next
      }
      input_id <- endpoint_working_delete_input_id(vid)
      cur_count <- scalar_int(input[[input_id]], default = 0L)
      has_prev <- input_id %in% names(prev_counts)
      prev_raw <- if (has_prev) prev_counts[[input_id]] else cur_count
      prev_count <- scalar_int(prev_raw, default = 0L)
      if (has_prev && is.finite(cur_count) && cur_count > prev_count) {
        updated <- remove_working_endpoint_vertex_state(
          state = if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx),
          vertex_id = vid
        )
        save_working_endpoint_state(updated, ctx = ctx)
        shiny::showNotification(sprintf("Deleted hidden endpoint v%d.", vid), type = "message")
      }
      next_counts[[input_id]] <- if (is.finite(cur_count)) as.integer(cur_count) else 0L
    }

    set_reactive_val_if_changed(endpoint_working_delete_counts, next_counts)
  })

  shiny::observeEvent(input$endpoint_working_clear, {
    ctx <- current_endpoint_graph_context()
    if (!is.list(ctx)) {
      return()
    }
    st <- endpoint_panel_state()
    current <- if (is.list(st)) st$working else empty_working_endpoint_state(ctx = ctx)
    cleared <- working_endpoint_mark_modified(current)
    cleared$rows <- empty_working_endpoint_rows()
    save_working_endpoint_state(cleared, ctx = ctx)
    endpoint_draft_banner_dismissed(FALSE)
    shiny::showNotification("Working endpoint set cleared.", type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$endpoint_working_snapshot, {
    save_working_endpoint_snapshot()
  }, ignoreInit = TRUE)

  endpoint_overlay_active <- shiny::reactive({
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    working <- if (is.list(st)) st$working else empty_working_endpoint_state()

    vertices_all <- integer(0)
    label_lookup <- structure(character(0), names = character(0))

    if (is.data.frame(rows) && nrow(rows) > 0L) {
      selected <- intersect(endpoint_overlay_selection(), as.character(rows$dataset_id))
      if (length(selected) > 0L) {
        rows_sel <- rows[rows$dataset_id %in% selected, , drop = FALSE]
        for (ii in seq_len(nrow(rows_sel))) {
          res <- read_endpoint_dataset_from_row(rows_sel[ii, , drop = FALSE])
          norm <- normalize_endpoint_labels(res$vertices, res$labels)
          vv <- norm$vertices
          labs <- norm$labels
          if (length(vv) < 1L) {
            next
          }
          vertices_all <- c(vertices_all, vv)
          for (jj in seq_along(vv)) {
            nm <- as.character(vv[[jj]])
            if (!nm %in% names(label_lookup) || !nzchar(label_lookup[[nm]])) {
              label_lookup[[nm]] <- labs[[jj]]
            }
          }
        }
      }
    }

    working_rows <- accepted_visible_working_rows(working)
    if (isTRUE(endpoint_show_working_set_effective(working)) && nrow(working_rows) > 0L) {
      if (nrow(working_rows) > 0L) {
        vertices_all <- c(vertices_all, as.integer(working_rows$vertex))
        for (ii in seq_len(nrow(working_rows))) {
          nm <- as.character(working_rows$vertex[[ii]])
          label_lookup[[nm]] <- as.character(working_rows$label[[ii]] %||% sprintf("v%d", working_rows$vertex[[ii]]))
        }
      }
    }

    vertices_all <- sort(unique(suppressWarnings(as.integer(vertices_all))))
    vertices_all <- vertices_all[is.finite(vertices_all) & vertices_all > 0L]
    list(vertices = vertices_all, labels = label_lookup)
  })

  current_arm_graph_context <- shiny::reactive({
    ctx <- current_endpoint_graph_context()
    if (is.list(ctx)) ctx else NULL
  })

  arm_context_key <- shiny::reactive({
    ctx <- current_arm_graph_context()
    if (!is.list(ctx)) {
      return("")
    }
    sprintf("%s|%s", ctx$project_id, ctx$graph_set_id)
  })

  shiny::observeEvent(arm_context_key(), {
    if (!nzchar(arm_context_key())) {
      return()
    }
    arm_workspace_revision(isolate(arm_workspace_revision()) + 1L)
    arm_overlay_selection(character(0))
    arm_show_working_set(NA)
    arm_datasets_open(FALSE)
    arm_preview_layout_open(FALSE)
    arm_preview_variant(NULL)
    arm_preview_revision(isolate(arm_preview_revision()) + 1L)
    arm_pending_load_dataset_id("")
    arm_selected_id("")
    arm_draft_banner_dismissed(FALSE)
  }, ignoreInit = TRUE)

  arm_graph_data <- shiny::reactive({
    st <- reference_view_state()
    if (!is.list(st) || !is.null(st$error) || !is.list(st$adj_list)) {
      return(NULL)
    }
    list(
      adj_list = st$adj_list,
      weight_list = st$weight_list %||% lapply(st$adj_list, function(nb) rep(1, length(nb %||% integer(0)))),
      coords = st$coords,
      k = suppressWarnings(as.integer(st$k_actual %||% NA_integer_))
    )
  })

  arm_virtual_endpoints <- shiny::reactive({
    gd <- arm_graph_data()
    if (!is.list(gd) || !is.matrix(gd$coords)) {
      return(data.frame())
    }
    center_vertex <- closest_vertex_to_centroid(gd$coords)
    if (!is.finite(center_vertex)) {
      return(data.frame())
    }
    data.frame(
      key = "virtual:center",
      label = "CENTER",
      vertex = as.integer(center_vertex),
      is_virtual = TRUE,
      stringsAsFactors = FALSE
    )
  })

  working_endpoint_choice_rows <- shiny::reactive({
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.list(st$working)) accepted_visible_working_rows(st$working) else empty_working_endpoint_rows()
    if (!is.data.frame(rows) || nrow(rows) < 1L) {
      return(data.frame())
    }
    rows
  })

  arm_builder_endpoint_choices <- shiny::reactive({
    ep_rows <- working_endpoint_choice_rows()
    out <- c("NONE" = "none")
    virt <- arm_virtual_endpoints()
    if (is.data.frame(virt) && nrow(virt) > 0L) {
      out <- c(out, stats::setNames(as.character(virt$key), as.character(virt$label)))
    }
    if (is.data.frame(ep_rows) && nrow(ep_rows) > 0L) {
      ep_labels <- vapply(seq_len(nrow(ep_rows)), function(ii) {
        rr <- ep_rows[ii, , drop = FALSE]
        lbl <- as.character(rr$label[[1]] %||% sprintf("v%d", rr$vertex[[1]]))
        sprintf("%s (v%d)", lbl, as.integer(rr$vertex[[1]]))
      }, character(1))
      ep_vals <- sprintf("vertex:%d", suppressWarnings(as.integer(ep_rows$vertex)))
      out <- c(out, stats::setNames(ep_vals, ep_labels))
    }
    out
  })

  resolve_arm_endpoint_choice <- function(choice_value) {
    choice_chr <- as.character(choice_value %||% "")
    if (!nzchar(choice_chr)) {
      return(NULL)
    }
    if (identical(choice_chr, "virtual:center")) {
      virt <- arm_virtual_endpoints()
      if (!is.data.frame(virt) || nrow(virt) < 1L) {
        return(NULL)
      }
      return(list(
        key = as.character(virt$key[[1]]),
        label = as.character(virt$label[[1]]),
        vertex = suppressWarnings(as.integer(virt$vertex[[1]])),
        is_virtual = TRUE
      ))
    }
    if (startsWith(choice_chr, "vertex:")) {
      vid <- suppressWarnings(as.integer(sub("^vertex:", "", choice_chr)))
      ep_rows <- working_endpoint_choice_rows()
      label_use <- sprintf("v%d", as.integer(vid))
      if (is.data.frame(ep_rows) && nrow(ep_rows) > 0L && is.finite(vid)) {
        hit <- which(suppressWarnings(as.integer(ep_rows$vertex)) == as.integer(vid))
        if (length(hit) > 0L) {
          label_use <- as.character(ep_rows$label[[hit[[1]]]] %||% label_use)
        }
      }
      return(list(
        key = sprintf("v%d", as.integer(vid)),
        label = label_use,
        vertex = as.integer(vid),
        is_virtual = FALSE
      ))
    }
    NULL
  }

  shiny::observeEvent(list(input$arm_endpoint_a, input$arm_endpoint_b), {
    collect_builder_virtual_marker <- function(choice_value, source_tag) {
      resolved <- resolve_arm_endpoint_choice(choice_value)
      if (!is.list(resolved) || !isTRUE(resolved$is_virtual)) {
        return(NULL)
      }
      vv <- suppressWarnings(as.integer(resolved$vertex %||% NA_integer_))
      if (!is.finite(vv) || vv < 1L) {
        return(NULL)
      }
      list(
        key = sprintf("%s|%d|%s", as.character(source_tag %||% "builder"), as.integer(vv), as.character(resolved$label %||% "CENTER")),
        vertex = as.integer(vv),
        label = as.character(resolved$label %||% "CENTER"),
        source = as.character(source_tag %||% "builder")
      )
    }
    next_markers <- list()
    for (mm in list(
      collect_builder_virtual_marker(input$arm_endpoint_a, "builder_a"),
      collect_builder_virtual_marker(input$arm_endpoint_b, "builder_b")
    )) {
      if (!is.list(mm)) {
        next
      }
      next_markers[[mm$key]] <- list(
        vertex = mm$vertex,
        label = mm$label,
        source = mm$source
      )
    }
    current_markers <- isolate(arm_builder_virtual_markers())
    if (!identical(current_markers, next_markers)) {
      arm_builder_virtual_markers(next_markers)
      restore_reference_plot_camera_proxy()
    }
  }, ignoreInit = FALSE)

  read_workspace_arm_dataset <- function(path) {
    obj <- read_rds_if_exists(path, default = NULL)
    if (!is.list(obj)) {
      return(NULL)
    }
    rows <- sanitize_working_arm_state(
      list(
        rows = if (is.data.frame(obj$rows)) obj$rows else empty_working_arm_rows(),
        project_id = as.character(obj$project_id %||% ""),
        graph_set_id = as.character(obj$graph_set_id %||% ""),
        k = suppressWarnings(as.integer(obj$source_k %||% obj$k %||% NA_integer_))
      )
    )$rows
    list(
      dataset_id = as.character(obj$dataset_id %||% tools::file_path_sans_ext(basename(path))),
      label = as.character(obj$label %||% obj$dataset_id %||% basename(path)),
      method = as.character(obj$method %||% "working_snapshot"),
      origin = as.character(obj$origin %||% "workspace"),
      graph_set_id = as.character(obj$graph_set_id %||% ""),
      k = suppressWarnings(as.integer(obj$source_k %||% obj$k %||% NA_integer_)),
      created_at = as.character(obj$created_at %||% ""),
      parameter_summary = as.character(obj$parameter_summary %||% "workspace arm dataset"),
      source_dataset_id = as.character(obj$source_dataset_id %||% ""),
      rows = rows,
      path = as.character(path %||% "")
    )
  }

  empty_arm_candidate_rows <- function() {
    data.frame(
      dataset_id = character(0),
      key = character(0),
      source_type = character(0),
      origin = character(0),
      label = character(0),
      method = character(0),
      k = integer(0),
      k_display = character(0),
      n_arms = integer(0),
      parameter_summary = character(0),
      workspace_file = character(0),
      created_at = character(0),
      can_load = logical(0),
      can_rename = logical(0),
      can_delete = logical(0),
      can_set_default = logical(0),
      is_default = logical(0),
      stringsAsFactors = FALSE
    )
  }

  normalize_arm_candidate_rows <- function(x) {
    template <- empty_arm_candidate_rows()
    if (!is.data.frame(x) || nrow(x) < 1L) {
      return(template[0, , drop = FALSE])
    }
    missing_cols <- setdiff(names(template), names(x))
    if (length(missing_cols) > 0L) {
      for (cc in missing_cols) {
        x[[cc]] <- template[[cc]]
      }
    }
    x <- x[, names(template), drop = FALSE]
    x$dataset_id <- as.character(x$dataset_id)
    x$key <- as.character(x$key)
    x$source_type <- as.character(x$source_type)
    x$origin <- as.character(x$origin)
    x$label <- as.character(x$label)
    x$method <- as.character(x$method)
    x$k <- suppressWarnings(as.integer(x$k))
    x$k_display <- as.character(x$k_display)
    x$n_arms <- suppressWarnings(as.integer(x$n_arms))
    x$parameter_summary <- as.character(x$parameter_summary)
    x$workspace_file <- as.character(x$workspace_file)
    x$created_at <- as.character(x$created_at)
    x$can_load <- as.logical(x$can_load)
    x$can_rename <- as.logical(x$can_rename)
    x$can_delete <- as.logical(x$can_delete)
    x$can_set_default <- as.logical(x$can_set_default)
    x$is_default <- as.logical(x$is_default)
    rownames(x) <- NULL
    x
  }

  arm_candidate_workspace_files <- function(ctx) {
    if (!is.list(ctx)) {
      return(character(0))
    }
    candidate_dir <- arm_candidates_dir(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    if (!nzchar(candidate_dir) || !dir.exists(candidate_dir)) {
      return(character(0))
    }
    normalizePath(
      list.files(candidate_dir, pattern = "\\.rds$", full.names = TRUE),
      mustWork = FALSE
    )
  }

  load_workspace_arm_candidates <- function(ctx) {
    files <- arm_candidate_workspace_files(ctx)
    if (length(files) < 1L) {
      return(empty_arm_candidate_rows())
    }
    rows <- lapply(seq_along(files), function(ii) {
      ds <- read_workspace_arm_dataset(files[[ii]])
      if (!is.list(ds) || !identical(as.character(ds$graph_set_id %||% ""), as.character(ctx$graph_set_id))) {
        return(NULL)
      }
      key <- sanitize_token_id(ds$dataset_id, fallback = sprintf("arm_dataset_%d", ii))
      data.frame(
        dataset_id = key,
        key = key,
        source_type = "workspace",
        origin = as.character(ds$origin %||% "workspace"),
        label = as.character(ds$label %||% key),
        method = as.character(ds$method %||% "working_snapshot"),
        k = suppressWarnings(as.integer(ds$k %||% NA_integer_)),
        k_display = if (is.finite(suppressWarnings(as.integer(ds$k)))) as.character(as.integer(ds$k)) else "-",
        n_arms = if (is.data.frame(ds$rows)) nrow(ds$rows) else 0L,
        parameter_summary = as.character(ds$parameter_summary %||% "workspace arm dataset"),
        workspace_file = as.character(ds$path %||% files[[ii]]),
        created_at = as.character(ds$created_at %||% ""),
        can_load = TRUE,
        can_rename = TRUE,
        can_delete = TRUE,
        can_set_default = TRUE,
        is_default = FALSE,
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) < 1L) {
      return(empty_arm_candidate_rows())
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    normalize_arm_candidate_rows(out)
  }

  read_arm_dataset_rows_from_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(empty_working_arm_rows())
    }
    row <- row_df[1, , drop = FALSE]
    if (!identical(as.character(row$source_type[[1]] %||% "workspace"), "workspace")) {
      return(empty_working_arm_rows())
    }
    ds <- read_workspace_arm_dataset(as.character(row$workspace_file[[1]] %||% ""))
    if (!is.list(ds) || !is.data.frame(ds$rows)) {
      return(empty_working_arm_rows())
    }
    sanitize_working_arm_state(list(rows = ds$rows), ctx = NULL)$rows
  }

  read_arm_dataset_meta <- function(ctx) {
    if (!is.list(ctx)) {
      return(empty_arm_dataset_meta(ctx = ctx))
    }
    meta <- read_rds_if_exists(
      arm_dataset_meta_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      default = NULL
    )
    sanitize_arm_dataset_meta(meta, ctx = ctx)
  }

  save_arm_dataset_meta <- function(meta, ctx) {
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    out <- sanitize_arm_dataset_meta(meta, ctx = ctx)
    out$updated_at <- .gflowui_now()
    save_rds_safely(
      out,
      arm_dataset_meta_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      )
    )
    arm_workspace_revision(isolate(arm_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  load_working_arm_state <- function(ctx) {
    if (!is.list(ctx)) {
      st <- empty_working_arm_state(ctx = ctx)
      attr(st, "state_exists") <- FALSE
      return(st)
    }
    obj <- read_rds_if_exists(
      arm_working_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      default = NULL
    )
    if (is.list(obj)) {
      st <- sanitize_working_arm_state(obj, ctx = ctx)
      attr(st, "state_exists") <- TRUE
      return(st)
    }
    st <- empty_working_arm_state(ctx = ctx)
    attr(st, "state_exists") <- FALSE
    st
  }

  save_working_arm_state <- function(state, ctx) {
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    cleaned <- sanitize_working_arm_state(state, ctx = ctx)
    cleaned$updated_at <- .gflowui_now()
    cleaned$last_session_id <- arm_session_id
    save_rds_safely(
      cleaned,
      arm_working_file(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      )
    )
    arm_workspace_revision(isolate(arm_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  working_arm_state_from_dataset <- function(row_df) {
    ctx <- current_arm_graph_context()
    if (!is.list(ctx) || !is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(empty_working_arm_state(ctx = ctx))
    }
    row <- row_df[1, , drop = FALSE]
    rows <- read_arm_dataset_rows_from_row(row)
    sanitize_working_arm_state(
      list(
        version = "1",
        project_id = ctx$project_id,
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        base_dataset_id = as.character(row$dataset_id[[1]] %||% ""),
        base_dataset_label = as.character(row$label[[1]] %||% row$dataset_id[[1]] %||% ""),
        base_source_k = suppressWarnings(as.integer(row$k[[1]] %||% NA_integer_)),
        is_modified = FALSE,
        last_snapshot_id = as.character(row$dataset_id[[1]] %||% NA_character_),
        last_snapshot_label = as.character(row$label[[1]] %||% NA_character_),
        last_session_id = arm_session_id,
        rows = rows,
        updated_at = .gflowui_now()
      ),
      ctx = ctx
    )
  }

  use_arm_dataset_as_working_set <- function(dataset_id) {
    st <- arm_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    hit <- which(as.character(rows$dataset_id) == as.character(dataset_id))
    if (length(hit) < 1L) {
      return(invisible(FALSE))
    }
    next_state <- working_arm_state_from_dataset(rows[hit[[1]], , drop = FALSE])
    save_working_arm_state(next_state, ctx = current_arm_graph_context())
    arm_show_working_set(TRUE)
    arm_overlay_selection(character(0))
    arm_preview_layout_open(FALSE)
    arm_preview_variant(NULL)
    arm_preview_revision(isolate(arm_preview_revision()) + 1L)
    arm_selected_id("")
    shiny::showNotification(
      sprintf("Working arms loaded from '%s'.", as.character(rows$label[[hit[[1]]]] %||% dataset_id)),
      type = "message"
    )
    invisible(TRUE)
  }

  save_workspace_arm_dataset_object <- function(row_df, updater) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L || !is.function(updater)) {
      return(invisible(FALSE))
    }
    workspace_file <- as.character(row_df$workspace_file[[1]] %||% "")
    if (!nzchar(workspace_file) || !file.exists(workspace_file)) {
      return(invisible(FALSE))
    }
    obj <- read_rds_if_exists(workspace_file, default = NULL)
    if (!is.list(obj)) {
      return(invisible(FALSE))
    }
    obj <- updater(obj)
    save_rds_safely(obj, workspace_file)
    ctx <- current_arm_graph_context()
    if (is.list(ctx)) {
      snap_file <- file.path(
        arm_snapshot_dir(
          graph_set_id = ctx$graph_set_id,
          k = ctx$k,
          project_id = ctx$project_id
        ),
        basename(workspace_file)
      )
      if (file.exists(snap_file)) {
        save_rds_safely(obj, snap_file)
      }
    }
    arm_workspace_revision(isolate(arm_workspace_revision()) + 1L)
    invisible(TRUE)
  }

  arm_dataset_row_by_id <- function(dataset_id, panel_state = NULL) {
    st <- if (is.list(panel_state)) panel_state else arm_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    hit <- which(as.character(rows$dataset_id) == as.character(dataset_id))
    if (length(hit) < 1L) {
      return(NULL)
    }
    rows[hit[[1]], , drop = FALSE]
  }

  rename_workspace_arm_dataset <- function(dataset_id, label) {
    row <- arm_dataset_row_by_id(dataset_id)
    if (!is.data.frame(row) || nrow(row) < 1L || !isTRUE(row$can_rename[[1]])) {
      return(invisible(FALSE))
    }
    label_use <- trimws(as.character(label %||% ""))
    if (!nzchar(label_use)) {
      return(invisible(FALSE))
    }
    save_workspace_arm_dataset_object(row, function(obj) {
      obj$label <- label_use
      obj
    })
    st <- arm_panel_state()
    working <- if (is.list(st)) st$working else NULL
    ctx <- if (is.list(st)) st$context else NULL
    if (is.list(ctx) &&
        is.list(working) &&
        identical(as.character(working$base_dataset_id %||% ""), as.character(dataset_id))) {
      working$base_dataset_label <- label_use
      save_working_arm_state(working, ctx = ctx)
    }
    shiny::showNotification(sprintf("Renamed arm dataset to '%s'.", label_use), type = "message")
    invisible(TRUE)
  }

  delete_workspace_arm_dataset <- function(dataset_id) {
    st <- arm_panel_state()
    row <- arm_dataset_row_by_id(dataset_id, panel_state = st)
    ctx <- if (is.list(st)) st$context else NULL
    if (!is.data.frame(row) || nrow(row) < 1L || !isTRUE(row$can_delete[[1]]) || !is.list(ctx)) {
      return(invisible(FALSE))
    }
    workspace_file <- as.character(row$workspace_file[[1]] %||% "")
    if (nzchar(workspace_file) && file.exists(workspace_file)) {
      unlink(workspace_file, force = TRUE)
    }
    snap_file <- file.path(
      arm_snapshot_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      basename(workspace_file)
    )
    if (nzchar(snap_file) && file.exists(snap_file)) {
      unlink(snap_file, force = TRUE)
    }
    meta <- if (is.list(st)) st$meta else empty_arm_dataset_meta(ctx = ctx)
    if (identical(as.character(meta$default_dataset_id %||% ""), as.character(dataset_id))) {
      meta$default_dataset_id <- NA_character_
      save_arm_dataset_meta(meta, ctx = ctx)
    }
    working <- if (is.list(st)) st$working else NULL
    if (is.list(working) &&
        identical(as.character(working$base_dataset_id %||% ""), as.character(dataset_id))) {
      working$base_dataset_id <- NA_character_
      working$base_dataset_label <- NA_character_
      working$base_source_k <- NA_integer_
      save_working_arm_state(working, ctx = ctx)
    } else {
      arm_workspace_revision(isolate(arm_workspace_revision()) + 1L)
    }
    shiny::showNotification(sprintf("Deleted arm dataset '%s'.", as.character(row$label[[1]] %||% dataset_id)), type = "message")
    invisible(TRUE)
  }

  set_default_arm_dataset <- function(dataset_id) {
    st <- arm_panel_state()
    ctx <- if (is.list(st)) st$context else NULL
    row <- arm_dataset_row_by_id(dataset_id, panel_state = st)
    if (!is.list(ctx) || !is.data.frame(row) || nrow(row) < 1L) {
      return(invisible(FALSE))
    }
    meta <- if (is.list(st)) st$meta else empty_arm_dataset_meta(ctx = ctx)
    meta$default_dataset_id <- as.character(dataset_id)
    save_arm_dataset_meta(meta, ctx = ctx)
    shiny::showNotification(sprintf("Set '%s' as the default arm dataset.", as.character(row$label[[1]] %||% dataset_id)), type = "message")
    invisible(TRUE)
  }

  upsert_working_arm_variant_state <- function(state, variant, source_type = "manual", source_dataset_id = "") {
    out <- sanitize_working_arm_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_arm_rows()
    one <- working_arm_rows_from_variant(
      variant = variant,
      source_type = source_type,
      source_dataset_id = source_dataset_id
    )
    if (nrow(one) < 1L) {
      return(out)
    }
    hit <- which(as.character(rows$arm_id) == as.character(one$arm_id[[1]]))
    if (length(hit) < 1L) {
      rows <- rbind(rows, one)
    } else {
      ii <- hit[[1]]
      rows[ii, names(one)] <- one[1, names(one), drop = FALSE]
    }
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_arm_mark_modified(out, session_id = arm_session_id)
  }

  update_working_arm_label_state <- function(state, arm_id, label) {
    out <- sanitize_working_arm_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_arm_rows()
    hit <- which(as.character(rows$arm_id) == as.character(arm_id))
    if (length(hit) < 1L) {
      return(out)
    }
    ii <- hit[[1]]
    label_use <- trimws(as.character(label %||% ""))
    if (!nzchar(label_use)) {
      label_use <- as.character(rows$family_label[[ii]] %||% rows$label[[ii]] %||% rows$arm_id[[ii]])
    }
    rows$label[[ii]] <- label_use
    rows$updated_at[[ii]] <- .gflowui_now()
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_arm_mark_modified(out, session_id = arm_session_id)
  }

  set_working_arm_visibility_state <- function(state, arm_id, visible = TRUE) {
    out <- sanitize_working_arm_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_arm_rows()
    hit <- which(as.character(rows$arm_id) == as.character(arm_id))
    if (length(hit) < 1L) {
      return(out)
    }
    ii <- hit[[1]]
    rows$visible[[ii]] <- isTRUE(visible)
    rows$updated_at[[ii]] <- .gflowui_now()
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_arm_mark_modified(out, session_id = arm_session_id)
  }

  hide_working_arm_state <- function(state, arm_id) {
    set_working_arm_visibility_state(state = state, arm_id = arm_id, visible = FALSE)
  }

  restore_working_arm_state <- function(state, arm_id) {
    set_working_arm_visibility_state(state = state, arm_id = arm_id, visible = TRUE)
  }

  remove_working_arm_state <- function(state, arm_id) {
    out <- sanitize_working_arm_state(state, ctx = NULL)
    rows <- if (is.data.frame(out$rows)) out$rows else empty_working_arm_rows()
    rows <- rows[as.character(rows$arm_id) != as.character(arm_id), , drop = FALSE]
    out$rows <- rows
    out$updated_at <- .gflowui_now()
    working_arm_mark_modified(out, session_id = arm_session_id)
  }

  save_working_arm_snapshot <- function() {
    st <- arm_panel_state()
    working <- if (is.list(st)) st$working else NULL
    ctx <- current_arm_graph_context()
    if (!is.list(ctx) || !is.list(working)) {
      return(invisible(list(ok = FALSE)))
    }
    rows <- accepted_visible_working_arm_rows(working)
    if (nrow(rows) < 1L) {
      shiny::showNotification("Working arm set is empty.", type = "warning")
      return(invisible(list(ok = FALSE)))
    }
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    dataset_id <- sanitize_token_id(
      sprintf("working_%s_k%03d_%s", ctx$graph_set_id, as.integer(ctx$k), stamp),
      fallback = sprintf("working_arm_snapshot_%s", stamp)
    )
    label <- sprintf("Arm snapshot %s", format(Sys.time(), "%Y-%m-%d %H:%M"))
    out <- list(
      version = "1",
      dataset_id = dataset_id,
      label = label,
      method = "working_snapshot",
      origin = "workspace",
      project_id = as.character(ctx$project_id),
      graph_set_id = as.character(ctx$graph_set_id),
      k = as.integer(ctx$k),
      source_k = as.integer(ctx$k),
      created_at = .gflowui_now(),
      parameter_summary = sprintf(
        "snapshot from %s | source k=%s",
        as.character(working$base_dataset_label %||% working$base_dataset_id %||% "working arms"),
        as.character(ctx$k)
      ),
      source_dataset_id = as.character(working$base_dataset_id %||% ""),
      last_session_id = arm_session_id,
      rows = rows
    )
    candidate_path <- file.path(
      arm_candidates_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      sprintf("%s.rds", dataset_id)
    )
    snapshot_path <- file.path(
      arm_snapshot_dir(
        graph_set_id = ctx$graph_set_id,
        k = ctx$k,
        project_id = ctx$project_id
      ),
      sprintf("%s.rds", dataset_id)
    )
    save_rds_safely(out, candidate_path)
    save_rds_safely(out, snapshot_path)
    cleaned <- working_arm_mark_clean(
      working,
      base_dataset_id = dataset_id,
      base_dataset_label = label,
      base_source_k = ctx$k,
      session_id = arm_session_id
    )
    cleaned$last_snapshot_id <- dataset_id
    cleaned$last_snapshot_label <- label
    save_working_arm_state(cleaned, ctx = ctx)
    shiny::showNotification(sprintf("Saved arm snapshot '%s'.", label), type = "message")
    invisible(list(ok = TRUE, dataset_id = dataset_id, label = label, state = cleaned))
  }

  arm_panel_state <- shiny::reactive({
    arm_workspace_revision()
    ctx <- current_arm_graph_context()
    if (!is.list(ctx)) {
      return(list(
        rows = empty_arm_candidate_rows(),
        working = empty_working_arm_state(ctx = ctx),
        context = NULL,
        meta = empty_arm_dataset_meta(ctx = ctx),
        draft_banner = NULL
      ))
    }
    rows <- load_workspace_arm_candidates(ctx)
    meta <- read_arm_dataset_meta(ctx)
    working <- load_working_arm_state(ctx)
    working_state_exists <- isTRUE(attr(working, "state_exists", exact = TRUE))
    if (!working_state_exists && is.data.frame(rows) && nrow(rows) > 0L) {
      default_id <- as.character(meta$default_dataset_id %||% "")
      hit <- which(as.character(rows$dataset_id) == default_id)
      if (length(hit) > 0L) {
        working <- working_arm_state_from_dataset(rows[hit[[1]], , drop = FALSE])
      }
    }
    if (is.data.frame(rows) && nrow(rows) > 0L) {
      rows$is_default <- as.character(rows$dataset_id) == as.character(meta$default_dataset_id %||% "")
      rows$selected <- as.character(rows$dataset_id) %in% arm_overlay_selection()
      rows$is_working_source <- as.character(rows$dataset_id) == as.character(working$base_dataset_id %||% "")
      current_k <- suppressWarnings(as.integer(ctx$k %||% NA_integer_))
      ord <- order(
        !(as.character(rows$dataset_id) == as.character(working$base_dataset_id %||% "")),
        !as.logical(rows$is_default),
        abs(suppressWarnings(as.integer(rows$k)) - current_k),
        as.character(rows$label),
        na.last = TRUE
      )
      rows <- rows[ord, , drop = FALSE]
    }
    draft_banner <- if (working_arm_is_recovered(working, session_id = arm_session_id) && !isTRUE(arm_draft_banner_dismissed())) {
      list(kind = "recovered")
    } else {
      NULL
    }
    list(rows = rows, working = working, context = ctx, meta = meta, draft_banner = draft_banner)
  })

  working_arm_has_content <- function(state) {
    is.list(state) && is.data.frame(state$rows) && nrow(state$rows) > 0L
  }

  working_arm_needs_replace_prompt <- function(state) {
    working_arm_is_modified(state)
  }

  show_arm_dataset_load_modal <- function(dataset_id) {
    row <- arm_dataset_row_by_id(dataset_id)
    if (!is.data.frame(row) || nrow(row) < 1L) {
      return(invisible(FALSE))
    }
    arm_pending_load_dataset_id(as.character(dataset_id))
    shiny::showModal(
      shiny::modalDialog(
        title = "Replace Working Arms",
        easyClose = FALSE,
        shiny::p(sprintf(
          "The current working arm draft has unsaved modifications. What do you want to do before loading '%s'?",
          as.character(row$label[[1]] %||% dataset_id)
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("arm_replace_working_set", "Replace Working Set", class = "btn-secondary"),
          shiny::actionButton("arm_snapshot_replace_working_set", "Save Snapshot And Replace", class = "btn-primary")
        )
      )
    )
    invisible(TRUE)
  }

  maybe_load_arm_dataset <- function(dataset_id) {
    st <- arm_panel_state()
    working <- if (is.list(st)) st$working else NULL
    if (working_arm_needs_replace_prompt(working)) {
      show_arm_dataset_load_modal(dataset_id)
      return(invisible(FALSE))
    }
    use_arm_dataset_as_working_set(dataset_id)
  }

  discard_working_arm_draft <- function() {
    st <- arm_panel_state()
    ctx <- if (is.list(st)) st$context else NULL
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else empty_arm_candidate_rows()
    working <- if (is.list(st)) st$working else empty_working_arm_state(ctx = ctx)
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    target <- NULL
    base_id <- as.character(working$base_dataset_id %||% "")
    if (nzchar(base_id) && nrow(rows) > 0L) {
      hit <- which(as.character(rows$dataset_id) == base_id)
      if (length(hit) > 0L) {
        target <- rows[hit[[1]], , drop = FALSE]
      }
    }
    if (is.null(target) && nrow(rows) > 0L) {
      default_id <- as.character(st$meta$default_dataset_id %||% "")
      hit <- which(as.character(rows$dataset_id) == default_id)
      if (length(hit) > 0L) {
        target <- rows[hit[[1]], , drop = FALSE]
      }
    }
    next_state <- if (is.data.frame(target) && nrow(target) > 0L) {
      working_arm_state_from_dataset(target)
    } else {
      working_arm_mark_clean(empty_working_arm_state(ctx = ctx), session_id = arm_session_id)
    }
    save_working_arm_state(next_state, ctx = ctx)
    arm_show_working_set(nrow(accepted_visible_working_arm_rows(next_state)) > 0L)
    arm_draft_banner_dismissed(TRUE)
    shiny::showNotification("Discarded the recovered working arm draft.", type = "message")
    invisible(TRUE)
  }

  decode_arm_row <- function(row_df) {
    if (!is.data.frame(row_df) || nrow(row_df) < 1L) {
      return(NULL)
    }
    rr <- row_df[1, , drop = FALSE]
    list(
      arm_id = as.character(rr$arm_id[[1]] %||% ""),
      family_id = as.character(rr$family_id[[1]] %||% ""),
      label = as.character(rr$label[[1]] %||% ""),
      family_label = as.character(rr$family_label[[1]] %||% ""),
      endpoint_a = suppressWarnings(as.integer(rr$endpoint_a[[1]] %||% NA_integer_)),
      endpoint_b = suppressWarnings(as.integer(rr$endpoint_b[[1]] %||% NA_integer_)),
      endpoint_a_label = as.character(rr$endpoint_a_label[[1]] %||% ""),
      endpoint_b_label = as.character(rr$endpoint_b_label[[1]] %||% ""),
      path_method = as.character(rr$path_method[[1]] %||% "weighted_shortest_path"),
      thickening_method = as.character(rr$thickening_method[[1]] %||% "path_only"),
      path_vertices = decode_arm_integer_json(rr$path_vertices_json[[1]] %||% "[]"),
      arm_vertices = decode_arm_integer_json(rr$arm_vertices_json[[1]] %||% "[]"),
      arm_coords = decode_arm_numeric_json(rr$arm_coords_json[[1]] %||% "[]"),
      parameter_summary = as.character(rr$parameter_summary[[1]] %||% ""),
      params = decode_arm_params_json(rr$params_json[[1]] %||% "{}"),
      source_k = suppressWarnings(as.integer(rr$source_k[[1]] %||% NA_integer_)),
      is_preview = FALSE
    )
  }

  arm_show_working_set_effective <- function(working_state) {
    working_rows <- accepted_visible_working_arm_rows(working_state)
    if (nrow(working_rows) < 1L) {
      return(FALSE)
    }
    pref <- arm_show_working_set()
    if (isFALSE(pref)) {
      return(FALSE)
    }
    TRUE
  }

  arm_overlay_active <- shiny::reactive({
    st <- arm_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else empty_arm_candidate_rows()
    working <- if (is.list(st)) st$working else empty_working_arm_state()
    arms <- list()
    virtual_markers <- list()
    idx_out <- 1L
    add_virtual_marker <- function(vertex, label = "CENTER", source = "builder") {
      vv <- suppressWarnings(as.integer(vertex))
      if (!is.finite(vv) || vv < 1L) {
        return(invisible(NULL))
      }
      key <- sprintf("%s|%d|%s", as.character(source %||% "builder"), as.integer(vv), as.character(label %||% "CENTER"))
      virtual_markers[[key]] <<- list(
        vertex = as.integer(vv),
        label = as.character(label %||% "CENTER"),
        source = as.character(source %||% "builder")
      )
      invisible(NULL)
    }
    if (is.data.frame(rows) && nrow(rows) > 0L) {
      selected <- intersect(arm_overlay_selection(), as.character(rows$dataset_id))
      if (length(selected) > 0L) {
        rows_sel <- rows[rows$dataset_id %in% selected, , drop = FALSE]
        for (ii in seq_len(nrow(rows_sel))) {
          ds_rows <- read_arm_dataset_rows_from_row(rows_sel[ii, , drop = FALSE])
          if (!is.data.frame(ds_rows) || nrow(ds_rows) < 1L) {
            next
          }
          for (jj in seq_len(nrow(ds_rows))) {
            one <- decode_arm_row(ds_rows[jj, , drop = FALSE])
            if (is.list(one)) {
              one$source_dataset_id <- as.character(rows_sel$dataset_id[[ii]])
              arms[[idx_out]] <- one
              idx_out <- idx_out + 1L
            }
          }
        }
      }
    }
    if (isTRUE(arm_show_working_set_effective(working))) {
      wr <- accepted_visible_working_arm_rows(working)
      if (nrow(wr) > 0L) {
        for (ii in seq_len(nrow(wr))) {
          one <- decode_arm_row(wr[ii, , drop = FALSE])
          if (is.list(one)) {
            one$source_dataset_id <- as.character(working$base_dataset_id %||% "")
            one$is_working <- TRUE
            arms[[idx_out]] <- one
            idx_out <- idx_out + 1L
          }
        }
      }
    }
    preview <- arm_preview_variant()
    if (is.list(preview)) {
      preview$is_preview <- TRUE
      arms[[idx_out]] <- preview
    }
    builder_markers <- arm_builder_virtual_markers()
    if (is.list(builder_markers) && length(builder_markers) > 0L) {
      for (mm in builder_markers) {
        if (!is.list(mm)) {
          next
        }
        add_virtual_marker(mm$vertex, label = mm$label %||% "CENTER", source = mm$source %||% "builder")
      }
    }
    if (length(arms) > 0L) {
      for (aa in arms) {
        if (!is.list(aa)) {
          next
        }
        if (isTRUE(aa$endpoint_a_virtual)) {
          add_virtual_marker(aa$endpoint_a, label = aa$endpoint_a_label %||% "CENTER", source = aa$arm_id %||% "arm")
        }
        if (isTRUE(aa$endpoint_b_virtual)) {
          add_virtual_marker(aa$endpoint_b, label = aa$endpoint_b_label %||% "CENTER", source = aa$arm_id %||% "arm")
        }
      }
    }
    list(
      arms = arms,
      virtual_markers = unname(virtual_markers),
      selected_id = as.character(arm_selected_id() %||% ""),
      preview_id = if (is.list(preview)) as.character(preview$arm_id %||% "") else ""
    )
  })

  shiny::observe({
    vv <- input$arm_show_working_set
    if (!is.null(vv)) {
      set_reactive_val_if_changed(arm_show_working_set, isTRUE(vv))
    }
  })

  shiny::observe({
    vv <- input$arm_datasets_open
    if (!is.null(vv)) {
      set_reactive_val_if_changed(arm_datasets_open, isTRUE(vv))
    }
  })

  shiny::observe({
    vv <- input$arm_preview_layout_open
    if (!is.null(vv)) {
      set_reactive_val_if_changed(arm_preview_layout_open, isTRUE(vv))
    }
  })

  build_arm_preview_from_inputs <- function(show_error = TRUE) {
    gd <- arm_graph_data()
    ctx <- current_arm_graph_context()
    if (!is.list(gd) || !is.list(ctx)) {
      if (show_error) {
        shiny::showNotification("No active graph is available for arm construction.", type = "error")
      }
      return(NULL)
    }
    a <- resolve_arm_endpoint_choice(input$arm_endpoint_a)
    b <- resolve_arm_endpoint_choice(input$arm_endpoint_b)
    if (!is.list(a) || !is.list(b)) {
      if (show_error) {
        shiny::showNotification("Choose two arm endpoints first.", type = "warning")
      }
      return(NULL)
    }
    thickening_method <- as.character(input$arm_thickening_method %||% "path_only")
    path_relative_radius <- suppressWarnings(as.numeric(input$arm_path_relative_radius %||% 0.10))
    excess_tolerance <- suppressWarnings(as.numeric(input$arm_excess_tolerance %||% NA_real_))
    res <- tryCatch(
      compute_arm_variant(
        adj.list = gd$adj_list,
        weight.list = gd$weight_list,
        coords = gd$coords,
        endpoint_a = a$vertex,
        endpoint_b = b$vertex,
        endpoint_a_key = a$key,
        endpoint_b_key = b$key,
        endpoint_a_label = a$label,
        endpoint_b_label = b$label,
        endpoint_a_virtual = a$is_virtual,
        endpoint_b_virtual = b$is_virtual,
        thickening_method = thickening_method,
        path_relative_radius = path_relative_radius,
        excess_tolerance = excess_tolerance
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      if (show_error) {
        shiny::showNotification(conditionMessage(res), type = "error")
      }
      return(NULL)
    }
    res$source_k <- as.integer(ctx$k)
    res
  }

  add_preview_arm_to_working_set <- function() {
    ctx <- current_arm_graph_context()
    if (!is.list(ctx)) {
      return(invisible(FALSE))
    }
    preview <- arm_preview_variant()
    if (!is.list(preview)) {
      preview <- build_arm_preview_from_inputs(show_error = TRUE)
      if (!is.list(preview)) {
        return(invisible(FALSE))
      }
      arm_preview_variant(preview)
      arm_preview_layout_open(TRUE)
      arm_preview_revision(isolate(arm_preview_revision()) + 1L)
    }
    st <- arm_panel_state()
    working <- if (is.list(st) && is.list(st$working)) st$working else empty_working_arm_state(ctx = ctx)
    updated <- upsert_working_arm_variant_state(
      state = working,
      variant = preview,
      source_type = "manual",
      source_dataset_id = as.character(working$base_dataset_id %||% "")
    )
    save_working_arm_state(updated, ctx = ctx)
    arm_show_working_set(TRUE)
    arm_overlay_selection(character(0))
    arm_selected_id(as.character(preview$arm_id %||% ""))
    shiny::showNotification(sprintf("Added '%s' to Working Arms.", as.character(preview$label %||% preview$family_label %||% "arm")), type = "message")
    invisible(TRUE)
  }

  shiny::observeEvent(input$arm_preview_build_request, {
    req <- input$arm_preview_build_request
    if (is.list(req) && is.list(req$camera)) {
      cam_norm <- normalize_plotly_camera(req$camera)
      if (is.list(cam_norm)) {
        set_reactive_val_if_changed(reference_plot_camera_state, cam_norm)
      }
    }
    preview <- build_arm_preview_from_inputs(show_error = TRUE)
    arm_preview_variant(preview)
    arm_preview_revision(isolate(arm_preview_revision()) + 1L)
    if (is.list(preview)) {
      arm_preview_layout_open(TRUE)
      arm_selected_id(as.character(preview$arm_id %||% ""))
    }
    restore_reference_plot_camera_proxy()
  }, ignoreInit = TRUE)

  shiny::observeEvent(
    arm_builder_preview_inputs(),
    {
      current_preview <- isolate(arm_preview_variant())
      if (!is.list(current_preview)) {
        return()
      }
      preview <- build_arm_preview_from_inputs(show_error = FALSE)
      if (is.list(preview)) {
        arm_preview_variant(preview)
        arm_selected_id(as.character(preview$arm_id %||% ""))
      }
      arm_preview_revision(isolate(arm_preview_revision()) + 1L)
      restore_reference_plot_camera_proxy()
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$arm_add_preview_to_working, {
    add_preview_arm_to_working_set()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_working_snapshot, {
    save_working_arm_snapshot()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_working_clear, {
    ctx <- current_arm_graph_context()
    if (!is.list(ctx)) {
      return()
    }
    next_state <- working_arm_mark_clean(
      empty_working_arm_state(ctx = ctx),
      session_id = arm_session_id
    )
    save_working_arm_state(next_state, ctx = ctx)
    arm_show_working_set(FALSE)
    arm_preview_layout_open(FALSE)
    arm_preview_variant(NULL)
    arm_preview_revision(isolate(arm_preview_revision()) + 1L)
    arm_selected_id("")
    shiny::showNotification("Cleared Working Arms.", type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_dataset_action, {
    event_val <- input$arm_dataset_action
    if (!is.list(event_val)) {
      return()
    }
    action <- as.character(event_val$action %||% "")
    dataset_id <- as.character(event_val$dataset_id %||% "")
    if (!nzchar(action) || !nzchar(dataset_id)) {
      return()
    }
    if (identical(action, "load")) {
      maybe_load_arm_dataset(dataset_id)
    } else if (identical(action, "rename")) {
      row <- arm_dataset_row_by_id(dataset_id)
      if (is.data.frame(row) && nrow(row) > 0L) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Rename Arm Dataset",
            shiny::textInput("arm_dataset_rename_value", "Name", value = as.character(row$label[[1]] %||% dataset_id)),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("arm_dataset_rename_confirm", "Rename", class = "btn-primary")
            ),
            easyClose = FALSE
          )
        )
        arm_pending_load_dataset_id(dataset_id)
      }
    } else if (identical(action, "delete")) {
      delete_workspace_arm_dataset(dataset_id)
    } else if (identical(action, "default")) {
      set_default_arm_dataset(dataset_id)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_dataset_toggle, {
    event_val <- input$arm_dataset_toggle
    if (!is.list(event_val)) {
      return()
    }
    dataset_id <- as.character(event_val$dataset_id %||% "")
    checked <- isTRUE(event_val$checked)
    if (!nzchar(dataset_id)) {
      return()
    }
    prev <- arm_overlay_selection()
    next_sel <- if (checked) {
      unique(c(prev, dataset_id))
    } else {
      setdiff(prev, dataset_id)
    }
    arm_overlay_selection(next_sel)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_dataset_rename_confirm, {
    dataset_id <- as.character(arm_pending_load_dataset_id() %||% "")
    arm_pending_load_dataset_id("")
    shiny::removeModal()
    if (nzchar(dataset_id)) {
      rename_workspace_arm_dataset(dataset_id, input$arm_dataset_rename_value %||% "")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_replace_working_set, {
    dataset_id <- as.character(arm_pending_load_dataset_id() %||% "")
    arm_pending_load_dataset_id("")
    shiny::removeModal()
    if (nzchar(dataset_id)) {
      use_arm_dataset_as_working_set(dataset_id)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_snapshot_replace_working_set, {
    dataset_id <- as.character(arm_pending_load_dataset_id() %||% "")
    arm_pending_load_dataset_id("")
    shiny::removeModal()
    snap <- save_working_arm_snapshot()
    if (isTRUE(snap$ok) && nzchar(dataset_id)) {
      use_arm_dataset_as_working_set(dataset_id)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_recovered_continue, {
    arm_draft_banner_dismissed(TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_recovered_save_snapshot, {
    save_working_arm_snapshot()
    arm_draft_banner_dismissed(TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_recovered_discard, {
    discard_working_arm_draft()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_working_action, {
    event_val <- input$arm_working_action
    if (!is.list(event_val)) {
      return()
    }
    action <- as.character(event_val$action %||% "")
    arm_id <- as.character(event_val$arm_id %||% "")
    if (!nzchar(action) || !nzchar(arm_id)) {
      return()
    }
    st <- arm_panel_state()
    ctx <- if (is.list(st)) st$context else current_arm_graph_context()
    working <- if (is.list(st)) st$working else empty_working_arm_state(ctx = ctx)
    if (!is.list(ctx)) {
      return()
    }
    if (identical(action, "select")) {
      arm_selected_id(arm_id)
      return()
    }
    next_state <- if (identical(action, "hide")) {
      hide_working_arm_state(working, arm_id)
    } else if (identical(action, "restore")) {
      restore_working_arm_state(working, arm_id)
    } else if (identical(action, "delete")) {
      remove_working_arm_state(working, arm_id)
    } else {
      working
    }
    save_working_arm_state(next_state, ctx = ctx)
    if (identical(action, "hide") || identical(action, "delete")) {
      if (identical(as.character(arm_selected_id() %||% ""), arm_id)) {
        arm_selected_id("")
      }
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$arm_working_label_edit, {
    event_val <- input$arm_working_label_edit
    if (!is.list(event_val)) {
      return()
    }
    arm_id <- as.character(event_val$arm_id %||% "")
    label_use <- as.character(event_val$label %||% "")
    if (!nzchar(arm_id)) {
      return()
    }
    st <- arm_panel_state()
    ctx <- if (is.list(st)) st$context else current_arm_graph_context()
    working <- if (is.list(st)) st$working else empty_working_arm_state(ctx = ctx)
    if (!is.list(ctx)) {
      return()
    }
    next_state <- update_working_arm_label_state(working, arm_id = arm_id, label = label_use)
    save_working_arm_state(next_state, ctx = ctx)
  }, ignoreInit = TRUE)

  occupation_density_panel_state <- shiny::reactive({
    manifest <- active_manifest()
    sets <- if (is.list(manifest)) {
      gflowui_occupation_density_sets(manifest)
    } else {
      list()
    }
    if (length(sets) < 1L) {
      return(list(has_assets = FALSE))
    }
    set_ids <- vapply(sets, function(x) as.character(x$id %||% ""), character(1))
    set_labels <- vapply(
      sets,
      function(x) as.character(x$label %||% x$id %||% "Occupation densities"),
      character(1)
    )
    set_id <- as.character(
      input$occupation_density_set %||%
        manifest$defaults$occupation_density_set_id %||%
        set_ids[[1L]]
    )
    if (!(set_id %in% set_ids)) {
      set_id <- set_ids[[1L]]
    }
    set <- sets[[match(set_id, set_ids)]]
    subjects <- as.character(set$subject_ids %||% character(0))
    subjects <- subjects[nzchar(subjects)]
    methods <- set$methods %||% list()
    method_ids <- vapply(methods, function(x) as.character(x$id %||% ""), character(1))
    method_labels <- vapply(
      methods,
      function(x) as.character(x$label %||% x$id %||% "Method"),
      character(1)
    )
    subject_selected <- as.character(
      input$occupation_density_subject %||%
        manifest$defaults$occupation_density_subject_id %||%
        subjects[[1L]]
    )
    if (!(subject_selected %in% subjects)) {
      subject_selected <- subjects[[1L]]
    }
    method_selected <- as.character(
      input$occupation_density_method %||%
        manifest$defaults$occupation_density_method_id %||%
        method_ids[[1L]]
    )
    if (!(method_selected %in% method_ids)) {
      method_selected <- method_ids[[1L]]
    }
    method <- methods[[match(method_selected, method_ids)]]
    is_precomputed_path <- identical(
      as.character(method$source %||% ""),
      "precomputed_path"
    )
    mode_default <- if (isTRUE(is_precomputed_path)) "parameters" else "selected"
    mode <- as.character(
      input$occupation_density_mode %||%
        manifest$defaults$occupation_density_mode %||%
        mode_default
    )
    if (isTRUE(is_precomputed_path)) {
      mode <- "parameters"
    }
    list(
      has_assets = TRUE,
      set = set,
      method = method,
      set_id = set_id,
      set_choices = stats::setNames(set_ids, set_labels),
      subject_choices = stats::setNames(subjects, paste("Subject", subjects)),
      subject_selected = subject_selected,
      method_choices = stats::setNames(method_ids, method_labels),
      method_selected = method_selected,
      is_precomputed_path = is_precomputed_path,
      mode = mode,
      selector = as.character(input$occupation_density_selector %||% "minimum_brier")
    )
  })

  output$occupation_density_parameters <- shiny::renderUI({
    st <- occupation_density_panel_state()
    display_settings <- shiny::isolate(density_display_snapshot())
    mode <- as.character(
      input$occupation_density_mode %||% st$mode %||% "selected"
    )
    if (!isTRUE(st$has_assets) || !identical(mode, "parameters")) {
      return(NULL)
    }
    method_id <- as.character(input$occupation_density_method %||% st$method_selected)
    if (identical(method_id, "graph_heat_kernel")) {
      method <- gflowui_occupation_density_method(st$set, method_id)
      if (identical(as.character(method$source %||% ""), "precomputed_path")) {
        asset <- tryCatch(
          gflowui_precomputed_density_path(
            st$set,
            active_manifest()$project_root,
            method_id
          ),
          error = function(e) e
        )
        if (inherits(asset, "error")) {
          return(shiny::p(
            class = "gf-hint",
            sprintf("Precomputed path unavailable: %s", conditionMessage(asset))
          ))
        }
        path_summary <- asset$path_summary
        selected_index <- gflowui_precomputed_selected_eta_index(path_summary)
        eta_index <- suppressWarnings(as.integer(
          input$occupation_density_eta_index %||% selected_index
        ))
        if (!(eta_index %in% suppressWarnings(as.integer(path_summary$eta.index)))) {
          eta_index <- selected_index
        }
        current_row <- path_summary[
          suppressWarnings(as.integer(path_summary$eta.index)) == eta_index,
          ,
          drop = FALSE
        ]
        selected_row <- path_summary[
          suppressWarnings(as.integer(path_summary$eta.index)) == selected_index,
          ,
          drop = FALSE
        ]
        log_eta <- function(row) {
          log10(suppressWarnings(as.numeric(row$eta[[1L]])))
        }
        shiny::tagList(
          shiny::sliderInput(
            "occupation_density_eta_index",
            "Diffusion time",
            min = min(path_summary$eta.index),
            max = max(path_summary$eta.index),
            value = eta_index,
            step = 1L,
            ticks = FALSE
          ),
          shiny::div(
            class = "gf-status-block",
            shiny::p(sprintf(
              "Current: index %d of %d; t=%s; log10(t)=%s.",
              eta_index,
              nrow(path_summary),
              formatC(
                suppressWarnings(as.numeric(current_row$eta[[1L]])),
                digits = 7,
                format = "fg"
              ),
              formatC(log_eta(current_row), digits = 5, format = "fg")
            )),
            shiny::p(sprintf(
              "Brier-selected: index %d; t=%s; log10(t)=%s; mean Brier=%s.",
              selected_index,
              formatC(
                suppressWarnings(as.numeric(selected_row$eta[[1L]])),
                digits = 7,
                format = "fg"
              ),
              formatC(log_eta(selected_row), digits = 5, format = "fg"),
              formatC(
                suppressWarnings(as.numeric(selected_row$mean.brier[[1L]])),
                digits = 6,
                format = "fg"
              )
            ))
          ),
          shiny::tags$fieldset(
            class = "gf-density-color-scheme",
            shiny::tags$legend("Density color scheme"),
            shiny::div(
              class = "gf-density-color-control",
              shiny::radioButtons(
                "occupation_density_low_color",
                "Low-density color",
                choiceNames = density_color_choice_names(),
                choiceValues = as.list(names(gflowui_density_colors())),
                selected = as.character(
                  display_settings$low %||% "yellow"
                ),
                inline = TRUE
              ),
              shiny::sliderInput(
                "occupation_density_low_alpha",
                "Low-density opacity",
                min = 0,
                max = 1,
                value = suppressWarnings(as.numeric(
                  display_settings$low_alpha %||% 0.2
                )),
                step = 0.05
              )
            ),
            shiny::div(
              class = "gf-density-color-control",
              shiny::radioButtons(
                "occupation_density_mid_color",
                "Mid-range color",
                choiceNames = density_color_choice_names(include_none = TRUE),
                choiceValues = as.list(
                  names(gflowui_density_colors(include_none = TRUE))
                ),
                selected = as.character(
                  display_settings$midpoint %||% "none"
                ),
                inline = TRUE
              ),
              shiny::sliderInput(
                "occupation_density_mid_alpha",
                "Mid-range opacity",
                min = 0,
                max = 1,
                value = suppressWarnings(as.numeric(
                  display_settings$midpoint_alpha %||% 1
                )),
                step = 0.05
              )
            ),
            shiny::div(
              class = "gf-density-color-control",
              shiny::radioButtons(
                "occupation_density_high_color",
                "High-density color",
                choiceNames = density_color_choice_names(),
                choiceValues = as.list(names(gflowui_density_colors())),
                selected = as.character(
                  display_settings$high %||% "red"
                ),
                inline = TRUE
              ),
              shiny::sliderInput(
                "occupation_density_high_alpha",
                "High-density opacity",
                min = 0,
                max = 1,
                value = suppressWarnings(as.numeric(
                  display_settings$high_alpha %||% 1
                )),
                step = 0.05
              )
            )
          )
        )
      } else {
        k_values <- as.integer(st$set$graph_k_values %||% 3L:25L)
        k_selected <- suppressWarnings(as.integer(
          input$occupation_density_graph_k %||%
            st$set$default_graph_k %||%
            k_values[[1L]]
        ))
        if (!(k_selected %in% k_values)) {
          k_selected <- k_values[[1L]]
        }
        method <- st$set$methods[[match(
          method_id,
          vapply(
            st$set$methods,
            function(x) as.character(x$id %||% ""),
            character(1)
          )
        )]]
        basis_path <- sprintf(as.character(method$basis_file_template), k_selected)
        basis_path <- tryCatch(
          gflowui_occupation_density_path(
            basis_path,
            active_manifest()$project_root
          ),
          error = function(e) ""
        )
        eta_grid <- if (nzchar(basis_path)) {
          readRDS(basis_path)$eta.grid
        } else {
          numeric(0)
        }
        eta_choices <- stats::setNames(
          seq_along(eta_grid),
          sprintf(
            "%02d: eta=%s",
            seq_along(eta_grid),
            formatC(eta_grid, digits = 5, format = "fg")
          )
        )
        shiny::tagList(
          shiny::selectInput(
            "occupation_density_graph_k", "Graph k",
            choices = k_values, selected = k_selected
          ),
          shiny::selectInput(
            "occupation_density_eta_index", "Heat-time candidate",
            choices = eta_choices,
            selected = as.character(input$occupation_density_eta_index %||% "1")
          )
        )
      }
    } else if (identical(method_id, "chart_kernel")) {
      grid <- st$set$chart_parameter_grid
      shiny::tagList(
        shiny::selectInput(
          "occupation_density_support", "Support size",
          choices = as.integer(grid$support_size),
          selected = as.character(input$occupation_density_support %||% grid$support_size[[1L]])
        ),
        shiny::selectInput(
          "occupation_density_chart_dim", "Chart dimension",
          choices = as.integer(grid$chart_dim),
          selected = as.character(input$occupation_density_chart_dim %||% grid$chart_dim[[1L]])
        ),
        shiny::selectInput(
          "occupation_density_bandwidth", "Bandwidth multiplier",
          choices = as.numeric(grid$bandwidth_multiplier),
          selected = as.character(input$occupation_density_bandwidth %||% grid$bandwidth_multiplier[[1L]])
        )
      )
    } else {
      shiny::p(class = "gf-hint", "Parameterized evaluation is unavailable for this method.")
    }
  })

  output$occupation_density_status <- shiny::renderText({
    occupation_density_status()
  })

  show_occupation_density_selection <- function(notify_errors = TRUE) {
    # Capture the live controls before changing the displayed estimate. The
    # result update can rebuild/suspend this dynamic UI while another workflow
    # panel is opened, so the renderer must not rely on a later input rebind.
    sync_density_display_settings_from_inputs()
    st <- occupation_density_panel_state()
    if (!isTRUE(st$has_assets)) {
      return(invisible(NULL))
    }
    mode <- as.character(input$occupation_density_mode %||% "selected")
    method_id <- as.character(input$occupation_density_method %||% st$method_selected)
    subject_id <- as.character(input$occupation_density_subject %||% st$subject_selected)
    params <- if (identical(method_id, "graph_heat_kernel")) {
      list(
        graph_k = suppressWarnings(as.integer(input$occupation_density_graph_k)),
        eta_index = suppressWarnings(as.integer(input$occupation_density_eta_index)),
        display_mode = "density"
      )
    } else {
      list(
        support_size = suppressWarnings(as.integer(input$occupation_density_support)),
        chart_dim = suppressWarnings(as.integer(input$occupation_density_chart_dim)),
        bandwidth_multiplier = suppressWarnings(as.numeric(input$occupation_density_bandwidth))
      )
    }
    result <- tryCatch(
      gflowui_evaluate_occupation_density(
        manifest = active_manifest(),
        set_id = st$set_id,
        subject_id = subject_id,
        method_id = method_id,
        mode = mode,
        selector = as.character(input$occupation_density_selector %||% "minimum_brier"),
        parameters = params
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      occupation_density_status(sprintf("Density evaluation failed: %s", conditionMessage(result)))
      if (isTRUE(notify_errors)) {
        shiny::showNotification(conditionMessage(result), type = "error")
      }
      return(invisible(NULL))
    }
    selected <- result$selected
    graph_k <- if (is.data.frame(selected) && "graph.k" %in% names(selected)) {
      suppressWarnings(as.integer(selected$graph.k[[1L]]))
    } else {
      NA_integer_
    }
    method_label <- as.character(result$method$label %||% method_id)
    selector_label <- if (identical(mode, "selected")) {
      if (identical(input$occupation_density_selector, "minimum_bernoulli_nll")) {
        "Bernoulli NLL-selected"
      } else {
        "Brier-selected"
      }
    } else if (identical(
      as.character(result$method$source %||% ""),
      "precomputed_path"
    )) {
      sprintf(
        "time index %d",
        suppressWarnings(as.integer(result$selected$eta.index[[1L]]))
      )
    } else {
      "parameter-selected"
    }
    result$subject_id <- subject_id
    result$method_id <- method_id
    result$method_label <- method_label
    result$mode <- mode
    result$selector_label <- selector_label
    result$graph_k <- graph_k
    occupation_density_result(result)
    subject_state$selected_ids <- subject_id
    subject_state$edge_mode <- "temporal"
    shiny::updateSelectInput(session, "subject_ids", selected = subject_id)
    shiny::updateSelectInput(session, "subject_edge_mode", selected = "temporal")
    graph_layout_state$color_by <- "occupation_density_active"
    shiny::updateSelectInput(
      session, "graph_layout_color_by",
      selected = "occupation_density_active"
    )
    graph_note <- if (is.finite(graph_k) &&
                      identical(as.character(result$method$source %||% ""),
                                "precomputed_path")) {
      sprintf(" The field is shown on frozen G_%d.", graph_k)
    } else if (is.finite(graph_k)) {
      sprintf(" The estimate selected G_%d; use the button below to display that graph.", graph_k)
    } else {
      " Chart kernel does not select graph k; the density is shown on the currently displayed graph."
    }
    occupation_density_status(sprintf(
      "Showing subject %s, %s, %s.%s",
      subject_id, method_label, selector_label, graph_note
    ))
    invisible(result)
  }

  shiny::observeEvent(input$occupation_density_show, {
    show_occupation_density_selection(notify_errors = TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(
    input$occupation_density_eta_index,
    {
      st <- occupation_density_panel_state()
      already_showing <- is.list(shiny::isolate(occupation_density_result()))
      if (isTRUE(st$is_precomputed_path) && isTRUE(already_showing)) {
        show_occupation_density_selection(notify_errors = FALSE)
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$occupation_density_use_graph_k, {
    result <- occupation_density_result()
    graph_k <- suppressWarnings(as.integer(result$graph_k %||% NA_integer_))
    if (!is.finite(graph_k)) {
      shiny::showNotification("This density estimate does not select graph k.", type = "message")
      return()
    }
    graph_selection_state$k <- graph_k
    shiny::updateSelectInput(session, "graph_k", selected = as.character(graph_k))
    shiny::showNotification(sprintf("Displaying symmetric kNN graph k=%d.", graph_k), type = "message")
  }, ignoreInit = TRUE)

  reference_view_state <- shiny::reactive({
    sel <- current_graph_selection()
    if (!is.list(sel) || !is.null(sel$error)) {
      return(list(error = as.character(sel$error %||% "No graph assets found for this project.")))
    }

    manifest <- sel$manifest
    selected_set <- scalar_chr(sel$set_id %||% "", default = "")
    selected_k <- scalar_int(sel$k_selected, default = NA_integer_)

    spec <- resolve_reference_spec(
      manifest,
      preferred_set_id = selected_set,
      preferred_k = selected_k
    )
    if (is.null(spec)) {
      return(list(error = "No graph assets found for this project."))
    }

    graph_file <- as.character(spec$graph_set$graph_file %||% "")
    if (!nzchar(graph_file) || !file.exists(graph_file)) {
      return(list(error = "Reference graph file is missing."))
    }

    graph_obj <- tryCatch(readRDS(graph_file), error = function(e) e)
    if (inherits(graph_obj, "error")) {
      return(list(error = sprintf("Could not read graph file: %s", conditionMessage(graph_obj))))
    }

    collection <- extract_graph_collection(graph_obj)
    picked <- select_graph_for_k(collection, target_k = spec$k_ref)
    if (is.null(picked) || is.null(picked$graph$adj_list)) {
      return(list(error = "Could not resolve graph structure for reference k."))
    }

    adj_list <- picked$graph$adj_list
    weight_list <- picked$graph$weight_list
    if (!is.list(weight_list) || length(weight_list) != length(adj_list)) {
      weight_list <- picked$graph$edge.length.list
    }
    if (!is.list(weight_list) || length(weight_list) != length(adj_list)) {
      weight_list <- lapply(adj_list, function(nb) rep(1, length(nb %||% integer(0))))
    }
    n_vertices <- length(adj_list)
    if (n_vertices < 1L) {
      return(list(error = "Reference graph has no vertices."))
    }
    vertex.ids <- graph_obj$vertex_ids %||%
      picked$graph$vertex_ids %||%
      names(adj_list)
    vertex.ids <- as.character(vertex.ids %||% character())
    valid.vertex.ids <- length(vertex.ids) == n_vertices &&
      !anyNA(vertex.ids) &&
      all(nzchar(vertex.ids)) &&
      !anyDuplicated(vertex.ids)
    if (!isTRUE(valid.vertex.ids)) {
      vertex.ids <- NULL
    } else {
      vertex.ids <- enc2utf8(vertex.ids)
    }
    graph.asset.fingerprint <- unname(tools::md5sum(graph_file))
    graph.contract <- spec$graph_set$basin_source_contract %||% list()
    graph.identity <- tryCatch(
      gflowui_basin_graph_identity(
        adj_list = adj_list,
        edge_length_list = weight_list,
        vertex_id = vertex.ids,
        graph_id = graph.contract$graph.id,
        graph_k = picked$k_actual,
        source_vertex_id = graph.contract$source.vertex.id,
        declared_display_vertex_fingerprint =
          graph.contract$display.vertex.id.fingerprint
      ),
      error = function(e) e
    )

    component_ids <- rep.int(1L, n_vertices)
    comp_res <- tryCatch(
      dgraphs::graph.connected.components(adj_list),
      error = function(e) NULL
    )
    comp_int <- suppressWarnings(as.integer(comp_res))
    if (length(comp_int) == n_vertices && any(is.finite(comp_int))) {
      bad <- !is.finite(comp_int)
      if (any(bad)) {
        comp_int[bad] <- -seq_len(sum(bad))
      }
      component_ids <- comp_int
    }
    comp_tab <- table(component_ids)
    lcc_id <- if (length(comp_tab) > 0L) {
      as.integer(names(comp_tab)[which.max(comp_tab)])
    } else {
      1L
    }
    lcc_index <- which(component_ids == lcc_id)
    if (length(lcc_index) < 1L) {
      lcc_index <- seq_len(n_vertices)
    }
    components <- list(
      ids = component_ids,
      n_components = as.integer(length(unique(component_ids))),
      lcc_id = as.integer(lcc_id),
      lcc_index = as.integer(lcc_index),
      lcc_size = as.integer(length(lcc_index)),
      n_vertices = as.integer(n_vertices)
    )

    manifest_layout_coords <- grip_layout_matrix_for_graph_set(
      graph_set = spec$graph_set,
      k_ref = picked$k_actual
    )
    if (!is.matrix(manifest_layout_coords)) {
      manifest_layout_coords <- project_layout_manifest_matrix(
        project_root = manifest$project_root %||% "",
        spec = spec
      )
    }
    if (!is.matrix(manifest_layout_coords) || nrow(manifest_layout_coords) != n_vertices || ncol(manifest_layout_coords) < 3L) {
      manifest_layout_coords <- NULL
    }

    condexp <- collect_reference_condexp_sources(
      manifest = manifest,
      set_id = spec$set_id,
      k_use = picked$k_actual,
      n_vertices = n_vertices,
      reference_adj_list = adj_list
    )
    endpoint_sources <- collect_reference_endpoint_sources(
      manifest = manifest,
      k_use = picked$k_actual,
      n_vertices = n_vertices,
      reference_adj_list = adj_list
    )

    cache_key <- sprintf(
      "%s|%s|%s|%s",
      rv$project.id %||% "project",
      spec$set_id %||% "set",
      picked$k_actual %||% "k",
      n_vertices
    )
    coords <- if (is.matrix(manifest_layout_coords)) {
      normalize_coord_matrix(manifest_layout_coords)
    } else {
      compute_reference_layout(
        adj_list = adj_list,
        cache_key = cache_key,
        spectral_coords = condexp$spectral_coords
      )
    }

    sources <- list()
    add_source <- function(
        key,
        label,
        values,
        type = c("numeric", "categorical"),
        colorbar_title = NULL,
        color_transform = "identity",
        source_fingerprint = "",
        alignment_contract = NULL,
        density_low = "yellow",
        density_midpoint = "none",
        density_high = "red") {
      type <- match.arg(type)
      vv <- values
      if (length(vv) != n_vertices) {
        return(invisible(NULL))
      }
      if (all(is.na(vv))) {
        return(invisible(NULL))
      }
      k <- sanitize_token_id(key, fallback = "source")
      while (k %in% names(sources)) {
        k <- sprintf("%s_%d", k, length(sources) + 1L)
      }
      sources[[k]] <<- list(
        key = k,
        label = label,
        type = type,
        values = vv,
        colorbar_title = as.character(colorbar_title %||% label),
        color_transform = as.character(color_transform %||% "identity"),
        source_fingerprint = as.character(source_fingerprint %||% ""),
        alignment_contract = alignment_contract,
        density_low = as.character(density_low %||% "yellow"),
        density_midpoint = as.character(density_midpoint %||% "none"),
        density_high = as.character(density_high %||% "red")
      )
      invisible(NULL)
    }

    add_source_entry <- function(src) {
      if (!is.list(src)) {
        return(invisible(NULL))
      }
      add_source(
        key = as.character(src$key %||% src$label %||% "source"),
        label = as.character(src$label %||% src$key %||% "source"),
        values = src$values %||% numeric(0),
        type = if (identical(as.character(src$type %||% "numeric"), "categorical")) "categorical" else "numeric",
        source_fingerprint = as.character(
          src$source_fingerprint %||%
            gflowui_basin_hash(list(
              key = src$key,
              values = src$values
            ))
        ),
        alignment_contract = src$alignment_contract
      )
      invisible(NULL)
    }

    meta_sources <- collect_reference_metadata_sources(
      manifest = manifest,
      graph_set = spec$graph_set,
      n_vertices = n_vertices
    )
    if (length(meta_sources) > 0L) {
      for (src in meta_sources) {
        add_source_entry(src)
      }
    }
    if (is.list(condexp$sources) && length(condexp$sources) > 0L) {
      for (src in condexp$sources) {
        add_source_entry(src)
      }
    }
    if (is.list(endpoint_sources) && length(endpoint_sources) > 0L) {
      for (src in endpoint_sources) {
        add_source_entry(src)
      }
    }
    occupation_density <- occupation_density_result()
    if (is.list(occupation_density) &&
        length(occupation_density$values %||% numeric(0)) == n_vertices) {
      occupation_type <- if (identical(
        as.character(occupation_density$color_type %||% "numeric"),
        "categorical"
      )) {
        "categorical"
      } else {
        "numeric"
      }
      add_source(
        key = "occupation_density_active",
        label = sprintf(
          "EOD: subject %s, %s, %s",
          as.character(occupation_density$subject_id %||% ""),
          as.character(occupation_density$method_label %||% "method"),
          as.character(occupation_density$selector_label %||% "estimate")
        ),
        values = occupation_density$values,
        type = occupation_type,
        colorbar_title = as.character(
          occupation_density$colorbar_title %||% "EOD mass"
        ),
        color_transform = if (identical(occupation_type, "numeric")) {
          "density_log10"
        } else {
          "identity"
        },
        source_fingerprint = as.character(
          occupation_density$source_fingerprint %||% ""
        ),
        alignment_contract = occupation_density$alignment_contract,
        density_low = "yellow",
        density_midpoint = "none",
        density_high = "red"
      )
    }
    basins <- basin_result()
    if (is.list(basins) &&
        identical(
          as.character(basins$project_id %||% ""),
          as.character(rv$project.id %||% "")
        ) &&
        identical(
          as.character(basins$graph_set_id %||% ""),
          as.character(spec$set_id %||% "")
        ) &&
        identical(
          suppressWarnings(as.integer(basins$graph_k %||% NA_integer_)),
          suppressWarnings(as.integer(picked$k_actual %||% NA_integer_))
        ) &&
        length(basins$values %||% character(0)) == n_vertices) {
      add_source(
        key = "basin_active",
        label = sprintf(
          "Basins: %s",
          as.character(basins$source_label %||% "selected estimate")
        ),
        values = as.character(basins$values),
        type = "categorical",
        colorbar_title = sprintf(
          "Top %d basins",
          suppressWarnings(as.integer(basins$top_k %||% 0L))
        )
      )
    }

    dat <- data_state()
    if (!is.null(dat$data) && nrow(dat$data) == n_vertices) {
      cols <- names(dat$data)
      for (cc in cols) {
        vv <- dat$data[[cc]]
        if (is.numeric(vv)) {
          add_source(
            key = sprintf("data_%s", cc),
            label = sprintf("Data %s", cc),
            values = suppressWarnings(as.numeric(vv)),
            type = "numeric"
          )
        } else if (is.factor(vv) || is.character(vv) || is.logical(vv)) {
          vv_chr <- as.character(vv)
          nlev <- length(unique(vv_chr[!is.na(vv_chr)]))
          if (nlev >= 2L && nlev <= 30L) {
            add_source(
              key = sprintf("data_%s", cc),
              label = sprintf("Data %s", cc),
              values = vv_chr,
              type = "categorical"
            )
          }
        }
      }
    }

    if (length(sources) < 1L) {
      degree <- suppressWarnings(as.numeric(lengths(adj_list)))
      add_source("vertex_degree", "Vertex Degree", degree, type = "numeric")
    }

    labels <- vapply(sources, function(src) as.character(src$label %||% src$key), character(1))
    keys <- names(sources)
    choices <- stats::setNames(keys, labels)

    default_key <- keys[1]
    yhat_idx <- grep("y\\.hat|yhat", labels, ignore.case = TRUE)
    if (length(yhat_idx) > 0L) {
      default_key <- keys[yhat_idx[1]]
    } else {
      cst_idx <- grep("cst|subcst", labels, ignore.case = TRUE)
      if (length(cst_idx) > 0L) {
        default_key <- keys[cst_idx[1]]
      }
    }

    list(
      error = NULL,
      project_id = rv$project.id %||% "",
      set_id = spec$set_id,
      set_label = spec$set_label,
      data_type_label = infer_data_type_label(spec$graph_set),
      k_actual = suppressWarnings(as.integer(picked$k_actual)),
      reference_summary = spec$reference$summary,
      n_vertices = n_vertices,
      coords = coords,
      adj_list = adj_list,
      weight_list = weight_list,
      components = components,
      graph_set = spec$graph_set,
      graph_file = graph_file,
      graph_asset_fingerprint = graph.asset.fingerprint,
      graph_identity = if (inherits(graph.identity, "error")) {
        NULL
      } else {
        graph.identity
      },
      graph_identity_error = if (inherits(graph.identity, "error")) {
        conditionMessage(graph.identity)
      } else {
        NULL
      },
      vertex_ids = vertex.ids,
      sources = sources,
      choices = choices,
      default_key = default_key
    )
  })

  basin_panel_state <- shiny::reactive({
    st <- reference_view_state()
    if (!is.list(st) || !is.null(st$error) || !is.list(st$sources)) {
      return(list(
        has_sources = FALSE,
        choices = character(0),
        selected = ""
      ))
    }
    source_keys <- names(st$sources)
    estimate_keys <- source_keys[vapply(source_keys, function(key) {
      src <- st$sources[[key]]
      label <- as.character(src$label %||% "")
      is_numeric <- identical(as.character(src$type %||% ""), "numeric")
      is_estimate <- identical(key, "occupation_density_active") ||
        grepl("condexp", label, ignore.case = TRUE) ||
        grepl("rel\\.y\\.hat", label, ignore.case = TRUE)
      isTRUE(is_numeric && is_estimate)
    }, logical(1))]
    if (length(estimate_keys) < 1L) {
      return(list(
        has_sources = FALSE,
        choices = character(0),
        selected = ""
      ))
    }
    labels <- vapply(
      estimate_keys,
      function(key) as.character(st$sources[[key]]$label %||% key),
      character(1)
    )
    selected <- as.character(input$basin_source %||% "")
    if (!(selected %in% estimate_keys)) {
      selected <- if ("occupation_density_active" %in% estimate_keys) {
        "occupation_density_active"
      } else {
        estimate_keys[[1L]]
      }
    }
    list(
      has_sources = TRUE,
      choices = stats::setNames(estimate_keys, labels),
      selected = selected
    )
  })

  basin_source_state <- shiny::reactive({
    panel <- basin_panel_state()
    st <- reference_view_state()
    if (!isTRUE(panel$has_sources) ||
        !is.list(st) ||
        !is.list(st$sources)) {
      return(NULL)
    }
    key <- as.character(input$basin_source %||% panel$selected %||% "")
    if (!(key %in% names(st$sources))) {
      return(NULL)
    }
    src <- st$sources[[key]]
    list(
      key = key,
      label = as.character(src$label %||% key),
      values = suppressWarnings(as.numeric(src$values)),
      source_type = if (identical(key, "occupation_density_active")) {
        "occupation_probability"
      } else {
        "conditional_expectation"
      },
      source_fingerprint = as.character(
        src$source_fingerprint %||% ""
      ),
      alignment_contract = src$alignment_contract,
      vertex_id = st$vertex_ids,
      graph = st
    )
  })

  basin_construction_request <- function(source) {
    if (!is.list(source) || !is.list(source$graph)) {
      stop(
        "Apply an occupation density or choose a conditional-expectation estimate first.",
        call. = FALSE
      )
    }
    vertex.id <- enc2utf8(as.character(source$vertex_id %||% character()))
    if (length(vertex.id) != length(source$values) ||
        anyNA(vertex.id) || any(!nzchar(vertex.id)) ||
        anyDuplicated(vertex.id)) {
      stop(
        paste(
          "The selected graph does not provide reviewed, unique external",
          "vertex IDs in graph order; basin construction was not started."
        ),
        call. = FALSE
      )
    }
    graph.identity <- source$graph$graph_identity
    if (!is.list(graph.identity)) {
      stop(
        as.character(
          source$graph$graph_identity_error %||%
            paste(
              "The displayed graph lacks a source-independent graph ID,",
              "k, fingerprint, or ordered-vertex fingerprint."
            )
        ),
        call. = FALSE
      )
    }
    alignment <- gflowui_validate_basin_source_alignment(
      source_contract = source$alignment_contract,
      graph_identity = graph.identity,
      field = source$values,
      source_fingerprint = source$source_fingerprint
    )
    source.fingerprint <- gflowui_basin_sha256(list(
      schema = "gflowui_basin_source_request/2",
      source.key = source$key,
      source.asset.fingerprint = source$source_fingerprint,
      field.fingerprint = gflowui_basin_field_fingerprint(source$values),
      alignment.evidence.fingerprint = alignment$evidence.fingerprint
    ))
    is.occupation <- identical(
      as.character(source$key),
      "occupation_density_active"
    )
    mass.provenance <- if (is.occupation) {
      gflowui_basin_mass_provenance(
        mass_kind = "occupation_probability",
        source_id = alignment$source.id,
        source_fingerprint = source.fingerprint,
        authority = sprintf(
          "gflowui project manifest %s",
          source$graph$project_id %||% "unknown"
        ),
        validator = alignment$validator,
        validator_version = alignment$validator.version,
        algorithm = alignment$algorithm,
        evidence_fingerprint = alignment$evidence.fingerprint,
        contract_version = alignment$contract.version,
        evidence = alignment$evidence,
        validation_status = alignment$status
      )
    } else {
      NULL
    }
    build.identity <- gflow::get.gflow.build.identity()
    identity <- gflowui_basin_construction_identity(
      project_id = source$graph$project_id,
      graph_set_id = source$graph$set_id,
      graph_identity = graph.identity,
      source_key = source$key,
      source_fingerprint = source.fingerprint,
      field = source$values,
      vertex_mass = if (is.occupation) source$values else NULL,
      vertex_mass_provenance = mass.provenance,
      alignment_validation = alignment,
      build_identity = build.identity
    )
    list(
      source = source,
      vertex_id = vertex.id,
      graph_identity = graph.identity,
      alignment = alignment,
      source_fingerprint = source.fingerprint,
      is_occupation = is.occupation,
      mass_provenance = mass.provenance,
      build_identity = build.identity,
      construction_identity = identity
    )
  }

  invalidate_basin_result_if_needed <- function(
      request,
      message = paste(
        "Estimate or graph inputs changed.",
        "Compute and Open Basin Inspector again."
      )) {
    result <- shiny::isolate(basin_result())
    if (!is.list(result)) {
      return(invisible(FALSE))
    }
    current <- as.character(
      result$construction_identity$fingerprint %||% ""
    )
    active <- if (is.list(request)) {
      as.character(request$construction_identity$fingerprint %||% "")
    } else {
      ""
    }
    if (nzchar(current) && identical(current, active)) {
      return(invisible(FALSE))
    }
    basin_result(NULL)
    basin_inspector_open(FALSE)
    if (identical(
      as.character(graph_layout_state$color_by %||% ""),
      "basin_active"
    )) {
      next.color <- if (is.list(request)) {
        as.character(request$source$key %||% "")
      } else {
        ""
      }
      graph_layout_state$color_by <- next.color
      shiny::updateSelectInput(
        session,
        "graph_layout_color_by",
        selected = next.color
      )
    }
    basin_status(message)
    invisible(TRUE)
  }

  output$basin_status <- shiny::renderText({
    basin_status()
  })

  basin_input_suffix <- function(key) {
    gsub("[^A-Za-z0-9_]", "_", as.character(key))
  }

  basin_color_choices <- function(current = NULL) {
    colors <- c(
      "Crimson" = "#DC2626",
      "Orange" = "#F97316",
      "Gold" = "#EAB308",
      "Green" = "#16A34A",
      "Teal" = "#0F8B77",
      "Cyan" = "#06B6D4",
      "Blue" = "#2563EB",
      "Purple" = "#7C3AED",
      "Magenta" = "#DB2777",
      "Black" = "#111827"
    )
    current <- as.character(current %||% "")
    if (nzchar(current) && !(current %in% unname(colors))) {
      colors <- c("Current" = current, colors)
    }
    colors
  }

  update_basin_display_result <- function(result) {
    if (!is.list(result) || !is.list(result$basin) ||
        !is.data.frame(result$table)) {
      return(result)
    }
    selected <- as.character(basin_selected_keys())
    colors <- basin_color_map()
    result$table$selected <- result$table$key %in% selected
    color.match <- unname(colors[result$table$key])
    keep.color <- !is.na(color.match) & nzchar(color.match)
    result$table$color[keep.color] <- color.match[keep.color]
    result$values_max <- gflowui_basin_display_values(
      result$basin,
      result$table,
      selected,
      "max"
    )
    result$values_min <- gflowui_basin_display_values(
      result$basin,
      result$table,
      selected,
      "min"
    )
    display.mode <- as.character(
      basin_display_settings$display_mode %||% "both"
    )
    result$values <- if (identical(display.mode, "minimum")) {
      result$values_min
    } else {
      result$values_max
    }
    result
  }

  basin_displayed_table <- function(result) {
    table <- if (is.list(result) && is.data.frame(result$table)) {
      result$table
    } else {
      data.frame()
    }
    type.filter <- as.character(
      basin_display_settings$inspector_filter %||% "all"
    )
    if (nrow(table) > 0L && type.filter %in% c("max", "min")) {
      table <- table[table$type == type.filter, , drop = FALSE]
    }
    table
  }

  basin_extrema_scope_rows <- function(result, direction, scope) {
    scope <- as.character(scope %||% "none")
    if (identical(scope, "none") ||
        !is.list(result) ||
        !is.data.frame(result$table)) {
      return(data.frame(
        vertex = integer(),
        value = numeric(),
        type = character(),
        rank = integer(),
        label = character(),
        stringsAsFactors = FALSE
      ))
    }
    table <- if (identical(scope, "all") &&
        is.data.frame(result$all_table)) {
      result$all_table
    } else {
      result$table
    }
    table <- table[
      as.character(table$type) == as.character(direction),
      ,
      drop = FALSE
    ]
    if (identical(scope, "selected")) {
      table <- table[table$selected, , drop = FALSE]
    }
    data.frame(
      vertex = suppressWarnings(as.integer(table$extremum.vertex)),
      value = suppressWarnings(as.numeric(table$extremum.value)),
      type = if (identical(direction, "max")) "maximum" else "minimum",
      rank = suppressWarnings(as.integer(table$rank)),
      label = as.character(table$display.label),
      stringsAsFactors = FALSE
    )
  }

  resummarize_basin_result <- function(result) {
    if (!is.list(result) || !inherits(result$basin, "basin_complex")) {
      return(result)
    }
    top.max <- suppressWarnings(as.integer(
      basin_display_settings$top_k_max %||% 6L
    ))
    top.min <- suppressWarnings(as.integer(
      basin_display_settings$top_k_min %||% 6L
    ))
    if (!is.finite(top.max) || top.max < 0L) top.max <- 6L
    if (!is.finite(top.min) || top.min < 0L) top.min <- 6L
    rank.by <- as.character(basin_display_settings$rank_by %||% "auto")
    summary <- summary(
      result$basin,
      rank.by = rank.by,
      top.k.max = top.max,
      top.k.min = top.min,
      include.vertex.lists = FALSE
    )
    table <- gflowui_basin_table(summary, result$prominence_complex)
    all.summary <- summary(
      result$basin,
      rank.by = rank.by,
      top.k.max = sum(result$basin$basin.table$type == "max"),
      top.k.min = sum(result$basin$basin.table$type == "min"),
      include.vertex.lists = FALSE
    )
    result$all_table <- gflowui_basin_table(
      all.summary,
      result$prominence_complex
    )
    existing.colors <- basin_color_map()
    defaults <- stats::setNames(table$color, table$key)
    defaults[names(existing.colors)] <- existing.colors
    basin_color_map(defaults)
    result$summary <- summary
    result$table <- table
    result$top_k_max <- top.max
    result$top_k_min <- top.min
    result$rank_by <- rank.by
    result$ranking_resolved <- summary$rank.resolved
    update_basin_display_result(result)
  }

  output$basin_inspector_ui <- shiny::renderUI({
    result <- basin_result()
    if (!isTRUE(basin_inspector_open()) ||
        !is.list(result) ||
        !is.data.frame(result$table)) {
      return(NULL)
    }
    table <- basin_displayed_table(result)
    type.filter <- as.character(
      basin_display_settings$inspector_filter %||% "all"
    )
    selected <- basin_selected_keys()
    colors <- basin_color_map()
    definitions <- result$summary$column.definitions %||% data.frame()
    definition_for <- function(field, fallback) {
      if (is.data.frame(definitions) &&
          all(c("field", "definition") %in% names(definitions))) {
        hit <- which(definitions$field == field)
        if (length(hit) > 0L) {
          return(as.character(definitions$definition[[hit[[1L]]]]))
        }
      }
      fallback
    }
    ranking_measure_label <- function(measure) {
      labels <- c(
        "primary.support.mass" = "Mass",
        "primary.support.size" = "Support",
        "raw.allocated.mass" = "Allocated mass",
        "raw.support.mass" = "Raw mass",
        "raw.support.size" = "Raw support",
        "retained.support.mass" = "Retained mass",
        "retained.support.size" = "Retained support"
      )
      measure <- as.character(measure %||% "")
      label <- unname(labels[measure])
      if (length(label) == 1L && !is.na(label) && nzchar(label)) {
        return(label)
      }
      if (nzchar(measure)) {
        return(gsub(".", " ", measure, fixed = TRUE))
      }
      "Unavailable"
    }
    ranking.resolved <- result$ranking_resolved %||%
      result$summary$rank.resolved %||% character()
    ranking_for_direction <- function(direction) {
      if (direction %in% names(ranking.resolved)) {
        ranking.resolved[[direction]]
      } else {
        ""
      }
    }
    max.ranking <- ranking_measure_label(ranking_for_direction("max"))
    min.ranking <- ranking_measure_label(ranking_for_direction("min"))
    ranking.description <- if (identical(max.ranking, min.ranking)) {
      sprintf(
        paste(
          "Basins are ranked separately within maxima and minima.",
          "Ranking measure: %s."
        ),
        max.ranking
      )
    } else {
      sprintf(
        paste(
          "Basins are ranked separately within maxima and minima.",
          "Ranking measures: maxima use %s; minima use %s."
        ),
        max.ranking,
        min.ranking
      )
    }
    show.extremum.vertex <- isTRUE(
      basin_display_settings$inspector_show_extremum_vertex
    )
    header <- shiny::tags$tr(
      shiny::tags$th(class = "gf-basin-show-column", "Show"),
      shiny::tags$th(class = "gf-basin-color-column", "Color"),
      shiny::tags$th(
        class = "gf-basin-label-column",
        title = paste(
          "M denotes a maximum basin and m denotes a minimum basin.",
          "The number is its direction-specific rank under the selected",
          "ranking measure."
        ),
        "Extremum / basin"
      ),
      if (show.extremum.vertex) {
        shiny::tags$th(
          title = definition_for(
            "extremum.vertex",
            "Internal integer vertex index of the representative extremum."
          ),
          "Extremum vertex"
        )
      } else NULL,
      shiny::tags$th(
        title = definition_for("extremum.value", "Raw extremum value."),
        "Extremum value"
      ),
      shiny::tags$th(
        title = definition_for(
          "primary.support.size",
          "Number of uniquely assigned vertices."
        ),
        "Support"
      ),
      shiny::tags$th(
        title = definition_for(
          "primary.support.mass",
          "Normalized uniquely assigned mass."
        ),
        "Mass"
      ),
      shiny::tags$th(
        title = definition_for(
          "persistence",
          paste(
            "Field-level prominence: peak minus merge level for maxima,",
            "or merge level minus trough for minima."
          )
        ),
        "Prominence"
      ),
      shiny::tags$th(
        class = "gf-basin-full-column",
        title = definition_for("raw.support.size", "Raw support size."),
        "Raw support"
      ),
      shiny::tags$th(
        class = "gf-basin-full-column",
        title = definition_for(
          "retained.support.size",
          "Retained support size."
        ),
        "Retained support"
      ),
      shiny::tags$th(
        class = "gf-basin-full-column",
        title = definition_for(
          "retained.support.mass",
          "Retained overlapping coverage mass."
        ),
        "Retained mass"
      ),
      shiny::tags$th(
        class = "gf-basin-full-column",
        title = definition_for(
          "retention.status",
          "Canonical retention status."
        ),
        "Retention"
      )
    )
    rows <- lapply(seq_len(nrow(table)), function(index) {
      row <- table[index, , drop = FALSE]
      key <- as.character(row$key)
      suffix <- basin_input_suffix(key)
      color <- as.character(colors[[key]] %||% row$color %||% "#2563EB")
      shiny::tags$tr(
        shiny::tags$td(
          class = "gf-basin-show-column",
          shiny::tagAppendAttributes(
            shiny::checkboxInput(
              paste0("basin_select_", suffix),
              label = NULL,
              value = key %in% selected
            ),
            `aria-label` = sprintf("Show %s", row$display.label),
            `data-gf-basin-key` = key,
            `data-gf-basin-role` = "selection"
          )
        ),
        shiny::tags$td(
          class = "gf-basin-color-column",
          shiny::tagAppendAttributes(
            shiny::selectInput(
              paste0("basin_color_", suffix),
              label = NULL,
              choices = basin_color_choices(color),
              selected = color,
              width = "118px",
              selectize = FALSE
            ),
            `aria-label` = sprintf("Color for %s", row$display.label),
            `data-gf-basin-key` = key,
            `data-gf-basin-role` = "color"
          )
        ),
        shiny::tags$td(
          class = "gf-basin-label-column",
          title = if (row$type == "max") {
            sprintf("Maximum basin, rank %d", as.integer(row$rank))
          } else {
            sprintf("Minimum basin, rank %d", as.integer(row$rank))
          },
          as.character(row$display.label)
        ),
        if (show.extremum.vertex) {
          shiny::tags$td(as.integer(row$extremum.vertex))
        } else NULL,
        shiny::tags$td(formatC(
          as.numeric(row$extremum.value),
          digits = 5,
          format = "g"
        )),
        shiny::tags$td(as.integer(row$primary.support.size)),
        shiny::tags$td(if (is.finite(row$primary.support.mass)) {
          formatC(as.numeric(row$primary.support.mass), digits = 4, format = "fg")
        } else "\u2014"),
        shiny::tags$td(if (is.finite(row$prominence)) {
          formatC(as.numeric(row$prominence), digits = 4, format = "g")
        } else "\u2014"),
        shiny::tags$td(
          class = "gf-basin-full-column",
          as.integer(row$raw.support.size)
        ),
        shiny::tags$td(
          class = "gf-basin-full-column",
          as.integer(row$retained.support.size)
        ),
        shiny::tags$td(
          class = "gf-basin-full-column",
          if (is.finite(row$retained.support.mass)) {
            formatC(
              as.numeric(row$retained.support.mass),
              digits = 4,
              format = "fg"
            )
          } else "\u2014"
        ),
        shiny::tags$td(
          class = "gf-basin-full-column",
          as.character(row$retention.status)
        )
      )
    })
    build <- result$build_identity %||% list()
    mass <- result$summary$mass.provenance
    mass.details <- if (is.null(mass)) {
      "Mass vector: absent. Mass semantics: not applicable."
    } else {
      attestation <- if (length(mass$upstream.attestations %||% list()) > 0L) {
        mass$upstream.attestations[[1L]]
      } else {
        list()
      }
      sprintf(
        paste(
          "Mass vector: constructor verified.",
          "Mass semantics: %s (attested by %s, validator %s %s).",
          "Evidence: %s."
        ),
        as.character(
          mass$validated.declarations$mass.kind %||% "unspecified_explicit"
        ),
        as.character(attestation$authority %||% "no external authority"),
        as.character(attestation$validator %||% "unspecified"),
        as.character(attestation$validator.version %||% ""),
        as.character(attestation$evidence.fingerprint %||% "not supplied")
      )
    }
    shiny::div(
      id = "gf_basin_inspector",
      class = paste(
        "gf-basin-inspector",
        paste0(
          "gf-basin-columns-",
          basin_display_settings$inspector_columns %||% "compact"
        )
      ),
      `data-storage-key` = sprintf(
        "gflowui-general-inspector-width:%s:%s:%s",
        result$project_id %||% "project",
        result$graph_set_id %||% "set",
        result$graph_k %||% "k"
      ),
      shiny::div(
        class = "gf-basin-inspector-header",
        shiny::h4("Basin Inspector"),
        shiny::actionButton(
          "basin_inspector_close",
          "Close",
          class = "btn-light btn-sm"
        )
      ),
      shiny::div(
        class = "gf-basin-inspector-summary-controls",
        shiny::numericInput(
          "basin_top_k_max",
          "Largest maximum basins",
          value = basin_display_settings$top_k_max %||% 6L,
          min = 0L,
          step = 1L,
          width = "170px"
        ),
        shiny::numericInput(
          "basin_top_k_min",
          "Largest minimum basins",
          value = basin_display_settings$top_k_min %||% 6L,
          min = 0L,
          step = 1L,
          width = "170px"
        ),
        shiny::selectInput(
          "basin_rank_by",
          "Ranking measure",
          choices = c(
            "Auto" = "auto",
            "Mass" = "primary.support.mass",
            "Support" = "primary.support.size"
          ),
          selected = basin_display_settings$rank_by %||% "auto",
          width = "170px"
        ),
        shiny::selectInput(
          "basin_extrema_max_scope",
          "Maximum extrema",
          choices = c(
            "Hide" = "none",
            "Selected basins" = "selected",
            "Listed top-K" = "listed",
            "All" = "all"
          ),
          selected = basin_display_settings$maxima_scope %||% "none",
          width = "165px"
        ),
        shiny::checkboxInput(
          "basin_label_maxima",
          "Label maxima",
          value = isTRUE(basin_display_settings$label_maxima),
          width = "125px"
        ),
        shiny::selectInput(
          "basin_extrema_min_scope",
          "Minimum extrema",
          choices = c(
            "Hide" = "none",
            "Selected basins" = "selected",
            "Listed top-K" = "listed",
            "All" = "all"
          ),
          selected = basin_display_settings$minima_scope %||% "none",
          width = "165px"
        ),
        shiny::checkboxInput(
          "basin_label_minima",
          "Label minima",
          value = isTRUE(basin_display_settings$label_minima),
          width = "125px"
        )
      ),
      shiny::div(
        class = "gf-basin-inspector-toolbar",
        shiny::selectInput(
          "basin_inspector_filter",
          "Rows",
          choices = c(
            "All basins" = "all",
            "Maximum only" = "max",
            "Minimum only" = "min"
          ),
          selected = type.filter,
          width = "160px"
        ),
        shiny::selectInput(
          "basin_inspector_columns",
          "Columns",
          choices = c("Compact" = "compact", "Full" = "full"),
          selected = basin_display_settings$inspector_columns %||% "compact",
          width = "130px"
        ),
        shiny::checkboxInput(
          "basin_inspector_show_extremum_vertex",
          "Show extremum vertex",
          value = show.extremum.vertex,
          width = "180px"
        ),
        shiny::selectInput(
          "basin_display_mode",
          "Display",
          choices = c(
            "Maximum fill + minimum halo" = "both",
            "Maximum basins" = "maximum",
            "Minimum basins" = "minimum"
          ),
          selected = basin_display_settings$display_mode %||% "both",
          width = "250px"
        ),
        shiny::sliderInput(
          "basin_global_opacity",
          "Basin opacity",
          min = 0,
          max = 1,
          step = 0.05,
          value = basin_display_settings$opacity %||% 0.85,
          width = "210px"
        ),
        shiny::selectInput(
          "basin_unselected_color",
          "Unselected color",
          choices = basin_color_choices(
            basin_display_settings$unselected_color %||% "#D1D5DB"
          ),
          selected = basin_display_settings$unselected_color %||% "#D1D5DB",
          width = "170px"
        ),
        shiny::sliderInput(
          "basin_unselected_opacity",
          "Unselected opacity",
          min = 0,
          max = 1,
          step = 0.05,
          value = basin_display_settings$unselected_opacity %||% 0.28,
          width = "190px"
        ),
        shiny::actionButton(
          "basin_show_colors",
          "Show basin colors",
          class = "btn-primary btn-sm",
          title = paste(
            "Switch the graph from its current color source to basin colors.",
            "Unselected vertices use the configured unselected color."
          )
        ),
        shiny::actionButton(
          "basin_select_displayed",
          "Select displayed",
          class = "btn-light btn-sm"
        ),
        shiny::actionButton(
          "basin_clear_displayed",
          "Clear displayed",
          class = "btn-light btn-sm"
        ),
        shiny::actionButton(
          "basin_clear_all",
          "Clear all",
          class = "btn-light btn-sm"
        ),
        shiny::actionButton(
          "basin_reset_colors",
          "Reset basin colors",
          class = "btn-light btn-sm",
          title = "Restore the default maximum/minimum basin palette."
        )
      ),
      shiny::div(
        class = "gf-basin-export",
        shiny::div(
          class = "gf-basin-export-controls",
          shiny::textInput(
            "basin_export_directory",
            "Bundle directory",
            value = shiny::isolate(basin_export_directory()),
            placeholder = "~/",
            width = "390px"
          ),
          shiny::actionButton(
            "basin_export_bundle",
            "Save full basin bundle",
            class = "btn-primary btn-sm"
          )
        ),
        shiny::p(
          class = "gf-basin-export-description",
          sprintf(
            paste(
              "Exports all %d basins in raw coordinates.",
              "Rows, top-K, selections, and display filters are ignored.",
              "Use ~ or an absolute directory path."
            ),
            nrow(result$all_table)
          )
        ),
        shiny::p(
          class = "gf-basin-export-status",
          shiny::textOutput("basin_export_status", inline = TRUE)
        )
      ),
      shiny::tags$section(
        class = "gf-basin-characteristics",
        shiny::h5("Basin characteristics"),
        shiny::p(
          class = "gf-basin-table-description",
          ranking.description,
          paste(
            "Support is the number of primarily assigned vertices;",
            "Mass is their normalized mass;",
            "Prominence is the extremum-to-merge field difference."
          )
        ),
        shiny::div(
          class = "table-responsive gf-basin-table-scroll",
          shiny::tags$table(
            class = "table table-sm gf-basin-table",
            shiny::tags$thead(header),
            shiny::tags$tbody(rows)
          )
        )
      ),
      shiny::tags$details(
        class = "gf-basin-construction-details",
        shiny::tags$summary("Construction details and column meanings"),
        shiny::p(mass.details),
        shiny::p(sprintf(
          "CLOSEST both-direction construction; connected exact plateaus; all edges admissible; build %s; runtime %s; cache %s.",
          as.character(build$build.id %||% "unavailable"),
          as.character(build$runtime$id %||% "unavailable"),
          if (isTRUE(result$cache_hit)) "hit" else "miss"
        )),
        shiny::p(sprintf(
          paste(
            "Prominence: exact plateau-aware superlevel merge tree on the",
            "same graph and field (cache %s)."
          ),
          if (isTRUE(result$prominence_cache_hit)) "hit" else "miss"
        )),
        shiny::p(
          "Plotly shows selected minimum basins as outlined halos. ",
          "RGL uses enlarged translucent minimum-basin markers because its ",
          "point renderer does not expose the same outline behavior."
        ),
        if (is.data.frame(definitions) && nrow(definitions) > 0L) {
          shiny::tags$ul(lapply(seq_len(nrow(definitions)), function(index) {
            shiny::tags$li(
              shiny::strong(definitions$label[[index]]),
              ": ",
              definitions$definition[[index]]
            )
          }))
        } else NULL
      )
    )
  })
  shiny::outputOptions(
    output,
    "basin_inspector_ui",
    suspendWhenHidden = FALSE
  )
  output$basin_export_status <- shiny::renderText({
    basin_export_status()
  })
  shiny::outputOptions(
    output,
    "basin_export_status",
    suspendWhenHidden = FALSE
  )

  basin_plot_scope_choices <- c(
    "All basins" = "all",
    "Listed top-K" = "listed",
    "Selected basins" = "selected"
  )
  basin_plot_type_choices <- c(
    "Maximum and minimum" = "both",
    "Maximum only" = "max",
    "Minimum only" = "min"
  )
  basin_plot_color_choices <- c(
    "Blue" = "#2563EB",
    "Crimson" = "#DC2626",
    "Orange" = "#EA580C",
    "Gold" = "#CA8A04",
    "Green" = "#16A34A",
    "Teal" = "#0F766E",
    "Cyan" = "#0891B2",
    "Purple" = "#7C3AED",
    "Magenta" = "#C026D3",
    "Black" = "#111827"
  )
  basin_plot_point_color_choices <- c(
    "By maximum/minimum" = "type",
    basin_plot_color_choices
  )
  basin_plot_glyph_choices <- c(
    "Filled circle" = "19",
    "Open circle" = "1",
    "Filled triangle" = "17",
    "Filled square" = "15",
    "Diamond" = "18",
    "Cross" = "4"
  )
  basin_plot_scale_choices <- gflowui_basin_plot_scale_choices()
  basin_plot_input_value <- function(id, default) {
    value <- shiny::isolate(input[[id]])
    if (is.null(value) || length(value) < 1L) default else value
  }
  install_basin_plot_remove_observer <- function(card.id) {
    key <- as.character(card.id)
    if (exists(key, envir = basin_plot_remove_observers, inherits = FALSE)) {
      return(invisible(NULL))
    }
    remove.id <- paste0("basin_plot_remove_", key)
    observer <- shiny::observeEvent(input[[remove.id]], {
      specs <- basin_plot_specs()
      keep <- !vapply(specs, function(spec) {
        identical(as.character(spec$id), key)
      }, logical(1))
      basin_plot_specs(specs[keep])
    }, ignoreInit = TRUE)
    assign(key, observer, envir = basin_plot_remove_observers)
    invisible(NULL)
  }
  basin_plot_card_ui <- function(spec, result) {
    card.id <- as.character(spec$id)
    output.id <- paste0("basin_plot_", card.id)
    status.id <- paste0("basin_plot_status_", card.id)
    scope.id <- paste0("basin_plot_scope_", card.id)
    type.id <- paste0("basin_plot_type_", card.id)
    bins.id <- paste0("basin_plot_bins_", card.id)
    histogram.color.id <- paste0("basin_plot_hist_color_", card.id)
    point.color.id <- paste0("basin_plot_point_color_", card.id)
    glyph.id <- paste0("basin_plot_glyph_", card.id)
    size.id <- paste0("basin_plot_point_size_", card.id)
    opacity.id <- paste0("basin_plot_point_opacity_", card.id)
    x.scale.id <- paste0("basin_plot_x_scale_", card.id)
    y.scale.id <- paste0("basin_plot_y_scale_", card.id)
    default.x.scale <- "log10"
    default.y.scale <- if (identical(as.character(spec$kind), "scatter")) {
      "log10"
    } else {
      "raw"
    }
    default.point.size <- 0.5
    current.fingerprint <- as.character(
      result$construction_identity$fingerprint %||% ""
    )
    stale <- !identical(
      as.character(spec$construction_fingerprint %||% ""),
      current.fingerprint
    )
    install_basin_plot_remove_observer(card.id)
    local({
      card.spec <- spec
      card.stale <- stale
      card.scope.id <- scope.id
      card.type.id <- type.id
      card.bins.id <- bins.id
      card.histogram.color.id <- histogram.color.id
      card.point.color.id <- point.color.id
      card.glyph.id <- glyph.id
      card.size.id <- size.id
      card.opacity.id <- opacity.id
      card.x.scale.id <- x.scale.id
      card.y.scale.id <- y.scale.id
      card.default.x.scale <- default.x.scale
      card.default.y.scale <- default.y.scale
      card.default.point.size <- default.point.size
      output[[output.id]] <- shiny::renderPlot({
        active <- basin_result()
        active.fingerprint <- as.character(
          active$construction_identity$fingerprint %||% ""
        )
        if (isTRUE(card.stale) ||
            !identical(
              as.character(card.spec$construction_fingerprint %||% ""),
              active.fingerprint
            )) {
          graphics::plot.new()
          graphics::text(
            0.5,
            0.5,
            "Stale plot: recreate it for the active estimate.",
            col = "#B45309"
          )
          return(invisible(NULL))
        }
        scope <- as.character(
          input[[card.scope.id]] %||% card.spec$scope %||% "all"
        )
        type <- as.character(
          input[[card.type.id]] %||% card.spec$type %||% "both"
        )
        data <- gflowui_basin_plot_data(
          active,
          scope = scope,
          type = type,
          selected_keys = basin_selected_keys()
        )
        gflowui_draw_basin_plot(
          data = data,
          spec = card.spec,
          bins = input[[card.bins.id]] %||% 20L,
          histogram_color =
            input[[card.histogram.color.id]] %||% "#2563EB",
          point_color = input[[card.point.color.id]] %||% "type",
          point_glyph = input[[card.glyph.id]] %||% 19L,
          point_size =
            input[[card.size.id]] %||% card.default.point.size,
          point_opacity = input[[card.opacity.id]] %||% 0.75,
          x_scale = input[[card.x.scale.id]] %||% card.default.x.scale,
          y_scale = input[[card.y.scale.id]] %||% card.default.y.scale
        )
      }, res = 110)
      output[[status.id]] <- shiny::renderText({
        active <- basin_result()
        active.fingerprint <- as.character(
          active$construction_identity$fingerprint %||% ""
        )
        if (isTRUE(card.stale) ||
            !identical(
              as.character(card.spec$construction_fingerprint %||% ""),
              active.fingerprint
            )) {
          return("Stale: underlying estimate or graph changed.")
        }
        scope <- as.character(
          input[[card.scope.id]] %||% card.spec$scope %||% "all"
        )
        type <- as.character(
          input[[card.type.id]] %||% card.spec$type %||% "both"
        )
        data <- gflowui_basin_plot_data(
          active,
          scope = scope,
          type = type,
          selected_keys = basin_selected_keys()
        )
        scaled <- gflowui_basin_plot_scaled_data(
          data,
          card.spec,
          x_scale = input[[card.x.scale.id]] %||% card.default.x.scale,
          y_scale = input[[card.y.scale.id]] %||% card.default.y.scale
        )
        excluded <- attr(scaled, "gflowui_nonpositive_excluded") %||% 0L
        scope.label <- names(basin_plot_scope_choices)[
          match(scope, basin_plot_scope_choices)
        ]
        type.label <- names(basin_plot_type_choices)[
          match(type, basin_plot_type_choices)
        ]
        if (length(scope.label) != 1L || is.na(scope.label)) {
          scope.label <- scope
        }
        if (length(type.label) != 1L || is.na(type.label)) {
          type.label <- type
        }
        status <- sprintf(
          "%s; %s; n=%d finite rows",
          scope.label,
          type.label,
          nrow(scaled)
        )
        if (excluded > 0L) {
          status <- sprintf(
            "%s; %d non-positive row%s excluded by log10",
            status,
            as.integer(excluded),
            if (identical(as.integer(excluded), 1L)) "" else "s"
          )
        }
        status
      })
    })
    is.histogram <- identical(as.character(spec$kind), "histogram")
    is.matrix <- identical(as.character(spec$kind), "matrix")
    shiny::div(
      id = paste0("gf_basin_plot_card_", card.id),
      class = paste(
        "gf-basin-plot-card",
        if (stale) "gf-basin-plot-card-stale" else ""
      ),
      shiny::div(
        class = "gf-basin-plot-card-header",
        shiny::h5(gflowui_basin_plot_title(spec)),
        shiny::actionButton(
          paste0("basin_plot_remove_", card.id),
          "Remove",
          class = "btn-light btn-sm"
        )
      ),
      shiny::p(
        class = "gf-basin-plot-card-status",
        shiny::textOutput(status.id, inline = TRUE)
      ),
      shiny::tags$details(
        class = "gf-basin-plot-card-controls",
        open = NA,
        shiny::tags$summary("Plot controls"),
        shiny::div(
          class = "gf-basin-plot-control-grid",
          shiny::selectInput(
            scope.id,
            "Data scope",
            choices = basin_plot_scope_choices,
            selected = basin_plot_input_value(
              scope.id,
              spec$scope %||% "all"
            )
          ),
          shiny::selectInput(
            type.id,
            "Extrema",
            choices = basin_plot_type_choices,
            selected = basin_plot_input_value(
              type.id,
              spec$type %||% "both"
            )
          ),
          if (is.histogram) {
            shiny::selectInput(
              x.scale.id,
              "Value scale",
              choices = basin_plot_scale_choices,
              selected = basin_plot_input_value(
                x.scale.id,
                default.x.scale
              )
            )
          } else NULL,
          if (!is.histogram && !is.matrix) {
            shiny::selectInput(
              x.scale.id,
              "X-axis scale",
              choices = basin_plot_scale_choices,
              selected = basin_plot_input_value(
                x.scale.id,
                default.x.scale
              )
            )
          } else NULL,
          if (!is.histogram && !is.matrix) {
            shiny::selectInput(
              y.scale.id,
              "Y-axis scale",
              choices = basin_plot_scale_choices,
              selected = basin_plot_input_value(
                y.scale.id,
                default.y.scale
              )
            )
          } else NULL,
          if (is.matrix) {
            shiny::selectInput(
              x.scale.id,
              "All coordinate scales",
              choices = basin_plot_scale_choices,
              selected = basin_plot_input_value(
                x.scale.id,
                default.x.scale
              )
            )
          } else NULL,
          if (is.histogram || is.matrix) {
            shiny::sliderInput(
              bins.id,
              "Histogram bins",
              min = 3L,
              max = 80L,
              step = 1L,
              value = suppressWarnings(as.integer(
                basin_plot_input_value(bins.id, 20L)
              ))
            )
          } else NULL,
          if (is.histogram || is.matrix) {
            shiny::selectInput(
              histogram.color.id,
              "Bin color",
              choices = basin_plot_color_choices,
              selected = basin_plot_input_value(
                histogram.color.id,
                "#2563EB"
              )
            )
          } else NULL,
          if (!is.histogram) {
            shiny::selectInput(
              glyph.id,
              "Glyph",
              choices = basin_plot_glyph_choices,
              selected = as.character(
                basin_plot_input_value(glyph.id, "19")
              )
            )
          } else NULL,
          if (!is.histogram) {
            shiny::selectInput(
              point.color.id,
              "Glyph color",
              choices = basin_plot_point_color_choices,
              selected = basin_plot_input_value(point.color.id, "type")
            )
          } else NULL,
          if (!is.histogram) {
            shiny::sliderInput(
              size.id,
              "Glyph size",
              min = 0.4,
              max = 3,
              step = 0.1,
              value = suppressWarnings(as.numeric(
                basin_plot_input_value(size.id, default.point.size)
              ))
            )
          } else NULL,
          if (!is.histogram) {
            shiny::sliderInput(
              opacity.id,
              "Glyph opacity",
              min = 0.1,
              max = 1,
              step = 0.05,
              value = suppressWarnings(as.numeric(
                basin_plot_input_value(opacity.id, 0.75)
              ))
            )
          } else NULL
        )
      ),
      shiny::div(
        class = "gf-basin-plot-frame",
        shiny::plotOutput(
          output.id,
          width = "100%",
          height = "100%"
        )
      )
    )
  }

  output$basin_plot_workspace_ui <- shiny::renderUI({
    result <- basin_result()
    if (!isTRUE(basin_inspector_open()) || !is.list(result)) {
      return(NULL)
    }
    feature.choices <- gflowui_basin_plot_feature_choices()
    selected.features <- basin_plot_input_value(
      "basin_plot_features",
      c("support", "mass")
    )
    builder.scope <- basin_plot_input_value("basin_plot_builder_scope", "all")
    builder.type <- basin_plot_input_value("basin_plot_builder_type", "both")
    specs <- basin_plot_specs()
    shiny::tags$section(
      id = "gf_basin_plot_workspace",
      class = "gf-basin-plot-workspace",
      shiny::div(
        class = "gf-basin-plot-workspace-header",
        shiny::div(
          shiny::h4("Basin Plot Workspace"),
          shiny::p(
            "Plots persist as independent cards until removed or cleared."
          )
        ),
        shiny::actionButton(
          "basin_plot_clear_all",
          "Clear plots",
          class = "btn-light btn-sm"
        )
      ),
      shiny::div(
        class = "gf-basin-plot-builder",
        shiny::checkboxGroupInput(
          "basin_plot_features",
          "Characteristics",
          choices = feature.choices,
          selected = selected.features,
          inline = TRUE
        ),
        shiny::selectInput(
          "basin_plot_builder_scope",
          "Initial data scope",
          choices = basin_plot_scope_choices,
          selected = builder.scope,
          width = "170px"
        ),
        shiny::selectInput(
          "basin_plot_builder_type",
          "Initial extrema",
          choices = basin_plot_type_choices,
          selected = builder.type,
          width = "190px"
        ),
        shiny::div(
          class = "gf-basin-plot-builder-actions",
          shiny::actionButton(
            "basin_plot_add_histograms",
            "Add histograms",
            class = "btn-primary btn-sm"
          ),
          shiny::actionButton(
            "basin_plot_add_pairs",
            "Add pair plots",
            class = "btn-light btn-sm"
          ),
          shiny::actionButton(
            "basin_plot_add_matrix",
            "Add matrix",
            class = "btn-light btn-sm"
          )
        )
      ),
      if (length(specs) < 1L) {
        shiny::p(
          class = "gf-basin-plot-empty",
          paste(
            "Choose characteristics and add histograms, pair plots,",
            "or a combined matrix."
          )
        )
      } else {
        shiny::div(
          class = "gf-basin-plot-card-grid",
          lapply(specs, basin_plot_card_ui, result = result)
        )
      }
    )
  })
  shiny::outputOptions(
    output,
    "basin_plot_workspace_ui",
    suspendWhenHidden = FALSE
  )

  add_basin_plot_specs <- function(mode) {
    result <- basin_result()
    if (!is.list(result)) {
      return(invisible(NULL))
    }
    features <- as.character(input$basin_plot_features %||% character())
    needed <- if (identical(mode, "histograms")) 1L else 2L
    if (length(unique(features)) < needed) {
      shiny::showNotification(
        if (needed == 1L) {
          "Select at least one basin characteristic."
        } else {
          "Select at least two basin characteristics."
        },
        type = "warning"
      )
      return(invisible(NULL))
    }
    first.id <- basin_plot_next_id() + 1L
    candidates <- gflowui_basin_new_plot_specs(
      features = features,
      mode = mode,
      first_id = first.id,
      scope = input$basin_plot_builder_scope %||% "all",
      type = input$basin_plot_builder_type %||% "both",
      construction_fingerprint = as.character(
        result$construction_identity$fingerprint %||% ""
      )
    )
    existing <- basin_plot_specs()
    filtered <- gflowui_basin_filter_new_plot_specs(existing, candidates)
    added <- filtered$specs
    if (length(added) < 1L) {
      shiny::showNotification(
        "All requested plots are already present in the workspace.",
        type = "message"
      )
      return(invisible(NULL))
    }
    if (length(existing) + length(added) > 36L) {
      shiny::showNotification(
        "The Basin Plot Workspace is limited to 36 plot cards.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    basin_plot_next_id(max(vapply(added, `[[`, integer(1), "id")))
    basin_plot_specs(c(existing, added))
    if (filtered$skipped > 0L) {
      shiny::showNotification(
        sprintf(
          "Added %d new plot%s; skipped %d already present.",
          length(added),
          if (identical(length(added), 1L)) "" else "s",
          filtered$skipped
        ),
        type = "message"
      )
    }
    invisible(NULL)
  }

  shiny::observeEvent(input$basin_plot_add_histograms, {
    add_basin_plot_specs("histograms")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$basin_plot_add_pairs, {
    add_basin_plot_specs("pairs")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$basin_plot_add_matrix, {
    add_basin_plot_specs("matrix")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$basin_plot_clear_all, {
    basin_plot_specs(list())
  }, ignoreInit = TRUE)

  shiny::observe({
    source <- basin_source_state()
    if (!is.list(shiny::isolate(basin_result()))) {
      return()
    }
    request <- tryCatch(
      basin_construction_request(source),
      error = function(e) e
    )
    if (inherits(request, "error")) {
      invalidate_basin_result_if_needed(
        NULL,
        sprintf(
          "Basin result is stale because the active source cannot be validated: %s",
          conditionMessage(request)
        )
      )
    } else {
      invalidate_basin_result_if_needed(request)
    }
  })

  shiny::observe({
    source <- basin_source_state()
    if (is.list(source) && !is.list(basin_result())) {
      current <- as.character(basin_status() %||% "")
      if (grepl(
        "^Apply an occupation density|^Estimate source changed",
        current
      )) {
        basin_status(sprintf(
          "Ready to reconstruct maximum and minimum basins for %s.",
          source$label
        ))
      }
    }
  })

  shiny::observeEvent(
    list(input$basin_top_k_max, input$basin_top_k_min, input$basin_rank_by),
    {
      top.max <- suppressWarnings(as.integer(input$basin_top_k_max))
      top.min <- suppressWarnings(as.integer(input$basin_top_k_min))
      rank.by <- as.character(input$basin_rank_by %||% "auto")
      if (length(top.max) == 1L && is.finite(top.max) && top.max >= 0L) {
        basin_display_settings$top_k_max <- top.max
      }
      if (length(top.min) == 1L && is.finite(top.min) && top.min >= 0L) {
        basin_display_settings$top_k_min <- top.min
      }
      if (rank.by %in% c(
          "auto",
          "primary.support.mass",
          "primary.support.size"
      )) {
        basin_display_settings$rank_by <- rank.by
      }
      result <- shiny::isolate(basin_result())
      if (is.list(result)) {
        needs.update <- !identical(
          suppressWarnings(as.integer(result$top_k_max)),
          suppressWarnings(as.integer(basin_display_settings$top_k_max))
        ) || !identical(
          suppressWarnings(as.integer(result$top_k_min)),
          suppressWarnings(as.integer(basin_display_settings$top_k_min))
        ) || !identical(
          as.character(result$rank_by %||% "auto"),
          as.character(basin_display_settings$rank_by %||% "auto")
        )
        if (!isTRUE(needs.update)) {
          return()
        }
        updated <- tryCatch(
          resummarize_basin_result(result),
          error = function(e) e
        )
        if (inherits(updated, "error")) {
          basin_status(sprintf(
            "Basin summary failed: %s",
            conditionMessage(updated)
          ))
        } else {
          basin_result(updated)
          resolved <- updated$summary$rank.resolved
          basin_status(sprintf(
            "Basin summary updated without reconstruction (max: %s; min: %s).",
            resolved[["max"]] %||% "\u2014",
            resolved[["min"]] %||% "\u2014"
          ))
        }
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$basin_compute, {
    source <- basin_source_state()
    request <- tryCatch(
      basin_construction_request(source),
      error = function(e) e
    )
    if (inherits(request, "error")) {
      basin_result(NULL)
      basin_inspector_open(FALSE)
      basin_status(sprintf(
        "Basin reconstruction was not started: %s",
        conditionMessage(request)
      ))
      shiny::showNotification(conditionMessage(request), type = "error")
      return()
    }
    current <- shiny::isolate(basin_result())
    current.identity <- as.character(
      current$construction_identity$fingerprint %||% ""
    )
    request.identity <- as.character(
      request$construction_identity$fingerprint %||% ""
    )
    if (is.list(current) &&
        nzchar(current.identity) &&
        identical(current.identity, request.identity)) {
      current <- tryCatch(
        resummarize_basin_result(current),
        error = function(e) e
      )
      if (inherits(current, "error")) {
        basin_status(sprintf(
          "Basin summary failed: %s",
          conditionMessage(current)
        ))
        shiny::showNotification(conditionMessage(current), type = "error")
        return()
      }
      basin_result(current)
      basin_inspector_open(TRUE)
      graph_layout_state$color_by <- "basin_active"
      shiny::updateSelectInput(
        session,
        "graph_layout_color_by",
        selected = "basin_active"
      )
      basin_status(sprintf(
        "Opened the current Basin Inspector for %s without reconstruction.",
        current$source_label %||% request$source$label
      ))
      return()
    }
    source <- request$source
    top.max <- suppressWarnings(as.integer(
      basin_display_settings$top_k_max %||% 6L
    ))
    top.min <- suppressWarnings(as.integer(
      basin_display_settings$top_k_min %||% 6L
    ))
    if (!is.finite(top.max) || top.max < 0L) top.max <- 6L
    if (!is.finite(top.min) || top.min < 0L) top.min <- 6L
    rank.by <- as.character(basin_display_settings$rank_by %||% "auto")
    result <- tryCatch(
      gflowui_estimate_basin_overlay(
        adj_list = source$graph$adj_list,
        edge_length_list = source$graph$weight_list,
        field = source$values,
        direction = "both",
        top_k_max = top.max,
        top_k_min = top.min,
        rank_by = rank.by,
        vertex_mass = if (request$is_occupation) source$values else NULL,
        vertex_id = request$vertex_id,
        vertex_mass_provenance = request$mass_provenance,
        source_key = source$key,
        source_fingerprint = request$source_fingerprint,
        alignment_validation = request$alignment
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      basin_result(NULL)
      basin_status(sprintf(
        "Basin reconstruction failed: %s",
        conditionMessage(result)
      ))
      shiny::showNotification(conditionMessage(result), type = "error")
      return()
    }

    result$source_key <- as.character(source$key)
    result$source_label <- as.character(source$label)
    result$field_values <- source$values
    result$project_id <- as.character(source$graph$project_id %||% "")
    result$graph_set_id <- as.character(source$graph$set_id %||% "")
    result$graph_k <- suppressWarnings(as.integer(
      source$graph$k_actual %||% NA_integer_
    ))
    result$construction_identity <- request$construction_identity
    result$alignment_validation <- request$alignment
    colors <- stats::setNames(result$table$color, result$table$key)
    old.colors <- basin_color_map()
    colors[names(old.colors)] <- old.colors
    basin_color_map(colors)
    basin_selected_keys(character())
    apply_basin_extrema_source_defaults(
      if (isTRUE(request$is_occupation)) {
        "occupation_probability"
      } else {
        "conditional_expectation"
      }
    )
    result <- update_basin_display_result(result)
    basin_result(result)
    basin_inspector_open(TRUE)
    graph_layout_state$color_by <- "basin_active"
    shiny::updateSelectInput(
      session,
      "graph_layout_color_by",
      selected = "basin_active"
    )
    basin_status(sprintf(
      paste(
        "Computed both directions and opened the Basin Inspector for %s:",
        "%d maximum and %d minimum basins",
        "(cache %s; max rank %s; min rank %s)."
      ),
      source$label,
      result$basin_count_max,
      result$basin_count_min,
      if (isTRUE(result$cache_hit)) "hit" else "miss",
      result$ranking_resolved[["max"]],
      result$ranking_resolved[["min"]]
    ))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_close, {
    basin_inspector_open(FALSE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(basin_result(), {
    result <- basin_result()
    fingerprint <- if (is.list(result)) {
      as.character(result$construction_identity$fingerprint %||% "")
    } else {
      ""
    }
    if (!identical(fingerprint, basin_export_last_fingerprint())) {
      basin_export_last_path("")
      basin_export_last_fingerprint("")
      basin_export_status(
        "No bundle saved for the active basin complex."
      )
    }
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$basin_export_directory, {
    value <- trimws(as.character(input$basin_export_directory %||% ""))
    if (length(value) == 1L && !is.na(value) && nzchar(value)) {
      basin_export_directory(value)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_export_bundle, {
    result <- shiny::isolate(basin_result())
    destination <- trimws(as.character(
      input$basin_export_directory %||% basin_export_directory()
    ))
    basin_export_status("Saving the full basin bundle...")
    saved <- tryCatch(
      gflowui_write_basin_export_bundle(
        result,
        destination = destination
      ),
      error = function(e) e
    )
    if (inherits(saved, "error")) {
      message <- sprintf(
        "Basin bundle was not saved: %s",
        conditionMessage(saved)
      )
      basin_export_status(message)
      shiny::showNotification(message, type = "error", duration = 8)
      return()
    }
    basin_export_directory(dirname(saved$path))
    basin_export_last_path(saved$path)
    basin_export_last_fingerprint(as.character(
      result$construction_identity$fingerprint %||% ""
    ))
    basin_export_status(sprintf(
      "Saved %d unfiltered basins to %s",
      saved$row_count,
      saved$path
    ))
    shiny::updateTextInput(
      session,
      "basin_export_directory",
      value = dirname(saved$path)
    )
    shiny::showNotification(
      sprintf("Saved basin bundle: %s", saved$path),
      type = "message",
      duration = 8
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_filter, {
    value <- as.character(input$basin_inspector_filter %||% "all")
    if (value %in% c("all", "max", "min")) {
      basin_display_settings$inspector_filter <- value
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_columns, {
    value <- as.character(input$basin_inspector_columns %||% "compact")
    if (value %in% c("compact", "full")) {
      basin_display_settings$inspector_columns <- value
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_show_extremum_vertex, {
    basin_display_settings$inspector_show_extremum_vertex <- isTRUE(
      input$basin_inspector_show_extremum_vertex
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_width, {
    value <- suppressWarnings(as.numeric(input$basin_inspector_width))
    if (is.finite(value)) {
      basin_display_settings$inspector_width <- as.integer(
        max(380, min(1200, value))
      )
    }
  }, ignoreInit = TRUE)

  activate_basin_color_source <- function(
      result = shiny::isolate(basin_result()),
      update_status = FALSE) {
    if (!is.list(result) ||
        !is.data.frame(result$table) ||
        length(result$values %||% character()) < 1L) {
      return(invisible(FALSE))
    }
    graph_layout_state$color_by <- "basin_active"
    shiny::updateSelectInput(
      session,
      "graph_layout_color_by",
      selected = "basin_active"
    )
    if (isTRUE(update_status)) {
      basin_status(sprintf(
        paste(
          "Showing basin colors for %s.",
          "Unselected vertices use the Basin Inspector background color."
        ),
        result$source_label %||% "the active basin complex"
      ))
    }
    invisible(TRUE)
  }

  shiny::observeEvent(input$basin_show_colors, {
    activate_basin_color_source(update_status = TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_select_displayed, {
    result <- basin_result()
    if (is.list(result) && is.data.frame(result$table)) {
      displayed <- basin_displayed_table(result)
      basin_selected_keys(unique(c(
        basin_selected_keys(),
        as.character(displayed$key)
      )))
      result <- update_basin_display_result(result)
      basin_result(result)
      activate_basin_color_source(result)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_clear_displayed, {
    result <- basin_result()
    if (is.list(result) && is.data.frame(result$table)) {
      displayed <- basin_displayed_table(result)
      basin_selected_keys(setdiff(
        basin_selected_keys(),
        as.character(displayed$key)
      ))
    }
    if (is.list(result)) {
      result <- update_basin_display_result(result)
      basin_result(result)
      activate_basin_color_source(result)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_clear_all, {
    result <- basin_result()
    basin_selected_keys(character())
    if (is.list(result)) {
      result <- update_basin_display_result(result)
      basin_result(result)
      activate_basin_color_source(result)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_reset_colors, {
    result <- basin_result()
    if (is.list(result) && is.data.frame(result$table)) {
      defaults <- gflowui_basin_default_colors(result$table)
      colors <- basin_color_map()
      colors[names(defaults)] <- defaults
      basin_color_map(colors)
      result <- update_basin_display_result(result)
      basin_result(result)
      activate_basin_color_source(result)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_display_mode, {
    mode <- as.character(input$basin_display_mode %||% "both")
    if (mode %in% c("both", "maximum", "minimum")) {
      basin_display_settings$display_mode <- mode
      result <- shiny::isolate(basin_result())
      if (is.list(result)) {
        result <- update_basin_display_result(result)
        basin_result(result)
        activate_basin_color_source(result)
      }
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_global_opacity, {
    opacity <- suppressWarnings(as.numeric(input$basin_global_opacity))
    if (is.finite(opacity)) {
      basin_display_settings$opacity <- max(0, min(1, opacity))
      activate_basin_color_source()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_unselected_color, {
    color <- as.character(input$basin_unselected_color %||% "")
    if (nzchar(color)) {
      basin_display_settings$unselected_color <- color
      activate_basin_color_source()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_unselected_opacity, {
    opacity <- suppressWarnings(as.numeric(input$basin_unselected_opacity))
    if (is.finite(opacity)) {
      basin_display_settings$unselected_opacity <- max(0, min(1, opacity))
      activate_basin_color_source()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$basin_inspector_row_event, {
    result <- basin_result()
    if (!is.list(result) || !is.data.frame(result$table) ||
        nrow(result$table) < 1L) {
      return()
    }
    event <- input$basin_inspector_row_event
    key <- as.character(event$key %||% "")
    role <- as.character(event$role %||% "")
    if (!nzchar(key) || !(key %in% as.character(result$table$key))) {
      return()
    }
    next.state <- gflowui_update_basin_row_state(
      selected_keys = basin_selected_keys(),
      color_map = basin_color_map(),
      valid_keys = result$table$key,
      key = key,
      role = role,
      checked = event$checked,
      value = event$value
    )
    if (!isTRUE(next.state$changed)) {
      return()
    }
    basin_selected_keys(next.state$selected_keys)
    basin_color_map(next.state$color_map)
    result <- update_basin_display_result(result)
    basin_result(result)
    activate_basin_color_source(result)
  }, ignoreInit = TRUE)

  reference_renderer_state <- shiny::reactive({
    if (isTRUE(quadform_project_active())) {
      return(list(
        st = list(error = "Quadform benchmark uses the benchmark viewer."),
        requested = "none",
        effective = "none",
        rgl_ready = requireNamespace("rgl", quietly = TRUE),
        plotly_ready = requireNamespace("plotly", quietly = TRUE),
        mode_note = "",
        color_mode = "source",
        src_key = "",
        color_label = "",
        solid_color = graph_solid_color_default,
        vertex_mode = "point",
        size_mult = 1,
        size_label = "1x",
        component_mode = "all",
        keep_idx = integer(0),
        component_note = ""
      ))
    }
    st <- reference_view_state()
    sel <- current_graph_selection()
    manifest <- if (is.list(sel) && is.null(sel$error)) sel$manifest else active_manifest()
    spec <- NULL
    if (is.list(manifest)) {
      spec <- resolve_reference_spec(
        manifest,
        preferred_set_id = scalar_chr(sel$set_id %||% st$set_id %||% "", default = ""),
        preferred_k = scalar_int(
          sel$k_selected,
          default = scalar_int(st$k_actual, default = NA_integer_)
        )
      )
    }
    if (is.list(spec) && is.finite(suppressWarnings(as.integer(st$k_actual)))) {
      spec$k_ref <- suppressWarnings(as.integer(st$k_actual))
    }

    requested_raw <- tolower(trimws(as.character(
      input$graph_layout_renderer %||% graph_layout_state$renderer %||% "plotly"
    )))
    requested <- normalize_live_renderer_choice(requested_raw, default = "plotly")

    layout_presets <- if (is.list(spec$graph_set$layout_assets$presets)) spec$graph_set$layout_assets$presets else list()
    src_key_raw <- as.character(
      input$graph_layout_color_by %||%
        graph_layout_state$color_by %||%
        layout_presets$color_by %||%
        st$default_key %||%
        ""
    )
    use_solid_color <- identical(src_key_raw, graph_solid_color_key)
    src_key <- src_key_raw
    if (!isTRUE(use_solid_color) && !(src_key %in% names(st$sources %||% list()))) {
      src_key <- as.character(st$default_key %||% "")
    }
    solid_color <- normalize_palette_choice(
      input$graph_layout_vertex_color %||%
        graph_layout_state$vertex_color %||%
        layout_presets$vertex_color %||%
        graph_solid_color_default,
      graph_vertex_color_choices(),
      default = graph_solid_color_default
    )
    color_label <- if (isTRUE(use_solid_color)) {
      "Solid color"
    } else if (nzchar(src_key) && src_key %in% names(st$sources %||% list())) {
      as.character(st$sources[[src_key]]$label %||% src_key)
    } else {
      ""
    }

    vertex_mode_default <- default_vertex_layout_for_graph(
      preset = layout_presets$vertex_layout %||% "point",
      n_vertices = st$n_vertices
    )
    vertex_mode <- tolower(trimws(as.character(
      input$graph_layout_vertex %||% graph_layout_state$vertex_layout %||% vertex_mode_default
    )))
    if (!vertex_mode %in% c("sphere", "point")) {
      vertex_mode <- vertex_mode_default
    }
    size_raw <- as.character(
      input$graph_layout_size %||%
        graph_layout_state$size_label %||%
        layout_presets$vertex_size %||%
        "1.0x"
    )
    size_mult <- suppressWarnings(as.numeric(gsub("[^0-9.]+", "", size_raw)))
    if (!is.finite(size_mult) || size_mult <= 0) {
      size_mult <- 1
    }
    size_label <- sprintf("%sx", format(size_mult, scientific = FALSE, trim = TRUE))
    component_mode <- tolower(trimws(as.character(
      input$graph_layout_component %||% graph_layout_state$component %||% layout_presets$component %||% "all"
    )))
    if (!component_mode %in% c("all", "lcc")) {
      component_mode <- "all"
    }
    endpoint_label_size <- parse_scale_multiplier(input$endpoint_label_size %||% "1x", default = 1)
    if (!is.finite(endpoint_label_size) || endpoint_label_size <= 0) {
      endpoint_label_size <- 1
    }
    endpoint_label_offset <- parse_scale_multiplier(input$endpoint_label_offset %||% "1x", default = 1)
    if (!is.finite(endpoint_label_offset) || endpoint_label_offset < 0) {
      endpoint_label_offset <- 1
    }
    endpoint_marker_size <- parse_scale_multiplier(input$endpoint_marker_size %||% "1x", default = 1)
    if (!is.finite(endpoint_marker_size) || endpoint_marker_size <= 0) {
      endpoint_marker_size <- 1
    }
    endpoint_marker_palette <- c(
      "Red" = "#ef4444",
      "Orange" = "#f97316",
      "Gold" = "#eab308",
      "Green" = "#22c55e",
      "Teal" = "#14b8a6",
      "Blue" = "#3b82f6",
      "Purple" = "#8b5cf6",
      "Pink" = "#ec4899",
      "Black" = "#111827"
    )
    endpoint_marker_color <- tolower(trimws(as.character(input$endpoint_marker_color %||% "#ef4444")))
    palette_values <- tolower(unname(endpoint_marker_palette))
    if (!(endpoint_marker_color %in% palette_values)) {
      endpoint_marker_color <- "#ef4444"
    }

    n_vertices <- suppressWarnings(as.integer(st$n_vertices %||% 0L))
    keep_idx <- seq_len(max(0L, n_vertices))
    component_note <- ""
    comp <- if (is.list(st$components)) st$components else list()
    comp_n <- suppressWarnings(as.integer(comp$n_components %||% 1L))
    comp_lcc <- suppressWarnings(as.integer(comp$lcc_index %||% integer(0)))
    if (is.finite(comp_n) && comp_n > 1L && identical(component_mode, "lcc") && length(comp_lcc) > 0L) {
      comp_lcc <- comp_lcc[is.finite(comp_lcc) & comp_lcc >= 1L & comp_lcc <= n_vertices]
      if (length(comp_lcc) > 0L) {
        keep_idx <- unique(comp_lcc)
        component_note <- sprintf(
          "Showing main connected component (%s/%s vertices).",
          format(length(keep_idx), big.mark = ","),
          format(max(1L, n_vertices), big.mark = ",")
        )
      }
    }

    plotly_ready <- requireNamespace("plotly", quietly = TRUE)
    rgl_ready <- requireNamespace("rgl", quietly = TRUE)
    effective <- requested
    note <- NULL

    if (identical(requested, "rglwidget")) {
      if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
      } else if (isTRUE(plotly_ready)) {
        effective <- "plotly"
        note <- paste(
          c(note, "RGL mode requested, but `rgl` is unavailable. Showing Plotly fallback."),
          collapse = " "
        )
      } else {
        effective <- "none"
        note <- paste(
          c(note, "RGL mode requested, but `rgl` is unavailable and no fallback renderer is ready."),
          collapse = " "
        )
      }
    } else if (identical(requested, "plotly")) {
      if (isTRUE(plotly_ready)) {
        effective <- "plotly"
      } else if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
        note <- paste(c(note, "Plotly is unavailable. Showing RGL fallback."), collapse = " ")
      } else {
        effective <- "none"
        note <- paste(
          c(note, "Plotly is unavailable and no fallback renderer is available."),
          collapse = " "
        )
      }
    }
    note <- trimws(gsub("\\s+", " ", as.character(note %||% "")))

    list(
      st = st,
      requested = requested,
      effective = effective,
      rgl_ready = rgl_ready,
      plotly_ready = plotly_ready,
      mode_note = note,
      color_mode = if (isTRUE(use_solid_color)) "solid" else "source",
      src_key = src_key,
      color_label = color_label,
      solid_color = solid_color,
      vertex_mode = vertex_mode,
      size_mult = size_mult,
      size_label = size_label,
      component_mode = component_mode,
      keep_idx = as.integer(keep_idx),
      component_note = component_note,
      endpoint_label_size = endpoint_label_size,
      endpoint_label_offset = endpoint_label_offset,
      endpoint_marker_size = endpoint_marker_size,
      endpoint_marker_color = endpoint_marker_color
    )
  })

  categorical_palette <- function(values, source_key = "", source_label = "") {
    to_hex <- function(col) {
      cc <- as.character(col %||% "")
      if (!nzchar(cc)) {
        return("#808080")
      }
      rgb <- tryCatch(grDevices::col2rgb(cc), error = function(e) NULL)
      if (is.null(rgb) || ncol(rgb) < 1L) {
        return(cc)
      }
      grDevices::rgb(rgb[1, 1], rgb[2, 1], rgb[3, 1], maxColorValue = 255)
    }

    cst_colors_raw <- c(
      Lactobacillus_crispatus = "red1",
      Lactobacillus_gasseri = "chartreuse",
      Lactobacillus_iners = "darkorange2",
      BVAB1 = "aquamarine4",
      Atopobium_vaginae = "orange",
      Gardnerella_vaginalis = "royalblue",
      Sneathia_sanguinegens = "limegreen",
      g_Anaerococcus = "blue",
      g_Corynebacterium_1 = "gold",
      g_Streptococcus = "brown",
      g_Enterococcus = "deeppink",
      g_Bifidobacterium = "darkorchid",
      Lactobacillus_jensenii = "yellow",
      "I" = "red1",
      "II" = "chartreuse",
      "III" = "darkorange2",
      "IV" = "aquamarine4",
      "IV-A" = "aquamarine4",
      "IV-B" = "royalblue",
      "IV-C" = "palevioletred4",
      "V" = "yellow",
      "I-A" = "red1",
      "I-B" = "palevioletred2",
      "III-A" = "darkorange2",
      "III-B" = "orange1",
      "IV-C0" = "blue",
      "IV-C1" = "brown",
      "IV-C2" = "deeppink",
      "IV-C3" = "darkorchid",
      "IV-C4" = "cyan"
    )
    cst_colors <- vapply(cst_colors_raw, to_hex, character(1))
    names(cst_colors) <- tolower(names(cst_colors_raw))

    vv <- as.character(values)
    vv[is.na(vv) | !nzchar(vv)] <- "NA"
    lev <- unique(vv)
    if (length(lev) < 1L) {
      lev <- "NA"
    }

    cols <- grDevices::hcl.colors(max(1L, length(lev)), "Dynamic")
    cols <- as.character(cols)[seq_len(length(lev))]
    lev_low <- tolower(lev)
    match_idx <- match(lev_low, names(cst_colors))
    n_match <- sum(!is.na(match_idx))
    src_txt <- tolower(sprintf("%s %s", as.character(source_key %||% ""), as.character(source_label %||% "")))
    if (identical(as.character(source_key), "basin_active")) {
      result <- basin_result()
      table <- if (is.list(result) && is.data.frame(result$table)) {
        result$table
      } else {
        data.frame()
      }
      basin.levels <- if (nrow(table) > 0L) {
        as.character(table$display.label[
          table$display.label %in% lev
        ])
      } else {
        character()
      }
      other.levels <- setdiff(lev, basin.levels)
      lev <- c(basin.levels, other.levels)
      color.lookup <- if (nrow(table) > 0L) {
        stats::setNames(as.character(table$color), table$display.label)
      } else {
        structure(character(), names = character())
      }
      basin.colors <- unname(color.lookup[basin.levels])
      basin.colors[is.na(basin.colors)] <- "#2563EB"
      cols <- c(
        stats::setNames(basin.colors, basin.levels),
        stats::setNames(
          rep(
            as.character(
              basin_display_settings$unselected_color %||% "#D1D5DB"
            ),
            length(other.levels)
          ),
          other.levels
        )
      )
      return(list(values = vv, levels = lev, colors = cols))
    }
    use_cst <- grepl("(^|[^a-z])cst([^a-z]|$)|subcst|linf", src_txt, perl = TRUE) ||
      (length(lev) > 0L && n_match >= max(2L, floor(length(lev) / 2L)))
    if (isTRUE(use_cst) && n_match > 0L) {
      for (ii in seq_along(lev)) {
        idx <- match_idx[[ii]]
        if (is.finite(idx)) {
          cols[[ii]] <- cst_colors[[idx]]
        }
      }
    }
    names(cols) <- lev
    list(values = vv, levels = lev, colors = cols)
  }

  arm_preview_body_color_choices <- c(
    "Solid color" = "solid",
    "Balanced position" = "t_balance",
    "Harmonic position" = "harmonic_t",
    "Distance to path" = "distance_to_path",
    "Excess" = "excess"
  )

  arm_preview_body_metric <- function(arm_variant, metric_name, vertices) {
    vv <- suppressWarnings(as.integer(vertices %||% integer(0)))
    vv <- vv[is.finite(vv)]
    if (length(vv) < 1L) {
      return(NULL)
    }
    metric_key <- as.character(metric_name %||% "solid")
    if (!nzchar(metric_key) || identical(metric_key, "solid")) {
      return(NULL)
    }
    metrics <- if (is.list(arm_variant$arm_metrics)) arm_variant$arm_metrics else list()
    vals <- suppressWarnings(as.numeric(metrics[[metric_key]] %||% numeric(0)))
    val_names <- names(metrics[[metric_key]] %||% numeric(0))
    if (length(vals) < 1L || length(val_names) != length(vals)) {
      return(NULL)
    }
    mm <- match(as.character(vv), as.character(val_names))
    ok <- is.finite(mm) & mm >= 1L & mm <= length(vals)
    if (!any(ok)) {
      return(NULL)
    }
    out <- rep(NA_real_, length(vv))
    out[ok] <- vals[mm[ok]]
    if (!any(is.finite(out))) {
      return(NULL)
    }
    choice_match <- names(arm_preview_body_color_choices)[match(metric_key, unname(arm_preview_body_color_choices))]
    metric_label <- if (length(choice_match) > 0L && !is.na(choice_match[[1]]) && nzchar(choice_match[[1]])) {
      as.character(choice_match[[1]])
    } else {
      metric_key
    }
    list(
      key = metric_key,
      label = metric_label,
      values = out
    )
  }

  numeric_arm_colors <- function(
      values,
      palette = "Viridis",
      alpha = 1,
      color_limits = NULL,
      palette_colors = NULL) {
    vv <- suppressWarnings(as.numeric(values %||% numeric(0)))
    out <- rep("#9ca3af", length(vv))
    ok <- is.finite(vv)
    if (!any(ok)) {
      return(out)
    }
    rng <- suppressWarnings(as.numeric(color_limits %||% numeric(0)))
    if (length(rng) != 2L || any(!is.finite(rng)) || rng[[1]] > rng[[2]]) {
      rng <- range(vv[ok], na.rm = TRUE)
    }
    if (!all(is.finite(rng))) {
      return(out)
    }
    if (diff(rng) <= 0) {
      idx <- rep(128L, sum(ok))
    } else {
      scaled <- (vv[ok] - rng[[1]]) / diff(rng)
      idx <- pmin(256L, pmax(1L, floor(scaled * 255) + 1L))
    }
    palette_colors <- as.character(palette_colors %||% character(0))
    palette_colors <- palette_colors[nzchar(palette_colors)]
    pal <- if (length(palette_colors) >= 2L) {
      palette_has_alpha <- any(
        grDevices::col2rgb(palette_colors, alpha = TRUE)[4L, ] < 255
      )
      grDevices::colorRampPalette(
        palette_colors,
        alpha = palette_has_alpha
      )(256)
    } else {
      grDevices::hcl.colors(256, palette)
    }
    cols <- pal[idx]
    if (is.finite(alpha) && alpha > 0 && alpha < 1) {
      cols <- grDevices::adjustcolor(cols, alpha.f = alpha)
    }
    out[ok] <- cols
    out
  }

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$quadform_original_plot <- plotly::renderPlotly({
      st <- quadform_view_state()
      req(is.list(st), st$status %in% c("ok", "missing_layout", "layout_error"))
      dataset <- st$dataset
      req(is.list(dataset), identical(dataset$status, "ok"))
      coords <- normalize_coord_matrix(dataset$coords)
      row <- st$selected_row
      title <- sprintf(
        "Original data: %s, n=%s, seed=%s",
        as.character(row$surface[[1]] %||% row$dataset_id[[1]] %||% ""),
        as.character(row$n[[1]] %||% ""),
        as.character(row$seed[[1]] %||% "")
      )
      plotly::plot_ly(
        x = coords[, 1],
        y = coords[, 2],
        z = coords[, 3],
        type = "scatter3d",
        mode = "markers",
        text = sprintf("vertex=%d", seq_len(nrow(coords))),
        hoverinfo = "text",
        marker = list(size = 3.5, color = "#2563eb", opacity = 0.88)
      ) %>%
        plotly::layout(
          title = list(text = title, font = list(size = 13)),
          margin = list(l = 0, r = 0, b = 0, t = 34),
          scene = list(
            xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            zaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE)
          )
        )
    })

    output$quadform_graph_plot <- plotly::renderPlotly({
      st <- quadform_view_state()
      req(is.list(st), identical(st$status, "ok"))
      coords <- normalize_coord_matrix(st$layout_coords)
      adj <- st$graph$adj_list
      req(is.matrix(coords), is.list(adj), nrow(coords) == length(adj))

      edges <- adj_to_edge_matrix(adj)
      if (is.matrix(edges) && nrow(edges) > 4000L) {
        set.seed(1L)
        edges <- edges[sort(sample.int(nrow(edges), 4000L)), , drop = FALSE]
      }
      edge_xyz <- matrix(NA_real_, nrow = 0L, ncol = 3L)
      if (is.matrix(edges) && nrow(edges) > 0L) {
        edge_xyz <- matrix(NA_real_, nrow = nrow(edges) * 3L, ncol = 3L)
        edge_xyz[seq(1L, nrow(edge_xyz), by = 3L), ] <- coords[edges[, 1], , drop = FALSE]
        edge_xyz[seq(2L, nrow(edge_xyz), by = 3L), ] <- coords[edges[, 2], , drop = FALSE]
      }

      row <- st$selected_row
      title <- sprintf(
        "Weighted GRIP: %s / %s / %s",
        as.character(row$graph_family[[1]] %||% ""),
        as.character(row$prune_method[[1]] %||% ""),
        as.character(row$stage[[1]] %||% "")
      )
      p <- plotly::plot_ly()
      if (nrow(edge_xyz) > 0L) {
        p <- p %>%
          plotly::add_trace(
            x = edge_xyz[, 1],
            y = edge_xyz[, 2],
            z = edge_xyz[, 3],
            type = "scatter3d",
            mode = "lines",
            hoverinfo = "skip",
            line = list(color = "rgba(17,24,39,0.20)", width = 1),
            showlegend = FALSE
          )
      }
      p %>%
        plotly::add_trace(
          x = coords[, 1],
          y = coords[, 2],
          z = coords[, 3],
          type = "scatter3d",
          mode = "markers",
          text = sprintf("vertex=%d", seq_len(nrow(coords))),
          hoverinfo = "text",
          marker = list(size = 3.5, color = "#0f8b77", opacity = 0.9),
          showlegend = FALSE
        ) %>%
        plotly::layout(
          title = list(text = title, font = list(size = 13)),
          margin = list(l = 0, r = 0, b = 0, t = 34),
          scene = list(
            xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            zaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE)
          )
        )
    })

    output$reference_plot <- plotly::renderPlotly({
      rr <- reference_renderer_state()
      st <- rr$st
      req(is.null(st$error))

      color_mode <- as.character(rr$color_mode %||% "source")
      src_key <- as.character(rr$src_key %||% st$default_key)
      solid_color <- normalize_palette_choice(
        rr$solid_color %||% graph_solid_color_default,
        graph_vertex_color_choices(),
        default = graph_solid_color_default
      )
      if (identical(color_mode, "solid")) {
        src <- list(
          key = graph_solid_color_key,
          label = "Solid color",
          type = "solid",
          values = rep.int(NA_character_, nrow(st$coords))
        )
      } else {
        if (!(src_key %in% names(st$sources))) {
          src_key <- st$default_key
        }
        src <- st$sources[[src_key]]
      }
      vals <- src$values
      coords <- st$coords
      nn <- nrow(coords)
      idx_all <- seq_len(nn)
      keep_idx <- suppressWarnings(as.integer(rr$keep_idx %||% idx_all))
      keep_idx <- keep_idx[is.finite(keep_idx) & keep_idx >= 1L & keep_idx <= nn]
      if (length(keep_idx) < 1L) {
        keep_idx <- idx_all
      }
      size_mult <- suppressWarnings(as.numeric(rr$size_mult %||% 1))
      if (!is.finite(size_mult) || size_mult <= 0) {
        size_mult <- 1
      }
      vertex_mode <- tolower(as.character(rr$vertex_mode %||% "sphere"))
      base_size <- if (identical(vertex_mode, "point")) 2.8 else 5.2
      point_size <- max(1.2, base_size * size_mult)
      endpoint_label_size <- suppressWarnings(as.numeric(rr$endpoint_label_size %||% 1))
      if (!is.finite(endpoint_label_size) || endpoint_label_size <= 0) {
        endpoint_label_size <- 1
      }
      endpoint_label_offset <- suppressWarnings(as.numeric(rr$endpoint_label_offset %||% 1))
      if (!is.finite(endpoint_label_offset) || endpoint_label_offset < 0) {
        endpoint_label_offset <- 1
      }
      endpoint_marker_size <- suppressWarnings(as.numeric(rr$endpoint_marker_size %||% 1))
      if (!is.finite(endpoint_marker_size) || endpoint_marker_size <= 0) {
        endpoint_marker_size <- 1
      }
      endpoint_marker_color <- as.character(rr$endpoint_marker_color %||% "#ef4444")
      if (length(endpoint_marker_color) < 1L || !nzchar(endpoint_marker_color[[1]])) {
        endpoint_marker_color <- "#ef4444"
      } else {
        endpoint_marker_color <- endpoint_marker_color[[1]]
      }
      subject_overlay <- subject_overlay_active()
      dim_background_active <- isTRUE(subject_overlay$dim_background) &&
        length(subject_overlay$vertices %||% integer(0)) > 0L
      background_opacity_use <- suppressWarnings(as.numeric(subject_overlay$background_opacity %||% 0.22))
      if (!is.finite(background_opacity_use) || background_opacity_use <= 0) {
        background_opacity_use <- 0.22
      }
      background_opacity_use <- min(1, max(0.05, background_opacity_use))
      base_marker_opacity <- if (isTRUE(dim_background_active)) {
        background_opacity_use
      } else if (identical(src_key, "basin_active")) {
        opacity <- suppressWarnings(as.numeric(
          basin_display_settings$opacity %||% 0.85
        ))
        if (!is.finite(opacity)) 0.85 else max(0, min(1, opacity))
      } else if (identical(vertex_mode, "point")) {
        0.82
      } else {
        0.93
      }
      idx <- keep_idx
      if (length(idx) < 1L) {
        p_empty <- plotly::plot_ly(source = reference_plotly_source) %>%
            plotly::layout(
              title = list(text = "No points to display for selected color source."),
              scene = list(
                uirevision = "reference-scene",
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                zaxis = list(visible = FALSE)
              )
            )
        p_empty <- plotly::event_register(p_empty, "plotly_click")
        p_empty <- attach_reference_plotly_camera_preserver(p_empty)
        return(p_empty)
      }

      plot_data <- data.frame(
        vertex = idx,
        x = coords[idx, 1],
        y = coords[idx, 2],
        z = coords[idx, 3],
        value = vals[idx],
        stringsAsFactors = FALSE
      )

      p <- plotly::plot_ly(source = reference_plotly_source)

      if (identical(color_mode, "solid")) {
        p <- p %>%
          plotly::add_trace(
            type = "scatter3d",
            mode = "markers",
            x = plot_data$x,
            y = plot_data$y,
            z = plot_data$z,
            key = plot_data$vertex,
            customdata = plot_data$vertex,
            text = sprintf("vertex=%d", plot_data$vertex),
            hoverinfo = "text",
            marker = list(
              size = point_size,
              color = solid_color,
              opacity = base_marker_opacity
            ),
            showlegend = FALSE
          )
      } else if (identical(src$type, "categorical") &&
                 identical(src_key, "basin_active") &&
                 identical(
                   as.character(
                     basin_display_settings$display_mode %||% "both"
                   ),
                   "both"
                 )) {
        specs <- gflowui_basin_layer_specs(
          basin_display = basin_result(),
          visible_vertices = idx,
          point_size = point_size,
          opacity = base_marker_opacity,
          unselected_color =
            basin_display_settings$unselected_color %||% "#D1D5DB",
          unselected_opacity =
            basin_display_settings$unselected_opacity %||% 0.28
        )
        p <- gflowui_add_plotly_basin_layers(p, specs, coords)
      } else if (identical(src$type, "categorical")) {
        pal_info <- categorical_palette(
          plot_data$value,
          source_key = src_key,
          source_label = src$label %||% src_key
        )
        fac <- factor(pal_info$values, levels = pal_info$levels)
        nlev <- nlevels(fac)
        pal <- pal_info$colors

        for (ii in seq_len(nlev)) {
          lvl <- levels(fac)[ii]
          sel <- fac == lvl
          trace.opacity <- if (
            identical(src_key, "basin_active") &&
              identical(lvl, "Other basins")
          ) {
            value <- suppressWarnings(as.numeric(
              basin_display_settings$unselected_opacity %||% 0.28
            ))
            if (!is.finite(value)) 0.28 else max(0, min(1, value))
          } else {
            base_marker_opacity
          }
          p <- p %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers",
              x = plot_data$x[sel],
              y = plot_data$y[sel],
              z = plot_data$z[sel],
              key = plot_data$vertex[sel],
              customdata = plot_data$vertex[sel],
              name = lvl,
              legendgroup = lvl,
              text = sprintf("vertex=%d<br>%s=%s", plot_data$vertex[sel], src$label, lvl),
              hoverinfo = "text",
              marker = list(
                size = point_size,
                color = pal[[lvl]],
                opacity = trace.opacity
              ),
              showlegend = FALSE
            ) %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers",
              x = if (nrow(plot_data) > 0L) plot_data$x[[1]] else 0,
              y = if (nrow(plot_data) > 0L) plot_data$y[[1]] else 0,
              z = if (nrow(plot_data) > 0L) plot_data$z[[1]] else 0,
              key = NA_integer_,
              customdata = NA_integer_,
              name = lvl,
              legendgroup = lvl,
              visible = "legendonly",
              hoverinfo = "skip",
              marker = list(
                size = max(10, point_size * 2.5),
                color = pal[[lvl]],
                opacity = 1,
                symbol = "square"
              )
            )
        }
      } else {
        vv <- suppressWarnings(as.numeric(plot_data$value))
        color_encoding <- gflowui_numeric_color_encoding(
          values = vv,
          transform = src$color_transform %||% "identity",
          title = src$colorbar_title %||% src$label
        )
        color_limits <- suppressWarnings(as.numeric(
          color_encoding$color_limits %||% numeric(0)
        ))
        color_min <- if (length(color_limits) == 2L && all(is.finite(color_limits))) {
          color_limits[[1L]]
        } else {
          NULL
        }
        color_max <- if (length(color_limits) == 2L && all(is.finite(color_limits))) {
          color_limits[[2L]]
        } else {
          NULL
        }
        density_palette <- if (identical(
          as.character(src$color_transform %||% "identity"),
          "density_log10"
        )) {
          gflowui_density_palette(
            low = density_display_settings$low %||% "yellow",
            midpoint = density_display_settings$midpoint %||% "none",
            high = density_display_settings$high %||% "red",
            low_alpha = density_display_settings$low_alpha %||% 0.2,
            midpoint_alpha = density_display_settings$midpoint_alpha %||% 1,
            high_alpha = density_display_settings$high_alpha %||% 1
          )
        } else {
          NULL
        }
        p <- p %>%
          plotly::add_trace(
            type = "scatter3d",
            mode = "markers",
            x = plot_data$x,
            y = plot_data$y,
            z = plot_data$z,
            key = plot_data$vertex,
            customdata = plot_data$vertex,
            text = sprintf(
              "vertex=%d<br>%s=%s",
              plot_data$vertex,
              src$label,
              formatC(color_encoding$raw_values, format = "g", digits = 4)
            ),
            hoverinfo = "text",
            marker = list(
              size = point_size,
              color = color_encoding$mapped_values,
              colorscale = if (is.null(density_palette)) {
                "Viridis"
              } else {
                gflowui_plotly_colorscale(density_palette)
              },
              cmin = color_min,
              cmax = color_max,
              opacity = base_marker_opacity,
              colorbar = color_encoding$colorbar
            ),
            showlegend = FALSE
          )
      }

      basin_source <- basin_source_state()
      show_density_maxima <- isTRUE(basin_display_settings$show_maxima)
      show_density_minima <- isTRUE(basin_display_settings$show_minima)
      if (is.list(basin_source) &&
          length(basin_source$values %||% numeric(0)) == length(st$adj_list) &&
          (show_density_maxima || show_density_minima)) {
        density_extrema <- rbind(
          basin_extrema_scope_rows(
            basin_result(),
            "max",
            basin_display_settings$maxima_scope %||% "none"
          ),
          basin_extrema_scope_rows(
            basin_result(),
            "min",
            basin_display_settings$minima_scope %||% "none"
          )
        )
        density_extrema <- density_extrema[
          density_extrema$vertex %in% idx,
          ,
          drop = FALSE
        ]
        extrema_specs <- list(
          maximum = list(
            show = show_density_maxima,
            labels = isTRUE(basin_display_settings$label_maxima),
            name = "Local maxima",
            color = "#111827",
            outline = "#FFFFFF",
            symbol = "diamond",
            textposition = "top center"
          ),
          minimum = list(
            show = show_density_minima,
            labels = isTRUE(basin_display_settings$label_minima),
            name = "Local minima",
            color = "#06B6D4",
            outline = "#111827",
            symbol = "x",
            textposition = "bottom center"
          )
        )
        for (extrema_type in names(extrema_specs)) {
          spec <- extrema_specs[[extrema_type]]
          rows <- density_extrema[
            density_extrema$type == extrema_type,
            ,
            drop = FALSE
          ]
          if (!isTRUE(spec$show) || nrow(rows) < 1L) {
            next
          }
          label_active <- isTRUE(spec$labels)
          p <- p %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = if (label_active) "markers+text" else "markers",
              x = coords[rows$vertex, 1],
              y = coords[rows$vertex, 2],
              z = coords[rows$vertex, 3],
              key = rows$vertex,
              customdata = rows$vertex,
              name = spec$name,
              text = if (label_active) rows$label else NULL,
              textposition = if (label_active) spec$textposition else NULL,
              hovertext = sprintf(
                "%s<br>vertex=%d<br>%s=%s",
                rows$label,
                rows$vertex,
                as.character(basin_source$label %||% "estimate"),
                formatC(rows$value, format = "g", digits = 5)
              ),
              hoverinfo = "text",
              marker = list(
                size = max(6, point_size * 1.55),
                color = spec$color,
                symbol = spec$symbol,
                line = list(color = spec$outline, width = 1.4)
              ),
              textfont = if (label_active) {
                list(
                  size = max(9, point_size * 2.2),
                  color = spec$color
                )
              } else {
                NULL
              },
              showlegend = TRUE
            )
        }
      }

      ep_overlay <- endpoint_overlay_active()
      ep_extra <- suppressWarnings(as.integer(ep_overlay$vertices %||% integer(0)))
      ep_extra <- ep_extra[is.finite(ep_extra) & ep_extra >= 1L & ep_extra <= nn]
      ep <- sort(unique(ep_extra))
      ep <- ep[ep %in% idx]

      ep_label_lookup <- ep_overlay$labels %||% structure(character(0), names = character(0))
      ep_label_lookup <- as.character(ep_label_lookup)
      ep_label_names <- names(ep_overlay$labels %||% character(0))
      if (length(ep_label_names) == length(ep_label_lookup)) {
        names(ep_label_lookup) <- as.character(ep_label_names)
      } else {
        names(ep_label_lookup) <- character(length(ep_label_lookup))
      }
      ep_label_text <- rep("", length(ep))
      if (length(ep) > 0L && length(ep_label_lookup) > 0L && !is.null(names(ep_label_lookup))) {
        mm <- match(as.character(ep), names(ep_label_lookup))
        ok <- is.finite(mm)
        ep_label_text[ok] <- as.character(ep_label_lookup[mm[ok]])
        ep_label_text[is.na(ep_label_text)] <- ""
      }

      if (length(ep) > 0L) {
        p <- p %>%
          plotly::add_trace(
            type = "scatter3d",
            mode = "markers",
            x = coords[ep, 1],
            y = coords[ep, 2],
            z = coords[ep, 3],
            key = ep,
            customdata = ep,
            name = "Endpoints",
            text = sprintf("endpoint vertex=%d", ep),
            hoverinfo = "text",
            marker = list(
              size = max(4.5, (point_size + 2.2) * endpoint_marker_size),
              color = endpoint_marker_color,
              line = list(color = "#111827", width = 1)
            )
          )
      }

      label_idx <- which(nzchar(ep_label_text))
      if (length(label_idx) > 0L) {
        label_xyz <- endpoint_label_positions(
          coords = coords,
          endpoint_idx = ep[label_idx],
          offset_mult = endpoint_label_offset
        )
        if (!is.matrix(label_xyz) || nrow(label_xyz) != length(label_idx)) {
          label_xyz <- coords[ep[label_idx], 1:3, drop = FALSE]
        }
        p <- p %>%
          plotly::add_trace(
            type = "scatter3d",
            mode = "text",
            x = label_xyz[, 1],
            y = label_xyz[, 2],
            z = label_xyz[, 3],
            key = ep[label_idx],
            customdata = ep[label_idx],
            text = ep_label_text[label_idx],
            textposition = "top center",
            hoverinfo = "skip",
            showlegend = FALSE,
            textfont = list(size = max(8, 12 * endpoint_label_size), color = "#111827")
          )
      }

      arm_overlay <- arm_overlay_active()
      arm_list <- if (is.list(arm_overlay$arms)) arm_overlay$arms else list()
      arm_color <- normalize_palette_choice(
        input$arm_color %||% "#2563eb",
        c(
          "Blue" = "#2563eb",
          "Orange" = "#f97316",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Black" = "#111827"
        ),
        default = "#2563eb"
      )
      arm_opacity <- suppressWarnings(as.numeric(input$arm_tube_opacity %||% 0.35))
      if (!is.finite(arm_opacity) || arm_opacity <= 0) {
        arm_opacity <- 0.35
      }
      arm_path_width <- suppressWarnings(as.numeric(input$arm_path_width %||% 4))
      if (!is.finite(arm_path_width) || arm_path_width <= 0) {
        arm_path_width <- 4
      }
      arm_vertex_size <- suppressWarnings(as.numeric(input$arm_vertex_size %||% 1))
      if (!is.finite(arm_vertex_size) || arm_vertex_size <= 0) {
        arm_vertex_size <- 1
      }
      arm_label_size <- suppressWarnings(as.numeric(input$arm_label_size %||% 1))
      if (!is.finite(arm_label_size) || arm_label_size <= 0) {
        arm_label_size <- 1
      }
      preview_layout <- arm_preview_layout_inputs()
      preview_path_color <- normalize_palette_choice(
        preview_layout$path_color %||% "#f97316",
        c(
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Red" = "#dc2626",
          "Black" = "#111827"
        ),
        default = "#f97316"
      )
      preview_body_color <- normalize_palette_choice(
        preview_layout$body_color %||% "#eab308",
        c(
          "Gold" = "#eab308",
          "Red" = "#dc2626",
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Black" = "#111827"
        ),
        default = "#eab308"
      )
      preview_body_opacity <- suppressWarnings(as.numeric(preview_layout$body_opacity %||% 0.75))
      if (!is.finite(preview_body_opacity) || preview_body_opacity <= 0) {
        preview_body_opacity <- 0.75
      }
      preview_path_width <- suppressWarnings(as.numeric(preview_layout$path_width %||% 5))
      if (!is.finite(preview_path_width) || preview_path_width <= 0) {
        preview_path_width <- 5
      }
      preview_body_size <- suppressWarnings(as.numeric(preview_layout$body_size %||% 1.8))
      if (!is.finite(preview_body_size) || preview_body_size <= 0) {
        preview_body_size <- 1.8
      }
      center_marker_color <- normalize_palette_choice(
        preview_layout$center_marker_color %||% "#111827",
        c(
          "Black" = "#111827",
          "Red" = "#dc2626",
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6"
        ),
        default = "#111827"
      )
      center_marker_size <- suppressWarnings(as.numeric(preview_layout$center_marker_size %||% 1.7))
      if (!is.finite(center_marker_size) || center_marker_size <= 0) {
        center_marker_size <- 1.7
      }
      show_arm_labels <- isTRUE(input$arm_show_labels %||% TRUE)
      selected_arm_id <- as.character(arm_overlay$selected_id %||% "")
      virtual_markers <- if (is.list(arm_overlay$virtual_markers)) arm_overlay$virtual_markers else list()

      if (length(arm_list) > 0L) {
        for (aa in arm_list) {
          if (!is.list(aa)) {
            next
          }
          arm_id <- as.character(aa$arm_id %||% "")
          arm_vertices <- suppressWarnings(as.integer(aa$arm_vertices %||% integer(0)))
          arm_vertices <- arm_vertices[is.finite(arm_vertices) & arm_vertices >= 1L & arm_vertices <= nn]
          arm_vertices <- arm_vertices[arm_vertices %in% idx]
          path_vertices <- suppressWarnings(as.integer(aa$path_vertices %||% integer(0)))
          path_vertices <- path_vertices[is.finite(path_vertices) & path_vertices >= 1L & path_vertices <= nn]
          path_vertices <- path_vertices[path_vertices %in% idx]
          if (length(arm_vertices) < 1L && length(path_vertices) < 1L) {
            next
          }

          is_preview <- isTRUE(aa$is_preview)
          is_selected <- nzchar(selected_arm_id) && identical(selected_arm_id, arm_id)
          path_color_use <- if (is_preview) {
            preview_path_color
          } else if (is_selected) {
            "#dc2626"
          } else {
            arm_color
          }
          body_color_use <- if (is_preview) {
            preview_body_color
          } else if (is_selected) {
            "#dc2626"
          } else {
            arm_color
          }
          body_vertices <- if (is_preview && !identical(as.character(aa$thickening_method %||% "path_only"), "path_only")) {
            setdiff(arm_vertices, path_vertices)
          } else {
            arm_vertices
          }
          opacity_use <- if (is_preview) preview_body_opacity else arm_opacity
          path_width_use <- if (is_preview) preview_path_width else max(2, arm_path_width * if (is_selected) 1.25 else 1)
          body_size_use <- if (is_preview) preview_body_size else arm_vertex_size

          if (length(body_vertices) > 0L) {
            body_metric <- if (is_preview) {
              arm_preview_body_metric(aa, preview_layout$body_color_mode %||% "solid", body_vertices)
            } else {
              NULL
            }
            hover_text <- if (is.list(body_metric) && any(is.finite(body_metric$values))) {
              sprintf(
                "arm=%s<br>vertex=%d<br>%s=%.4f",
                as.character(aa$label %||% aa$family_label %||% "arm"),
                body_vertices,
                as.character(body_metric$label %||% body_metric$key %||% "metric"),
                suppressWarnings(as.numeric(body_metric$values))
              )
            } else {
              sprintf("arm=%s<br>vertex=%d", as.character(aa$label %||% aa$family_label %||% "arm"), body_vertices)
            }
            marker_spec <- if (is.list(body_metric) && any(is.finite(body_metric$values))) {
              list(
                size = max(2.5, point_size * 0.55 * body_size_use),
                color = suppressWarnings(as.numeric(body_metric$values)),
                colorscale = "Viridis",
                opacity = opacity_use,
                colorbar = list(title = as.character(body_metric$label %||% body_metric$key %||% "metric"))
              )
            } else {
              list(
                size = max(2.5, point_size * 0.55 * body_size_use),
                color = body_color_use,
                opacity = opacity_use
              )
            }
            p <- p %>%
              plotly::add_trace(
                type = "scatter3d",
                mode = "markers",
                x = coords[body_vertices, 1],
                y = coords[body_vertices, 2],
                z = coords[body_vertices, 3],
                key = body_vertices,
                customdata = body_vertices,
                text = hover_text,
                hoverinfo = "text",
                marker = marker_spec,
                showlegend = FALSE
              )
          }

          if (length(path_vertices) > 1L) {
            p <- p %>%
              plotly::add_trace(
                type = "scatter3d",
                mode = "lines",
                x = coords[path_vertices, 1],
                y = coords[path_vertices, 2],
                z = coords[path_vertices, 3],
                text = sprintf("arm path=%s", as.character(aa$label %||% aa$family_label %||% "arm")),
                hoverinfo = "text",
                line = list(
                  color = path_color_use,
                  width = path_width_use
                ),
                showlegend = FALSE
              )
          }

          if (isTRUE(show_arm_labels) && length(path_vertices) > 0L) {
            mid_idx <- path_vertices[[ceiling(length(path_vertices) / 2)]]
            p <- p %>%
              plotly::add_trace(
                type = "scatter3d",
                mode = "text",
                x = coords[mid_idx, 1],
                y = coords[mid_idx, 2],
                z = coords[mid_idx, 3],
                text = as.character(aa$label %||% aa$family_label %||% "arm"),
                hoverinfo = "skip",
                showlegend = FALSE,
                textfont = list(size = max(8, 11 * arm_label_size), color = path_color_use)
              )
          }
        }
      }

      if (length(virtual_markers) > 0L) {
        v_vertices <- vapply(virtual_markers, function(mm) suppressWarnings(as.integer(mm$vertex %||% NA_integer_)), integer(1))
        v_labels <- vapply(virtual_markers, function(mm) as.character(mm$label %||% "CENTER"), character(1))
        keep_virtual <- is.finite(v_vertices) & v_vertices >= 1L & v_vertices <= nn & v_vertices %in% idx
        if (any(keep_virtual)) {
          v_vertices <- as.integer(v_vertices[keep_virtual])
          v_labels <- v_labels[keep_virtual]
          p <- p %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers+text",
              x = coords[v_vertices, 1],
              y = coords[v_vertices, 2],
              z = coords[v_vertices, 3],
              key = v_vertices,
              customdata = v_vertices,
              text = v_labels,
              textposition = "top center",
              hoverinfo = "text",
              hovertext = sprintf("%s<br>vertex=%d", v_labels, v_vertices),
              marker = list(
                size = max(5.5, (point_size + 2.8) * center_marker_size),
                color = center_marker_color,
                line = list(color = "#ffffff", width = 1.5),
                symbol = "diamond"
              ),
              textfont = list(size = max(9, 11 * arm_label_size), color = center_marker_color),
              showlegend = FALSE
            )
        }
      }

      subject_rows <- if (is.data.frame(subject_overlay$rows)) subject_overlay$rows else empty_subject_sample_rows()
      if (nrow(subject_rows) > 0L) {
        subject_rows <- subject_rows[subject_rows$vertex %in% idx, , drop = FALSE]
      }
      if (nrow(subject_rows) > 0L) {
        edge_groups <- if (is.list(subject_overlay$edge_groups)) subject_overlay$edge_groups else list()
        if (length(edge_groups) > 0L) {
          for (gg in edge_groups) {
            edge_use <- if (is.list(gg) && is.matrix(gg$edges)) gg$edges else matrix(integer(0), ncol = 2L)
            if (nrow(edge_use) < 1L) {
              next
            }
            edge_use <- edge_use[edge_use[, 1] %in% idx & edge_use[, 2] %in% idx, , drop = FALSE]
            if (nrow(edge_use) < 1L) {
              next
            }
            edge_xyz <- matrix(NA_real_, nrow = nrow(edge_use) * 3L, ncol = 3L)
            edge_xyz[seq(1L, nrow(edge_xyz), by = 3L), ] <- coords[edge_use[, 1], , drop = FALSE]
            edge_xyz[seq(2L, nrow(edge_xyz), by = 3L), ] <- coords[edge_use[, 2], , drop = FALSE]
            p <- p %>%
              plotly::add_trace(
                type = "scatter3d",
                mode = "lines",
                x = edge_xyz[, 1],
                y = edge_xyz[, 2],
                z = edge_xyz[, 3],
                hoverinfo = "skip",
                line = list(
                  color = as.character(gg$color %||% subject_overlay$edge_color %||% "#dc2626"),
                  width = max(1, as.numeric(subject_overlay$edge_width %||% 2))
                ),
                showlegend = FALSE
              )
          }
        }
        subject_groups <- split(subject_rows, as.character(subject_rows$subject_id %||% ""))
        for (sid in names(subject_groups)) {
          grp <- subject_groups[[sid]]
          if (!is.data.frame(grp) || nrow(grp) < 1L) {
            next
          }
          grp_vertices <- suppressWarnings(as.integer(grp$vertex))
          grp_color <- as.character(grp$color[[1]] %||% subject_overlay$color %||% "#dc2626")
          p <- p %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers",
              x = coords[grp_vertices, 1],
              y = coords[grp_vertices, 2],
              z = coords[grp_vertices, 3],
              key = grp_vertices,
              customdata = grp_vertices,
              name = sprintf("Subject %s", sid),
              text = as.character(grp$hover_text %||% rep("", nrow(grp))),
              hoverinfo = "text",
              marker = list(
                size = max(5, (point_size + 2.5) * max(0.75, as.numeric(subject_overlay$size %||% 1.8))),
                color = grp_color,
                opacity = 0.95,
                line = list(color = "#ffffff", width = 1.2)
              ),
              showlegend = FALSE
            )
          keep_label <- nzchar(as.character(grp$label_text %||% rep("", nrow(grp))))
          if (any(keep_label)) {
            p <- p %>%
              plotly::add_trace(
                type = "scatter3d",
                mode = "text",
                x = coords[grp_vertices[keep_label], 1],
                y = coords[grp_vertices[keep_label], 2],
                z = coords[grp_vertices[keep_label], 3],
                text = as.character(grp$label_text[keep_label]),
                hoverinfo = "skip",
                showlegend = FALSE,
                textfont = list(
                  size = max(8, 10 * as.numeric(subject_overlay$label_size %||% 1.0)),
                  color = grp_color
                )
              )
          }
        }
      }

      p <- p %>%
        plotly::layout(
          margin = list(l = 0, r = 0, b = 0, t = 10),
          legend = if (identical(src$type, "categorical")) {
            list(
              orientation = "v",
              x = 1.01,
              y = 1,
              xanchor = "left",
              yanchor = "top",
              itemsizing = "constant",
              font = list(size = 13)
            )
          } else {
            list(orientation = "h")
          },
          scene = {
            sc <- list(
              uirevision = "reference-scene",
              xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
              yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
              zaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE)
            )
            saved_cam <- isolate(reference_plot_camera_state())
            if (is.list(saved_cam)) {
              sc$camera <- saved_cam
            }
            sc
          }
        )
      p <- plotly::event_register(p, "plotly_click")
      p <- attach_reference_plotly_camera_preserver(p)
      p
    })
  }

  if (requireNamespace("rgl", quietly = TRUE)) {
    shiny::observe({
      gen <- rgl_gen()
      rgl_output_id <- paste0("reference_rgl_", gen)
      prev_output_id <- shiny::isolate(rgl_last_output_id())
      if (is.character(prev_output_id) &&
          length(prev_output_id) == 1L &&
          nzchar(prev_output_id) &&
          !identical(prev_output_id, rgl_output_id)) {
        output[[prev_output_id]] <- NULL
      }
      output[[rgl_output_id]] <- rgl::renderRglwidget({
      rr <- reference_renderer_state()
      st <- rr$st
      req(is.null(st$error))

      color_mode <- as.character(rr$color_mode %||% "source")
      src_key <- as.character(rr$src_key %||% st$default_key)
      solid_color <- normalize_palette_choice(
        rr$solid_color %||% graph_solid_color_default,
        graph_vertex_color_choices(),
        default = graph_solid_color_default
      )
      if (identical(color_mode, "solid")) {
        src <- list(
          key = graph_solid_color_key,
          label = "Solid color",
          type = "solid",
          values = rep.int(NA_character_, nrow(st$coords))
        )
      } else {
        if (!(src_key %in% names(st$sources))) {
          src_key <- st$default_key
        }
        src <- st$sources[[src_key]]
      }
      coords <- st$coords
      nn <- nrow(coords)
      req(is.matrix(coords), nn > 0L)

      size_mult <- suppressWarnings(as.numeric(rr$size_mult %||% 1))
      if (!is.finite(size_mult) || size_mult <= 0) {
        size_mult <- 1
      }
      vertex_mode <- tolower(as.character(rr$vertex_mode %||% "sphere"))
      if (!vertex_mode %in% c("sphere", "point")) {
        vertex_mode <- "sphere"
      }

      keep_idx <- suppressWarnings(as.integer(rr$keep_idx %||% seq_len(nn)))
      keep_idx <- keep_idx[is.finite(keep_idx) & keep_idx >= 1L & keep_idx <= nn]
      if (length(keep_idx) < 1L) {
        keep_idx <- seq_len(nn)
      }
      keep_idx <- unique(keep_idx)

      coords_view <- coords[keep_idx, , drop = FALSE]
      values_view <- src$values[keep_idx]
      nn_view <- nrow(coords_view)
      req(nn_view > 0L)

      span <- apply(coords_view, 2, function(vv) diff(range(vv, na.rm = TRUE)))
      span[!is.finite(span)] <- 0
      radius_base <- max(1e-8, 0.01 * mean(span))
      sphere_radius <- max(1e-8, radius_base * size_mult)
      point_size <- max(1.2, 3 * size_mult)
      endpoint_label_size <- suppressWarnings(as.numeric(rr$endpoint_label_size %||% 1))
      if (!is.finite(endpoint_label_size) || endpoint_label_size <= 0) {
        endpoint_label_size <- 1
      }
      endpoint_label_offset <- suppressWarnings(as.numeric(rr$endpoint_label_offset %||% 1))
      if (!is.finite(endpoint_label_offset) || endpoint_label_offset < 0) {
        endpoint_label_offset <- 1
      }
      endpoint_marker_size <- suppressWarnings(as.numeric(rr$endpoint_marker_size %||% 1))
      if (!is.finite(endpoint_marker_size) || endpoint_marker_size <= 0) {
        endpoint_marker_size <- 1
      }
      endpoint_marker_color <- as.character(rr$endpoint_marker_color %||% "#ef4444")
      if (length(endpoint_marker_color) < 1L || !nzchar(endpoint_marker_color[[1]])) {
        endpoint_marker_color <- "#ef4444"
      } else {
        endpoint_marker_color <- endpoint_marker_color[[1]]
      }
      arm_color <- normalize_palette_choice(
        input$arm_color %||% "#2563eb",
        c(
          "Blue" = "#2563eb",
          "Orange" = "#f97316",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Black" = "#111827"
        ),
        default = "#2563eb"
      )
      arm_opacity <- suppressWarnings(as.numeric(input$arm_tube_opacity %||% 0.35))
      if (!is.finite(arm_opacity) || arm_opacity <= 0) {
        arm_opacity <- 0.35
      }
      arm_path_width <- suppressWarnings(as.numeric(input$arm_path_width %||% 4))
      if (!is.finite(arm_path_width) || arm_path_width <= 0) {
        arm_path_width <- 4
      }
      arm_vertex_size <- suppressWarnings(as.numeric(input$arm_vertex_size %||% 1))
      if (!is.finite(arm_vertex_size) || arm_vertex_size <= 0) {
        arm_vertex_size <- 1
      }
      arm_label_size <- suppressWarnings(as.numeric(input$arm_label_size %||% 1))
      if (!is.finite(arm_label_size) || arm_label_size <= 0) {
        arm_label_size <- 1
      }
      preview_layout <- arm_preview_layout_inputs()
      preview_path_color <- normalize_palette_choice(
        preview_layout$path_color %||% "#f97316",
        c(
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Red" = "#dc2626",
          "Black" = "#111827"
        ),
        default = "#f97316"
      )
      preview_body_color <- normalize_palette_choice(
        preview_layout$body_color %||% "#eab308",
        c(
          "Gold" = "#eab308",
          "Red" = "#dc2626",
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6",
          "Black" = "#111827"
        ),
        default = "#eab308"
      )
      preview_body_opacity <- suppressWarnings(as.numeric(preview_layout$body_opacity %||% 0.75))
      if (!is.finite(preview_body_opacity) || preview_body_opacity <= 0) {
        preview_body_opacity <- 0.75
      }
      preview_path_width <- suppressWarnings(as.numeric(preview_layout$path_width %||% 5))
      if (!is.finite(preview_path_width) || preview_path_width <= 0) {
        preview_path_width <- 5
      }
      preview_body_size <- suppressWarnings(as.numeric(preview_layout$body_size %||% 1.8))
      if (!is.finite(preview_body_size) || preview_body_size <= 0) {
        preview_body_size <- 1.8
      }
      center_marker_color <- normalize_palette_choice(
        preview_layout$center_marker_color %||% "#111827",
        c(
          "Black" = "#111827",
          "Red" = "#dc2626",
          "Orange" = "#f97316",
          "Blue" = "#2563eb",
          "Green" = "#16a34a",
          "Purple" = "#8b5cf6"
        ),
        default = "#111827"
      )
      center_marker_size <- suppressWarnings(as.numeric(preview_layout$center_marker_size %||% 1.7))
      if (!is.finite(center_marker_size) || center_marker_size <= 0) {
        center_marker_size <- 1.7
      }
      show_arm_labels <- isTRUE(input$arm_show_labels %||% TRUE)
      subject_overlay <- subject_overlay_active()
      dim_background_active <- isTRUE(subject_overlay$dim_background) &&
        length(subject_overlay$vertices %||% integer(0)) > 0L
      background_alpha_use <- suppressWarnings(as.numeric(subject_overlay$background_opacity %||% 0.22))
      if (!is.finite(background_alpha_use) || background_alpha_use <= 0) {
        background_alpha_use <- 0.22
      }
      background_alpha_use <- min(1, max(0.05, background_alpha_use))

      ep_overlay <- endpoint_overlay_active()
      ep_extra <- suppressWarnings(as.integer(ep_overlay$vertices %||% integer(0)))
      ep_extra <- ep_extra[is.finite(ep_extra) & ep_extra >= 1L & ep_extra <= nn]
      ep <- sort(unique(ep_extra))
      ep <- ep[ep %in% keep_idx]

      ep_label_lookup <- ep_overlay$labels %||% structure(character(0), names = character(0))
      ep_label_lookup <- as.character(ep_label_lookup)
      ep_label_names <- names(ep_overlay$labels %||% character(0))
      if (length(ep_label_names) == length(ep_label_lookup)) {
        names(ep_label_lookup) <- as.character(ep_label_names)
      } else {
        names(ep_label_lookup) <- character(length(ep_label_lookup))
      }

      ep_labels <- rep("", length(ep))
      if (length(ep) > 0L && length(ep_label_lookup) > 0L && !is.null(names(ep_label_lookup))) {
        mm <- match(as.character(ep), names(ep_label_lookup))
        ok <- is.finite(mm)
        ep_labels[ok] <- as.character(ep_label_lookup[mm[ok]])
        ep_labels[is.na(ep_labels)] <- ""
      }

      ep_view <- match(ep, keep_idx)
      valid_ep <- is.finite(ep_view) & ep_view >= 1L & ep_view <= nn_view
      ep_view <- ep_view[valid_ep]
      ep_view_labels <- ep_labels[valid_ep]

      endpoint_layers <- if (length(ep_view) > 0L) {
        list(list(
          fun = function(ctx, endpoint_idx, endpoint_labels, draw_mode, endpoint_radius, endpoint_size, endpoint_label_size, endpoint_label_offset, endpoint_marker_size, endpoint_marker_color) {
            idx <- suppressWarnings(as.integer(endpoint_idx))
            idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
            if (length(idx) < 1L) {
              return(invisible(NULL))
            }
            if (identical(draw_mode, "sphere")) {
              rgl::spheres3d(
                ctx$X[idx, , drop = FALSE],
                col = endpoint_marker_color,
                radius = max(1e-8, endpoint_radius * 1.35 * endpoint_marker_size)
              )
            } else {
              rgl::points3d(
                ctx$X[idx, , drop = FALSE],
                col = endpoint_marker_color,
                size = max(4.5, (endpoint_size + 2.2) * endpoint_marker_size)
              )
            }

            labs <- as.character(endpoint_labels %||% character(0))
            if (length(labs) == length(idx)) {
              labs[is.na(labs)] <- ""
              show_idx <- which(nzchar(labs))
              if (length(show_idx) > 0L) {
                xyz <- endpoint_label_positions(
                  coords = ctx$X,
                  endpoint_idx = idx[show_idx],
                  offset_mult = endpoint_label_offset
                )
                if (!is.matrix(xyz) || nrow(xyz) != length(show_idx)) {
                  xyz <- ctx$X[idx[show_idx], , drop = FALSE]
                }
                label_cex <- max(0.5, 1.5 * endpoint_label_size)
                rgl::texts3d(
                  x = xyz[, 1],
                  y = xyz[, 2],
                  z = xyz[, 3],
                  texts = as.character(labs[show_idx]),
                  cex = label_cex,
                  col = "#111827",
                  useFreeType = TRUE,
                  fixedSize = TRUE,
                  lit = FALSE
                )
              }
            }
            invisible(NULL)
          },
          args = list(
            endpoint_idx = ep_view,
            endpoint_labels = ep_view_labels,
            draw_mode = vertex_mode,
            endpoint_radius = sphere_radius,
            endpoint_size = point_size,
            endpoint_label_size = endpoint_label_size,
            endpoint_label_offset = endpoint_label_offset,
            endpoint_marker_size = endpoint_marker_size,
            endpoint_marker_color = endpoint_marker_color
          ),
          with_ctx = TRUE
        ))
      } else {
        NULL
      }

      basin_layers <- list()
      basin.display <- basin_result()
      basin.mode <- as.character(
        basin_display_settings$display_mode %||% "both"
      )
      if (identical(src_key, "basin_active") &&
          identical(basin.mode, "both") &&
          is.list(basin.display) &&
          inherits(basin.display$basin, "basin_complex") &&
          is.data.frame(basin.display$table)) {
        basin.specs <- gflowui_basin_layer_specs(
          basin_display = basin.display,
          visible_vertices = keep_idx,
          point_size = point_size,
          opacity = basin_display_settings$opacity %||% 0.85,
          unselected_color =
            basin_display_settings$unselected_color %||% "#D1D5DB",
          unselected_opacity =
            basin_display_settings$unselected_opacity %||% 0.28
        )
        basin.specs <- basin.specs[vapply(
          basin.specs,
          function(spec) identical(spec$kind, "minimum_halo"),
          logical(1)
        )]
        for (spec in basin.specs) {
          view.index <- match(spec$vertices, keep_idx)
          view.index <- view.index[
            is.finite(view.index) &
              view.index >= 1L &
              view.index <= nn_view
          ]
          if (length(view.index) < 1L) {
            next
          }
          basin_layers[[length(basin_layers) + 1L]] <- list(
            fun = function(ctx, layer_spec) {
              gflowui_draw_rgl_basin_layers(
                ctx$X,
                list(layer_spec)
              )
              invisible(NULL)
            },
            args = list(
              layer_spec = utils::modifyList(
                spec,
                list(vertices = view.index)
              )
            ),
            with_ctx = TRUE
          )
        }
      }

      extrema_layers <- list()
      basin_source <- basin_source_state()
      show_density_maxima <- isTRUE(basin_display_settings$show_maxima)
      show_density_minima <- isTRUE(basin_display_settings$show_minima)
      if (is.list(basin_source) &&
          length(basin_source$values %||% numeric(0)) == length(st$adj_list) &&
          (show_density_maxima || show_density_minima)) {
        density_extrema <- rbind(
          basin_extrema_scope_rows(
            basin_result(),
            "max",
            basin_display_settings$maxima_scope %||% "none"
          ),
          basin_extrema_scope_rows(
            basin_result(),
            "min",
            basin_display_settings$minima_scope %||% "none"
          )
        )
        density_extrema <- density_extrema[
          density_extrema$vertex %in% keep_idx,
          ,
          drop = FALSE
        ]
        extrema_specs <- list(
          maximum = list(
            show = show_density_maxima,
            labels = isTRUE(basin_display_settings$label_maxima),
            color = "#111827"
          ),
          minimum = list(
            show = show_density_minima,
            labels = isTRUE(basin_display_settings$label_minima),
            color = "#06B6D4"
          )
        )
        for (extrema_type in names(extrema_specs)) {
          spec <- extrema_specs[[extrema_type]]
          rows <- density_extrema[
            density_extrema$type == extrema_type,
            ,
            drop = FALSE
          ]
          if (!isTRUE(spec$show) || nrow(rows) < 1L) {
            next
          }
          view_idx <- match(rows$vertex, keep_idx)
          keep_view <- is.finite(view_idx) &
            view_idx >= 1L &
            view_idx <= nn_view
          view_idx <- view_idx[keep_view]
          labels <- rows$label[keep_view]
          if (length(view_idx) < 1L) {
            next
          }
          extrema_layers[[length(extrema_layers) + 1L]] <- list(
            fun = function(
                ctx,
                extrema_idx,
                extrema_labels,
                extrema_color,
                draw_mode,
                marker_radius,
                marker_size,
                show_labels) {
              idx <- suppressWarnings(as.integer(extrema_idx))
              idx <- idx[
                is.finite(idx) &
                  idx >= 1L &
                  idx <= nrow(ctx$X)
              ]
              if (length(idx) < 1L) {
                return(invisible(NULL))
              }
              if (identical(draw_mode, "sphere")) {
                rgl::spheres3d(
                  ctx$X[idx, , drop = FALSE],
                  col = extrema_color,
                  radius = max(1e-8, marker_radius * 1.55)
                )
              } else {
                rgl::points3d(
                  ctx$X[idx, , drop = FALSE],
                  col = extrema_color,
                  size = max(6, marker_size * 1.8)
                )
              }
              if (isTRUE(show_labels)) {
                labs <- as.character(extrema_labels %||% character(0))
                if (length(labs) == length(idx)) {
                  rgl::texts3d(
                    x = ctx$X[idx, 1],
                    y = ctx$X[idx, 2],
                    z = ctx$X[idx, 3],
                    texts = labs,
                    cex = 1.15,
                    col = extrema_color,
                    useFreeType = TRUE,
                    fixedSize = TRUE,
                    lit = FALSE
                  )
                }
              }
              invisible(NULL)
            },
            args = list(
              extrema_idx = view_idx,
              extrema_labels = labels,
              extrema_color = spec$color,
              draw_mode = vertex_mode,
              marker_radius = sphere_radius,
              marker_size = point_size,
              show_labels = isTRUE(spec$labels)
            ),
            with_ctx = TRUE
          )
        }
      }

      arm_overlay <- arm_overlay_active()
      arm_list <- if (is.list(arm_overlay$arms)) arm_overlay$arms else list()
      virtual_markers <- if (is.list(arm_overlay$virtual_markers)) arm_overlay$virtual_markers else list()
      selected_arm_id <- as.character(arm_overlay$selected_id %||% "")
      arm_layers <- list()
      if (length(arm_list) > 0L) {
        for (aa in arm_list) {
          if (!is.list(aa)) {
            next
          }
          arm_vertices <- suppressWarnings(as.integer(aa$arm_vertices %||% integer(0)))
          arm_vertices <- arm_vertices[is.finite(arm_vertices) & arm_vertices >= 1L & arm_vertices <= nn]
          path_vertices <- suppressWarnings(as.integer(aa$path_vertices %||% integer(0)))
          path_vertices <- path_vertices[is.finite(path_vertices) & path_vertices >= 1L & path_vertices <= nn]
          arm_view <- match(arm_vertices, keep_idx)
          arm_view <- arm_view[is.finite(arm_view) & arm_view >= 1L & arm_view <= nn_view]
          path_view <- match(path_vertices, keep_idx)
          path_view <- path_view[is.finite(path_view) & path_view >= 1L & path_view <= nn_view]
          if (length(arm_view) < 1L && length(path_view) < 1L) {
            next
          }
          is_preview <- isTRUE(aa$is_preview)
          is_selected <- nzchar(selected_arm_id) && identical(selected_arm_id, as.character(aa$arm_id %||% ""))
          path_color_use <- if (is_preview) {
            preview_path_color
          } else if (is_selected) {
            "#dc2626"
          } else {
            arm_color
          }
          body_color_use <- if (is_preview) {
            grDevices::adjustcolor(preview_body_color, alpha.f = preview_body_opacity)
          } else if (is_selected) {
            grDevices::adjustcolor("#dc2626", alpha.f = arm_opacity)
          } else {
            grDevices::adjustcolor(arm_color, alpha.f = arm_opacity)
          }
          body_view <- if (is_preview && !identical(as.character(aa$thickening_method %||% "path_only"), "path_only")) {
            setdiff(arm_view, path_view)
          } else {
            arm_view
          }
          body_vertices <- if (is_preview && !identical(as.character(aa$thickening_method %||% "path_only"), "path_only")) {
            setdiff(arm_vertices, path_vertices)
          } else {
            arm_vertices
          }
          body_metric <- if (is_preview) {
            arm_preview_body_metric(aa, preview_layout$body_color_mode %||% "solid", body_vertices)
          } else {
            NULL
          }
          body_cols_use <- if (is.list(body_metric) && any(is.finite(body_metric$values))) {
            numeric_arm_colors(body_metric$values, palette = "Viridis", alpha = if (is_preview) preview_body_opacity else arm_opacity)
          } else {
            body_color_use
          }
          arm_layers[[length(arm_layers) + 1L]] <- list(
            fun = function(ctx, arm_idx, path_idx, arm_label, path_color_use, body_cols_use, arm_vertex_size, arm_path_width, arm_label_size, show_arm_labels) {
              idx <- suppressWarnings(as.integer(arm_idx))
              idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
              pidx <- suppressWarnings(as.integer(path_idx))
              pidx <- pidx[is.finite(pidx) & pidx >= 1L & pidx <= nrow(ctx$X)]
              if (length(idx) > 0L) {
                rgl::points3d(
                  ctx$X[idx, , drop = FALSE],
                  col = body_cols_use,
                  size = max(3.5, 4.5 * arm_vertex_size)
                )
              }
              if (length(pidx) > 1L) {
                rgl::lines3d(
                  ctx$X[pidx, , drop = FALSE],
                  col = path_color_use,
                  lwd = max(2, arm_path_width)
                )
              }
              if (isTRUE(show_arm_labels) && length(pidx) > 0L) {
                mid <- pidx[[ceiling(length(pidx) / 2)]]
                xyz <- ctx$X[mid, , drop = FALSE]
                rgl::texts3d(
                  x = xyz[, 1],
                  y = xyz[, 2],
                  z = xyz[, 3],
                  texts = as.character(arm_label %||% "arm"),
                  cex = max(0.7, 1.2 * arm_label_size),
                  col = path_color_use,
                  useFreeType = TRUE,
                  fixedSize = TRUE,
                  lit = FALSE
                )
              }
              invisible(NULL)
            },
            args = list(
              arm_idx = body_view,
              path_idx = path_view,
              arm_label = as.character(aa$label %||% aa$family_label %||% "arm"),
              path_color_use = path_color_use,
              body_cols_use = body_cols_use,
              arm_vertex_size = if (is_preview) preview_body_size else arm_vertex_size,
              arm_path_width = if (is_preview) preview_path_width else arm_path_width,
              arm_label_size = arm_label_size,
              show_arm_labels = show_arm_labels
            ),
            with_ctx = TRUE
          )
        }
      }
      if (length(virtual_markers) > 0L) {
        v_vertices <- vapply(virtual_markers, function(mm) suppressWarnings(as.integer(mm$vertex %||% NA_integer_)), integer(1))
        v_labels <- vapply(virtual_markers, function(mm) as.character(mm$label %||% "CENTER"), character(1))
        keep_virtual <- is.finite(v_vertices) & v_vertices >= 1L & v_vertices <= nn
        if (any(keep_virtual)) {
          v_view <- match(as.integer(v_vertices[keep_virtual]), keep_idx)
          keep_view <- is.finite(v_view) & v_view >= 1L & v_view <= nn_view
          v_view <- v_view[keep_view]
          v_labels <- v_labels[keep_virtual][keep_view]
          if (length(v_view) > 0L) {
            arm_layers[[length(arm_layers) + 1L]] <- list(
              fun = function(ctx, center_idx, center_labels, center_marker_color, center_marker_size, arm_label_size) {
                idx <- suppressWarnings(as.integer(center_idx))
                idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
                if (length(idx) < 1L) {
                  return(invisible(NULL))
                }
                rgl::points3d(
                  ctx$X[idx, , drop = FALSE],
                  col = center_marker_color,
                  size = max(4.5, 5.5 * center_marker_size)
                )
                rgl::texts3d(
                  x = ctx$X[idx, 1],
                  y = ctx$X[idx, 2],
                  z = ctx$X[idx, 3],
                  texts = as.character(center_labels %||% rep("CENTER", length(idx))),
                  cex = max(0.7, 1.15 * arm_label_size),
                  col = center_marker_color,
                  useFreeType = TRUE,
                  fixedSize = TRUE,
                  lit = FALSE
                )
                invisible(NULL)
              },
              args = list(
                center_idx = v_view,
                center_labels = v_labels,
                center_marker_color = center_marker_color,
                center_marker_size = center_marker_size,
                arm_label_size = arm_label_size
              ),
              with_ctx = TRUE
            )
          }
        }
      }
      subject_layers <- list()
      subject_rows <- if (is.data.frame(subject_overlay$rows)) subject_overlay$rows else empty_subject_sample_rows()
      subject_rows <- subject_rows[subject_rows$vertex %in% keep_idx, , drop = FALSE]
      if (nrow(subject_rows) > 0L) {
        edge_groups <- if (is.list(subject_overlay$edge_groups)) subject_overlay$edge_groups else list()
        if (length(edge_groups) > 0L) {
          for (gg in edge_groups) {
            edge_view <- if (is.list(gg) && is.matrix(gg$edges)) gg$edges else matrix(integer(0), ncol = 2L)
            if (nrow(edge_view) < 1L) {
              next
            }
            edge_view <- edge_view[edge_view[, 1] %in% keep_idx & edge_view[, 2] %in% keep_idx, , drop = FALSE]
            if (nrow(edge_view) < 1L) {
              next
            }
            from_view <- match(edge_view[, 1], keep_idx)
            to_view <- match(edge_view[, 2], keep_idx)
            keep_pair <- is.finite(from_view) & is.finite(to_view) &
              from_view >= 1L & from_view <= nn_view &
              to_view >= 1L & to_view <= nn_view
            from_view <- from_view[keep_pair]
            to_view <- to_view[keep_pair]
            if (length(from_view) > 0L) {
              subject_layers[[length(subject_layers) + 1L]] <- list(
                fun = function(ctx, from_idx, to_idx, edge_color, edge_width) {
                  ff <- suppressWarnings(as.integer(from_idx))
                  tt <- suppressWarnings(as.integer(to_idx))
                  keep <- is.finite(ff) & is.finite(tt) &
                    ff >= 1L & ff <= nrow(ctx$X) &
                    tt >= 1L & tt <= nrow(ctx$X)
                  ff <- ff[keep]
                  tt <- tt[keep]
                  if (length(ff) < 1L) {
                    return(invisible(NULL))
                  }
                  xyz <- matrix(NA_real_, nrow = length(ff) * 2L, ncol = 3L)
                  xyz[seq(1L, nrow(xyz), by = 2L), ] <- ctx$X[ff, , drop = FALSE]
                  xyz[seq(2L, nrow(xyz), by = 2L), ] <- ctx$X[tt, , drop = FALSE]
                  rgl::segments3d(
                    x = xyz[, 1],
                    y = xyz[, 2],
                    z = xyz[, 3],
                    col = as.character(edge_color %||% "#dc2626"),
                    lwd = max(1, as.numeric(edge_width %||% 2))
                  )
                  invisible(NULL)
                },
                args = list(
                  from_idx = from_view,
                  to_idx = to_view,
                  edge_color = as.character(gg$color %||% subject_overlay$edge_color %||% "#dc2626"),
                  edge_width = as.numeric(subject_overlay$edge_width %||% 2)
                ),
                with_ctx = TRUE
              )
            }
          }
        }
        subject_groups <- split(subject_rows, as.character(subject_rows$subject_id %||% ""))
        for (sid in names(subject_groups)) {
          grp <- subject_groups[[sid]]
          if (!is.data.frame(grp) || nrow(grp) < 1L) {
            next
          }
          subject_view <- match(suppressWarnings(as.integer(grp$vertex)), keep_idx)
          subject_view <- subject_view[is.finite(subject_view) & subject_view >= 1L & subject_view <= nn_view]
          if (length(subject_view) < 1L) {
            next
          }
          grp_color <- as.character(grp$color[[1]] %||% subject_overlay$color %||% "#dc2626")
          subject_layers[[length(subject_layers) + 1L]] <- list(
            fun = function(ctx, subject_idx, subject_color, subject_size) {
              idx <- suppressWarnings(as.integer(subject_idx))
              idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
              if (length(idx) < 1L) {
                return(invisible(NULL))
              }
              rgl::points3d(
                ctx$X[idx, , drop = FALSE],
                col = as.character(subject_color %||% "#dc2626"),
                size = max(5, 6 * max(0.75, as.numeric(subject_size %||% 1.8)))
              )
              invisible(NULL)
            },
            args = list(
              subject_idx = subject_view,
              subject_color = grp_color,
              subject_size = as.numeric(subject_overlay$size %||% 1.8)
            ),
            with_ctx = TRUE
          )
          keep_label <- nzchar(as.character(grp$label_text %||% rep("", nrow(grp))))
          if (any(keep_label)) {
            subject_layers[[length(subject_layers) + 1L]] <- list(
              fun = function(ctx, subject_idx, label_text, label_color, label_size) {
                idx <- suppressWarnings(as.integer(subject_idx))
                idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
                labs <- as.character(label_text %||% character(0))
                if (length(idx) < 1L || length(labs) != length(idx)) {
                  return(invisible(NULL))
                }
                rgl::texts3d(
                  x = ctx$X[idx, 1],
                  y = ctx$X[idx, 2],
                  z = ctx$X[idx, 3],
                  texts = labs,
                  cex = max(0.6, 1.1 * as.numeric(label_size %||% 1.0)),
                  col = as.character(label_color %||% "#dc2626"),
                  useFreeType = TRUE,
                  fixedSize = TRUE,
                  lit = FALSE
                )
                invisible(NULL)
              },
              args = list(
                subject_idx = subject_view[keep_label],
                label_text = as.character(grp$label_text[keep_label]),
                label_color = grp_color,
                label_size = as.numeric(subject_overlay$label_size %||% 1.0)
              ),
              with_ctx = TRUE
            )
          }
        }
      }
      post_layers <- c(
        basin_layers,
        extrema_layers,
        endpoint_layers,
        arm_layers,
        subject_layers
      )

      make_plain_widget <- function(base_color = "gray70") {
        base_color_use <- if (isTRUE(dim_background_active)) {
          grDevices::adjustcolor(base_color, alpha.f = background_alpha_use)
        } else {
          base_color
        }
        plot_fn <- resolve_gflow_plot3d_fn("plot3D.plain")
        plot_fn(
          X = coords_view,
          radius = if (identical(vertex_mode, "sphere")) sphere_radius else NULL,
          size = point_size,
          col = base_color_use,
          widget.width = 1700L,
          widget.height = 1000L,
          background.color = "white",
          post.layers = post_layers
        )
      }

      if (identical(color_mode, "solid")) {
        make_plain_widget(solid_color)
      } else if (identical(src$type, "categorical")) {
        pal_info <- categorical_palette(
          values_view,
          source_key = src_key,
          source_label = src$label %||% src_key
        )
        vv <- pal_info$values
        cltr_col_tbl <- pal_info$colors
        if (isTRUE(dim_background_active)) {
          cltr_col_tbl <- grDevices::adjustcolor(cltr_col_tbl, alpha.f = background_alpha_use)
        }
        tryCatch(
          resolve_gflow_plot3d_fn("plot3D.cltrs")(
            X = coords_view,
            cltr = vv,
            cltr.col.tbl = cltr_col_tbl,
            show.cltr.labels = FALSE,
            show.legend = FALSE,
            legend.title = as.character(src$label %||% src_key),
            radius = if (identical(vertex_mode, "sphere")) sphere_radius else NA_real_,
            widget.width = 1700L,
            widget.height = 1000L,
            background.color = "white",
            post.layers = post_layers
          ),
          error = function(e) make_plain_widget()
        )
      } else {
        vv <- suppressWarnings(as.numeric(values_view))
        if (all(!is.finite(vv))) {
          make_plain_widget()
        } else {
          color_encoding <- gflowui_numeric_color_encoding(
            values = vv,
            transform = src$color_transform %||% "identity",
            title = src$colorbar_title %||% src$label
          )
          if (identical(as.character(src$color_transform %||% "identity"), "density_log10")) {
            density_colors <- numeric_arm_colors(
              color_encoding$mapped_values,
              palette = "Viridis",
              alpha = if (isTRUE(dim_background_active)) background_alpha_use else 1,
              color_limits = color_encoding$color_limits,
              palette_colors = gflowui_density_palette(
                low = density_display_settings$low %||% "yellow",
                midpoint = density_display_settings$midpoint %||% "none",
                high = density_display_settings$high %||% "red",
                low_alpha = density_display_settings$low_alpha %||% 0.2,
                midpoint_alpha = density_display_settings$midpoint_alpha %||% 1,
                high_alpha = density_display_settings$high_alpha %||% 1
              )
            )
            make_plain_widget(density_colors)
          } else {
            cont_palette <- if (isTRUE(dim_background_active)) {
              function(x) grDevices::adjustcolor(grDevices::hcl.colors(length(x), "Viridis"), alpha.f = background_alpha_use)
            } else {
              NULL
            }
            tryCatch(
              resolve_gflow_plot3d_fn("plot3D.cont")(
                X = coords_view,
                y = color_encoding$mapped_values,
                subset = rep(TRUE, nn_view),
                non.highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
                highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
                point.size = point_size,
                radius = if (identical(vertex_mode, "sphere")) sphere_radius else NULL,
                color.palette = cont_palette,
                palette.type = "value",
                legend.title = as.character(src$label %||% src_key),
                legend.show = FALSE,
                widget.width = 1700L,
                widget.height = 1000L,
                background.color = "white",
                post.layers = post_layers
              ),
              error = function(e) make_plain_widget()
            )
          }
        }
      }
    })
      rgl_last_output_id(rgl_output_id)
    })
  }

  graph_structure_state <- shiny::reactive({
    sel <- current_graph_selection()
    if (!is.list(sel) || !is.null(sel$error)) {
      return(list(error = as.character(sel$error %||% "No graph sets are available.")))
    }

    manifest <- sel$manifest
    graph_sets <- sel$graph_sets
    choices <- sel$data_type_choices
    set_id <- scalar_chr(sel$set_id %||% "", default = "")
    k_choices <- sel$k_choices
    k_sel <- scalar_int(sel$k_selected, default = NA_integer_)

    spec <- resolve_reference_spec(
      manifest,
      preferred_set_id = set_id,
      preferred_k = k_sel
    )
    if (is.null(spec)) {
      return(list(error = "Unable to resolve selected graph set."))
    }

    optimal <- discover_optimal_k_methods(manifest, spec)
    optimal_choices <- optimal$choices
    optimal_selected <- as.character(input$graph_optimal_method %||% "")
    if (!(optimal_selected %in% unname(optimal_choices))) {
      optimal_selected <- as.character(optimal$default %||% "")
      if (!(optimal_selected %in% unname(optimal_choices)) && length(optimal_choices) > 0L) {
        optimal_selected <- unname(optimal_choices)[1]
      }
    }

    st <- reference_view_state()
    st_use <- NULL
    if (
      is.list(st) &&
      is.null(st$error) &&
      identical(as.character(st$set_id), as.character(set_id)) &&
      identical(
        scalar_int(st$k_actual, default = NA_integer_),
        scalar_int(k_sel, default = NA_integer_)
      )
    ) {
      st_use <- st
    }
    dat <- data_state()
    gs <- graph_set_by_id(graph_sets, set_id)
    layout_presets <- if (is.list(gs$layout_assets$presets)) gs$layout_assets$presets else list()
    n_samples <- infer_sample_count(gs, st = st_use)
    n_features <- infer_feature_count(gs)

    if (!is.finite(n_samples) || !is.finite(n_features)) {
      dims_meta <- infer_graph_dims_from_project_metadata(
        project_root = manifest$project_root %||% "",
        set_id = set_id,
        graph_set = gs
      )
      if (!is.finite(n_samples) && is.finite(suppressWarnings(as.integer(dims_meta$n_samples)))) {
        n_samples <- suppressWarnings(as.integer(dims_meta$n_samples))
      }
      if (!is.finite(n_features) && is.finite(suppressWarnings(as.integer(dims_meta$n_features)))) {
        n_features <- suppressWarnings(as.integer(dims_meta$n_features))
      }
    }

    if (!is.finite(n_samples) && !is.null(dat$data)) {
      n_samples <- as.integer(nrow(dat$data))
    }
    if (!is.finite(n_features) && !is.null(dat$data)) {
      sample_hint <- n_samples
      if (!is.finite(sample_hint) && is.list(st_use) && is.finite(suppressWarnings(as.integer(st_use$n_vertices)))) {
        sample_hint <- suppressWarnings(as.integer(st_use$n_vertices))
      }

      if (!is.finite(sample_hint) || identical(nrow(dat$data), as.integer(sample_hint))) {
        numeric_cols <- sum(vapply(dat$data, is.numeric, logical(1)))
        if (is.finite(numeric_cols) && numeric_cols > 0L) {
          n_features <- as.integer(numeric_cols)
        } else {
          n_features <- as.integer(ncol(dat$data))
        }
      }
    }

    dims_text <- sprintf(
      "(%s x %s)",
      if (is.finite(n_samples)) format(as.integer(n_samples), big.mark = ",") else "?",
      if (is.finite(n_features)) format(as.integer(n_features), big.mark = ",") else "?"
    )

    solid_vertex_color_choices <- graph_vertex_color_choices()
    color_choices <- c("Solid color..." = graph_solid_color_key, "Vertex Degree" = "vertex_degree")
    color_selected <- as.character(
      input$graph_layout_color_by %||%
        graph_layout_state$color_by %||%
        layout_presets$color_by %||%
        "vertex_degree"
    )
    if (is.list(st_use) && length(st_use$choices %||% c()) > 0L) {
      color_choices <- c("Solid color..." = graph_solid_color_key, st_use$choices)
      color_selected <- as.character(
        input$graph_layout_color_by %||%
          layout_presets$color_by %||%
          st_use$default_key %||%
          ""
      )
      if (!(color_selected %in% unname(color_choices))) {
        color_selected <- unname(color_choices)[1]
      }
    } else if (!(color_selected %in% unname(color_choices))) {
      color_selected <- "vertex_degree"
    }
    vertex_color_selected <- normalize_palette_choice(
      input$graph_layout_vertex_color %||%
        graph_layout_state$vertex_color %||%
        layout_presets$vertex_color %||%
        graph_solid_color_default,
      solid_vertex_color_choices,
      default = graph_solid_color_default
    )

    renderer_selected <- normalize_live_renderer_choice(
      input$graph_layout_renderer %||%
        graph_layout_state$renderer %||%
        layout_presets$renderer %||%
        "plotly",
      default = "plotly"
    )
    vertex_layout_default <- default_vertex_layout_for_graph(
      preset = layout_presets$vertex_layout %||% "point",
      n_vertices = n_samples
    )
    vertex_layout <- tolower(as.character(
      input$graph_layout_vertex %||%
        graph_layout_state$vertex_layout %||%
        vertex_layout_default
    ))
    if (!vertex_layout %in% c("sphere", "point")) {
      vertex_layout <- vertex_layout_default
    }
    size_selected <- normalize_scale_label(
      input$graph_layout_size %||%
        graph_layout_state$size_label %||%
        layout_presets$vertex_size %||%
        "1.0x",
      default = "1.0x"
    )
    component_choices <- c("All vertices" = "all", "Main connected component" = "lcc")
    component_selected <- tolower(as.character(
      input$graph_layout_component %||%
        graph_layout_state$component %||%
        layout_presets$component %||%
        "all"
    ))
    if (!(component_selected %in% unname(component_choices))) {
      component_selected <- "all"
    }
    component_hint <- ""
    if (is.list(st_use$components)) {
      nn <- suppressWarnings(as.integer(st_use$components$n_vertices))
      nlcc <- suppressWarnings(as.integer(st_use$components$lcc_size))
      nc <- suppressWarnings(as.integer(st_use$components$n_components))
      if (is.finite(nn) && is.finite(nlcc) && is.finite(nc) && nc > 1L) {
        component_hint <- sprintf(
          "Connected components: %s (LCC %s/%s vertices)",
          format(nc, big.mark = ","),
          format(nlcc, big.mark = ","),
          format(nn, big.mark = ",")
        )
      }
    }

    graph_metadata_tbl <- data.frame(Metric = character(0), Value = character(0), stringsAsFactors = FALSE)
    add_graph_meta <- function(metric, value) {
      val <- as.character(value %||% "")
      val <- val[!is.na(val)]
      if (length(val) < 1L || !nzchar(val[[1L]])) {
        return(invisible(NULL))
      }
      graph_metadata_tbl[nrow(graph_metadata_tbl) + 1L, ] <<- list(
        Metric = as.character(metric),
        Value = val[[1L]]
      )
      invisible(NULL)
    }

    add_graph_meta("Screen", gs$screen_label %||% gs$screen_name %||% "")
    add_graph_meta("Representation", gs$representation_label %||% gs$representation %||% "")

    k_top20 <- suppressWarnings(as.integer(gs$selected_k %||% k_sel))
    if (is.finite(k_top20)) {
      add_graph_meta("Selected k (top20)", format(k_top20, big.mark = ","))
    }

    k_top30 <- suppressWarnings(as.integer(gs$selected_k_top30 %||% NA_integer_))
    if (is.finite(k_top30)) {
      add_graph_meta("Selected k (top30)", format(k_top30, big.mark = ","))
    }

    stable_top20_top30 <- gs$selected_k_stable_top20_top30
    stable_txt <- ""
    if (!is.null(stable_top20_top30) && length(stable_top20_top30) > 0L) {
      stable_txt <- if (isTRUE(as.logical(stable_top20_top30) || identical(tolower(as.character(stable_top20_top30)), "true"))) {
        "yes"
      } else if (identical(tolower(as.character(stable_top20_top30)), "false")) {
        "no"
      } else {
        ""
      }
    }
    if (nzchar(stable_txt)) {
      add_graph_meta("Top20/top30 stable", stable_txt)
    }

    if (is.finite(n_samples)) {
      add_graph_meta("Graph samples", format(as.integer(n_samples), big.mark = ","))
    }
    if (is.finite(n_features)) {
      add_graph_meta("Graph features", format(as.integer(n_features), big.mark = ","))
    }

    edge_count <- suppressWarnings(as.integer(gs$graph_edge_count %||% NA_integer_))
    if (is.finite(edge_count)) {
      add_graph_meta("Graph edges", format(edge_count, big.mark = ","))
    }

    comp_count <- suppressWarnings(as.integer(gs$graph_component_count %||% NA_integer_))
    if (!is.finite(comp_count) && is.list(st_use$components)) {
      comp_count <- suppressWarnings(as.integer(st_use$components$n_components %||% NA_integer_))
    }
    if (is.finite(comp_count)) {
      add_graph_meta("Graph components", format(comp_count, big.mark = ","))
    }

    list(
      error = NULL,
      manifest = manifest,
      set_id = set_id,
      data_type_choices = choices,
      data_type_label = infer_data_type_label(gs),
      grouped_selector_enabled = isTRUE(sel$grouped_selector_enabled),
      selector_fields = sel$selector_fields %||% list(),
      selector_summary_label = as.character(sel$selector_summary_label %||% "Graph family"),
      selector_summary_value = as.character(sel$selector_summary_value %||% gs$label %||% infer_data_type_label(gs)),
      dims_text = dims_text,
      k_choices = k_choices,
      k_selected = k_sel,
      optimal_choices = optimal_choices,
      optimal_selected = optimal_selected,
      optimal_methods = optimal$methods,
      renderer_selected = renderer_selected,
      vertex_layout = vertex_layout,
      size_selected = size_selected,
      component_choices = component_choices,
      component_selected = component_selected,
      component_hint = component_hint,
      metadata_tbl = graph_metadata_tbl,
      color_choices = color_choices,
      color_selected = color_selected,
      vertex_color_choices = solid_vertex_color_choices,
      vertex_color_selected = vertex_color_selected
    )
  })

  project_overview_state <- shiny::reactive({
    manifest <- active_manifest()
    if (!is.list(manifest) || !is.list(manifest$metadata) || !is.list(manifest$metadata$overview)) {
      return(NULL)
    }

    overview <- manifest$metadata$overview
    summary_tbl <- if (is.data.frame(overview$summary_table)) {
      overview$summary_table
    } else {
      data.frame()
    }

    artifact_paths_raw <- overview$artifact_paths
    if (!is.list(artifact_paths_raw)) {
      artifact_paths_raw <- list()
    }
    artifact_paths <- list()
    for (nm in names(artifact_paths_raw)) {
      pp <- as.character(artifact_paths_raw[[nm]] %||% "")
      pp <- pp[!is.na(pp) & nzchar(pp)]
      if (length(pp) < 1L) {
        next
      }
      artifact_paths[[as.character(nm)]] <- pp[[1L]]
    }

    labels_raw <- overview$artifact_labels
    if (is.list(labels_raw)) {
      labels_raw <- unlist(labels_raw, recursive = TRUE, use.names = TRUE)
    }
    artifact_labels <- character(0)
    if (is.character(labels_raw) && length(labels_raw) > 0L && !is.null(names(labels_raw))) {
      keep <- nzchar(names(labels_raw)) & nzchar(as.character(labels_raw))
      artifact_labels <- as.character(labels_raw[keep])
      names(artifact_labels) <- as.character(names(labels_raw)[keep])
    }

    ids <- names(artifact_paths)
    ids <- ids[nzchar(ids)]
    artifact_choices <- character(0)
    if (length(ids) > 0L) {
      choice_labels <- vapply(ids, function(id) {
        as.character(artifact_labels[[id]] %||% id)
      }, character(1))
      artifact_choices <- stats::setNames(ids, choice_labels)
    }

    default_artifact_id <- if ("report_pdf" %in% ids) {
      "report_pdf"
    } else if (length(ids) > 0L) {
      ids[[1L]]
    } else {
      ""
    }

    list(
      summary_tbl = summary_tbl,
      artifact_paths = artifact_paths,
      artifact_choices = artifact_choices,
      default_artifact_id = default_artifact_id
    )
  })

  shiny::observeEvent(input$set_reference_graph_inline, {
    gs <- graph_structure_state()
    if (!is.null(gs$error)) {
      shiny::showNotification(gs$error, type = "error")
      return()
    }

    set_id <- as.character(gs$set_id %||% "")
    ref_k <- suppressWarnings(as.integer(gs$k_selected))
    if (!nzchar(set_id) || !is.finite(ref_k)) {
      shiny::showNotification("Select a valid data type and k value.", type = "error")
      return()
    }

    ctx <- active_project_context()
    if (is.null(ctx)) {
      shiny::showNotification("Active project context not available.", type = "error")
      return()
    }

    payload <- load_or_init_active_manifest(ctx)
    defaults <- payload$manifest$defaults
    defaults$reference_graph_set_id <- set_id
    defaults$reference_k <- as.integer(ref_k)
    defaults$graph_set_id <- set_id

    sel_method <- as.character(input$graph_optimal_method %||% gs$optimal_selected %||% "")
    lbl_idx <- match(sel_method, unname(gs$optimal_choices))
    reason <- if (length(lbl_idx) > 0L && is.finite(lbl_idx[[1]])) names(gs$optimal_choices)[lbl_idx[[1]]] else ""
    defaults$reference_reason <- if (nzchar(reason)) reason else NA_character_

    payload$manifest$defaults <- defaults

    ok <- tryCatch(save_active_manifest(payload), error = function(e) e)
    if (inherits(ok, "error")) {
      shiny::showNotification(
        sprintf("Failed to save reference graph: %s", conditionMessage(ok)),
        type = "error"
      )
      set_run_monitor_note(sprintf("Reference graph update failed: %s", conditionMessage(ok)))
      return()
    }

    note <- sprintf("Reference graph set to %s @ k=%d.", set_id, as.integer(ref_k))
    set_run_monitor_note(note)
    shiny::showNotification(note, type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$graph_optimal_show, {
    gs <- graph_structure_state()
    if (!is.null(gs$error)) {
      shiny::showNotification(gs$error, type = "error")
      return()
    }

    method_id <- as.character(input$graph_optimal_method %||% gs$optimal_selected %||% "")
    method <- gs$optimal_methods[[method_id]]
    if (!is.list(method)) {
      shiny::showNotification("No artifact is available for the selected criterion.", type = "error")
      return()
    }

    set_tokens <- graph_alias_tokens(gs$set_id, gs$data_type_label)
    cache_dir <- file.path(
      gflowui_projects_data_dir(),
      "cache",
      "optimal_k",
      rv$project.id %||% "project",
      sanitize_token_id(gs$set_id %||% "set", fallback = "set")
    )
    target <- resolve_optimal_k_display_path(
      method$path,
      set_tokens = set_tokens,
      cache_dir = cache_dir,
      method_id = method_id
    )
    if (!nzchar(target)) {
      shiny::showNotification("No plot/report file could be located for the selected criterion.", type = "error")
      return()
    }

    opened <- tryCatch(open_external_path(target), error = function(e) FALSE)
    if (!isTRUE(opened)) {
      shiny::showNotification("Unable to open the selected criterion file.", type = "error")
      return()
    }
    set_run_monitor_note(sprintf("Opened optimal-k artifact: %s", basename(target)))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_overview_open_artifact, {
    overview <- project_overview_state()
    if (!is.list(overview)) {
      shiny::showNotification("No overview artifacts are available for this project.", type = "error")
      return()
    }

    artifact_id <- as.character(input$project_overview_artifact %||% overview$default_artifact_id %||% "")
    if (!nzchar(artifact_id)) {
      shiny::showNotification("No overview artifact is available to open.", type = "error")
      return()
    }

    path <- as.character(overview$artifact_paths[[artifact_id]] %||% "")
    if (!nzchar(path)) {
      shiny::showNotification("The selected overview artifact is missing.", type = "error")
      return()
    }

    opened <- tryCatch(open_external_path(path), error = function(e) FALSE)
    if (!isTRUE(opened)) {
      shiny::showNotification("Unable to open the selected overview artifact.", type = "error")
      return()
    }
    set_run_monitor_note(sprintf("Opened project artifact: %s", basename(path)))
  }, ignoreInit = TRUE)

  output$project_controls <- shiny::renderUI({
    if (isTRUE(rv$project.active)) {
      return(NULL)
    }

    reg <- project_registry()
    choices <- c("Choose a project..." = "")
    if (nrow(reg) > 0L) {
      choices <- c(choices, stats::setNames(reg$id, reg$label))
    }

    shiny::div(
      class = "gf-sidebar-panel",
      shiny::h5("Projects"),
      shiny::selectInput(
        "project_select",
        label = NULL,
        choices = choices,
        selected = ""
      ),
      shiny::actionButton(
        "project_new",
        "New",
        class = "btn-secondary gf-btn-wide"
      )
    )
  })

  build_endpoint_metrics_table <- function(metrics_tbl) {
    if (!is.data.frame(metrics_tbl) || nrow(metrics_tbl) < 1L) {
      return(shiny::p(class = "gf-hint", "No endpoint metrics found for the selected vertex in the current candidate datasets."))
    }
    metrics_chr <- metrics_tbl
    for (cc in names(metrics_chr)) {
      metrics_chr[[cc]] <- format_endpoint_metric_value(metrics_chr[[cc]])
    }
    head_row <- shiny::tags$tr(lapply(names(metrics_chr), function(cc) shiny::tags$th(cc)))
    body_rows <- lapply(seq_len(nrow(metrics_chr)), function(ii) {
      shiny::tags$tr(lapply(metrics_chr[ii, , drop = FALSE], function(val) shiny::tags$td(as.character(val[[1]] %||% ""))))
    })
    shiny::div(
      class = "table-responsive gf-endpoint-table-scroll",
      shiny::tags$table(
        class = "table table-sm gf-asset-table",
        shiny::tags$thead(head_row),
        shiny::tags$tbody(body_rows)
      )
    )
  }

  endpoint_metrics_panel_state <- function(panel_state) {
    rows_df <- if (is.list(panel_state) && is.data.frame(panel_state$rows)) panel_state$rows else data.frame()
    selected_vid <- selected_endpoint_vertex()
    rows_for_metrics <- rows_df
    if (is.data.frame(rows_df) && nrow(rows_df) > 0L && "selected" %in% names(rows_df)) {
      metric_keep <- as.logical(rows_df$selected)
      metric_keep[is.na(metric_keep)] <- FALSE
      if ("is_working_source" %in% names(rows_df)) {
        metric_keep <- metric_keep | as.logical(rows_df$is_working_source)
        metric_keep[is.na(metric_keep)] <- FALSE
      }
      if (any(metric_keep)) {
        rows_for_metrics <- rows_df[metric_keep, , drop = FALSE]
      }
    }
    metrics_df <- endpoint_metrics_for_vertex(selected_vid, rows_for_metrics)
    if ((!is.data.frame(metrics_df) || nrow(metrics_df) < 1L) && is.data.frame(rows_df) && nrow(rows_df) > 0L) {
      metrics_df <- endpoint_metrics_for_vertex(selected_vid, rows_df)
    }
    metrics_df
  }

  build_endpoint_candidate_metrics_ui <- function(panel_state) {
    metrics_df <- endpoint_metrics_panel_state(panel_state)
    shiny::tags$details(
      class = "gf-endpoint-metrics-details",
      shiny::tags$summary("Candidate Metrics"),
      build_endpoint_metrics_table(metrics_df)
    )
  }

  build_endpoint_vertex_inspector_ui <- function(panel_state) {
    rows_df <- if (is.list(panel_state) && is.data.frame(panel_state$rows)) panel_state$rows else data.frame()
    working_state <- if (is.list(panel_state) && is.list(panel_state$working)) panel_state$working else empty_working_endpoint_state()
    working_rows <- if (is.data.frame(working_state$rows)) working_state$rows else empty_working_endpoint_rows()
    selected_vid <- selected_endpoint_vertex()
    selected_source <- as.character(endpoint_vertex_state$source %||% "")
    rr <- reference_renderer_state()
    renderer_name <- toupper(as.character(rr$effective %||% rr$requested %||% ""))
    label_suggestion <- endpoint_label_profile_suggestion(selected_vid, panel_state = panel_state)
    working_hit <- integer(0)
    if (is.data.frame(working_rows) && nrow(working_rows) > 0L && is.finite(selected_vid)) {
      working_hit <- which(as.integer(working_rows$vertex) == as.integer(selected_vid))
    }
    selected_working_row <- if (length(working_hit) > 0L) working_rows[working_hit[[1]], , drop = FALSE] else NULL

    build_profile_table <- function(profile_tbl) {
      profile_tbl <- normalize_endpoint_feature_profile(profile_tbl)
      if (!is.data.frame(profile_tbl) || nrow(profile_tbl) < 1L) {
        return(shiny::p(class = "gf-hint", "No feature profile is available for the selected vertex."))
      }
      profile_chr <- profile_tbl
      for (cc in names(profile_chr)) {
        profile_chr[[cc]] <- format_endpoint_metric_value(profile_chr[[cc]])
      }
      head_row <- shiny::tags$tr(lapply(names(profile_chr), function(cc) shiny::tags$th(cc)))
      body_rows <- lapply(seq_len(nrow(profile_chr)), function(ii) {
        shiny::tags$tr(lapply(profile_chr[ii, , drop = FALSE], function(val) shiny::tags$td(as.character(val[[1]] %||% ""))))
      })
      shiny::div(
        class = "table-responsive gf-endpoint-table-scroll",
        shiny::tags$table(
          class = "table table-sm gf-asset-table",
          shiny::tags$thead(head_row),
          shiny::tags$tbody(body_rows)
        )
      )
    }

    suggestion_label <- as.character(label_suggestion$label %||% "")
    suggestion_label <- suggestion_label[!is.na(suggestion_label) & nzchar(suggestion_label) & !identical(toupper(suggestion_label), "NA")]
    sample_id_text <- as.character(label_suggestion$sample_id %||% "")
    sample_id_text <- sample_id_text[!is.na(sample_id_text) & nzchar(sample_id_text) & !identical(toupper(sample_id_text), "NA")]
    source_detail_text <- as.character(label_suggestion$source_detail %||% "")
    source_detail_text <- source_detail_text[!is.na(source_detail_text) & nzchar(source_detail_text) & !identical(toupper(source_detail_text), "NA")]

    shiny::tagList(
      shiny::div(
        class = "gf-hint",
        if (identical(tolower(renderer_name), "plotly")) {
          "Click a vertex in the Plotly graph to inspect it."
        } else {
          sprintf("Plotly click selection is available when the renderer is Plotly. Current renderer: %s.", renderer_name)
        }
      ),
      shiny::div(
        class = "gf-endpoint-actions gf-endpoint-inspector-actions gf-endpoint-input-row",
        shiny::tags$label(
          `for` = "endpoint_vertex_id",
          class = "gf-endpoint-inline-label",
          "Vertex ID"
        ),
        shiny::div(
          class = "gf-endpoint-inspector-input",
          shiny::numericInput(
            "endpoint_vertex_id",
            label = NULL,
            value = if (is.finite(selected_vid)) as.integer(selected_vid) else NA,
            min = 1,
            step = 1,
            width = "100%"
          )
        ),
        shiny::actionButton(
          "endpoint_add_selected_vertex",
          "Add To Working Set",
          class = "btn-light btn-sm gf-btn-inline gf-endpoint-compact-btn"
        )
      ),
      if (length(suggestion_label) > 0L) {
        shiny::div(
          class = "gf-hint",
          shiny::tags$strong("Suggested label: "),
          suggestion_label[[1]]
        )
      } else {
        NULL
      },
      if (length(sample_id_text) > 0L) {
        shiny::div(
          class = "gf-hint",
          shiny::tags$strong("Sample ID: "),
          sample_id_text[[1]]
        )
      } else {
        NULL
      },
      if (length(source_detail_text) > 0L) {
        shiny::div(
          class = "gf-hint",
          shiny::tags$strong("Label source: "),
          source_detail_text[[1]]
        )
      } else {
        NULL
      },
      shiny::tags$details(
        class = "gf-endpoint-metrics-details",
        open = if (identical(selected_source, "working_table")) "open" else NULL,
        shiny::tags$summary("Feature Profile"),
        build_profile_table(label_suggestion$profile)
      ),
      shiny::div(
        class = "gf-endpoint-actions",
        shiny::actionButton(
          "endpoint_working_snapshot",
          "Save Snapshot",
          class = "btn-light btn-sm gf-btn-inline"
        ),
        shiny::actionButton(
          "endpoint_working_clear",
          "Clear Working Set",
          class = "btn-light btn-sm gf-btn-inline"
        )
      )
    )
  }

  output$endpoint_vertex_inspector <- shiny::renderUI({
    build_endpoint_vertex_inspector_ui(endpoint_panel_state())
  })

  output$endpoint_candidate_metrics <- shiny::renderUI({
    build_endpoint_candidate_metrics_ui(endpoint_panel_state())
  })

  output$workflow_controls <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(NULL)
    }

    manifest <- active_manifest()
    if (quadform_is_benchmark_manifest(manifest)) {
      idx <- quadform_index_state()
      sel <- quadform_selection_state()
      st <- quadform_view_state()

      if (!is.list(idx) || !is.null(idx$error)) {
        return(shiny::div(class = "gf-hint", as.character(idx$error %||% "Quadform benchmark index unavailable.")))
      }

      selector_controls <- if (is.list(sel$fields) && length(sel$fields) > 0L) {
        lapply(sel$fields, function(field_spec) {
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", as.character(field_spec$label %||% field_spec$id)),
            shiny::selectInput(
              inputId = as.character(field_spec$input_id %||% ""),
              label = NULL,
              choices = field_spec$choices,
              selected = as.character(field_spec$selected %||% ""),
              width = "220px"
            )
          )
        })
      } else {
        list(shiny::p(class = "gf-hint", "No benchmark selector fields are available."))
      }

      status_text <- if (is.list(st) && identical(st$status, "ok") &&
          is.list(sel) && identical(sel$mode, "optimal") && is.data.frame(sel$optimal_metric)) {
        metric <- sel$optimal_metric
        row <- st$selected_row
        err_col <- as.character(sel$error_column %||% "rel_rms_error")
        err_val <- if (err_col %in% names(metric)) suppressWarnings(as.numeric(metric[[err_col]][[1]])) else NA_real_
        params <- c(
          sprintf("target=%s", as.character(metric$target[[1]] %||% "")),
          sprintf("%s=%s", err_col, if (is.finite(err_val)) formatC(err_val, digits = 4, format = "fg") else ""),
          sprintf("family=%s", as.character(row$graph_family[[1]] %||% "")),
          sprintf("k=%s", as.character(row$k[[1]] %||% "")),
          sprintf("pruning=%s", as.character(row$prune_method[[1]] %||% "")),
          sprintf("stage=%s", as.character(row$stage[[1]] %||% ""))
        )
        params <- params[nzchar(sub("^[^=]+=", "", params))]
        sprintf("Optimal graph selected: %s.", paste(params, collapse = ", "))
      } else if (is.list(st) && identical(st$status, "ok")) {
        sprintf(
          "Selected %s / %s / %s.",
          as.character(st$selected_row$dataset_id[[1]] %||% ""),
          as.character(st$selected_row$setting_id[[1]] %||% ""),
          as.character(st$selected_row$stage[[1]] %||% "")
        )
      } else {
        as.character(st$message %||% "Select one benchmark graph stage.")
      }

      return(bslib::accordion(
        id = "quadform_workflow_accordion",
        open = "quadform_graph_stage",
        bslib::accordion_panel(
          "Quadform Benchmark",
          value = "quadform_graph_stage",
          shiny::tagList(
            selector_controls,
            shiny::hr(),
            shiny::p(class = "gf-hint", status_text),
            if (is.list(st) && st$status %in% c("missing_layout", "layout_error")) {
              shiny::actionButton(
                "quadform_generate_layout",
                "Generate Weighted Layout",
                class = "btn-primary gf-btn-wide"
              )
            } else {
              NULL
            },
            shiny::tags$details(
              class = "gf-endpoint-metrics-details",
              shiny::tags$summary("Benchmark assets"),
              build_html_table(
                data.frame(
                  Asset = c("datasets", "graph stages", "layouts", "metrics"),
                  Rows = c(
                    idx$dataset_assets |> nrow(),
                    idx$graph_assets |> nrow(),
                    idx$layout_assets |> nrow(),
                    idx$metrics |> nrow()
                  ),
                  stringsAsFactors = FALSE
                ),
                empty_text = "No benchmark assets loaded."
              )
            )
          )
        )
      ))
    }

    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    graph_ui <- graph_structure_state()
    overview_ui <- project_overview_state()

    graph_tbl <- summarize_graph_assets(
      graph_sets,
      default_id = as.character(defaults$graph_set_id %||% NA_character_)
    )
    condexp_tbl <- summarize_condexp_assets(
      condexp_sets,
      default_id = as.character(defaults$condexp_set_id %||% NA_character_)
    )
    endpoint_panel <- endpoint_panel_state()
    endpoint_rows <- if (is.list(endpoint_panel) && is.data.frame(endpoint_panel$rows)) endpoint_panel$rows else data.frame()
    endpoint_working <- if (is.list(endpoint_panel) && is.list(endpoint_panel$working)) endpoint_panel$working else empty_working_endpoint_state()
    subject_panel <- subject_panel_state()
    occupation_panel <- occupation_density_panel_state()
    basin_panel <- basin_panel_state()
    arm_panel <- arm_panel_state()
    arm_rows <- if (is.list(arm_panel) && is.data.frame(arm_panel$rows)) arm_panel$rows else empty_arm_candidate_rows()
    arm_working <- if (is.list(arm_panel) && is.list(arm_panel$working)) arm_panel$working else empty_working_arm_state()
    arm_virtual <- arm_virtual_endpoints()
    has_asset_views <- nrow(graph_tbl) > 0L || nrow(condexp_tbl) > 0L ||
      length(endpoint_runs) > 0L || isTRUE(occupation_panel$has_assets)

    build_endpoint_candidate_table <- function(rows_df) {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
        return(shiny::p(class = "gf-hint", "No endpoint datasets found for the current graph set."))
      }

      endpoint_dataset_display_label <- function(label, k_value) {
        label_use <- as.character(label %||% "")
        k_use <- suppressWarnings(as.integer(k_value))
        if (!nzchar(label_use) || !is.finite(k_use)) {
          return(label_use)
        }
        sub(sprintf("\\s*\\(k=%d\\)$", as.integer(k_use)), "", label_use)
      }

      head_row <- shiny::tags$tr(
        shiny::tags$th("show"),
        shiny::tags$th("loaded"),
        shiny::tags$th("dataset"),
        shiny::tags$th("method"),
        shiny::tags$th("k"),
        shiny::tags$th("n"),
        shiny::tags$th("origin"),
        shiny::tags$th("actions")
      )
      body_rows <- lapply(seq_len(nrow(rows_df)), function(ii) {
        rr <- rows_df[ii, , drop = FALSE]
        in_id <- as.character(rr$input_id[[1]] %||% "")
        load_id <- as.character(rr$load_input_id[[1]] %||% "")
        rename_id <- as.character(rr$rename_input_id[[1]] %||% "")
        delete_id <- as.character(rr$delete_input_id[[1]] %||% "")
        default_id <- as.character(rr$default_input_id[[1]] %||% "")
        checked <- isTRUE(rr$selected[[1]])
        restored_checked <- isTRUE(shiny::restoreInput(id = in_id, default = checked))
        loaded_mark <- if (isTRUE(rr$is_working_source[[1]])) {
          "\u2713"
        } else {
          ""
        }
        default_badge <- if (isTRUE(rr$is_default[[1]])) {
          shiny::tags$span(class = "badge bg-secondary", "default")
        } else {
          NULL
        }
        shiny::tags$tr(
          shiny::tags$td(
            shiny::tags$input(
              type = "checkbox",
              id = in_id,
              checked = if (isTRUE(restored_checked)) "checked" else NULL
            )
          ),
          shiny::tags$td(class = "gf-endpoint-loaded-col", loaded_mark),
          shiny::tags$td(
            shiny::div(endpoint_dataset_display_label(rr$label[[1]] %||% "", rr$k[[1]])),
            default_badge
          ),
          shiny::tags$td(as.character(rr$method[[1]] %||% "")),
          shiny::tags$td(as.character(rr$k_display[[1]] %||% "")),
          shiny::tags$td(as.character(rr$n_endpoints[[1]] %||% "")),
          shiny::tags$td(as.character(rr$origin[[1]] %||% "")),
          shiny::tags$td(
            class = "gf-endpoint-table-actions-cell",
            if (isTRUE(rr$can_load[[1]])) shiny::actionButton(
              load_id,
              "Load",
              class = "btn-light btn-sm gf-btn-inline"
            ),
            if (isTRUE(rr$can_rename[[1]])) shiny::actionButton(
              rename_id,
              "Rename",
              class = "btn-light btn-sm gf-btn-inline"
            ),
            if (isTRUE(rr$can_delete[[1]])) shiny::actionButton(
              delete_id,
              "Delete",
              class = "btn-light btn-sm gf-btn-inline"
            ),
            if (isTRUE(rr$can_set_default[[1]]) && !isTRUE(rr$is_default[[1]])) shiny::actionButton(
              default_id,
              "Set Default",
              class = "btn-light btn-sm gf-btn-inline"
            )
          )
        )
      })

      shiny::tagList(
        shiny::div(
          class = "gf-hint",
          "Saved endpoint sets live here. Checkboxes control graph overlays; actions load or manage datasets."
        ),
        shiny::div(
          class = "table-responsive gf-endpoint-table-scroll",
          shiny::tags$table(
            class = "table table-sm gf-asset-table",
            shiny::tags$thead(head_row),
            shiny::tags$tbody(body_rows)
          )
        )
      )
    }

    build_working_endpoint_table <- function(working_state) {
      rows_df <- accepted_visible_working_rows(working_state)
      hidden_rows_df <- accepted_hidden_working_rows(working_state)
      working_count <- nrow(rows_df)
      hidden_count <- nrow(hidden_rows_df)
      show_working_checked <- isTRUE(endpoint_show_working_set_effective(working_state))
      working_scroll_top <- endpoint_working_scroll_top()
      if (!is.finite(working_scroll_top) || working_scroll_top < 0L) {
        working_scroll_top <- 0L
      }
      status_label <- if (working_endpoint_is_recovered(working_state)) {
        "Recovered Draft"
      } else if (working_endpoint_is_modified(working_state)) {
        "Modified"
      } else {
        "Clean"
      }
      status_class <- if (working_endpoint_is_recovered(working_state)) {
        "gf-endpoint-status-badge gf-endpoint-status-recovered"
      } else if (working_endpoint_is_modified(working_state)) {
        "gf-endpoint-status-badge gf-endpoint-status-modified"
      } else {
        "gf-endpoint-status-badge gf-endpoint-status-clean"
      }
      selected_vid <- selected_endpoint_vertex()

      header <- shiny::div(
        class = "gf-endpoint-header-row",
        shiny::div(
          class = "gf-endpoint-header-main",
          shiny::h6(
            class = "gf-graph-layout-head gf-endpoint-section-head",
            sprintf("Working Endpoints (%d)", as.integer(working_count))
          ),
          shiny::tags$span(class = status_class, status_label)
        ),
        shiny::tags$label(
          class = "gf-endpoint-inline-check",
          shiny::tags$input(
            type = "checkbox",
            id = "endpoint_show_working_set",
            checked = if (isTRUE(show_working_checked)) "checked" else NULL
          ),
          shiny::tags$span("Show Working Set")
        )
      )

      build_visible_rows_table <- function(rows_use) {
        if (nrow(rows_use) > 250L) {
          rows_use <- rows_use[seq_len(250L), , drop = FALSE]
        }
        head_row <- shiny::tags$tr(
          shiny::tags$th("vertex"),
          shiny::tags$th("label"),
          shiny::tags$th("actions")
        )
        body_rows <- lapply(seq_len(nrow(rows_use)), function(ii) {
          rr <- rows_use[ii, , drop = FALSE]
          vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
          label_dom_id <- endpoint_working_label_dom_id(vid)
          label_event_id <- endpoint_working_label_event_id(vid)
          hide_id <- endpoint_working_hide_input_id(vid)
          select_id <- endpoint_working_select_input_id(vid)
          label_value <- normalize_working_endpoint_label(
            label = rr$label[[1]] %||% "",
            vertex_id = vid,
            auto_label = rr$auto_label[[1]] %||% sprintf("v%d", vid)
          )
          shiny::tags$tr(
            class = if (is.finite(selected_vid) && identical(as.integer(selected_vid), as.integer(vid))) "gf-endpoint-working-row-selected" else NULL,
            shiny::tags$td(
              class = "gf-endpoint-working-select-cell",
              shiny::tags$button(
                type = "button",
                id = select_id,
                class = "gf-endpoint-working-select-btn",
                onclick = sprintf(
                  "(function(btn){var wrap=document.getElementById('endpoint_working_table_scroll');Shiny.setInputValue('endpoint_working_select_vertex',{vertex:%d,scrollTop:(wrap?wrap.scrollTop:0)},{priority:'event'});})(this)",
                  as.integer(vid)
                ),
                sprintf("v%d", as.integer(rr$vertex[[1]]))
              )
            ),
            shiny::tags$td(
              shiny::tags$input(
                id = label_dom_id,
                type = "text",
                value = label_value,
                class = "form-control form-control-sm gf-endpoint-table-input",
                onchange = sprintf(
                  "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
                  label_event_id
                )
              )
            ),
            shiny::tags$td(
              class = "gf-endpoint-table-actions-cell",
              shiny::actionButton(
                hide_id,
                "Hide",
                class = "btn-light btn-sm gf-btn-inline gf-endpoint-remove-btn"
              )
            )
          )
        })

        shiny::tagList(
          shiny::div(
            id = "endpoint_working_table_scroll",
            class = "table-responsive gf-endpoint-table-scroll",
            `data-scroll-top` = as.character(as.integer(working_scroll_top)),
            shiny::tags$table(
              class = "table table-sm gf-asset-table",
              shiny::tags$thead(head_row),
              shiny::tags$tbody(body_rows)
            )
          ),
          shiny::tags$script(
            shiny::HTML(
              "(function(){var el=document.getElementById('endpoint_working_table_scroll'); if(!el){return;} var y=parseInt(el.dataset.scrollTop||'0',10); if(Number.isFinite(y)){el.scrollTop=y;}})();"
            )
          )
        )
      }

      build_hidden_rows_table <- function(rows_use) {
        if (nrow(rows_use) > 250L) {
          rows_use <- rows_use[seq_len(250L), , drop = FALSE]
        }
        head_row <- shiny::tags$tr(
          shiny::tags$th("vertex"),
          shiny::tags$th("label"),
          shiny::tags$th("actions")
        )
        body_rows <- lapply(seq_len(nrow(rows_use)), function(ii) {
          rr <- rows_use[ii, , drop = FALSE]
          vid <- suppressWarnings(as.integer(rr$vertex[[1]]))
          restore_id <- endpoint_working_restore_input_id(vid)
          delete_id <- endpoint_working_delete_input_id(vid)
          select_id <- endpoint_working_select_input_id(vid)
          label_value <- normalize_working_endpoint_label(
            label = rr$label[[1]] %||% "",
            vertex_id = vid,
            auto_label = rr$auto_label[[1]] %||% sprintf("v%d", vid)
          )
          shiny::tags$tr(
            class = if (is.finite(selected_vid) && identical(as.integer(selected_vid), as.integer(vid))) "gf-endpoint-working-row-selected" else NULL,
            shiny::tags$td(
              class = "gf-endpoint-working-select-cell",
              shiny::tags$button(
                type = "button",
                id = select_id,
                class = "gf-endpoint-working-select-btn",
                onclick = sprintf(
                  "Shiny.setInputValue('endpoint_working_select_vertex',{vertex:%d,scrollTop:0},{priority:'event'})",
                  as.integer(vid)
                ),
                sprintf("v%d", as.integer(rr$vertex[[1]]))
              )
            ),
            shiny::tags$td(label_value),
            shiny::tags$td(
              class = "gf-endpoint-table-actions-cell",
              shiny::actionButton(
                restore_id,
                "Restore",
                class = "btn-light btn-sm gf-btn-inline"
              ),
              shiny::actionButton(
                delete_id,
                "Delete",
                class = "btn-light btn-sm gf-btn-inline gf-endpoint-remove-btn"
              )
            )
          )
        })

        shiny::tags$details(
          class = "gf-endpoint-metrics-details",
          shiny::tags$summary(sprintf("Hidden Endpoints (%d)", as.integer(hidden_count))),
          shiny::div(
            class = "table-responsive gf-endpoint-table-scroll",
            shiny::tags$table(
              class = "table table-sm gf-asset-table",
              shiny::tags$thead(head_row),
              shiny::tags$tbody(body_rows)
            )
          )
        )
      }

      content <- list()
      if (nrow(rows_df) > 0L) {
        content <- c(content, list(build_visible_rows_table(rows_df)))
      } else {
        content <- c(content, list(shiny::p(class = "gf-hint", "No visible working endpoints. Hidden endpoints stay available below.")))
      }
      if (hidden_count > 0L) {
        content <- c(content, list(build_hidden_rows_table(hidden_rows_df)))
      } else if (nrow(rows_df) < 1L) {
        content <- c(content, list(shiny::p(class = "gf-hint", "Working endpoint set is empty.")))
      }

      shiny::tagList(header, content)
    }

    build_subject_sample_table <- function(rows_df) {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
        return(shiny::p(class = "gf-hint", "No subjects are selected."))
      }
      rows_use <- rows_df
      if (nrow(rows_use) > 250L) {
        rows_use <- rows_use[seq_len(250L), , drop = FALSE]
      }
      show_subject_col <- length(unique(as.character(rows_use$subject_id %||% character(0)))) > 1L
      show_order_col <- "time_order" %in% names(rows_use) &&
        any(is.finite(suppressWarnings(as.numeric(rows_use$time_order))))
      head_row <- shiny::tags$tr(
        if (show_subject_col) shiny::tags$th("subject") else NULL,
        shiny::tags$th("vertex"),
        shiny::tags$th("sample"),
        if (show_order_col) shiny::tags$th("order") else NULL,
        shiny::tags$th("week"),
        shiny::tags$th("day")
      )
      body_rows <- lapply(seq_len(nrow(rows_use)), function(ii) {
        rr <- rows_use[ii, , drop = FALSE]
        shiny::tags$tr(
          if (show_subject_col) shiny::tags$td(as.character(rr$subject_id[[1]] %||% "")) else NULL,
          shiny::tags$td(sprintf("v%d", suppressWarnings(as.integer(rr$vertex[[1]])))),
          shiny::tags$td(as.character(rr$sample_id[[1]] %||% "")),
          if (show_order_col) {
            shiny::tags$td(
              if (is.finite(suppressWarnings(as.numeric(rr$time_order[[1]])))) {
                as.character(suppressWarnings(as.numeric(rr$time_order[[1]])))
              } else {
                ""
              }
            )
          } else {
            NULL
          },
          shiny::tags$td(
            if (is.finite(suppressWarnings(as.integer(rr$week[[1]])))) {
              as.character(suppressWarnings(as.integer(rr$week[[1]])))
            } else {
              ""
            }
          ),
          shiny::tags$td(
            if (is.finite(suppressWarnings(as.integer(rr$day[[1]])))) {
              as.character(suppressWarnings(as.integer(rr$day[[1]])))
            } else {
              ""
            }
          )
        )
      })
      shiny::div(
        class = "table-responsive gf-endpoint-table-scroll",
        shiny::tags$table(
          class = "table table-sm gf-asset-table",
          shiny::tags$thead(head_row),
          shiny::tags$tbody(body_rows)
        )
      )
    }

    build_subject_panel_ui <- function(subject_state_panel) {
      if (!is.list(subject_state_panel) || !isTRUE(subject_state_panel$available)) {
        prov <- if (is.list(subject_state_panel)) subject_state_panel$provider else NULL
        hint <- if (is.null(prov)) {
          "Subject metadata is not available for this project."
        } else {
          "Subject metadata loaded but no matching vertices found in the current graph."
        }
        return(shiny::p(class = "gf-hint", hint))
      }
      selected_ids <- unique(as.character(subject_state_panel$selected_ids %||% character(0)))
      selected_rows <- if (is.data.frame(subject_state_panel$selected_rows)) subject_state_panel$selected_rows else empty_subject_sample_rows()
      shiny::tagList(
        shiny::div(
          class = "gf-endpoint-section",
          shiny::h6(class = "gf-graph-layout-head", "Subject Selection"),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Subject IDs:"),
            {
              rows_for_choices <- subject_state_panel$rows
              if (is.data.frame(rows_for_choices) && nrow(rows_for_choices) > 0L) {
                sid_vec <- sort(unique(as.character(rows_for_choices$subject_id)))
                sid_vec <- sid_vec[nzchar(sid_vec)]
              } else {
                sid_vec <- character(0)
              }
              sel <- if (length(selected_ids) > 0L) selected_ids[[1]] else ""
              option_tags <- list(shiny::tags$option(value = "", "Choose subject..."))
              for (ss in sid_vec) {
                nn <- sum(as.character(rows_for_choices$subject_id) == ss, na.rm = TRUE)
                lbl <- sprintf("%s (%d)", ss, nn)
                if (identical(ss, sel)) {
                  option_tags[[length(option_tags) + 1L]] <- shiny::tags$option(value = ss, selected = "selected", lbl)
                } else {
                  option_tags[[length(option_tags) + 1L]] <- shiny::tags$option(value = ss, lbl)
                }
              }
              shiny::div(
                class = "shiny-input-container",
                style = "width: 280px;",
                shiny::tags$select(
                  id = "subject_ids",
                  class = "shiny-input-select form-control",
                  option_tags
                )
              )
            }
          ),
          shiny::tags$label(
            class = "gf-endpoint-inline-check",
            shiny::tags$input(
              type = "checkbox",
              id = "subject_show_overlay",
              checked = if (isTRUE(subject_state_panel$show_overlay)) "checked" else NULL
            ),
            shiny::tags$span("Show Subject Overlay")
          ),
          shiny::tags$label(
            class = "gf-endpoint-inline-check",
            shiny::tags$input(
              type = "checkbox",
              id = "subject_dim_background",
              checked = if (isTRUE(subject_state_panel$dim_background)) "checked" else NULL
            ),
            shiny::tags$span("Dim Background")
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Background opacity:"),
            shiny::selectInput(
              "subject_background_opacity",
              label = NULL,
              choices = c("10%" = "0.10", "15%" = "0.15", "22%" = "0.22", "30%" = "0.30", "40%" = "0.40", "50%" = "0.50", "65%" = "0.65"),
              selected = {
                opacity_use <- formatC(as.numeric(subject_state_panel$background_opacity %||% 0.22), format = "f", digits = 2)
                if (opacity_use %in% c("0.10", "0.15", "0.22", "0.30", "0.40", "0.50", "0.65")) opacity_use else "0.22"
              },
              width = "180px"
            )
          ),
          if (length(selected_ids) > 0L) {
            shiny::div(
              class = "gf-hint",
              if (length(selected_ids) == 1L) {
                sprintf("Subject %s has %d graph samples in the current vertex set.", selected_ids[[1]], as.integer(nrow(selected_rows)))
              } else {
                sprintf("%d selected subjects contribute %d graph samples in the current vertex set.", as.integer(length(selected_ids)), as.integer(nrow(selected_rows)))
              }
            )
          } else {
            shiny::div(class = "gf-hint", "Choose one or more subjects to inspect their graph samples.")
          }
        ),
        shiny::div(
          class = "gf-endpoint-section",
          shiny::h6(class = "gf-graph-layout-head", "Subject Samples"),
          build_subject_sample_table(selected_rows)
        ),
        shiny::div(
          class = "gf-endpoint-section",
          shiny::h6(class = "gf-graph-layout-head", "Subject Layout"),
          if (length(selected_ids) > 1L) {
            shiny::div(class = "gf-hint", "Multiple subjects are colored automatically by subject for easier comparison.")
          } else {
            NULL
          },
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Vertex color:"),
            shiny::selectInput(
              "subject_vertex_color",
              label = NULL,
              choices = subject_vertex_color_choices(),
              selected = as.character(subject_state_panel$vertex_color %||% "#dc2626"),
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Vertex size:"),
            shiny::selectInput(
              "subject_vertex_size",
              label = NULL,
              choices = c(
                "0.75x" = "0.75",
                "1.0x" = "1.0",
                "1.25x" = "1.25",
                "1.5x" = "1.5",
                "1.8x" = "1.8",
                "2.0x" = "2.0",
                "2.5x" = "2.5",
                "3.0x" = "3.0"
              ),
              selected = {
                requested <- suppressWarnings(as.numeric(
                  subject_state_panel$vertex_size %||% 1.0
                ))
                allowed <- c(0.75, 1.0, 1.25, 1.5, 1.8, 2.0, 2.5, 3.0)
                labels <- c(
                  "0.75", "1.0", "1.25", "1.5",
                  "1.8", "2.0", "2.5", "3.0"
                )
                matched <- which(abs(allowed - requested) < 1e-8)
                if (length(matched) > 0L) {
                  labels[[matched[[1L]]]]
                } else {
                  "1.0"
                }
              },
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Edge mode:"),
            shiny::selectInput(
              "subject_edge_mode",
              label = NULL,
              choices = subject_edge_mode_choices,
              selected = as.character(subject_state_panel$edge_mode %||% "none"),
              width = "220px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Edge color:"),
            shiny::selectInput(
              "subject_edge_color",
              label = NULL,
              choices = subject_vertex_color_choices(),
              selected = as.character(subject_state_panel$edge_color %||% "#dc2626"),
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Edge width:"),
            shiny::selectInput(
              "subject_edge_width",
              label = NULL,
              choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6"),
              selected = if (as.character(as.integer(subject_state_panel$edge_width %||% 2)) %in% c("1", "2", "3", "4", "5", "6")) {
                as.character(as.integer(subject_state_panel$edge_width %||% 2))
              } else {
                "2"
              },
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Label mode:"),
            shiny::selectInput(
              "subject_label_mode",
              label = NULL,
              choices = subject_state_panel$label_choices %||% c("None" = "none", "Vertex ID" = "vertex"),
              selected = as.character(subject_state_panel$label_mode %||% "none"),
              width = "220px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Label size:"),
            shiny::selectInput(
              "subject_label_size",
              label = NULL,
              choices = c("0.8x" = "0.8", "1.0x" = "1.0", "1.2x" = "1.2", "1.5x" = "1.5", "1.8x" = "1.8", "2.2x" = "2.2"),
              selected = if (formatC(as.numeric(subject_state_panel$label_size %||% 1.0), format = "f", digits = 1) %in% c("0.8", "1.0", "1.2", "1.5", "1.8", "2.2")) {
                formatC(as.numeric(subject_state_panel$label_size %||% 1.0), format = "f", digits = 1)
              } else {
                "1.0"
              },
              width = "180px"
            )
          )
        )
      )
    }

    build_arm_dataset_table <- function(rows_df) {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
        return(shiny::p(class = "gf-hint", "No saved arm datasets found for the current graph set."))
      }

      head_row <- shiny::tags$tr(
        shiny::tags$th("show"),
        shiny::tags$th("loaded"),
        shiny::tags$th("dataset"),
        shiny::tags$th("method"),
        shiny::tags$th("k"),
        shiny::tags$th("n"),
        shiny::tags$th("origin"),
        shiny::tags$th("actions")
      )
      body_rows <- lapply(seq_len(nrow(rows_df)), function(ii) {
        rr <- rows_df[ii, , drop = FALSE]
        dataset_id <- as.character(rr$dataset_id[[1]] %||% "")
        loaded_mark <- if (isTRUE(rr$is_working_source[[1]])) "\u2713" else ""
        default_badge <- if (isTRUE(rr$is_default[[1]])) {
          shiny::tags$span(class = "badge bg-secondary", "default")
        } else {
          NULL
        }
        shiny::tags$tr(
          shiny::tags$td(
            shiny::tags$input(
              type = "checkbox",
              checked = if (isTRUE(rr$selected[[1]])) "checked" else NULL,
              onclick = sprintf(
                "Shiny.setInputValue('arm_dataset_toggle',{dataset_id:'%s',checked:this.checked},{priority:'event'})",
                dataset_id
              )
            )
          ),
          shiny::tags$td(class = "gf-endpoint-loaded-col", loaded_mark),
          shiny::tags$td(
            shiny::div(as.character(rr$label[[1]] %||% "")),
            default_badge
          ),
          shiny::tags$td(as.character(rr$method[[1]] %||% "")),
          shiny::tags$td(as.character(rr$k_display[[1]] %||% "")),
          shiny::tags$td(as.character(rr$n_arms[[1]] %||% "")),
          shiny::tags$td(as.character(rr$origin[[1]] %||% "")),
          shiny::tags$td(
            class = "gf-endpoint-table-actions-cell",
            if (isTRUE(rr$can_load[[1]])) shiny::tags$button(
              type = "button",
              class = "btn btn-light btn-sm gf-btn-inline",
              onclick = sprintf(
                "Shiny.setInputValue('arm_dataset_action',{action:'load',dataset_id:'%s'},{priority:'event'})",
                dataset_id
              ),
              "Load"
            ),
            if (isTRUE(rr$can_rename[[1]])) shiny::tags$button(
              type = "button",
              class = "btn btn-light btn-sm gf-btn-inline",
              onclick = sprintf(
                "Shiny.setInputValue('arm_dataset_action',{action:'rename',dataset_id:'%s'},{priority:'event'})",
                dataset_id
              ),
              "Rename"
            ),
            if (isTRUE(rr$can_delete[[1]])) shiny::tags$button(
              type = "button",
              class = "btn btn-light btn-sm gf-btn-inline",
              onclick = sprintf(
                "Shiny.setInputValue('arm_dataset_action',{action:'delete',dataset_id:'%s'},{priority:'event'})",
                dataset_id
              ),
              "Delete"
            ),
            if (isTRUE(rr$can_set_default[[1]]) && !isTRUE(rr$is_default[[1]])) shiny::tags$button(
              type = "button",
              class = "btn btn-light btn-sm gf-btn-inline",
              onclick = sprintf(
                "Shiny.setInputValue('arm_dataset_action',{action:'default',dataset_id:'%s'},{priority:'event'})",
                dataset_id
              ),
              "Set Default"
            )
          )
        )
      })

      shiny::tagList(
        shiny::div(
          class = "gf-hint",
          "Saved arm sets live here. Checkboxes control graph overlays; actions load or manage datasets."
        ),
        shiny::div(
          class = "table-responsive gf-endpoint-table-scroll",
          shiny::tags$table(
            class = "table table-sm gf-asset-table",
            shiny::tags$thead(head_row),
            shiny::tags$tbody(body_rows)
          )
        )
      )
    }

    build_working_arm_table <- function(working_state) {
      rows_df <- accepted_visible_working_arm_rows(working_state)
      hidden_rows_df <- accepted_hidden_working_arm_rows(working_state)
      selected_arm <- as.character(arm_selected_id() %||% "")
      header <- shiny::div(
        class = "gf-endpoint-header-row",
        shiny::div(
          class = "gf-endpoint-header-main",
          shiny::h6(
            class = "gf-graph-layout-head gf-endpoint-section-head",
            sprintf("Working Arms (%d)", as.integer(nrow(rows_df)))
          ),
          shiny::tags$span(
            class = if (working_arm_is_modified(working_state)) {
              "gf-endpoint-status-badge gf-endpoint-status-modified"
            } else {
              "gf-endpoint-status-badge gf-endpoint-status-clean"
            },
            if (working_arm_is_modified(working_state)) "Modified" else "Clean"
          )
        ),
        shiny::tags$label(
          class = "gf-endpoint-inline-check",
          shiny::tags$input(
            type = "checkbox",
            id = "arm_show_working_set",
            checked = if (isTRUE(arm_show_working_set_effective(working_state))) "checked" else NULL
          ),
          shiny::tags$span("Show Working Set")
        )
      )

      build_rows_table <- function(rows_use, hidden = FALSE) {
        head_row <- shiny::tags$tr(
          shiny::tags$th("arm"),
          shiny::tags$th("pair"),
          shiny::tags$th("method"),
          shiny::tags$th("n"),
          shiny::tags$th("actions")
        )
        body_rows <- lapply(seq_len(nrow(rows_use)), function(ii) {
          rr <- rows_use[ii, , drop = FALSE]
          arm_id <- as.character(rr$arm_id[[1]] %||% "")
          arm_n <- length(decode_arm_integer_json(rr$arm_vertices_json[[1]] %||% "[]"))
          shiny::tags$tr(
            class = if (identical(selected_arm, arm_id)) "gf-endpoint-working-row-selected" else NULL,
            shiny::tags$td(
              class = "gf-endpoint-working-select-cell",
              shiny::tags$button(
                type = "button",
                class = "gf-endpoint-working-select-btn",
                onclick = sprintf(
                  "Shiny.setInputValue('arm_working_action',{action:'select',arm_id:'%s'},{priority:'event'})",
                  arm_id
                ),
                as.character(rr$label[[1]] %||% rr$family_label[[1]] %||% arm_id)
              )
            ),
            shiny::tags$td(as.character(rr$family_label[[1]] %||% "")),
            shiny::tags$td(arm_thickening_label(rr$thickening_method[[1]] %||% "")),
            shiny::tags$td(as.character(arm_n)),
            shiny::tags$td(
              class = "gf-endpoint-table-actions-cell",
              if (!hidden) shiny::tags$button(
                type = "button",
                class = "btn btn-light btn-sm gf-btn-inline gf-endpoint-remove-btn",
                onclick = sprintf(
                  "Shiny.setInputValue('arm_working_action',{action:'hide',arm_id:'%s'},{priority:'event'})",
                  arm_id
                ),
                "Hide"
              ),
              if (hidden) shiny::tags$button(
                type = "button",
                class = "btn btn-light btn-sm gf-btn-inline",
                onclick = sprintf(
                  "Shiny.setInputValue('arm_working_action',{action:'restore',arm_id:'%s'},{priority:'event'})",
                  arm_id
                ),
                "Restore"
              ),
              if (hidden) shiny::tags$button(
                type = "button",
                class = "btn btn-light btn-sm gf-btn-inline gf-endpoint-remove-btn",
                onclick = sprintf(
                  "Shiny.setInputValue('arm_working_action',{action:'delete',arm_id:'%s'},{priority:'event'})",
                  arm_id
                ),
                "Delete"
              )
            )
          )
        })
        shiny::div(
          class = "table-responsive gf-endpoint-table-scroll",
          shiny::tags$table(
            class = "table table-sm gf-asset-table",
            shiny::tags$thead(head_row),
            shiny::tags$tbody(body_rows)
          )
        )
      }

      preview <- arm_preview_variant()
      shiny::tagList(
        header,
        if (nrow(rows_df) > 0L) build_rows_table(rows_df, hidden = FALSE) else shiny::p(class = "gf-hint", "No visible working arms."),
        if (nrow(hidden_rows_df) > 0L) {
          shiny::tags$details(
            class = "gf-endpoint-metrics-details",
            shiny::tags$summary(sprintf("Hidden Arms (%d)", as.integer(nrow(hidden_rows_df)))),
            build_rows_table(hidden_rows_df, hidden = TRUE)
          )
        } else {
          NULL
        },
        if (is.list(preview)) {
          shiny::div(
            class = "gf-hint",
            sprintf(
              "Preview: %s | path=%d | vertices=%d | %s",
              as.character(preview$label %||% preview$family_label %||% "arm"),
              length(preview$path_vertices %||% integer(0)),
              length(preview$arm_vertices %||% integer(0)),
              as.character(preview$parameter_summary %||% "")
            )
          )
        } else {
          shiny::div(class = "gf-hint", "No arm preview is active.")
        }
      )
    }

    build_arm_builder_ui <- function() {
      choices <- arm_builder_endpoint_choices()
      choice_vals <- unname(choices)
      selectable_vals <- setdiff(choice_vals[nzchar(choice_vals)], "none")
      if (length(selectable_vals) < 2L) {
        return(shiny::p(class = "gf-hint", "Create or load at least two endpoints first. Working Endpoints supply the endpoint choices for arm construction."))
      }
      sel_a <- as.character(isolate(input$arm_endpoint_a %||% "none"))
      if (!(sel_a %in% choice_vals)) {
        sel_a <- "none"
      }
      sel_b <- as.character(isolate(input$arm_endpoint_b %||% "none"))
      if (!(sel_b %in% choice_vals)) {
        sel_b <- "none"
      }
      thick_choices <- c(
        "Path only" = "path_only",
        "Tube lens corridor" = "tube_lens_corridor",
        "Tube lens excess corridor" = "tube_lens_excess_corridor"
      )
      thick_sel <- as.character(input$arm_thickening_method %||% "path_only")
      if (!(thick_sel %in% unname(thick_choices))) {
        thick_sel <- "path_only"
      }
      center_hint <- NULL
      if (is.data.frame(arm_virtual) && nrow(arm_virtual) > 0L) {
        center_hint <- sprintf(
          "CENTER is a virtual endpoint mapped to vertex v%d and is shown on the graph when selected.",
          suppressWarnings(as.integer(arm_virtual$vertex[[1]]))
        )
      }
      shiny::tagList(
        shiny::div(class = "gf-hint", "Build one arm variant from an endpoint pair using the weighted graph shortest path."),
        if (nzchar(as.character(center_hint %||% ""))) shiny::div(class = "gf-hint", center_hint) else NULL,
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Endpoint A:"),
          shiny::selectInput("arm_endpoint_a", label = NULL, choices = choices, selected = sel_a, width = "220px")
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Endpoint B:"),
          shiny::selectInput("arm_endpoint_b", label = NULL, choices = choices, selected = sel_b, width = "220px")
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Path method:"),
          shiny::span(class = "gf-hint", "Weighted shortest path")
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Variant:"),
          shiny::selectInput("arm_thickening_method", label = NULL, choices = thick_choices, selected = thick_sel, width = "220px")
        ),
        if (thick_sel %in% c("tube_lens_corridor", "tube_lens_excess_corridor")) {
          shiny::tagList(
            shiny::div(
              class = "gf-graph-row gf-graph-layout-row",
              shiny::span(class = "gf-graph-row-label", "Path rel. radius:"),
              shiny::numericInput("arm_path_relative_radius", label = NULL, value = suppressWarnings(as.numeric(isolate(input$arm_path_relative_radius %||% 0.10))), min = 0, step = 0.01, width = "130px")
            ),
            if (identical(thick_sel, "tube_lens_excess_corridor")) {
              shiny::div(
                class = "gf-graph-row gf-graph-layout-row",
                shiny::span(class = "gf-graph-row-label", "Excess tol.:"),
                shiny::textInput(
                  "arm_excess_tolerance",
                  label = NULL,
                  value = as.character(isolate(input$arm_excess_tolerance %||% "")),
                  width = "130px",
                  placeholder = "auto"
                )
              )
            } else {
              NULL
            },
            shiny::div(
              class = "gf-hint",
              "Path rel. radius sets corridor width as a fraction of shortest-path length. Leave Excess tol. blank to use the tube radius."
            )
          )
        } else {
          NULL
        },
        shiny::div(
          class = "gf-endpoint-actions",
          shiny::tags$button(
            type = "button",
            class = "btn btn-light btn-sm gf-btn-inline",
            onclick = arm_preview_build_request_js(),
            "Preview Arm"
          ),
          shiny::actionButton("arm_add_preview_to_working", "Add To Working Arms", class = "btn-light btn-sm gf-btn-inline"),
          shiny::actionButton("arm_working_snapshot", "Save Snapshot", class = "btn-light btn-sm gf-btn-inline"),
          shiny::actionButton("arm_working_clear", "Clear Working Arms", class = "btn-light btn-sm gf-btn-inline")
        ),
        shiny::tags$script(HTML(arm_builder_camera_hook_script()))
      )
    }

    build_preview_arm_layout_ui <- function(preview) {
      if (!is.list(preview)) {
        return(NULL)
      }
      is_corridor_preview <- identical(as.character(preview$thickening_method %||% ""), "tube_lens_corridor") ||
        identical(as.character(preview$thickening_method %||% ""), "tube_lens_excess_corridor")
      body_color_mode <- as.character(isolate(input$arm_preview_body_color_mode %||% "solid"))
      if (!(body_color_mode %in% unname(arm_preview_body_color_choices))) {
        body_color_mode <- "solid"
      }
      path_n <- length(preview$path_vertices %||% integer(0))
      arm_n <- length(preview$arm_vertices %||% integer(0))
      body_n <- max(0L, arm_n - length(intersect(preview$path_vertices %||% integer(0), preview$arm_vertices %||% integer(0))))
      shiny::tags$details(
        class = "gf-endpoint-metrics-details",
        open = if (isTRUE(arm_preview_layout_open())) "open" else NULL,
        ontoggle = "Shiny.setInputValue('arm_preview_layout_open', this.open, {priority: 'event'})",
        shiny::tags$summary("Preview Arm Layout"),
        shiny::div(
          class = "gf-hint",
          sprintf(
            "Preview: %s | path=%d | arm=%d | off-path body=%d | %s",
            as.character(preview$label %||% preview$family_label %||% "arm"),
            as.integer(path_n),
            as.integer(arm_n),
            as.integer(body_n),
            as.character(preview$parameter_summary %||% "")
          )
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Path color:"),
          shiny::selectInput(
            "arm_preview_path_color",
            label = NULL,
            choices = c(
              "Orange" = "#f97316",
              "Blue" = "#2563eb",
              "Green" = "#16a34a",
              "Purple" = "#8b5cf6",
              "Red" = "#dc2626",
              "Black" = "#111827"
            ),
            selected = as.character(isolate(input$arm_preview_path_color %||% "#f97316")),
            width = "180px"
          )
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Path width:"),
          shiny::sliderInput(
            "arm_preview_path_width",
            label = NULL,
            min = 1,
            max = 10,
            step = 1,
            value = suppressWarnings(as.numeric(isolate(input$arm_preview_path_width %||% 5))),
            width = "205px"
          )
        ),
        if (is_corridor_preview) {
          shiny::tagList(
            shiny::div(
              class = "gf-graph-row gf-graph-layout-row",
              shiny::span(class = "gf-graph-row-label", "Color corridor by:"),
              shiny::selectInput(
                "arm_preview_body_color_mode",
                label = NULL,
                choices = arm_preview_body_color_choices,
                selected = body_color_mode,
                width = "220px"
              )
            ),
            if (identical(body_color_mode, "solid")) {
              shiny::div(
                class = "gf-graph-row gf-graph-layout-row",
                shiny::span(class = "gf-graph-row-label", "Body color:"),
                shiny::selectInput(
                  "arm_preview_body_color",
                  label = NULL,
                  choices = c(
                    "Gold" = "#eab308",
                    "Red" = "#dc2626",
                    "Orange" = "#f97316",
                    "Blue" = "#2563eb",
                    "Green" = "#16a34a",
                    "Purple" = "#8b5cf6",
                    "Black" = "#111827"
                  ),
                  selected = as.character(isolate(input$arm_preview_body_color %||% "#eab308")),
                  width = "180px"
                )
              )
            } else {
              shiny::div(
                class = "gf-hint",
                "Corridor body vertices are colored by the selected parameterization."
              )
            },
            shiny::div(
              class = "gf-graph-row gf-graph-layout-row",
              shiny::span(class = "gf-graph-row-label", "Body opacity:"),
              shiny::sliderInput(
                "arm_preview_body_opacity",
                label = NULL,
                min = 0.10,
                max = 1.0,
                step = 0.05,
                value = suppressWarnings(as.numeric(isolate(input$arm_preview_body_opacity %||% 0.75))),
                width = "205px"
              )
            ),
            shiny::div(
              class = "gf-graph-row gf-graph-layout-row",
              shiny::span(class = "gf-graph-row-label", "Body size:"),
              shiny::sliderInput(
                "arm_preview_body_size",
                label = NULL,
                min = 0.5,
                max = 4.0,
                step = 0.1,
                value = suppressWarnings(as.numeric(isolate(input$arm_preview_body_size %||% 1.8))),
                width = "205px"
              )
            )
          )
        } else {
          NULL
        },
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Center color:"),
          shiny::selectInput(
            "arm_center_marker_color",
            label = NULL,
            choices = c(
              "Black" = "#111827",
              "Red" = "#dc2626",
              "Orange" = "#f97316",
              "Blue" = "#2563eb",
              "Green" = "#16a34a",
              "Purple" = "#8b5cf6"
            ),
            selected = as.character(isolate(input$arm_center_marker_color %||% "#111827")),
            width = "180px"
          )
        ),
        shiny::div(
          class = "gf-graph-row gf-graph-layout-row",
          shiny::span(class = "gf-graph-row-label", "Center size:"),
          shiny::sliderInput(
            "arm_center_marker_size",
            label = NULL,
            min = 0.8,
            max = 4.0,
            step = 0.1,
            value = suppressWarnings(as.numeric(isolate(input$arm_center_marker_size %||% 1.7))),
            width = "205px"
          )
        )
      )
    }

    panels <- list()
    open.panels <- c("workflow_graph_structure")

    if (is.list(overview_ui)) {
      panels <- c(
        panels,
        list(
          bslib::accordion_panel(
            "Overview",
            value = "workflow_overview",
            shiny::tagList(
              build_html_table(overview_ui$summary_tbl, empty_text = "No overview summary available."),
              if (length(overview_ui$artifact_choices %||% c()) > 0L) {
                shiny::div(
                  class = "gf-endpoint-section",
                  shiny::div(
                    class = "gf-graph-row gf-graph-row-tight",
                    shiny::span(class = "gf-graph-row-label", "Artifact:"),
                    shiny::selectInput(
                      "project_overview_artifact",
                      label = NULL,
                      choices = overview_ui$artifact_choices,
                      selected = as.character(input$project_overview_artifact %||% overview_ui$default_artifact_id %||% ""),
                      width = "230px"
                    ),
                    shiny::actionButton(
                      "project_overview_open_artifact",
                      "Open",
                      class = "btn-light btn-sm gf-btn-inline"
                    )
                  )
                )
              } else {
                NULL
              }
            )
          )
        )
      )
      open.panels <- c("workflow_overview", open.panels)
    }

    if (isTRUE(rv$project.show.data)) {
      panels <- c(
        panels,
        list(bslib::accordion_panel("Data", value = "workflow_data", mod_data_ui("data")))
      )
      open.panels <- c("workflow_data", open.panels)
    }

    if (isTRUE(has_asset_views)) {
      graph_panel <- if (!is.null(graph_ui$error)) {
        shiny::p(class = "gf-hint", graph_ui$error)
      } else {
        size_choices <- c(
          paste0(format(seq(0.1, 0.9, by = 0.1), nsmall = 1, trim = TRUE), "x"),
          "0.75x", "1.0x", "1.25x", "1.50x", "2.0x"
        )
        if (!(graph_ui$size_selected %in% size_choices)) {
          size_choices <- unique(c(size_choices, graph_ui$size_selected))
        }

        selector_rows <- if (isTRUE(graph_ui$grouped_selector_enabled) &&
            length(graph_ui$selector_fields %||% list()) > 0L) {
          rows <- lapply(graph_ui$selector_fields, function(spec) {
            shiny::div(
              class = "gf-graph-row gf-graph-row-tight",
              shiny::span(class = "gf-graph-row-label", paste0(as.character(spec$label %||% "Selector"), ":")),
              shiny::selectInput(
                as.character(spec$input_id %||% ""),
                label = NULL,
                choices = spec$choices %||% c(),
                selected = as.character(spec$selected %||% ""),
                width = "205px"
              )
            )
          })

          c(rows, list(
            shiny::div(
              class = "gf-graph-row gf-graph-row-tight",
              shiny::span(class = "gf-graph-row-label", paste0(as.character(graph_ui$selector_summary_label %||% "Graph family"), ":")),
              shiny::span(class = "gf-graph-row-value", as.character(graph_ui$selector_summary_value %||% graph_ui$data_type_label %||% "")),
              shiny::span(class = "gf-graph-dims", graph_ui$dims_text)
            )
          ))
        } else {
          list(
            shiny::div(
              class = "gf-graph-row gf-graph-row-tight",
              shiny::span(class = "gf-graph-row-label", "Data Type:"),
              shiny::selectInput(
                "graph_data_type",
                label = NULL,
                choices = graph_ui$data_type_choices,
                selected = graph_ui$set_id,
                width = "160px"
              ),
              shiny::span(class = "gf-graph-dims", graph_ui$dims_text)
            )
          )
        }

        shiny::tagList(
          selector_rows,
          shiny::div(
            class = "gf-graph-row gf-graph-row-tight gf-graph-row-k",
            shiny::span(class = "gf-graph-row-label", "k:"),
            shiny::selectInput(
              "graph_k",
              label = NULL,
              choices = graph_ui$k_choices,
              selected = if (is.finite(graph_ui$k_selected)) as.character(graph_ui$k_selected) else "",
              width = "105px"
            ),
            shiny::actionButton(
              "set_reference_graph_inline",
              "Set Reference",
              class = "btn-light btn-sm gf-btn-inline"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-row-tight gf-graph-row-optimal",
            shiny::span(class = "gf-graph-row-label", "Optimal k:"),
            shiny::selectInput(
              "graph_optimal_method",
              label = NULL,
              choices = graph_ui$optimal_choices,
              selected = graph_ui$optimal_selected,
              width = "180px"
            ),
            shiny::actionButton(
              "graph_optimal_show",
              "Show",
              class = "btn-light btn-sm gf-btn-inline"
            )
          ),
          shiny::actionButton(
            "graph_update_placeholder",
            "Update / Expand Graphs...",
            class = "btn-light gf-btn-wide"
          ),
          shiny::hr(),
          shiny::h6(class = "gf-graph-layout-head", "Graph Layout"),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Renderer:"),
            shiny::selectInput(
              "graph_layout_renderer",
              label = NULL,
              choices = c("Plotly" = "plotly", "RGL" = "rglwidget"),
              selected = graph_ui$renderer_selected,
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Vertex Layout:"),
            shiny::selectInput(
              "graph_layout_vertex",
              label = NULL,
              choices = c("Sphere" = "sphere", "Point" = "point"),
              selected = graph_ui$vertex_layout,
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Vertex size:"),
            shiny::selectInput(
              "graph_layout_size",
              label = NULL,
              choices = stats::setNames(size_choices, size_choices),
              selected = graph_ui$size_selected,
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Color by:"),
            shiny::selectInput(
              "graph_layout_color_by",
              label = NULL,
              choices = graph_ui$color_choices,
              selected = graph_ui$color_selected,
              width = "205px"
            )
          ),
          if (identical(
            as.character(input$graph_layout_color_by %||% graph_ui$color_selected %||% ""),
            graph_solid_color_key
          )) {
            shiny::div(
              class = "gf-graph-row gf-graph-layout-row",
              shiny::span(class = "gf-graph-row-label", "Vertex color:"),
              shiny::selectInput(
                "graph_layout_vertex_color",
                label = NULL,
                choices = graph_ui$vertex_color_choices,
                selected = as.character(input$graph_layout_vertex_color %||% graph_ui$vertex_color_selected %||% graph_solid_color_default),
                width = "205px"
              )
            )
          } else {
            NULL
          },
          shiny::div(
            class = "gf-graph-row gf-graph-layout-row",
            shiny::span(class = "gf-graph-row-label", "Component:"),
            shiny::selectInput(
              "graph_layout_component",
              label = NULL,
              choices = graph_ui$component_choices,
              selected = graph_ui$component_selected,
              width = "205px"
            )
          ),
          if (nzchar(as.character(graph_ui$component_hint %||% ""))) {
            shiny::div(class = "gf-hint", graph_ui$component_hint)
          } else {
            NULL
          },
          if (is.data.frame(graph_ui$metadata_tbl) && nrow(graph_ui$metadata_tbl) > 0L) {
            shiny::tags$details(
              class = "gf-endpoint-metrics-details",
              shiny::tags$summary("Graph metadata"),
              build_html_table(graph_ui$metadata_tbl, empty_text = "No graph metadata available.")
            )
          } else {
            NULL
          }
        )
      }

      panels <- c(
        panels,
        list(
          bslib::accordion_panel(
            "Graphs",
            value = "workflow_graph_structure",
            graph_panel
          ),
          bslib::accordion_panel(
            "Subjects",
            value = "workflow_subject_structure",
            build_subject_panel_ui(subject_panel)
          ),
          if (isTRUE(occupation_panel$has_assets)) {
            bslib::accordion_panel(
              "Occupation Densities",
              value = "workflow_occupation_density",
              shiny::tagList(
                shiny::selectInput(
                  "occupation_density_set",
                  "Density set",
                  choices = occupation_panel$set_choices,
                  selected = occupation_panel$set_id
                ),
                shiny::selectizeInput(
                  "occupation_density_subject",
                  "Subject",
                  choices = occupation_panel$subject_choices,
                  selected = occupation_panel$subject_selected,
                  multiple = FALSE,
                  options = list(placeholder = "Choose subject")
                ),
                shiny::selectInput(
                  "occupation_density_method",
                  "Method",
                  choices = occupation_panel$method_choices,
                  selected = occupation_panel$method_selected
                ),
                shiny::selectInput(
                  "occupation_density_mode",
                  "Estimate",
                  choices = if (isTRUE(occupation_panel$is_precomputed_path)) {
                    c("Explore diffusion-time path" = "parameters")
                  } else {
                    c(
                      "CV-selected parameters" = "selected",
                      "Choose parameters" = "parameters"
                    )
                  },
                  selected = occupation_panel$mode
                ),
                shiny::conditionalPanel(
                  condition = "input.occupation_density_mode == 'selected'",
                  shiny::selectInput(
                    "occupation_density_selector",
                    "CV selector",
                    choices = c(
                      "Brier score" = "minimum_brier",
                      "Bernoulli negative log likelihood" = "minimum_bernoulli_nll"
                    ),
                    selected = occupation_panel$selector
                  )
                ),
                shiny::uiOutput("occupation_density_parameters"),
                shiny::actionButton(
                  "occupation_density_show",
                  "Show Density on Graph",
                  class = "btn-primary gf-btn-wide"
                ),
                if (!isTRUE(occupation_panel$is_precomputed_path)) {
                  shiny::actionButton(
                    "occupation_density_use_graph_k",
                    "Use Estimate's Graph k",
                    class = "btn-light gf-btn-wide"
                  )
                } else {
                  NULL
                },
                shiny::div(
                  class = "gf-density-status",
                  shiny::textOutput("occupation_density_status")
                )
              )
            )
          } else {
            NULL
          },
          bslib::accordion_panel(
            "Conditional Expectations",
            value = "workflow_condexp_structure",
            shiny::tagList(
              build_html_table(condexp_tbl, empty_text = "No conditional expectation assets found."),
              shiny::actionButton(
                "condexp_update_placeholder",
                "Update / Refit CondExp...",
                class = "btn-light gf-btn-wide"
              )
            )
          ),
          bslib::accordion_panel(
            "Basins",
            value = "workflow_basin_structure",
            shiny::tagList(
              if (isTRUE(basin_panel$has_sources)) {
                shiny::tagList(
                  shiny::selectInput(
                    "basin_source",
                    "Estimate source",
                    choices = basin_panel$choices,
                    selected = basin_panel$selected
                  )
                )
              } else {
                shiny::p(
                  class = "gf-hint",
                  "Apply an occupation density or load a conditional-expectation estimate first."
                )
              },
              shiny::actionButton(
                "basin_compute",
                "Compute & Open Basin Inspector",
                class = "btn-primary gf-btn-wide"
              ),
              shiny::div(
                class = "gf-density-status",
                shiny::textOutput("basin_status")
              ),
              shiny::tags$details(
                class = "gf-endpoint-metrics-details",
                shiny::tags$summary("Construction details"),
                shiny::p(
                  class = "gf-hint",
                  paste(
                    "Canonical gflow trajectory_flow in both directions;",
                    "CLOSEST; connected exact plateaus; all graph edges",
                    "admissible; backend primary assignments."
                  )
                )
              )
            )
          ),
          bslib::accordion_panel(
            "Endpoints",
            value = "workflow_endpoint_structure",
            shiny::tagList(
              if (is.list(endpoint_panel$draft_banner)) {
                shiny::div(
                  class = "gf-endpoint-section gf-endpoint-draft-banner",
                  shiny::div(class = "gf-hint", "Recovered unsaved working draft."),
                  shiny::div(
                    class = "gf-endpoint-actions",
                    shiny::actionButton("endpoint_recovered_continue", "Continue Editing", class = "btn-light btn-sm gf-btn-inline"),
                    shiny::actionButton("endpoint_recovered_save_snapshot", "Save Snapshot", class = "btn-light btn-sm gf-btn-inline"),
                    shiny::actionButton("endpoint_recovered_discard", "Discard Draft", class = "btn-light btn-sm gf-btn-inline")
                  )
                )
              } else {
                NULL
              },
              shiny::div(
                class = "gf-endpoint-section",
                build_working_endpoint_table(endpoint_working)
              ),
              shiny::div(
                class = "gf-endpoint-section",
                shiny::h6(class = "gf-graph-layout-head", "Vertex Inspector"),
                shiny::uiOutput("endpoint_vertex_inspector")
              ),
              shiny::div(
                class = "gf-endpoint-section",
                shiny::tags$details(
                  id = "endpoint_datasets_details",
                  class = "gf-endpoint-metrics-details gf-endpoint-candidates-details",
                  open = if (isTRUE(endpoint_datasets_open())) "open" else NULL,
                  ontoggle = "Shiny.setInputValue('endpoint_datasets_open', this.open, {priority: 'event'})",
                  shiny::tags$summary(
                    sprintf("Endpoint Datasets (%d)", as.integer(nrow(endpoint_rows)))
                  ),
                  build_endpoint_candidate_table(endpoint_rows)
                )
              ),
              shiny::div(
                class = "gf-endpoint-section",
                shiny::uiOutput("endpoint_candidate_metrics")
              ),
              shiny::hr(),
              shiny::tags$details(
                class = "gf-endpoint-metrics-details",
                shiny::tags$summary("Endpoint Layout"),
                shiny::div(
                  class = "gf-graph-row gf-graph-layout-row",
                  shiny::span(class = "gf-graph-row-label", "Label size:"),
                  shiny::sliderInput(
                    "endpoint_label_size",
                    label = NULL,
                    min = 0.4,
                    max = 3.0,
                    value = parse_scale_multiplier(input$endpoint_label_size %||% 1, default = 1),
                    step = 0.1,
                    width = "205px"
                  ),
                  shiny::span(
                    class = "gf-graph-dims",
                    sprintf(
                      "%.1fx",
                      parse_scale_multiplier(input$endpoint_label_size %||% 1, default = 1)
                    )
                  )
                ),
                shiny::div(
                  class = "gf-graph-row gf-graph-layout-row",
                  shiny::span(class = "gf-graph-row-label", "Label offset:"),
                  shiny::selectInput(
                    "endpoint_label_offset",
                    label = NULL,
                    choices = stats::setNames(
                      c(
                        "0x", "0.50x", "1x", "1.50x", "2x", "2.50x", "3x",
                        "3.50x", "4x", "4.50x", "5x"
                      ),
                      c(
                        "0x", "0.50x", "1x", "1.50x", "2x", "2.50x", "3x",
                        "3.50x", "4x", "4.50x", "5x"
                      )
                    ),
                    selected = as.character(input$endpoint_label_offset %||% "1x"),
                    width = "170px"
                  )
                ),
                shiny::div(
                  class = "gf-graph-row gf-graph-layout-row",
                  shiny::span(class = "gf-graph-row-label", "Marker size:"),
                  shiny::selectInput(
                    "endpoint_marker_size",
                    label = NULL,
                    choices = stats::setNames(
                      c("0.75x", "1x", "1.25x", "1.50x", "2x", "2.50x", "3x"),
                      c("0.75x", "1x", "1.25x", "1.50x", "2x", "2.50x", "3x")
                    ),
                    selected = as.character(input$endpoint_marker_size %||% "1x"),
                    width = "170px"
                  )
                ),
                shiny::div(
                  class = "gf-graph-row gf-graph-layout-row",
                  shiny::span(class = "gf-graph-row-label", "Marker color:"),
                  shiny::selectInput(
                    "endpoint_marker_color",
                    label = NULL,
                    choices = c(
                      "Red" = "#ef4444",
                      "Orange" = "#f97316",
                      "Gold" = "#eab308",
                      "Green" = "#22c55e",
                      "Teal" = "#14b8a6",
                      "Blue" = "#3b82f6",
                      "Purple" = "#8b5cf6",
                      "Pink" = "#ec4899",
                      "Black" = "#111827"
                    ),
                    selected = as.character(input$endpoint_marker_color %||% "#ef4444"),
                    width = "170px"
                  )
                )
              )
            )
          ),
          bslib::accordion_panel(
            "Arms",
            value = "workflow_arm_structure",
            shiny::tagList(
              if (is.list(arm_panel$draft_banner)) {
                shiny::div(
                  class = "gf-endpoint-section gf-endpoint-draft-banner",
                  shiny::div(class = "gf-hint", "Recovered unsaved working arm draft."),
                  shiny::div(
                    class = "gf-endpoint-actions",
                    shiny::actionButton("arm_recovered_continue", "Continue Editing", class = "btn-light btn-sm gf-btn-inline"),
                    shiny::actionButton("arm_recovered_save_snapshot", "Save Snapshot", class = "btn-light btn-sm gf-btn-inline"),
                    shiny::actionButton("arm_recovered_discard", "Discard Draft", class = "btn-light btn-sm gf-btn-inline")
                  )
                )
              } else {
                NULL
              },
              shiny::div(
                class = "gf-endpoint-section",
                build_working_arm_table(arm_working)
              ),
              shiny::div(
                class = "gf-endpoint-section",
                shiny::h6(class = "gf-graph-layout-head", "Arm Builder"),
                build_arm_builder_ui()
              ),
              if (is.list(arm_preview_variant())) shiny::div(
                class = "gf-endpoint-section",
                build_preview_arm_layout_ui(arm_preview_variant())
              ) else NULL,
              shiny::div(
                class = "gf-endpoint-section",
                shiny::tags$details(
                  class = "gf-endpoint-metrics-details",
                  open = if (isTRUE(arm_datasets_open())) "open" else NULL,
                  ontoggle = "Shiny.setInputValue('arm_datasets_open', this.open, {priority: 'event'})",
                  shiny::tags$summary(sprintf("Arm Datasets (%d)", as.integer(nrow(arm_rows)))),
                  build_arm_dataset_table(arm_rows)
                )
              ),
              shiny::div(
                class = "gf-endpoint-section",
                shiny::tags$details(
                  class = "gf-endpoint-metrics-details",
                  shiny::tags$summary("Arm Layout"),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::span(class = "gf-graph-row-label", "Arm color:"),
                    shiny::selectInput(
                      "arm_color",
                      label = NULL,
                      choices = c(
                        "Blue" = "#2563eb",
                        "Orange" = "#f97316",
                        "Green" = "#16a34a",
                        "Purple" = "#8b5cf6",
                        "Black" = "#111827"
                      ),
                      selected = as.character(input$arm_color %||% "#2563eb"),
                      width = "180px"
                    )
                  ),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::span(class = "gf-graph-row-label", "Tube opacity:"),
                    shiny::sliderInput(
                      "arm_tube_opacity",
                      label = NULL,
                      min = 0.10,
                      max = 1.0,
                      step = 0.05,
                      value = suppressWarnings(as.numeric(input$arm_tube_opacity %||% 0.35)),
                      width = "205px"
                    )
                  ),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::span(class = "gf-graph-row-label", "Path width:"),
                    shiny::sliderInput(
                      "arm_path_width",
                      label = NULL,
                      min = 1,
                      max = 8,
                      step = 1,
                      value = suppressWarnings(as.numeric(input$arm_path_width %||% 4)),
                      width = "205px"
                    )
                  ),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::span(class = "gf-graph-row-label", "Vertex size:"),
                    shiny::sliderInput(
                      "arm_vertex_size",
                      label = NULL,
                      min = 0.5,
                      max = 3.0,
                      step = 0.1,
                      value = suppressWarnings(as.numeric(input$arm_vertex_size %||% 1.0)),
                      width = "205px"
                    )
                  ),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::span(class = "gf-graph-row-label", "Label size:"),
                    shiny::sliderInput(
                      "arm_label_size",
                      label = NULL,
                      min = 0.5,
                      max = 2.5,
                      step = 0.1,
                      value = suppressWarnings(as.numeric(input$arm_label_size %||% 1.0)),
                      width = "205px"
                    )
                  ),
                  shiny::div(
                    class = "gf-graph-row gf-graph-layout-row",
                    shiny::tags$label(
                      class = "gf-endpoint-inline-check",
                      shiny::tags$input(
                        type = "checkbox",
                        id = "arm_show_labels",
                        checked = if (isTRUE(input$arm_show_labels %||% TRUE)) "checked" else NULL
                      ),
                      shiny::tags$span("Show arm labels")
                    )
                  )
                )
              )
            )
          ),
          bslib::accordion_panel("Analysis", value = "workflow_analysis", shiny::div(
            class = "gf-analysis-placeholder",
            shiny::p("Analysis tools section placeholder."),
            shiny::p("Future versions will expose downstream comparison, trajectory summaries, and reporting workflows.")
          ))
        )
      )
    } else {
      panels <- c(
        panels,
        list(
          bslib::accordion_panel("Graph(s) Construction", value = "workflow_graph", shiny::tagList(
            mod_graph_ui("graph"),
            shiny::hr(),
            mod_visualize_ui("viz")
          )),
          bslib::accordion_panel(
            "Conditional Expectation Estimation",
            value = "workflow_condexp",
            mod_condexp_ui("condexp")
          ),
          bslib::accordion_panel("Analysis", value = "workflow_analysis", shiny::div(
            class = "gf-analysis-placeholder",
            shiny::p("Analysis tools section placeholder."),
            shiny::p("Future versions will expose downstream comparison, trajectory summaries, and reporting workflows.")
          ))
        )
      )
      open.panels <- c(
        if (isTRUE(rv$project.show.data)) "workflow_data" else character(0),
        "workflow_graph"
      )
    }

    available_panels <- c(
      if (isTRUE(rv$project.show.data)) "workflow_data" else character(0),
      if (isTRUE(has_asset_views)) {
        c(
          "workflow_graph_structure",
          "workflow_subject_structure",
          if (isTRUE(occupation_panel$has_assets)) "workflow_occupation_density" else character(0),
          "workflow_condexp_structure",
          "workflow_basin_structure",
          "workflow_endpoint_structure",
          "workflow_arm_structure",
          "workflow_analysis"
        )
      } else {
        c("workflow_graph", "workflow_condexp", "workflow_analysis")
      }
    )
    if (isTRUE(has_asset_views)) {
      project_defaults <- project_open_selection_defaults(
        project_id = rv$project.id,
        manifest = manifest,
        graph_sets = graph_sets
      )
      default_open <- intersect(
        as.character(project_defaults$open_panels %||% character(0)),
        available_panels
      )
      if (length(default_open) > 0L) {
        open.panels <- default_open
      }
    }

    live_open <- input$workflow_accordion
    if (!is.null(live_open)) {
      mapped_live_open <- intersect(as.character(live_open %||% character(0)), available_panels)
      if (length(mapped_live_open) > 0L || length(live_open) < 1L) {
        open.panels <- mapped_live_open
      }
    } else {
      remembered_open <- workflow_open_panels()
      if (!is.null(remembered_open)) {
        mapped_open <- intersect(as.character(remembered_open), available_panels)
        if (length(mapped_open) > 0L || length(remembered_open) < 1L) {
          open.panels <- mapped_open
        }
      }
    }

    shiny::div(
      class = "gf-sidebar-panel gf-accordion-wrap",
      do.call(
        bslib::accordion,
        c(
          list(id = "workflow_accordion", open = open.panels, multiple = TRUE),
          panels
        )
      )
    )
  })

  output$run_monitor_panel <- shiny::renderUI({
    if (!isTRUE(rv$project.active) || !isTRUE(rv$run.monitor.visible)) {
      return(NULL)
    }

    shiny::div(
      class = "gf-sidebar-panel gf-run-monitor-panel",
      shiny::div(
        class = "gf-run-monitor-head",
        shiny::strong("Run Monitor"),
        shiny::actionButton("hide_run_monitor", "Hide", class = "btn-light btn-sm")
      ),
      shiny::div(
        class = "gf-status-block",
        shiny::verbatimTextOutput("run_monitor")
      )
    )
  })

  output$project_middle_actions <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(NULL)
    }

    button_id <- if (isTRUE(rv$project.show.data)) "hide_data_section" else "add_data_section"
    button_label <- if (isTRUE(rv$project.show.data)) "Hide Data Section" else "Add Data"

    shiny::div(
      class = "gf-sidebar-panel gf-middle-actions",
      shiny::actionButton(
        button_id,
        button_label,
        class = "btn-light gf-btn-wide"
      )
    )
  })

  output$workspace_actions <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(NULL)
    }

    shiny::div(
      class = "gf-sidebar-panel gf-workspace-actions-panel",
      if (isTRUE(rv$project.dirty)) {
        shiny::div(class = "gf-dirty-flag", "Unsaved changes")
      } else {
        shiny::div(class = "gf-dirty-flag gf-dirty-clean", "All changes saved")
      },
      shiny::actionButton(
        "project_settings",
        "Settings",
        class = "btn-light gf-btn-wide"
      ),
      shiny::actionButton(
        "save_project",
        "Save Project",
        class = "btn-light gf-btn-wide"
      ),
      shiny::actionButton(
        "exit_project",
        "Exit Project",
        class = "btn-outline-secondary gf-btn-wide"
      )
    )
  })

  shiny::observeEvent(input$project_settings, {
    if (!isTRUE(rv$project.active)) {
      return()
    }

    manifest <- active_manifest()
    if (!is.list(manifest)) {
      shiny::showNotification("Project manifest not found.", type = "error")
      return()
    }

    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()

    graph_choices <- c("None" = "")
    if (length(graph_sets) > 0L) {
      ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
      labels <- vapply(graph_sets, function(gs) as.character(gs$label %||% gs$id %||% ""), character(1))
      keep <- nzchar(ids)
      if (any(keep)) {
        graph_choices <- c(graph_choices, stats::setNames(ids[keep], labels[keep]))
      }
    }

    condexp_choices <- c("None" = "")
    if (length(condexp_sets) > 0L) {
      ids <- vapply(condexp_sets, function(cs) as.character(cs$id %||% ""), character(1))
      labels <- vapply(condexp_sets, function(cs) as.character(cs$label %||% cs$id %||% ""), character(1))
      keep <- nzchar(ids)
      if (any(keep)) {
        condexp_choices <- c(condexp_choices, stats::setNames(ids[keep], labels[keep]))
      }
    }

    endpoint_choices <- c("None" = "")
    if (length(endpoint_runs) > 0L) {
      ids <- vapply(endpoint_runs, function(ep) as.character(ep$id %||% ""), character(1))
      labels <- vapply(endpoint_runs, function(ep) as.character(ep$label %||% ep$id %||% ""), character(1))
      keep <- nzchar(ids)
      if (any(keep)) {
        endpoint_choices <- c(endpoint_choices, stats::setNames(ids[keep], labels[keep]))
      }
    }

    profile_choices <- c(
      "workspace" = "workspace",
      "symptoms_restart" = "symptoms_restart",
      "agp_restart" = "agp_restart",
      "custom" = "custom"
    )

    settings_project_name <- scalar_chr(manifest$project_name %||% rv$project.name %||% "", default = "")
    settings_project_root <- scalar_chr(manifest$project_root %||% "", default = "")
    settings_profile <- scalar_chr(manifest$profile %||% "workspace", default = "workspace")
    settings_default_graph_set <- scalar_chr(defaults$graph_set_id %||% "", default = "")
    settings_reference_graph_set <- scalar_chr(
      defaults$reference_graph_set_id %||% defaults$graph_set_id %||% "",
      default = ""
    )
    settings_reference_k <- scalar_int(defaults$reference_k, default = NA_integer_)
    settings_reference_reason <- scalar_chr(defaults$reference_reason %||% "", default = "")
    settings_default_condexp_set <- scalar_chr(defaults$condexp_set_id %||% "", default = "")
    settings_default_endpoint_run <- scalar_chr(defaults$endpoint_run_id %||% "", default = "")

    shiny::showModal(
      shiny::modalDialog(
        title = "Project Settings",
        easyClose = TRUE,
        size = "l",
        shiny::textInput(
          "settings_project_name",
          "Project Name",
          value = settings_project_name
        ),
        shiny::textInput(
          "settings_project_root",
          "Project Root",
          value = settings_project_root
        ),
        shiny::selectInput(
          "settings_profile",
          "Profile",
          choices = profile_choices,
          selected = settings_profile
        ),
        shiny::hr(),
        shiny::selectInput(
          "settings_default_graph_set",
          "Default Graph Set",
          choices = graph_choices,
          selected = settings_default_graph_set
        ),
        shiny::selectInput(
          "settings_reference_graph_set",
          "Reference Graph Set",
          choices = graph_choices,
          selected = settings_reference_graph_set
        ),
        shiny::textInput(
          "settings_reference_k",
          "Reference k",
          value = if (is.finite(settings_reference_k)) as.character(settings_reference_k) else ""
        ),
        shiny::textInput(
          "settings_reference_reason",
          "Reference Reason",
          value = settings_reference_reason
        ),
        shiny::selectInput(
          "settings_default_condexp_set",
          "Default Conditional Expectation Set",
          choices = condexp_choices,
          selected = settings_default_condexp_set
        ),
        shiny::selectInput(
          "settings_default_endpoint_run",
          "Default Endpoint Run",
          choices = endpoint_choices,
          selected = settings_default_endpoint_run
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("save_project_settings", "Save Settings", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_project_settings, {
    if (!isTRUE(rv$project.active)) {
      return()
    }

    ctx <- active_project_context()
    if (is.null(ctx)) {
      shiny::showNotification("Active project context not available.", type = "error")
      return()
    }

    payload <- load_or_init_active_manifest(ctx)
    manifest <- payload$manifest
    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()

    project_name <- trimws(scalar_chr(input$settings_project_name %||% "", default = ""))
    if (!nzchar(project_name)) {
      project_name <- scalar_chr(rv$project.name %||% "Untitled Project", default = "Untitled Project")
    }
    project_root <- trimws(scalar_chr(input$settings_project_root %||% "", default = ""))
    profile <- trimws(scalar_chr(input$settings_profile %||% "workspace", default = "workspace"))
    if (!nzchar(profile)) {
      profile <- "workspace"
    }

    defaults$graph_set_id <- scalar_chr(input$settings_default_graph_set %||% "", default = "")
    defaults$reference_graph_set_id <- scalar_chr(input$settings_reference_graph_set %||% "", default = "")
    ref_k <- suppressWarnings(as.integer(input$settings_reference_k))
    defaults$reference_k <- if (is.finite(ref_k) && ref_k > 0L) as.integer(ref_k) else NA_integer_
    ref_reason <- trimws(scalar_chr(input$settings_reference_reason %||% "", default = ""))
    defaults$reference_reason <- if (nzchar(ref_reason)) ref_reason else NA_character_
    defaults$condexp_set_id <- scalar_chr(input$settings_default_condexp_set %||% "", default = "")
    defaults$endpoint_run_id <- scalar_chr(input$settings_default_endpoint_run %||% "", default = "")

    manifest$project_name <- project_name
    manifest$project_root <- if (nzchar(project_root)) project_root else NA_character_
    manifest$profile <- profile
    manifest$defaults <- defaults

    payload$manifest <- manifest
    payload$reg$label[[payload$idx]] <- project_name
    payload$reg$project_root[[payload$idx]] <- if (nzchar(project_root)) project_root else NA_character_
    if (profile %in% c("symptoms_restart", "agp_restart", "custom")) {
      payload$reg$origin[[payload$idx]] <- sprintf("registered:%s", profile)
    }

    ok <- tryCatch(save_active_manifest(payload), error = function(e) e)
    if (inherits(ok, "error")) {
      shiny::showNotification(
        sprintf("Failed to save project settings: %s", conditionMessage(ok)),
        type = "error"
      )
      set_run_monitor_note(sprintf("Project settings save failed: %s", conditionMessage(ok)))
      return()
    }

    rv$project.name <- project_name
    rv$project.origin <- scalar_chr(payload$reg$origin[[payload$idx]] %||% rv$project.origin %||% "workspace", default = "workspace")
    shiny::removeModal()
    set_run_monitor_note("Project settings saved.")
    shiny::showNotification("Project settings saved.", type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_project, {
    st <- endpoint_panel_state()
    working <- if (is.list(st)) st$working else NULL
    if (working_endpoint_needs_replace_prompt(working)) {
      show_endpoint_project_action_modal("save_project")
      return()
    }
    ok <- save_current_project()
    if (isTRUE(ok)) {
      shiny::showNotification(
        sprintf("Project '%s' saved.", rv$project.name %||% "Untitled Project"),
        type = "message"
      )
    } else {
      shiny::showNotification(
        "Unable to save current project.",
        type = "error"
      )
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$exit_project, {
    if (!isTRUE(rv$project.active)) {
      return()
    }

    st <- endpoint_panel_state()
    working <- if (is.list(st)) st$working else NULL
    if (working_endpoint_needs_replace_prompt(working)) {
      show_endpoint_project_action_modal("exit_project")
      return()
    }

    if (!isTRUE(rv$project.dirty)) {
      close_project()
      return()
    }

    shiny::showModal(
      shiny::modalDialog(
        title = "Unsaved Changes",
        easyClose = FALSE,
        shiny::p("Do you want to save current work before leaving this project?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("exit_without_save", "Exit Without Saving", class = "btn-secondary"),
          shiny::actionButton("save_and_exit", "Save and Exit", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$exit_without_save, {
    shiny::removeModal()
    close_project()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_and_exit, {
    ok <- save_current_project()
    shiny::removeModal()
    if (isTRUE(ok)) {
      close_project()
    } else {
      shiny::showNotification("Unable to save current project.", type = "error")
    }
  }, ignoreInit = TRUE)

  output$chip_backend <- shiny::renderUI({
    backend <- if (requireNamespace("gflow", quietly = TRUE)) {
      sprintf("R + gflow backend: gflow %s", as.character(utils::packageVersion("gflow")))
    } else {
      "R + gflow backend: gflow not installed"
    }
    shiny::span(class = "gf-chip", backend)
  })

  output$chip_renderer <- shiny::renderUI({
    rr <- reference_renderer_state()

    if (!isTRUE(rv$project.active)) {
      return(shiny::span(class = "gf-chip", "3D renderer: waiting for project"))
    }

    mode_label <- switch(
      rr$effective,
      rglwidget = "RGL",
      plotly = "Plotly",
      none = "none",
      "unknown"
    )
    req_label <- switch(
      rr$requested,
      rglwidget = "RGL",
      plotly = "Plotly",
      rr$requested
    )

    shiny::span(
      class = "gf-chip",
      sprintf("3D renderer: %s [%s]", mode_label, req_label)
    )
  })

  output$chip_project <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(shiny::span(class = "gf-chip", "Project: not started"))
    }
    suffix <- if (isTRUE(rv$project.dirty)) " *" else ""
    shiny::span(
      class = "gf-chip",
      sprintf("Project: %s%s", rv$project.name %||% "Untitled Project", suffix)
    )
  })

  output$run_monitor <- shiny::renderText({
    note.msg <- trimws(as.character(rv$run.monitor.note %||% ""))
    if (!nzchar(note.msg)) {
      return("No recent job message.")
    }
    sprintf("Job: %s", note.msg)
  })

  output$workspace_view <- shiny::renderUI({
    if (isTRUE(quadform_project_active())) {
      st <- quadform_view_state()

      metric_table <- function(metrics) {
        if (!is.data.frame(metrics) || nrow(metrics) < 1L) {
          return(data.frame())
        }
        keep <- intersect(
          c("target", "status", "rel_rms_error", "rel_abs_error_median", "rel_abs_error_q95", "pearson_cor", "spearman_cor"),
          names(metrics)
        )
        out <- metrics[, keep, drop = FALSE]
        for (cc in names(out)) {
          if (is.numeric(out[[cc]])) {
            out[[cc]] <- ifelse(is.finite(out[[cc]]), formatC(out[[cc]], digits = 4, format = "fg"), "")
          } else {
            out[[cc]] <- as.character(out[[cc]])
          }
        }
        out
      }

      header <- if (is.list(st) && !is.null(st$selected_row) && is.data.frame(st$selected_row)) {
        row <- st$selected_row
        sprintf(
          "%s / n=%s / seed=%s / %s / %s",
          as.character(row$surface[[1]] %||% row$dataset_id[[1]] %||% ""),
          as.character(row$n[[1]] %||% ""),
          as.character(row$seed[[1]] %||% ""),
          as.character(row$graph_family[[1]] %||% ""),
          as.character(row$stage[[1]] %||% "")
        )
      } else {
        "Quadform Benchmark"
      }

      status <- as.character(st$status %||% "error")
      message <- as.character(st$message %||% "")
      plot_body <- if (identical(status, "ok")) {
        if (!requireNamespace("plotly", quietly = TRUE)) {
          shiny::div(
            class = "gf-viewer-canvas",
            shiny::div(
              class = "gf-viewer-overlay",
              shiny::h3("Quadform Benchmark"),
              shiny::p("Install `plotly` to enable the two-panel 3D benchmark viewer.")
            )
          )
        } else {
          shiny::div(
            class = "gf-quadform-grid",
            shiny::div(class = "gf-quadform-panel", plotly::plotlyOutput("quadform_original_plot", height = "58vh")),
            shiny::div(class = "gf-quadform-panel", plotly::plotlyOutput("quadform_graph_plot", height = "58vh"))
          )
        }
      } else if (identical(status, "missing_layout")) {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Weighted Layout Missing"),
            shiny::p(message),
            shiny::p("Use the sidebar action to generate a weighted GRIP layout from the saved graph-stage asset.")
          )
        )
      } else if (identical(status, "missing_graph")) {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Graph Asset Missing"),
            shiny::p(message),
            shiny::p("gflowui will not reconstruct benchmark graph assets in this implementation pass.")
          )
        )
      } else {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Quadform Benchmark"),
            shiny::p(if (nzchar(message)) message else sprintf("State: %s", status))
          )
        )
      }

      shiny::div(
        class = "gf-reference-view gf-reference-view-plain",
        shiny::div(
          class = "gf-quadform-header",
          shiny::h4(header),
          if (is.list(st) && nzchar(as.character(st$key %||% ""))) {
            shiny::p(class = "gf-hint", sprintf("graph-stage key: %s", as.character(st$key)))
          } else {
            NULL
          }
        ),
        plot_body,
        if (is.list(st) && is.data.frame(st$metrics) && nrow(st$metrics) > 0L) {
          shiny::div(
            class = "gf-quadform-metrics",
            shiny::h5("Metrics"),
            build_html_table(metric_table(st$metrics), empty_text = "No metrics found.")
          )
        } else {
          NULL
        },
        if (is.list(st) && nzchar(as.character(st$layout_source %||% ""))) {
          shiny::p(
            class = "gf-mode-note",
            sprintf("Layout source: %s", as.character(st$layout_source))
          )
        } else {
          NULL
        }
      )
    } else {
    rr <- reference_renderer_state()
    st <- rr$st
    mode_note <- as.character(rr$mode_note %||% "")
    if (length(mode_note) < 1L || !nzchar(mode_note[[1]])) {
      mode_note <- ""
    } else {
      mode_note <- mode_note[[1]]
    }
    component_note <- as.character(rr$component_note %||% "")
    if (length(component_note) < 1L || !nzchar(component_note[[1]])) {
      component_note <- ""
    } else {
      component_note <- component_note[[1]]
    }

    build_rgl_legend <- function(rr_state, st_state) {
      if (!identical(rr_state$effective, "rglwidget")) {
        return(NULL)
      }
      if (identical(as.character(rr_state$color_mode %||% "source"), "solid")) {
        return(NULL)
      }
      src_key <- as.character(rr_state$src_key %||% st_state$default_key %||% "")
      if (!(src_key %in% names(st_state$sources %||% list()))) {
        return(NULL)
      }
      src <- st_state$sources[[src_key]]
      nn <- suppressWarnings(as.integer(st_state$n_vertices %||% length(src$values)))
      keep_idx <- suppressWarnings(as.integer(rr_state$keep_idx %||% seq_len(max(0L, nn))))
      keep_idx <- keep_idx[is.finite(keep_idx) & keep_idx >= 1L & keep_idx <= nn]
      if (length(keep_idx) < 1L) {
        keep_idx <- seq_len(max(0L, nn))
      }
      values_view <- src$values[keep_idx]
      src_type <- as.character(src$type %||% "")
      col_tbl <- character(0)
      labs <- character(0)

      if (identical(src_type, "categorical")) {
        pal_info <- categorical_palette(
          values_view,
          source_key = src_key,
          source_label = src$label %||% src_key
        )
        lev <- pal_info$levels
        if (length(lev) < 1L) {
          return(NULL)
        }
        col_tbl <- unname(as.character(pal_info$colors[lev]))
        counts <- table(factor(pal_info$values, levels = lev))
        labs <- sprintf("%s (%s)", lev, format(as.integer(counts), big.mark = ","))
      } else if (identical(src_type, "numeric")) {
        if (!requireNamespace("gflow", quietly = TRUE)) {
          return(NULL)
        }
        vals <- suppressWarnings(as.numeric(values_view))
        vals <- vals[is.finite(vals)]
        if (length(vals) < 2L) {
          return(NULL)
        }
        quantize_for_legend <- tryCatch(
          utils::getFromNamespace("quantize.for.legend", "gflow"),
          error = function(e) NULL
        )
        if (!is.function(quantize_for_legend)) {
          return(NULL)
        }
        q <- tryCatch(
          quantize_for_legend(
            y = vals,
            quantize.method = "uniform",
            quantize.wins.p = 0.01,
            quantize.round = FALSE,
            quantize.dig.lab = 2,
            start = 1 / 6,
            end = 0,
            n.levels = 10
          ),
          error = function(e) NULL
        )
        if (is.null(q) || length(q$y.col.tbl %||% character(0)) < 1L) {
          return(NULL)
        }
        col_tbl <- unname(as.character(q$y.col.tbl))
        labs <- as.character(q$legend.labs %||% names(q$y.col.tbl))
        if (length(labs) != length(col_tbl)) {
          labs <- as.character(names(q$y.col.tbl))
        }
      } else {
        return(NULL)
      }

      items <- lapply(seq_along(col_tbl), function(ii) {
        shiny::div(
          class = "gf-rgl-legend-item",
          shiny::span(
            class = "gf-rgl-legend-swatch",
            style = sprintf("background:%s;", col_tbl[[ii]])
          ),
          shiny::span(class = "gf-rgl-legend-label", labs[[ii]])
        )
      })
      if (identical(src_key, "basin_active") &&
          identical(
            as.character(basin_display_settings$display_mode %||% "both"),
            "both"
          )) {
        basin.display <- basin_result()
        minimum.rows <- if (
          is.list(basin.display) &&
            is.data.frame(basin.display$table)
        ) {
          basin.display$table[
            basin.display$table$type == "min" &
              basin.display$table$selected,
            ,
            drop = FALSE
          ]
        } else {
          data.frame()
        }
        if (nrow(minimum.rows) > 0L) {
          items <- c(items, lapply(seq_len(nrow(minimum.rows)), function(ii) {
            row <- minimum.rows[ii, , drop = FALSE]
            shiny::div(
              class = "gf-rgl-legend-item",
              shiny::span(
                class = paste(
                  "gf-rgl-legend-swatch",
                  "gf-rgl-legend-swatch-secondary"
                ),
                style = sprintf(
                  "border-color:%s;",
                  as.character(row$color)
                )
              ),
              shiny::span(
                class = "gf-rgl-legend-label",
                sprintf(
                  "%s marker (%s)",
                  as.character(row$display.label),
                  format(
                    as.integer(row$primary.support.size),
                    big.mark = ","
                  )
                )
              )
            )
          }))
        }
      }
      shiny::div(
        class = "gf-rgl-legend",
        shiny::div(
          class = "gf-rgl-legend-title",
          as.character(src$label %||% src_key)
        ),
        items
      )
    }

    if (!is.null(st$error)) {
      return(
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Reference Graph View"),
            shiny::p(st$error)
          )
        )
      )
    }

    view_body <- if (identical(rr$effective, "rglwidget")) {
      if (!isTRUE(rr$rgl_ready)) {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Graph View"),
            shiny::p("Install `rgl` to enable live WebGL rendering.")
          )
        )
      } else {
        shiny::div(
          class = "gf-rgl-wrap",
          rgl::rglwidgetOutput(paste0("reference_rgl_", rgl_gen()), width = "100%", height = "78vh"),
          build_rgl_legend(rr, st)
        )
      }
    } else if (identical(rr$effective, "plotly")) {
      if (!isTRUE(rr$plotly_ready)) {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Graph View"),
            shiny::p("Install `plotly` to enable interactive 3D rendering.")
          )
        )
      } else {
        plotly::plotlyOutput("reference_plot", height = "78vh")
      }
    } else {
      shiny::div(
        class = "gf-viewer-canvas",
        shiny::div(
          class = "gf-viewer-overlay",
          shiny::h3("Graph View"),
          shiny::p("No renderer is available. Install `rgl` or `plotly`.")
        )
      )
    }

    notes <- unique(c(mode_note, component_note))
    notes <- notes[nzchar(notes)]
    note.body <- if (length(notes) > 0L) {
      shiny::p(class = "gf-mode-note", paste(notes, collapse = " "))
    } else {
      NULL
    }

    shiny::div(
      class = "gf-reference-view gf-reference-view-plain",
      shiny::div(
        id = "gf_reference_split",
        class = "gf-reference-split",
        style = sprintf(
          "--gf-general-inspector-width:%dpx;",
          as.integer(basin_display_settings$inspector_width %||% 620)
        ),
        shiny::div(
          class = "gf-reference-graph-pane",
          view_body,
          note.body
        ),
        shiny::div(
          id = "gf_general_inspector_resize",
          class = "gf-general-inspector-resize",
          role = "separator",
          tabindex = "0",
          `aria-label` = "Resize General Inspector",
          `aria-orientation` = "vertical",
          title = paste(
            "Drag to resize the General Inspector.",
            "Use arrow keys for precise adjustment."
          ),
          shiny::span(class = "gf-general-inspector-resize-grip")
        ),
        shiny::tags$aside(
          id = "gf_general_inspector",
          class = "gf-general-inspector-pane",
          `aria-label` = "General Inspector",
          shiny::div(
            class = "gf-general-inspector-header",
            shiny::h3("General Inspector"),
            shiny::p("Visualization and diagnostic panels")
          ),
          shiny::div(
            class = "gf-general-inspector-stack",
            shiny::uiOutput("basin_inspector_ui"),
            shiny::uiOutput("basin_plot_workspace_ui")
          )
        )
      )
    )
    }
  })
}
