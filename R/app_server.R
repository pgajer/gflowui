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
  if (requireNamespace("rgl", quietly = TRUE) && !isTRUE(old_rgl_use_null)) {
    options(rgl.useNULL = TRUE)
    session$onSessionEnded(function() {
      options(rgl.useNULL = old_rgl_use_null)
    })
  }

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
    rv$project.dirty <- !identical(current_state_signature(), rv$project.baseline.signature)
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

  discover_layout_variants_for_graph <- function(graph_path, set_id = "", k_values = integer(0)) {
    gp <- as.character(graph_path %||% "")
    if (!nzchar(gp) || !file.exists(gp)) {
      return(list())
    }

    root <- dirname(gp)
    html_files <- list.files(
      root,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.html?$",
      ignore.case = TRUE
    )
    html_files <- html_files[file.exists(html_files)]
    if (length(html_files) < 1L) {
      return(list())
    }
    if (length(html_files) > 300L) {
      html_files <- html_files[seq_len(300L)]
    }

    sid <- tolower(as.character(set_id %||% ""))
    graph_token <- tolower(tools::file_path_sans_ext(basename(gp)))
    k_vals <- suppressWarnings(as.integer(k_values))
    k_vals <- k_vals[is.finite(k_vals)]

    score_one <- function(path) {
      low <- tolower(path)
      base <- tolower(basename(path))
      sc <- 0
      if (nzchar(sid) && (grepl(sid, low, fixed = TRUE) || grepl(sub("^top", "hv", sid), low, fixed = TRUE))) {
        sc <- sc + 4
      }
      if (nzchar(graph_token) && grepl(graph_token, low, fixed = TRUE)) {
        sc <- sc + 3
      }
      if (length(k_vals) > 0L && any(vapply(k_vals, function(k) grepl(sprintf("k0*%d", k), base, perl = TRUE), logical(1)))) {
        sc <- sc + 3
      }
      if (grepl("index\\.html?$", base, ignore.case = TRUE)) {
        sc <- sc + 1
      }
      sc
    }

    ord <- order(-vapply(html_files, score_one, numeric(1)), nchar(html_files), html_files)
    html_files <- html_files[ord]
    if (length(html_files) > 120L) {
      html_files <- html_files[seq_len(120L)]
    }
    gflowui_infer_layout_variants(html_files)
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
      renderer = tolower(as.character(input$graph_layout_renderer %||% "rglwidget")),
      vertex_layout = tolower(as.character(input$graph_layout_vertex %||% "sphere")),
      vertex_size = as.character(input$graph_layout_size %||% "1x"),
      color_by = as.character(input$graph_layout_color_by %||% "vertex_degree"),
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

      existing_variants <- if (is.list(graph_obj$layout_assets) && is.list(graph_obj$layout_assets$variants)) {
        graph_obj$layout_assets$variants
      } else {
        list()
      }
      legacy_variant_paths <- c(graph_obj$html_file, graph_obj$html_files, graph_obj$html_candidates)
      legacy_variants <- gflowui_infer_layout_variants(legacy_variant_paths)
      discovered_variants <- discover_layout_variants_for_graph(
        graph_path = graph_path,
        set_id = set_id,
        k_values = k_vals
      )
      layout_variants <- c(existing_variants, legacy_variants, discovered_variants)

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
        layout_assets = list(presets = layout_presets, variants = layout_variants),
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
  workflow_open_panels <- shiny::reactiveVal(NULL)
  ## Generation counter: incremented whenever an endpoint-label
  ## parameter changes so the renderUI emits a *new* output ID for
  ## the rglwidget, forcing the browser to destroy the old WebGL
  ## context and create a fresh one (avoids stale-texture black
  ## rectangles on in-place widget updates).
  rgl_gen <- shiny::reactiveVal(0L)
  rgl_last_output_id <- shiny::reactiveVal(NULL)
  shiny::observeEvent(rv$project.id, {
    endpoint_overlay_selection(character(0))
    workflow_open_panels(NULL)
    rgl_last_output_id(NULL)
    rgl_gen(0L)
  }, ignoreInit = TRUE)
  shiny::observeEvent(
    list(input$endpoint_label_size, input$endpoint_label_offset,
         input$endpoint_marker_size, input$endpoint_marker_color),
    {
      rgl_gen(shiny::isolate(rgl_gen()) + 1L)
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(endpoint_overlay_selection(), {
    rgl_gen(shiny::isolate(rgl_gen()) + 1L)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$workflow_accordion, {
    if (!isTRUE(rv$project.active) || is.null(input$workflow_accordion)) {
      return()
    }
    vals <- as.character(input$workflow_accordion %||% character(0))
    vals <- unique(vals[nzchar(vals)])
    workflow_open_panels(vals)
  }, ignoreInit = TRUE)

  read_csv_safely <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp) || !file.exists(pp)) {
      return(NULL)
    }
    tryCatch(utils::read.csv(pp, stringsAsFactors = FALSE), error = function(e) NULL)
  }

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

  endpoint_panel_state <- shiny::reactive({
    if (!isTRUE(rv$project.active)) {
      return(list(rows = data.frame(), run_id = "", run_label = ""))
    }
    manifest <- active_manifest()
    if (!is.list(manifest)) {
      return(list(rows = data.frame(), run_id = "", run_label = ""))
    }

    gs <- graph_structure_state()
    k_pref <- if (is.list(gs) && is.null(gs$error)) suppressWarnings(as.integer(gs$k_selected)) else NA_integer_
    run <- resolve_endpoint_run(manifest, preferred_k = k_pref)
    if (!is.list(run)) {
      return(list(rows = data.frame(), run_id = "", run_label = ""))
    }

    rows <- endpoint_rows_for_run(run)
    if (!is.data.frame(rows) || nrow(rows) < 1L) {
      return(list(
        rows = data.frame(),
        run_id = as.character(run$id %||% ""),
        run_label = as.character(run$label %||% run$id %||% "")
      ))
    }

    ord <- order(!is.finite(rows$k), rows$k, rows$method, rows$key)
    rows <- rows[ord, , drop = FALSE]
    rows$selected <- rows$key %in% endpoint_overlay_selection()

    list(
      rows = rows,
      run_id = as.character(run$id %||% ""),
      run_label = as.character(run$label %||% run$id %||% "")
    )
  })

  shiny::observe({
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    if (nrow(rows) < 1L) {
      endpoint_overlay_selection(character(0))
      return()
    }

    prev <- endpoint_overlay_selection()
    sel <- character(0)
    for (ii in seq_len(nrow(rows))) {
      in_id <- as.character(rows$input_id[[ii]] %||% "")
      key <- as.character(rows$key[[ii]] %||% "")
      if (!nzchar(in_id) || !nzchar(key)) {
        next
      }
      vv <- input[[in_id]]
      if (isTRUE(vv)) {
        sel <- c(sel, key)
      } else if (is.null(vv) && key %in% prev) {
        sel <- c(sel, key)
      }
    }
    endpoint_overlay_selection(unique(sel))
  })

  endpoint_overlay_active <- shiny::reactive({
    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    if (nrow(rows) < 1L) {
      return(list(vertices = integer(0), labels = structure(character(0), names = character(0))))
    }

    selected <- intersect(endpoint_overlay_selection(), as.character(rows$key))
    if (length(selected) < 1L) {
      return(list(vertices = integer(0), labels = structure(character(0), names = character(0))))
    }

    rows_sel <- rows[rows$key %in% selected, , drop = FALSE]
    vertices_all <- integer(0)
    label_lookup <- structure(character(0), names = character(0))

    for (ii in seq_len(nrow(rows_sel))) {
      res <- read_endpoint_labels_from_row(rows_sel[ii, , drop = FALSE])
      vv <- suppressWarnings(as.integer(res$vertices %||% integer(0)))
      vv <- vv[is.finite(vv) & vv > 0L]
      if (length(vv) < 1L) {
        next
      }
      labs <- as.character(res$labels %||% character(0))
      if (length(labs) != length(vv)) {
        labs <- rep("", length(vv))
      }
      labs[is.na(labs)] <- ""
      vertices_all <- c(vertices_all, vv)
      for (jj in seq_along(vv)) {
        nm <- as.character(vv[[jj]])
        if (!nm %in% names(label_lookup) || !nzchar(label_lookup[[nm]])) {
          label_lookup[[nm]] <- labs[[jj]]
        }
      }
    }

    vertices_all <- sort(unique(suppressWarnings(as.integer(vertices_all))))
    vertices_all <- vertices_all[is.finite(vertices_all) & vertices_all > 0L]
    list(vertices = vertices_all, labels = label_lookup)
  })

  reference_view_state <- shiny::reactive({
    if (!isTRUE(rv$project.active)) {
      return(list(error = "No project selected."))
    }

    manifest <- active_manifest()
    if (is.null(manifest)) {
      return(list(error = "Project manifest not found."))
    }

    selected_set <- scalar_chr(input$graph_data_type %||% "", default = "")
    selected_k <- scalar_int(input$graph_k, default = NA_integer_)

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
    n_vertices <- length(adj_list)
    if (n_vertices < 1L) {
      return(list(error = "Reference graph has no vertices."))
    }

    component_ids <- rep.int(1L, n_vertices)
    comp_res <- tryCatch(
      gflow::graph.connected.components(adj_list),
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
    add_source <- function(key, label, values, type = c("numeric", "categorical")) {
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
        values = vv
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
        type = if (identical(as.character(src$type %||% "numeric"), "categorical")) "categorical" else "numeric"
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
      components = components,
      graph_set = spec$graph_set,
      sources = sources,
      choices = choices,
      default_key = default_key
    )
  })

  reference_renderer_state <- shiny::reactive({
    st <- reference_view_state()
    manifest <- active_manifest()
    spec <- NULL
    if (is.list(manifest)) {
      spec <- resolve_reference_spec(
        manifest,
        preferred_set_id = scalar_chr(input$graph_data_type %||% st$set_id %||% "", default = ""),
        preferred_k = scalar_int(input$graph_k %||% st$k_actual %||% NA_integer_, default = NA_integer_)
      )
    }
    if (is.list(spec) && is.finite(suppressWarnings(as.integer(st$k_actual)))) {
      spec$k_ref <- suppressWarnings(as.integer(st$k_actual))
    }

    requested <- tolower(trimws(as.character(input$graph_layout_renderer %||% "rglwidget")))
    if (identical(requested, "rgl")) {
      requested <- "rglwidget"
    }
    if (!requested %in% c("rglwidget", "html", "plotly")) {
      requested <- "rglwidget"
    }

    src_key <- as.character(input$graph_layout_color_by %||% st$default_key %||% "")
    if (!(src_key %in% names(st$sources %||% list()))) {
      src_key <- as.character(st$default_key %||% "")
    }
    color_label <- if (nzchar(src_key) && src_key %in% names(st$sources %||% list())) {
      as.character(st$sources[[src_key]]$label %||% src_key)
    } else {
      ""
    }

    vertex_mode <- tolower(trimws(as.character(input$graph_layout_vertex %||% "sphere")))
    if (!vertex_mode %in% c("sphere", "point")) {
      vertex_mode <- "sphere"
    }
    size_raw <- as.character(input$graph_layout_size %||% "1x")
    size_mult <- suppressWarnings(as.numeric(gsub("[^0-9.]+", "", size_raw)))
    if (!is.finite(size_mult) || size_mult <= 0) {
      size_mult <- 1
    }
    size_label <- sprintf("%sx", format(size_mult, scientific = FALSE, trim = TRUE))
    component_mode <- tolower(trimws(as.character(input$graph_layout_component %||% "all")))
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

    html_candidates <- if (is.list(manifest) && is.list(spec)) {
      discover_reference_html_candidates(
        manifest = manifest,
        spec = spec,
        color_label = color_label,
        extra_tokens = c(vertex_mode, size_label, requested),
        requested_renderer = requested,
        vertex_mode = vertex_mode,
        size_label = size_label
      )
    } else {
      character(0)
    }

    project_root <- as.character(manifest$project_root %||% NA_character_)
    html_choices <- html_candidate_choices(html_candidates, project_root = project_root)
    html_selected <- if (length(html_choices) > 0L) unname(html_choices)[1] else ""

    plotly_ready <- requireNamespace("plotly", quietly = TRUE)
    rgl_ready <- requireNamespace("rgl", quietly = TRUE)
    effective <- requested
    note <- NULL

    if (identical(requested, "rglwidget")) {
      if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
      } else if (length(html_choices) > 0L) {
        effective <- "html"
        note <- "RGL mode requested, but `rgl` is unavailable. Showing HTML fallback."
      } else if (isTRUE(plotly_ready)) {
        effective <- "plotly"
        note <- "RGL mode requested, but `rgl` is unavailable. Showing Plotly fallback."
      } else {
        effective <- "none"
        note <- "RGL mode requested, but `rgl` is unavailable and no fallback renderer is ready."
      }
    } else if (identical(requested, "html")) {
      if (length(html_choices) > 0L) {
        effective <- "html"
      } else if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
        note <- "HTML mode requested, but no HTML assets were found. Showing RGL fallback."
      } else if (isTRUE(plotly_ready)) {
        effective <- "plotly"
        note <- "HTML mode requested, but no HTML assets were found. Showing Plotly fallback."
      } else {
        effective <- "none"
        note <- "HTML mode requested, but no HTML assets were found and no interactive renderer is available."
      }
    } else if (identical(requested, "plotly")) {
      if (isTRUE(plotly_ready)) {
        effective <- "plotly"
      } else if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
        note <- "Plotly is unavailable. Showing RGL fallback."
      } else if (length(html_choices) > 0L) {
        effective <- "html"
        note <- "Plotly is unavailable. Showing HTML fallback."
      } else {
        effective <- "none"
        note <- "Plotly is unavailable and no fallback renderer is available."
      }
    }

    if (identical(component_mode, "lcc") && identical(effective, "html")) {
      if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
        note <- paste(
          c(note, "Main connected component filtering is interactive-only; showing RGL."),
          collapse = " "
        )
      } else if (isTRUE(plotly_ready)) {
        effective <- "plotly"
        note <- paste(
          c(note, "Main connected component filtering is interactive-only; showing Plotly."),
          collapse = " "
        )
      } else {
        note <- paste(
          c(note, "Main connected component filtering is unavailable in HTML mode."),
          collapse = " "
        )
      }
      note <- trimws(gsub("\\s+", " ", note))
    }

    html_url <- ""
    html_url_error <- ""
    if (identical(effective, "html") && nzchar(html_selected)) {
      url_res <- tryCatch(
        list(url = local_html_resource_url(html_selected), error = NULL),
        error = function(e) list(url = "", error = conditionMessage(e))
      )
      html_url <- as.character(url_res$url %||% "")
      if (length(html_url) < 1L || !nzchar(html_url[[1]])) {
        html_url <- ""
      } else {
        html_url <- html_url[[1]]
      }
      if (!is.null(url_res$error) && nzchar(as.character(url_res$error)[[1]])) {
        html_url_error <- as.character(url_res$error)[[1]]
      }
    }

    list(
      st = st,
      requested = requested,
      effective = effective,
      rgl_ready = rgl_ready,
      plotly_ready = plotly_ready,
      html_choices = html_choices,
      html_selected = html_selected,
      html_url = html_url,
      html_url_error = html_url_error,
      mode_note = note,
      src_key = src_key,
      color_label = color_label,
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

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$reference_plot <- plotly::renderPlotly({
      rr <- reference_renderer_state()
      st <- rr$st
      req(is.null(st$error))

      src_key <- as.character(rr$src_key %||% st$default_key)
      if (!(src_key %in% names(st$sources))) {
        src_key <- st$default_key
      }
      src <- st$sources[[src_key]]
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

      idx <- keep_idx
      if (length(idx) < 1L) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = list(text = "No points to display for selected color source."),
              scene = list(
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                zaxis = list(visible = FALSE)
              )
            )
        )
      }

      plot_data <- data.frame(
        vertex = idx,
        x = coords[idx, 1],
        y = coords[idx, 2],
        z = coords[idx, 3],
        value = vals[idx],
        stringsAsFactors = FALSE
      )

      p <- plotly::plot_ly()

      if (identical(src$type, "categorical")) {
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
          p <- p %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers",
              x = plot_data$x[sel],
              y = plot_data$y[sel],
              z = plot_data$z[sel],
              name = lvl,
              legendgroup = lvl,
              text = sprintf("vertex=%d<br>%s=%s", plot_data$vertex[sel], src$label, lvl),
              hoverinfo = "text",
              marker = list(
                size = point_size,
                color = pal[[lvl]],
                opacity = if (identical(vertex_mode, "point")) 0.82 else 0.93
              ),
              showlegend = FALSE
            ) %>%
            plotly::add_trace(
              type = "scatter3d",
              mode = "markers",
              x = if (nrow(plot_data) > 0L) plot_data$x[[1]] else 0,
              y = if (nrow(plot_data) > 0L) plot_data$y[[1]] else 0,
              z = if (nrow(plot_data) > 0L) plot_data$z[[1]] else 0,
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
        p <- p %>%
          plotly::add_trace(
            type = "scatter3d",
            mode = "markers",
            x = plot_data$x,
            y = plot_data$y,
            z = plot_data$z,
            text = sprintf("vertex=%d<br>%s=%s", plot_data$vertex, src$label, signif(vv, 4)),
            hoverinfo = "text",
            marker = list(
              size = point_size,
              color = vv,
              colorscale = "Viridis",
              opacity = if (identical(vertex_mode, "point")) 0.82 else 0.93,
              colorbar = list(title = src$label)
            ),
            showlegend = FALSE
          )
      }

      ep <- viz_state()$endpoint.result$endpoints
      ep <- suppressWarnings(as.integer(ep))
      ep <- ep[is.finite(ep) & ep >= 1L & ep <= nn]

      ep_overlay <- endpoint_overlay_active()
      ep_extra <- suppressWarnings(as.integer(ep_overlay$vertices %||% integer(0)))
      ep_extra <- ep_extra[is.finite(ep_extra) & ep_extra >= 1L & ep_extra <= nn]
      ep <- sort(unique(c(ep, ep_extra)))
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
            text = ep_label_text[label_idx],
            textposition = "top center",
            hoverinfo = "skip",
            showlegend = FALSE,
            textfont = list(size = max(8, 12 * endpoint_label_size), color = "#111827")
          )
      }

      p %>%
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
          scene = list(
            xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            zaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
            camera = list(eye = list(x = 1.6, y = 1.55, z = 1.2))
          )
        )
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

      src_key <- as.character(rr$src_key %||% st$default_key)
      if (!(src_key %in% names(st$sources))) {
        src_key <- st$default_key
      }
      src <- st$sources[[src_key]]
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

      ep <- viz_state()$endpoint.result$endpoints
      ep <- suppressWarnings(as.integer(ep))
      ep <- ep[is.finite(ep) & ep >= 1L & ep <= nn]

      ep_overlay <- endpoint_overlay_active()
      ep_extra <- suppressWarnings(as.integer(ep_overlay$vertices %||% integer(0)))
      ep_extra <- ep_extra[is.finite(ep_extra) & ep_extra >= 1L & ep_extra <= nn]
      ep <- sort(unique(c(ep, ep_extra)))
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

      make_plain_widget <- function() {
        gflow::plot3D.plain.html(
          X = coords_view,
          radius = if (identical(vertex_mode, "sphere")) sphere_radius else NULL,
          size = point_size,
          col = "gray70",
          widget.width = 1700L,
          widget.height = 1000L,
          background.color = "white",
          post.layers = endpoint_layers
        )
      }

      if (identical(src$type, "categorical")) {
        pal_info <- categorical_palette(
          values_view,
          source_key = src_key,
          source_label = src$label %||% src_key
        )
        vv <- pal_info$values
        cltr_col_tbl <- pal_info$colors
        tryCatch(
          gflow::plot3D.cltrs.html(
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
            post.layers = endpoint_layers
          ),
          error = function(e) make_plain_widget()
        )
      } else {
        vv <- suppressWarnings(as.numeric(values_view))
        if (all(!is.finite(vv))) {
          make_plain_widget()
        } else {
          tryCatch(
              gflow::plot3D.cont.html(
                X = coords_view,
                y = vv,
                subset = rep(TRUE, nn_view),
                non.highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
                highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
                point.size = point_size,
                radius = if (identical(vertex_mode, "sphere")) sphere_radius else NULL,
                legend.title = as.character(src$label %||% src_key),
                legend.show = FALSE,
                widget.width = 1700L,
                widget.height = 1000L,
                background.color = "white",
                post.layers = endpoint_layers
              ),
            error = function(e) make_plain_widget()
          )
        }
      }
    })
      rgl_last_output_id(rgl_output_id)
    })
  }

  graph_structure_state <- shiny::reactive({
    if (!isTRUE(rv$project.active)) {
      return(list(error = "No project selected."))
    }

    manifest <- active_manifest()
    if (!is.list(manifest)) {
      return(list(error = "Project manifest not found."))
    }

    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    if (length(graph_sets) < 1L) {
      return(list(error = "No graph sets are available."))
    }

    choices <- graph_data_type_choices(graph_sets)
    set_id <- scalar_chr(input$graph_data_type %||% "", default = "")
    if (!(set_id %in% unname(choices))) {
      ref <- current_reference_info(manifest)
      fallback <- scalar_chr(ref$set_id %||% manifest$defaults$graph_set_id %||% "", default = "")
      if (fallback %in% unname(choices)) {
        set_id <- fallback
      } else {
        set_id <- unname(choices)[1]
      }
    }

    k_choices <- graph_k_choices(graph_sets, set_id)
    k_sel <- scalar_int(input$graph_k, default = NA_integer_)
    k_sel_is_valid <- is.finite(k_sel) && (as.character(k_sel) %in% unname(k_choices))
    if (!isTRUE(k_sel_is_valid)) {
      ref <- current_reference_info(manifest)
      if (is.finite(ref$k) && as.character(ref$k) %in% unname(k_choices)) {
        k_sel <- scalar_int(ref$k, default = NA_integer_)
      } else {
        vals <- suppressWarnings(as.integer(unname(k_choices)))
        vals <- vals[is.finite(vals)]
        k_sel <- if (length(vals) > 0L) vals[1] else NA_integer_
      }
    }

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
    if (is.list(st) && is.null(st$error) && identical(as.character(st$set_id), as.character(set_id))) {
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

    color_choices <- c("Vertex Degree" = "vertex_degree")
    color_selected <- "vertex_degree"
    if (is.list(st_use) && length(st_use$choices %||% c()) > 0L) {
      color_choices <- st_use$choices
      color_selected <- as.character(
        input$graph_layout_color_by %||%
          layout_presets$color_by %||%
          st_use$default_key %||%
          ""
      )
      if (!(color_selected %in% unname(color_choices))) {
        color_selected <- unname(color_choices)[1]
      }
    }

    renderer_selected <- tolower(as.character(input$graph_layout_renderer %||% layout_presets$renderer %||% "rglwidget"))
    if (identical(renderer_selected, "rgl")) {
      renderer_selected <- "rglwidget"
    }
    if (!renderer_selected %in% c("rglwidget", "html", "plotly")) {
      renderer_selected <- "rglwidget"
    }
    vertex_layout <- tolower(as.character(input$graph_layout_vertex %||% layout_presets$vertex_layout %||% "sphere"))
    if (!vertex_layout %in% c("sphere", "point")) {
      vertex_layout <- "sphere"
    }
    size_selected <- as.character(input$graph_layout_size %||% layout_presets$vertex_size %||% "1x")
    component_choices <- c("All vertices" = "all", "Main connected component" = "lcc")
    component_selected <- tolower(as.character(input$graph_layout_component %||% layout_presets$component %||% "all"))
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

    list(
      error = NULL,
      manifest = manifest,
      set_id = set_id,
      data_type_choices = choices,
      data_type_label = infer_data_type_label(gs),
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
      color_choices = color_choices,
      color_selected = color_selected
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

  output$workflow_controls <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(NULL)
    }

    manifest <- active_manifest()
    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    graph_ui <- graph_structure_state()

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
    has_asset_views <- nrow(graph_tbl) > 0L || nrow(condexp_tbl) > 0L || length(endpoint_runs) > 0L

    build_endpoint_method_k_table <- function(rows_df) {
      if (!is.data.frame(rows_df) || nrow(rows_df) < 1L) {
        return(shiny::p(class = "gf-hint", "No endpoint options found."))
      }

      head_row <- shiny::tags$tr(
        shiny::tags$th(""),
        shiny::tags$th("method"),
        shiny::tags$th("k")
      )
      body_rows <- lapply(seq_len(nrow(rows_df)), function(ii) {
        rr <- rows_df[ii, , drop = FALSE]
        in_id <- as.character(rr$input_id[[1]] %||% "")
        checked <- isTRUE(rr$selected[[1]])
        shiny::tags$tr(
          shiny::tags$td(
            shiny::tags$input(
              type = "checkbox",
              id = in_id,
              checked = if (isTRUE(checked)) "checked" else NULL
            )
          ),
          shiny::tags$td(as.character(rr$method[[1]] %||% "")),
          shiny::tags$td(as.character(rr$k_display[[1]] %||% ""))
        )
      })

      shiny::div(
        class = "table-responsive",
        shiny::tags$table(
          class = "table table-sm gf-asset-table",
          shiny::tags$thead(head_row),
          shiny::tags$tbody(body_rows)
        )
      )
    }

    panels <- list()
    open.panels <- c("workflow_graph_structure")

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
        size_choices <- c("0.50x", "0.75x", "1x", "1.25x", "1.50x", "2x")
        if (!(graph_ui$size_selected %in% size_choices)) {
          size_choices <- unique(c(size_choices, graph_ui$size_selected))
        }

        shiny::tagList(
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
          ),
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
              choices = c("RGL" = "rglwidget", "HTML" = "html", "Plotly" = "plotly"),
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
            "Endpoints",
            value = "workflow_endpoint_structure",
            shiny::tagList(
              build_endpoint_method_k_table(endpoint_rows),
              shiny::actionButton(
                "endpoint_update_placeholder",
                "Update / Recompute Endpoints...",
                class = "btn-light gf-btn-wide"
              ),
              shiny::hr(),
              shiny::h6(class = "gf-graph-layout-head", "Endpoint Layout"),
              shiny::div(
                class = "gf-graph-row gf-graph-layout-row",
                shiny::span(class = "gf-graph-row-label", "Label size:"),
                shiny::selectInput(
                  "endpoint_label_size",
                  label = NULL,
                  choices = stats::setNames(
                    c("0.75x", "1x", "1.25x", "1.50x", "2x", "2.5x", "3.0x"),
                    c("0.75x", "1x", "1.25x", "1.50x", "2x", "2.5x", "3.0x")
                  ),
                  selected = as.character(input$endpoint_label_size %||% "1x"),
                  width = "170px"
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
          ),
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
          "workflow_endpoint_structure",
          "workflow_condexp_structure",
          "workflow_analysis"
        )
      } else {
        c("workflow_graph", "workflow_condexp", "workflow_analysis")
      }
    )
    remembered_open <- workflow_open_panels()
    if (!is.null(remembered_open)) {
      mapped_open <- intersect(as.character(remembered_open), available_panels)
      if (length(mapped_open) > 0L || length(remembered_open) < 1L) {
        open.panels <- mapped_open
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
      html = "HTML",
      plotly = "Plotly",
      none = "none",
      "unknown"
    )
    req_label <- switch(
      rr$requested,
      rglwidget = "RGL",
      html = "HTML",
      plotly = "Plotly",
      rr$requested
    )
    suffix <- if (identical(rr$effective, "html")) {
      sprintf(" (%d html)", length(rr$html_choices))
    } else {
      ""
    }

    shiny::span(
      class = "gf-chip",
      sprintf("3D renderer: %s [%s]%s", mode_label, req_label, suffix)
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
    rr <- reference_renderer_state()
    st <- rr$st
    html_url <- as.character(rr$html_url %||% "")
    if (length(html_url) < 1L || !nzchar(html_url[[1]])) {
      html_url <- ""
    } else {
      html_url <- html_url[[1]]
    }
    html_error <- as.character(rr$html_url_error %||% "")
    if (length(html_error) < 1L || !nzchar(html_error[[1]])) {
      html_error <- ""
    } else {
      html_error <- html_error[[1]]
    }
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
        q <- tryCatch(
          gflow::quantize.for.legend(
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
    } else if (identical(rr$effective, "html")) {
      if (nzchar(html_error) || !nzchar(html_url)) {
        shiny::div(
          class = "gf-viewer-canvas",
          shiny::div(
            class = "gf-viewer-overlay",
            shiny::h3("Graph View"),
            shiny::p(
              if (nzchar(html_error)) {
                sprintf("Failed to load HTML asset: %s", html_error)
              } else {
                "No HTML asset selected."
              }
            )
          )
        )
      } else {
        shiny::div(
          class = "gf-html-frame-wrap",
          shiny::tags$iframe(
            class = "gf-html-frame",
            src = html_url,
            loading = "lazy"
          )
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
          shiny::p("No renderer is available. Install `rgl`/`plotly` or register HTML assets.")
        )
      )
    }

    shiny::div(
      class = "gf-reference-view gf-reference-view-plain",
      view_body,
      {
        notes <- unique(c(mode_note, component_note))
        notes <- notes[nzchar(notes)]
        if (length(notes) > 0L) {
          shiny::p(class = "gf-mode-note", paste(notes, collapse = " "))
        }
      }
    )
  })
}
