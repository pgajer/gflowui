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
      color_by = as.character(input$graph_layout_color_by %||% "vertex_index")
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
        layout_assets = list(presets = layout_presets, variants = layout_variants),
        selected_k = suppressWarnings(as.integer(res$selected.k)),
        selection_method = method,
        source = "gflowui_build",
        updated_at = .gflowui_now()
      )
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

    condexp <- collect_reference_condexp_sources(
      manifest = manifest,
      set_id = spec$set_id,
      k_use = picked$k_actual,
      n_vertices = n_vertices
    )

    cache_key <- sprintf(
      "%s|%s|%s|%s",
      rv$project.id %||% "project",
      spec$set_id %||% "set",
      picked$k_actual %||% "k",
      n_vertices
    )
    coords <- compute_reference_layout(
      adj_list = adj_list,
      cache_key = cache_key,
      spectral_coords = condexp$spectral_coords
    )

    sources <- condexp$sources
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

    degree <- suppressWarnings(as.numeric(lengths(adj_list)))
    add_source("graph_degree", "Graph Degree", degree, type = "numeric")

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
      add_source("vertex_index", "Vertex Index", seq_len(n_vertices), type = "numeric")
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
        note <- "RGL live mode requested, but `rgl` is unavailable. Showing HTML fallback."
      } else if (isTRUE(plotly_ready)) {
        effective <- "plotly"
        note <- "RGL live mode requested, but `rgl` is unavailable. Showing Plotly fallback."
      } else {
        effective <- "none"
        note <- "RGL live mode requested, but `rgl` is unavailable and no fallback renderer is ready."
      }
    } else if (identical(requested, "html")) {
      if (length(html_choices) > 0L) {
        effective <- "html"
      } else if (isTRUE(rgl_ready)) {
        effective <- "rglwidget"
        note <- "HTML mode requested, but no HTML assets were found. Showing live RGL fallback."
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
        note <- "Plotly is unavailable. Showing live RGL fallback."
      } else if (length(html_choices) > 0L) {
        effective <- "html"
        note <- "Plotly is unavailable. Showing HTML fallback."
      } else {
        effective <- "none"
        note <- "Plotly is unavailable and no fallback renderer is available."
      }
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
      size_label = size_label
    )
  })

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
      size_mult <- suppressWarnings(as.numeric(rr$size_mult %||% 1))
      if (!is.finite(size_mult) || size_mult <= 0) {
        size_mult <- 1
      }
      vertex_mode <- tolower(as.character(rr$vertex_mode %||% "sphere"))
      base_size <- if (identical(vertex_mode, "point")) 2.8 else 5.2
      point_size <- max(1.2, base_size * size_mult)

      keep <- rep(TRUE, nn)

      idx <- idx_all[keep]
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
        vv <- as.character(plot_data$value)
        vv[is.na(vv) | !nzchar(vv)] <- "NA"
        fac <- factor(vv, levels = unique(vv))
        nlev <- nlevels(fac)
        pal <- grDevices::hcl.colors(max(1L, nlev), "Dynamic")

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
              text = sprintf("vertex=%d<br>%s=%s", plot_data$vertex[sel], src$label, lvl),
              hoverinfo = "text",
              marker = list(
                size = point_size,
                color = pal[ii],
                opacity = if (identical(vertex_mode, "point")) 0.82 else 0.93
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
              size = max(4.5, point_size + 2.2),
              color = "#ef4444",
              line = list(color = "#111827", width = 1)
            )
          )
      }

      p %>%
        plotly::layout(
          margin = list(l = 0, r = 0, b = 0, t = 10),
          legend = list(orientation = "h"),
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
    output$reference_rgl <- rgl::renderRglwidget({
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

      span <- apply(coords, 2, function(vv) diff(range(vv, na.rm = TRUE)))
      span[!is.finite(span)] <- 0
      radius_base <- max(1e-8, 0.01 * mean(span))
      sphere_radius <- max(1e-8, radius_base * size_mult)
      point_size <- max(1.2, 3 * size_mult)

      ep <- viz_state()$endpoint.result$endpoints
      ep <- suppressWarnings(as.integer(ep))
      ep <- ep[is.finite(ep) & ep >= 1L & ep <= nn]

      endpoint_layers <- if (length(ep) > 0L) {
        list(list(
          fun = function(ctx, endpoint_idx, draw_mode, endpoint_radius, endpoint_size) {
            idx <- suppressWarnings(as.integer(endpoint_idx))
            idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(ctx$X)]
            if (length(idx) < 1L) {
              return(invisible(NULL))
            }
            if (identical(draw_mode, "sphere")) {
              rgl::spheres3d(
                ctx$X[idx, , drop = FALSE],
                col = "#ef4444",
                radius = max(1e-8, endpoint_radius * 1.35)
              )
            } else {
              rgl::points3d(
                ctx$X[idx, , drop = FALSE],
                col = "#ef4444",
                size = max(4.5, endpoint_size + 2.2)
              )
            }
            invisible(NULL)
          },
          args = list(
            endpoint_idx = ep,
            draw_mode = vertex_mode,
            endpoint_radius = sphere_radius,
            endpoint_size = point_size
          ),
          with_ctx = TRUE
        ))
      } else {
        NULL
      }

      make_plain_widget <- function() {
        gflow::plot3D.plain.html(
          X = coords,
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
        vv <- as.character(src$values)
        vv[is.na(vv) | !nzchar(vv)] <- "NA"
        tryCatch(
          gflow::plot3D.cltrs.html(
            X = coords,
            cltr = vv,
            show.cltr.labels = FALSE,
            show.legend = TRUE,
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
        vv <- suppressWarnings(as.numeric(src$values))
        if (all(!is.finite(vv))) {
          make_plain_widget()
        } else {
          tryCatch(
            gflow::plot3D.cont.html(
              X = coords,
              y = vv,
              subset = rep(TRUE, nn),
              non.highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
              highlight.type = if (identical(vertex_mode, "sphere")) "sphere" else "point",
              point.size = point_size,
              radius = if (identical(vertex_mode, "sphere")) sphere_radius else NULL,
              legend.title = as.character(src$label %||% src_key),
              legend.show = TRUE,
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
    gs <- graph_set_by_id(graph_sets, set_id)
    layout_presets <- if (is.list(gs$layout_assets$presets)) gs$layout_assets$presets else list()
    n_samples <- infer_sample_count(gs, st = st_use)
    n_features <- infer_feature_count(gs)
    dims_text <- sprintf(
      "(%s x %s)",
      if (is.finite(n_samples)) format(as.integer(n_samples), big.mark = ",") else "?",
      if (is.finite(n_features)) format(as.integer(n_features), big.mark = ",") else "?"
    )

    color_choices <- c("Vertex Index" = "vertex_index")
    color_selected <- "vertex_index"
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
    target <- resolve_optimal_k_display_path(method$path, set_tokens = set_tokens)
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
      ),
      shiny::div(
        class = "gf-inline-status",
        shiny::textOutput("project_status")
      )
    )
  })

  output$project_status <- shiny::renderText({
    if (!isTRUE(rv$project.active)) {
      return("Select an existing project or click 'New'.")
    }

    manifest <- active_manifest()
    ref_info <- current_reference_info(manifest)
    origin_value <- rv$project.origin %||% "unknown"
    origin_txt <- if (grepl("^registered:", origin_value)) {
      sprintf("registered project (%s)", sub("^registered:", "", origin_value))
    } else {
      switch(
        origin_value,
        existing = "existing project",
        template = "new project from template",
        clone = "cloned project",
        scratch = "new project from scratch",
        "project"
      )
    }

    graph_txt <- if (isTRUE(rv$project.has.graphs)) "graphs ready" else "graphs not built yet"

    sprintf(
      "Workspace: %s | %s | %s | Ref: %s",
      rv$project.name %||% "Untitled Project",
      origin_txt,
      graph_txt,
      ref_info$summary
    )
  })

  output$workflow_controls <- shiny::renderUI({
    if (!isTRUE(rv$project.active)) {
      return(
        shiny::div(
          class = "gf-sidebar-note",
          shiny::strong("Workspace Locked"),
          shiny::p("Select a project from the dropdown or click 'New' to reveal workflow controls.")
        )
      )
    }

    manifest <- active_manifest()
    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    ref_info <- current_reference_info(manifest)
    graph_ui <- graph_structure_state()

    graph_tbl <- summarize_graph_assets(
      graph_sets,
      default_id = as.character(defaults$graph_set_id %||% NA_character_)
    )
    condexp_tbl <- summarize_condexp_assets(
      condexp_sets,
      default_id = as.character(defaults$condexp_set_id %||% NA_character_)
    )
    endpoint_tbl <- summarize_endpoint_assets(
      endpoint_runs,
      default_id = as.character(defaults$endpoint_run_id %||% NA_character_)
    )
    has_asset_views <- nrow(graph_tbl) > 0L || nrow(condexp_tbl) > 0L || nrow(endpoint_tbl) > 0L

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
            class = "gf-reference-summary",
            shiny::p(sprintf("Current reference: %s", ref_info$summary)),
            shiny::p(sprintf("Outcome overrides: %s", ref_info$by_outcome_text)),
            if (nzchar(ref_info$reason)) shiny::p(sprintf("Reference reason: %s", ref_info$reason))
          ),
          shiny::div(
            class = "gf-graph-row",
            shiny::span(class = "gf-graph-row-label", "Data Type:"),
            shiny::selectInput(
              "graph_data_type",
              label = NULL,
              choices = graph_ui$data_type_choices,
              selected = graph_ui$set_id,
              width = "245px"
            ),
            shiny::span(class = "gf-graph-dims", graph_ui$dims_text)
          ),
          shiny::div(
            class = "gf-graph-row",
            shiny::span(class = "gf-graph-row-label", "k:"),
            shiny::selectInput(
              "graph_k",
              label = NULL,
              choices = graph_ui$k_choices,
              selected = if (is.finite(graph_ui$k_selected)) as.character(graph_ui$k_selected) else "",
              width = "140px"
            ),
            shiny::actionButton(
              "set_reference_graph_inline",
              "Set As Reference Graph",
              class = "btn-secondary"
            )
          ),
          shiny::div(
            class = "gf-graph-row",
            shiny::span(class = "gf-graph-row-label", "Optimal k:"),
            shiny::selectInput(
              "graph_optimal_method",
              label = NULL,
              choices = graph_ui$optimal_choices,
              selected = graph_ui$optimal_selected,
              width = "250px"
            ),
            shiny::actionButton(
              "graph_optimal_show",
              "Show",
              class = "btn-light"
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
            class = "gf-graph-row",
            shiny::span(class = "gf-graph-row-label", "Renderer:"),
            shiny::selectInput(
              "graph_layout_renderer",
              label = NULL,
              choices = c("RGL (live)" = "rglwidget", "HTML" = "html", "Plotly" = "plotly"),
              selected = graph_ui$renderer_selected,
              width = "180px"
            )
          ),
          shiny::div(
            class = "gf-graph-row",
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
            class = "gf-graph-row",
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
            class = "gf-graph-row",
            shiny::span(class = "gf-graph-row-label", "Color by:"),
            shiny::selectInput(
              "graph_layout_color_by",
              label = NULL,
              choices = graph_ui$color_choices,
              selected = graph_ui$color_selected,
              width = "320px"
            )
          )
        )
      }

      panels <- c(
        panels,
        list(
          bslib::accordion_panel(
            "Graph(s) Structure",
            value = "workflow_graph_structure",
            graph_panel
          ),
          bslib::accordion_panel(
            "Conditional Expectation Structure",
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
            "Endpoints Structure",
            value = "workflow_endpoint_structure",
            shiny::tagList(
              build_html_table(endpoint_tbl, empty_text = "No endpoint runs found."),
              shiny::actionButton(
                "endpoint_update_placeholder",
                "Update / Recompute Endpoints...",
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
      rglwidget = "RGL live",
      html = "HTML",
      plotly = "Plotly",
      none = "none",
      "unknown"
    )
    req_label <- switch(
      rr$requested,
      rglwidget = "RGL live",
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
    dat <- data_state()
    g <- graph_state()
    cfit <- condexp_state()
    vz <- viz_state()
    manifest <- active_manifest()
    ref_info <- current_reference_info(manifest)
    rr <- reference_renderer_state()

    proj.msg <- if (!isTRUE(rv$project.active)) {
      "Project: not started"
    } else {
      sprintf(
        "Project: %s | Origin: %s | Graph-ready: %s | Dirty: %s",
        rv$project.name %||% "Untitled Project",
        rv$project.origin %||% "unknown",
        if (isTRUE(rv$project.has.graphs)) "yes" else "no",
        if (isTRUE(rv$project.dirty)) "yes" else "no"
      )
    }

    data.msg <- if (is.null(dat$data)) {
      "Data: not loaded"
    } else {
      sprintf("Data: %d rows x %d columns | Source: %s", nrow(dat$data), ncol(dat$data), dat$source %||% "unknown")
    }

    graph.msg <- sprintf("Graphs: %s", g$status %||% "not run")
    ref.msg <- sprintf("Reference Graph: %s | Overrides: %s", ref_info$summary, ref_info$by_outcome_text)
    renderer.msg <- sprintf(
      "Renderer: %s [%s]%s",
      rr$effective %||% "unknown",
      rr$requested %||% "rglwidget",
      if (identical(rr$effective, "html")) sprintf(" | html assets: %d", length(rr$html_choices)) else ""
    )
    condexp.msg <- sprintf("CondExp: %s", cfit$status %||% "not run")
    viz.msg <- sprintf("Endpoints/View: %s", vz$status %||% "not run")
    note.msg <- trimws(as.character(rv$run.monitor.note %||% ""))

    parts <- c()
    if (nzchar(note.msg)) {
      parts <- c(parts, sprintf("Job: %s", note.msg))
    }
    parts <- c(parts, proj.msg, data.msg, graph.msg, ref.msg, renderer.msg, condexp.msg, viz.msg)
    paste(parts, collapse = "\n\n")
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
        rgl::rglwidgetOutput("reference_rgl", width = "100%", height = "78vh")
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
      if (nzchar(mode_note)) {
        shiny::p(class = "gf-mode-note", mode_note)
      }
    )
  })
}
