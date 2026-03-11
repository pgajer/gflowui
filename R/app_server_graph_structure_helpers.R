gflowui_make_server_graph_structure_helpers <- function(rv) {
  `%||%` <- function(x, y) {
    if (
      is.null(x) ||
      length(x) < 1L ||
      (is.character(x) && length(x) > 0L && !nzchar(x[[1]]))
    ) y else x
  }

  scalar_int <- function(x, default = NA_integer_) {
    vals <- suppressWarnings(as.integer(x))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 1L) {
      dv <- suppressWarnings(as.integer(default))
      if (length(dv) < 1L) {
        return(NA_integer_)
      }
      return(dv[[1]])
    }
    vals[[1]]
  }

  scalar_chr <- function(x, default = "") {
    vals <- as.character(x)
    vals <- vals[!is.na(vals)]
    if (length(vals) < 1L) {
      as.character(default)[[1]]
    } else {
      vals[[1]]
    }
  }

  fmt_k_values <- function(x) {
    if (is.null(x)) {
      return("-")
    }
    x <- suppressWarnings(as.integer(x))
    x <- x[is.finite(x)]
    if (length(x) < 1L) {
      return("-")
    }
    x <- sort(unique(x))
    if (length(x) > 7L) {
      return(sprintf("%d-%d (%d)", min(x), max(x), length(x)))
    }
    paste(x, collapse = ",")
  }

  compact_values <- function(x, max_n = 3L) {
    vals <- unique(as.character(x %||% character(0)))
    vals <- vals[nzchar(vals)]
    if (length(vals) < 1L) {
      return("-")
    }
    if (length(vals) <= max_n) {
      return(paste(vals, collapse = ", "))
    }
    sprintf("%s +%d", paste(vals[seq_len(max_n)], collapse = ", "), length(vals) - max_n)
  }

  default_vertex_layout_for_graph <- function(preset = "point", n_vertices = NA_integer_) {
    base <- tolower(scalar_chr(preset, default = "point"))
    if (!(base %in% c("sphere", "point"))) {
      base <- "point"
    }

    nn <- scalar_int(n_vertices, default = NA_integer_)
    if (is.finite(nn) && nn >= 10000L) {
      return("point")
    }

    base
  }

  normalize_paths <- function(paths) {
    x <- as.character(paths %||% character(0))
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  }

  summarize_graph_assets <- function(graph_sets, default_id = NA_character_) {
    if (!is.list(graph_sets) || length(graph_sets) < 1L) {
      return(data.frame())
    }
    rows <- lapply(graph_sets, function(gs) {
      id <- as.character(gs$id %||% "")
      label <- as.character(gs$label %||% id)
      graph_file <- as.character(gs$graph_file %||% "")
      status <- if (nzchar(graph_file) && file.exists(graph_file)) "ok" else "missing"
      data.frame(
        id = if (!is.na(default_id) && identical(id, default_id)) sprintf("%s *", id) else id,
        label = label,
        k = fmt_k_values(gs$k_values),
        file = status,
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  summarize_condexp_assets <- function(condexp_sets, default_id = NA_character_) {
    if (!is.list(condexp_sets) || length(condexp_sets) < 1L) {
      return(data.frame())
    }

    rows <- lapply(condexp_sets, function(cs) {
      id <- as.character(cs$id %||% "")
      label <- as.character(cs$label %||% id)

      paths <- c(
        normalize_paths(cs$long_table_file),
        normalize_paths(cs$summary_file),
        normalize_paths(cs$gcv_summary_file)
      )
      if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        for (fr in cs$family_runs) {
          paths <- c(
            paths,
            normalize_paths(fr$summary_file),
            normalize_paths(fr$fits_dir),
            normalize_paths(fr$fit_files)
          )
        }
      }
      paths <- unique(paths)
      status <- if (length(paths) > 0L && any(file.exists(paths))) "ok" else "missing"

      outputs <- if (length(cs$outcomes %||% character(0)) > 0L) {
        compact_values(cs$outcomes)
      } else if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        compact_values(vapply(cs$family_runs, function(fr) as.character(fr$family %||% ""), character(1)))
      } else {
        "-"
      }

      data.frame(
        id = if (!is.na(default_id) && identical(id, default_id)) sprintf("%s *", id) else id,
        label = label,
        outputs = outputs,
        k = fmt_k_values(cs$k_values),
        file = status,
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  summarize_endpoint_assets <- function(endpoint_runs, default_id = NA_character_) {
    if (!is.list(endpoint_runs) || length(endpoint_runs) < 1L) {
      return(data.frame())
    }

    rows <- lapply(endpoint_runs, function(ep) {
      id <- as.character(ep$id %||% "")
      label <- as.character(ep$label %||% id)
      methods <- compact_values(c(ep$method, ep$methods))

      paths <- c(
        normalize_paths(ep$run_dir),
        normalize_paths(ep$summary_csv),
        normalize_paths(ep$labels_csv),
        normalize_paths(ep$bundle_file),
        normalize_paths(ep$per_k_bundles)
      )
      status <- if (length(paths) > 0L && any(file.exists(paths))) "ok" else "missing"

      data.frame(
        id = if (!is.na(default_id) && identical(id, default_id)) sprintf("%s *", id) else id,
        label = label,
        method = methods,
        k = fmt_k_values(ep$k_values),
        file = status,
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  build_html_table <- function(df, empty_text = "No assets found.") {
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(shiny::p(class = "gf-hint", empty_text))
    }

    header <- lapply(names(df), function(nm) shiny::tags$th(nm))
    body_rows <- lapply(seq_len(nrow(df)), function(ii) {
      shiny::tags$tr(
        lapply(names(df), function(nm) shiny::tags$td(as.character(df[[nm]][[ii]] %||% "")))
      )
    })

    shiny::div(
      class = "table-responsive",
      shiny::tags$table(
        class = "table table-sm gf-asset-table",
        shiny::tags$thead(shiny::tags$tr(header)),
        shiny::tags$tbody(body_rows)
      )
    )
  }

  set_run_monitor_note <- function(msg) {
    rv$run.monitor.note <- as.character(msg %||% "")
    rv$run.monitor.visible <- TRUE
    invisible(NULL)
  }

  sanitize_token_id <- function(x, fallback = "asset") {
    id <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(x %||% "")))
    id <- gsub("^_+|_+$", "", id)
    if (!nzchar(id)) {
      id <- fallback
    }
    id
  }

  parse_k_values_text <- function(x) {
    txt <- as.character(x %||% "")
    if (!nzchar(txt)) {
      return(integer(0))
    }
    parts <- unlist(strsplit(txt, "[^0-9]+", perl = TRUE), use.names = FALSE)
    parts <- parts[nzchar(parts)]
    if (length(parts) < 1L) {
      return(integer(0))
    }
    vals <- suppressWarnings(as.integer(parts))
    vals <- vals[is.finite(vals)]
    sort(unique(vals))
  }

  graph_set_choices <- function(graph_sets) {
    if (!is.list(graph_sets) || length(graph_sets) < 1L) {
      return(c())
    }
    ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    labels <- vapply(graph_sets, function(gs) as.character(gs$label %||% gs$id %||% ""), character(1))
    stats::setNames(ids, sprintf("%s (%s)", labels, ids))
  }

  graph_set_k_values <- function(graph_sets, set_id) {
    if (!is.list(graph_sets) || length(graph_sets) < 1L || !nzchar(as.character(set_id %||% ""))) {
      return(integer(0))
    }
    ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    idx <- match(as.character(set_id), ids)
    if (is.na(idx)) {
      return(integer(0))
    }
    vals <- suppressWarnings(as.integer(graph_sets[[idx]]$k_values %||% integer(0)))
    vals <- vals[is.finite(vals)]
    sort(unique(vals))
  }

  graph_k_choices <- function(graph_sets, set_id) {
    kvals <- graph_set_k_values(graph_sets, set_id)
    if (length(kvals) < 1L) {
      return(c("unknown" = ""))
    }
    stats::setNames(as.character(kvals), as.character(kvals))
  }

  collect_outcomes_from_condexp <- function(condexp_sets) {
    if (!is.list(condexp_sets) || length(condexp_sets) < 1L) {
      return(character(0))
    }

    out <- character(0)
    for (cs in condexp_sets) {
      out <- c(out, as.character(cs$outcomes %||% character(0)))
      if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        out <- c(out, vapply(cs$family_runs, function(fr) as.character(fr$family %||% ""), character(1)))
      }
    }
    out <- out[nzchar(out)]
    sort(unique(out))
  }

  current_reference_info <- function(manifest) {
    defaults <- if (is.list(manifest$defaults)) manifest$defaults else list()
    ref_set <- scalar_chr(defaults$reference_graph_set_id %||% defaults$graph_set_id %||% "", default = "")
    ref_k <- scalar_int(defaults$reference_k, default = NA_integer_)
    if (!is.finite(ref_k)) {
      ep_id <- as.character(defaults$endpoint_run_id %||% "")
      if (nzchar(ep_id)) {
        mm <- regexec("k0*([0-9]+)", ep_id, perl = TRUE)
        rr <- regmatches(ep_id, mm)[[1]]
        if (length(rr) >= 2L) {
          ref_k <- suppressWarnings(as.integer(rr[2]))
        }
      }
    }
    ref_reason <- trimws(scalar_chr(defaults$reference_reason %||% "", default = ""))
    ref_by_outcome <- defaults$reference_k_by_outcome
    if (!is.list(ref_by_outcome)) {
      ref_by_outcome <- list()
    }

    override_txt <- "-"
    if (length(ref_by_outcome) > 0L) {
      nm <- names(ref_by_outcome)
      vals <- suppressWarnings(as.integer(unlist(ref_by_outcome, use.names = FALSE)))
      keep <- nzchar(nm) & is.finite(vals)
      if (any(keep)) {
        pairs <- sprintf("%s=%d", nm[keep], vals[keep])
        override_txt <- paste(pairs, collapse = ", ")
      }
    }

    summary <- if (nzchar(ref_set) && is.finite(ref_k)) {
      sprintf("%s @ k=%d", ref_set, ref_k)
    } else if (nzchar(ref_set)) {
      sprintf("%s @ k=?", ref_set)
    } else {
      "not set"
    }

    list(
      set_id = ref_set,
      k = ref_k,
      reason = ref_reason,
      by_outcome = ref_by_outcome,
      by_outcome_text = override_txt,
      summary = summary
    )
  }

  resolve_graph_selection <- function(
      manifest,
      graph_sets,
      input_set_id = "",
      input_k = NA_integer_,
      preferred_default_set_id = "",
      preferred_default_k = NA_integer_,
      sticky_set_id = "",
      sticky_k = NA_integer_) {
    if (!is.list(manifest) || !is.list(graph_sets) || length(graph_sets) < 1L) {
      return(list(
        set_id = "",
        k_selected = NA_integer_,
        data_type_choices = c(),
        k_choices = c()
      ))
    }

    choices <- graph_data_type_choices(graph_sets)
    if (length(choices) < 1L) {
      return(list(
        set_id = "",
        k_selected = NA_integer_,
        data_type_choices = c(),
        k_choices = c()
      ))
    }

    ref <- current_reference_info(manifest)

    set_id <- scalar_chr(input_set_id %||% "", default = "")
    if (!(set_id %in% unname(choices))) {
      sticky_set <- scalar_chr(sticky_set_id %||% "", default = "")
      if (sticky_set %in% unname(choices)) {
        set_id <- sticky_set
      } else {
        preferred_set <- scalar_chr(preferred_default_set_id %||% "", default = "")
        fallback <- if (preferred_set %in% unname(choices)) {
          preferred_set
        } else {
          scalar_chr(ref$set_id %||% manifest$defaults$graph_set_id %||% "", default = "")
        }
        if (fallback %in% unname(choices)) {
          set_id <- fallback
        } else {
          set_id <- unname(choices)[1]
        }
      }
    }

    k_choices <- graph_k_choices(graph_sets, set_id)
    kvals <- suppressWarnings(as.integer(unname(k_choices)))
    kvals <- kvals[is.finite(kvals)]
    k_sel <- scalar_int(input_k, default = NA_integer_)
    k_sel_is_valid <- is.finite(k_sel) && (as.character(k_sel) %in% unname(k_choices))
    if (!isTRUE(k_sel_is_valid)) {
      sticky_k_use <- scalar_int(sticky_k, default = NA_integer_)
      if (is.finite(sticky_k_use) && as.character(sticky_k_use) %in% unname(k_choices)) {
        k_sel <- sticky_k_use
      } else {
        preferred_k_use <- scalar_int(preferred_default_k, default = NA_integer_)
        if (is.finite(preferred_k_use) && as.character(preferred_k_use) %in% unname(k_choices)) {
          k_sel <- preferred_k_use
        } else if (is.finite(ref$k) && as.character(ref$k) %in% unname(k_choices)) {
          k_sel <- scalar_int(ref$k, default = NA_integer_)
        } else {
          k_sel <- if (length(kvals) > 0L) kvals[1] else NA_integer_
        }
      }
    }

    list(
      set_id = set_id,
      k_selected = k_sel,
      data_type_choices = choices,
      k_choices = k_choices
    )
  }

  graph_set_by_id <- function(graph_sets, set_id = NA_character_) {
    if (!is.list(graph_sets) || length(graph_sets) < 1L) {
      return(NULL)
    }
    ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    idx <- match(as.character(set_id %||% ""), ids)
    if (is.na(idx)) {
      return(graph_sets[[1L]])
    }
    graph_sets[[idx]]
  }

  infer_data_type_label <- function(graph_set) {
    explicit <- trimws(as.character(graph_set$data_type_label %||% ""))
    if (nzchar(explicit)) {
      return(explicit)
    }

    set_id <- tolower(trimws(as.character(graph_set$id %||% "")))
    label <- trimws(as.character(graph_set$label %||% ""))

    if (grepl("^top[0-9]+$", set_id, perl = TRUE)) {
      return(sprintf("ASV-top%s", sub("^top", "", set_id)))
    }
    if (grepl("^asv[-_]?top[0-9]+$", set_id, perl = TRUE)) {
      dig <- sub("^asv[-_]?top", "", set_id)
      return(sprintf("ASV-top%s", dig))
    }
    if (set_id %in% c("all", "asv", "shared_all_asv")) {
      return("ASV")
    }

    lbl_low <- tolower(label)
    hv_match <- regexec("hv\\s*([0-9]+)", lbl_low, perl = TRUE)
    hv_caps <- regmatches(lbl_low, hv_match)[[1]]
    if (length(hv_caps) >= 2L) {
      return(sprintf("ASV-top%s", hv_caps[2]))
    }
    if (grepl("full", lbl_low, fixed = TRUE) || grepl("all", lbl_low, fixed = TRUE)) {
      return("ASV")
    }
    if (grepl("asv", lbl_low, fixed = TRUE)) {
      return(toupper(gsub("\\s+", "-", label)))
    }

    if (nzchar(label)) {
      return(label)
    }
    if (nzchar(set_id)) {
      return(toupper(set_id))
    }
    "Graph"
  }

  infer_feature_count <- function(graph_set) {
    direct <- suppressWarnings(as.integer(c(
      graph_set$n_features,
      graph_set$ncols,
      graph_set$p
    )))
    direct <- direct[is.finite(direct) & direct > 0L]
    if (length(direct) > 0L) {
      return(as.integer(direct[1]))
    }

    tokens <- c(
      tolower(as.character(graph_set$id %||% "")),
      tolower(as.character(graph_set$label %||% ""))
    )
    for (tok in tokens) {
      mm <- regexec("(top|hv)\\s*([0-9]+)", tok, perl = TRUE)
      rr <- regmatches(tok, mm)[[1]]
      if (length(rr) >= 3L) {
        vv <- suppressWarnings(as.integer(rr[3]))
        if (is.finite(vv) && vv > 0L) {
          return(vv)
        }
      }
    }

    NA_integer_
  }

  infer_sample_count <- function(graph_set, st = NULL) {
    direct <- suppressWarnings(as.integer(c(
      graph_set$n_samples,
      graph_set$nrows,
      graph_set$n
    )))
    direct <- direct[is.finite(direct) & direct > 0L]
    if (length(direct) > 0L) {
      return(as.integer(direct[1]))
    }

    if (is.list(st) && is.null(st$error) && is.finite(suppressWarnings(as.integer(st$n_vertices)))) {
      return(suppressWarnings(as.integer(st$n_vertices)))
    }
    NA_integer_
  }

  infer_graph_dims_from_project_metadata <- function(project_root, set_id = "", graph_set = NULL) {
    root <- as.character(project_root %||% "")
    if (!nzchar(root) || identical(root, "NA")) {
      return(list(n_samples = NA_integer_, n_features = NA_integer_))
    }

    root <- tryCatch(normalizePath(path.expand(root), mustWork = TRUE), error = function(e) "")
    if (!nzchar(root)) {
      return(list(n_samples = NA_integer_, n_features = NA_integer_))
    }

    sid <- tolower(trimws(as.character(set_id %||% graph_set$id %||% "")))
    out <- list(n_samples = NA_integer_, n_features = NA_integer_)

    read_csv <- function(path) {
      if (!file.exists(path)) {
        return(NULL)
      }
      tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    }
    first_pos_int <- function(x) {
      vals <- suppressWarnings(as.integer(x))
      vals <- vals[is.finite(vals) & vals > 0L]
      if (length(vals) < 1L) {
        return(NA_integer_)
      }
      vals[[1]]
    }

    hv_summary <- read_csv(file.path(root, "results", "asv_hv_k_gcv_sweep", "summary.across.feature.sets.csv"))
    if (is.data.frame(hv_summary) && nrow(hv_summary) > 0L && sid %in% tolower(as.character(hv_summary$set.tag %||% character(0)))) {
      row <- hv_summary[tolower(as.character(hv_summary$set.tag)) == sid, , drop = FALSE]
      out$n_samples <- first_pos_int(row$n.samples)
      out$n_features <- first_pos_int(row$n.features)
      return(out)
    }

    hv_run_meta_path <- file.path(root, "results", "asv_hv_k_gcv_sweep", "run.metadata.rds")
    hv_run_meta <- if (file.exists(hv_run_meta_path)) {
      suppressWarnings(tryCatch(readRDS(hv_run_meta_path), error = function(e) NULL))
    } else {
      NULL
    }
    if (is.list(hv_run_meta)) {
      out$n_samples <- if (is.finite(out$n_samples)) out$n_samples else {
        first_pos_int(hv_run_meta$asv.samples %||% hv_run_meta$sample_set.count)
      }
      out$n_features <- if (is.finite(out$n_features)) out$n_features else {
        first_pos_int(hv_run_meta$asv.features)
      }
      if (is.finite(out$n_samples) || is.finite(out$n_features)) {
        return(out)
      }
    }

    full_summary <- read_csv(file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "summary.across.criteria.csv"))
    if (is.data.frame(full_summary) && nrow(full_summary) > 0L && sid %in% c("all", "asv", "shared_all_asv")) {
      out$n_samples <- first_pos_int(full_summary$n.samples)
      out$n_features <- first_pos_int(full_summary$graph.features %||% full_summary$n.features.in.criterion)
      if (is.finite(out$n_samples) || is.finite(out$n_features)) {
        return(out)
      }
    }

    run_meta_path <- file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "run.metadata.rds")
    run_meta <- if (file.exists(run_meta_path)) {
      suppressWarnings(tryCatch(readRDS(run_meta_path), error = function(e) NULL))
    } else {
      NULL
    }
    if (is.list(run_meta)) {
      out$n_samples <- if (is.finite(out$n_samples)) out$n_samples else first_pos_int(run_meta$asv.samples)
      out$n_features <- if (is.finite(out$n_features)) out$n_features else first_pos_int(run_meta$asv.features)
    }

    out
  }

  graph_data_type_choices <- function(graph_sets) {
    if (!is.list(graph_sets) || length(graph_sets) < 1L) {
      return(c())
    }
    ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    labels <- vapply(graph_sets, infer_data_type_label, character(1))
    display <- labels
    dup <- duplicated(display) | duplicated(display, fromLast = TRUE)
    if (any(dup)) {
      display[dup] <- sprintf("%s (%s)", display[dup], ids[dup])
    }
    stats::setNames(ids, display)
  }

  graph_alias_tokens <- function(set_id = "", set_label = "") {
    out <- unique(tolower(c(
      as.character(set_id %||% ""),
      as.character(set_label %||% "")
    )))
    sid <- tolower(as.character(set_id %||% ""))
    if (grepl("^top[0-9]+$", sid, perl = TRUE)) {
      out <- c(out, sub("^top", "hv", sid))
    }
    if (identical(sid, "all")) {
      out <- c(out, "full", "shared_all_asv")
    }
    out[nzchar(out)]
  }

  infer_optimal_method_id <- function(path) {
    low <- tolower(basename(as.character(path %||% "")))
    if (grepl("mean_median_vs_k", low, fixed = TRUE) || grepl("median_vs_k", low, fixed = TRUE)) {
      return("median_norm_gcv")
    }
    if (grepl("k\\.distribution\\.summary", low, perl = TRUE) || grepl("k\\.selection\\.summary", low, perl = TRUE)) {
      return("median_norm_gcv")
    }
    if (grepl("median", low, fixed = TRUE) && grepl("gcv", low, fixed = TRUE)) {
      return("median_norm_gcv")
    }
    if (grepl("norm", low, fixed = TRUE) && grepl("gcv", low, fixed = TRUE)) {
      return("median_norm_gcv")
    }
    if (grepl("edit", low, fixed = TRUE) && grepl("dist", low, fixed = TRUE)) {
      return("edit_distance")
    }
    if (grepl("response", low, fixed = TRUE) && grepl("gcv", low, fixed = TRUE)) {
      return("response_gcv")
    }
    if (grepl("gcv", low, fixed = TRUE)) {
      return("response_gcv")
    }
    "criterion"
  }

  optimal_method_label <- function(id) {
    id_chr <- as.character(id %||% "")
    if (grepl("^median_norm_gcv_(?:criterion_)?hv([0-9]+)$", id_chr, perl = TRUE)) {
      dig <- sub("^median_norm_gcv_(?:criterion_)?hv([0-9]+)$", "\\1", id_chr, perl = TRUE)
      return(sprintf("median norm-GCV (HV%s)", dig))
    }
    if (grepl("^response_gcv_hv([0-9]+)$", id_chr, perl = TRUE)) {
      dig <- sub("^response_gcv_hv([0-9]+)$", "\\1", id_chr, perl = TRUE)
      return(sprintf("response GCV (HV%s)", dig))
    }
    if (identical(id_chr, "median_norm_gcv_summary")) {
      return("median norm-GCV summary")
    }
    switch(
      id_chr,
      median_norm_gcv = "median norm-GCV",
      response_gcv = "response GCV",
      response_gcv_all = "response GCV (all)",
      edit_distance = "edit distance",
      mixing = "mixing score",
      connectivity = "connectivity summary",
      criterion = "criterion summary",
      criterion_summary = "criterion summary",
      id_chr
    )
  }

  discover_optimal_k_methods <- function(manifest, spec) {
    out <- list()

    add_one <- function(method_id, path, source = "artifact") {
      pp <- as.character(path %||% "")
      if (!nzchar(pp)) {
        return(invisible(NULL))
      }
      if (!grepl("^https?://", pp, ignore.case = TRUE)) {
        pp <- tryCatch(normalizePath(path.expand(pp), mustWork = TRUE), error = function(e) "")
      }
      if (!nzchar(pp)) {
        return(invisible(NULL))
      }
      if (length(out) >= 1L && any(vapply(out, function(x) identical(x$path, pp), logical(1)))) {
        return(invisible(NULL))
      }
      out[[length(out) + 1L]] <<- list(
        id = as.character(method_id),
        label = optimal_method_label(method_id),
        path = pp,
        source = as.character(source)
      )
      invisible(NULL)
    }

    gs <- spec$graph_set
    if (is.list(gs$optimal_k_artifacts) && length(gs$optimal_k_artifacts) > 0L) {
      for (nm in names(gs$optimal_k_artifacts)) {
        add_one(
          method_id = sanitize_token_id(nm, fallback = "criterion"),
          path = gs$optimal_k_artifacts[[nm]],
          source = "graph_set.optimal_k_artifacts"
        )
      }
    }

    direct_paths <- normalize_paths(c(
      gs$optimal_k_pdf,
      gs$optimal_k_plot_pdf,
      gs$optimal_k_file,
      gs$k_source
    ))
    for (pp in direct_paths) {
      add_one(infer_optimal_method_id(pp), pp, source = "graph_set")
    }

    project_root <- as.character(manifest$project_root %||% "")
    if (nzchar(project_root) && !identical(project_root, "NA") && dir.exists(project_root)) {
      root <- tryCatch(normalizePath(project_root, mustWork = TRUE), error = function(e) "")
      if (nzchar(root)) {
        sid <- tolower(as.character(spec$set_id %||% ""))
        if (sid %in% c("top20", "top30", "top50")) {
          fam <- sub("^top", "hv", sid)
          add_one(
            "median_norm_gcv",
            file.path(root, "results", "asv_hv_k_gcv_sweep", "figures", sprintf("%s_mean_median_vs_k.pdf", sid)),
            source = "project_root.figures"
          )
          add_one(
            "median_norm_gcv_summary",
            file.path(root, "results", "asv_hv_k_gcv_sweep", sid, "k.distribution.summary.csv"),
            source = "project_root.summary"
          )
          add_one(
            "response_gcv",
            file.path(root, "results", "vag_odor_asv_graph_gcv_sweep", "figures", sprintf("%s_vag_odor_gcv_vs_k.pdf", fam)),
            source = "project_root.figures"
          )
        } else if (identical(sid, "all")) {
          add_one(
            "median_norm_gcv",
            file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "figures", "criterion_hv20_hv30_hv50_mean_median_vs_k.pdf"),
            source = "project_root.figures"
          )
          add_one(
            "median_norm_gcv_summary",
            file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "summary.across.criteria.csv"),
            source = "project_root.summary"
          )
          add_one(
            "median_norm_gcv_hv20",
            file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "figures", "criterion.hv20_mean_median_vs_k.pdf"),
            source = "project_root.figures"
          )
          add_one(
            "median_norm_gcv_hv30",
            file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "figures", "criterion.hv30_mean_median_vs_k.pdf"),
            source = "project_root.figures"
          )
          add_one(
            "median_norm_gcv_hv50",
            file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "figures", "criterion.hv50_mean_median_vs_k.pdf"),
            source = "project_root.figures"
          )
          add_one(
            "response_gcv",
            file.path(root, "results", "vag_odor_asv_graph_gcv_sweep", "figures", "all_vag_odor_gcv_vs_k.pdf"),
            source = "project_root.figures"
          )
        }
      }
    }

    tokens <- graph_alias_tokens(spec$set_id, spec$set_label)
    path_matches <- function(path) {
      low <- tolower(as.character(path %||% ""))
      if (!nzchar(low)) {
        return(FALSE)
      }
      if (length(tokens) < 1L) {
        return(TRUE)
      }
      any(vapply(tokens, function(tok) grepl(tok, low, fixed = TRUE), logical(1)))
    }

    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    for (cs in condexp_sets) {
      cs_paths <- normalize_paths(c(cs$gcv_summary_file, cs$summary_file, cs$optimal_k_pdf))
      for (pp in cs_paths) {
        if (path_matches(pp) || length(condexp_sets) == 1L) {
          add_one(infer_optimal_method_id(pp), pp, source = as.character(cs$id %||% "condexp"))
        }
      }
      if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        for (fr in cs$family_runs) {
          pp <- as.character(fr$summary_file %||% "")
          if (!nzchar(pp)) {
            next
          }
          if (path_matches(pp)) {
            add_one(infer_optimal_method_id(pp), pp, source = as.character(fr$family %||% "family"))
          }
        }
      }
    }

    if (length(out) < 1L) {
      return(list(
        choices = c("No criterion available" = ""),
        methods = list(),
        default = ""
      ))
    }

    ids <- vapply(out, function(x) as.character(x$id %||% "criterion"), character(1))
    labs <- vapply(out, function(x) as.character(x$label %||% "criterion"), character(1))
    rank_id <- function(x) {
      if (identical(x, "median_norm_gcv")) return(1L)
      if (grepl("^median_norm_gcv_", x, fixed = TRUE)) return(2L)
      if (identical(x, "response_gcv")) return(3L)
      if (grepl("^response_gcv_", x, fixed = TRUE)) return(4L)
      if (identical(x, "edit_distance")) return(5L)
      if (identical(x, "mixing")) return(6L)
      if (identical(x, "connectivity")) return(7L)
      if (identical(x, "criterion") || identical(x, "criterion_summary")) return(8L)
      99L
    }
    ord <- order(vapply(ids, rank_id, integer(1)), labs)
    out <- out[ord]
    ids <- ids[ord]
    labs <- labs[ord]

    keep <- !duplicated(ids)
    out <- out[keep]
    ids <- ids[keep]
    labs <- labs[keep]
    choices <- stats::setNames(ids, labs)

    default <- if ("median_norm_gcv" %in% ids) {
      "median_norm_gcv"
    } else {
      ids[1]
    }

    list(
      choices = choices,
      methods = stats::setNames(out, ids),
      default = default
    )
  }

  resolve_optimal_k_display_path <- function(path, set_tokens = character(0), cache_dir = "", method_id = "") {
    pp <- as.character(path %||% "")
    if (!nzchar(pp)) {
      return("")
    }
    if (grepl("^https?://", pp, ignore.case = TRUE)) {
      return(pp)
    }
    if (!file.exists(pp)) {
      return("")
    }
    pp <- normalizePath(pp, mustWork = TRUE)
    ext <- tolower(tools::file_ext(pp))
    if (identical(ext, "pdf")) {
      return(pp)
    }

    base <- tolower(tools::file_path_sans_ext(basename(pp)))
    tokens <- unique(tolower(c(
      base,
      as.character(method_id %||% ""),
      as.character(set_tokens %||% character(0)),
      sub("^top", "hv", as.character(set_tokens %||% character(0)))
    )))
    tokens <- tokens[nzchar(tokens)]

    candidate_dirs <- character(0)
    dd <- dirname(pp)
    for (ii in seq_len(5L)) {
      if (!nzchar(dd) || identical(dd, ".") || identical(dd, "/")) {
        break
      }
      candidate_dirs <- c(candidate_dirs, dd, file.path(dd, "figures"))
      next_dd <- dirname(dd)
      if (identical(next_dd, dd)) {
        break
      }
      dd <- next_dd
    }
    candidate_dirs <- unique(candidate_dirs[file.exists(candidate_dirs) & dir.exists(candidate_dirs)])

    pdfs <- character(0)
    for (one_dir in candidate_dirs) {
      found <- list.files(one_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
      if (length(found) > 0L) {
        pdfs <- c(pdfs, found)
      }
    }
    pdfs <- unique(pdfs[file.exists(pdfs)])
    if (length(pdfs) > 0L) {
      pdfs <- normalizePath(pdfs, mustWork = TRUE)
    }

    score_pdf <- function(one) {
      low <- tolower(one)
      base_name <- tolower(basename(one))
      sc <- 0
      for (tok in tokens) {
        if (grepl(tok, low, fixed = TRUE)) {
          sc <- sc + 2
        }
      }
      requested_hv <- tokens[grepl("^hv[0-9]+$", tokens, perl = TRUE)]
      if (length(requested_hv) > 0L) {
        for (hv in requested_hv) {
          if (grepl(sprintf("(^|[_\\.-])%s([_\\.-]|$)", hv), base_name, perl = TRUE)) {
            sc <- sc + 4
          }
        }
        if (grepl("hv20_hv30_hv50_all", base_name, fixed = TRUE)) {
          sc <- sc - 2
        }
      }
      requested_all <- "all" %in% tokens
      if (requested_all && grepl("all", base_name, fixed = TRUE)) {
        sc <- sc + 2
      }
      if (!requested_all && length(requested_hv) > 0L && grepl("all", base_name, fixed = TRUE)) {
        sc <- sc - 1
      }
      if (grepl("gcv", low, fixed = TRUE)) {
        sc <- sc + 1
      }
      if (grepl("mean_median_vs_k", low, fixed = TRUE)) {
        sc <- sc + 2
      }
      sc
    }

    if (length(pdfs) > 0L) {
      scores <- vapply(pdfs, score_pdf, numeric(1))
      pick <- pdfs[which.max(scores)]
      if (nzchar(pick)) {
        return(pick)
      }
    }

    generate_pdf_from_csv <- function(csv_path) {
      tbl <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.data.frame(tbl) || nrow(tbl) < 2L) {
        return("")
      }

      nms <- tolower(names(tbl))
      pick_col <- function(cands) {
        idx <- match(tolower(cands), nms)
        idx <- idx[is.finite(idx)]
        if (length(idx) < 1L) {
          return(NA_integer_)
        }
        as.integer(idx[[1]])
      }

      x_idx <- pick_col(c("k", "k.requested", "k_reported", "k.reported", "k.selected", "k_selected"))
      if (!is.finite(x_idx)) {
        return("")
      }

      prefer_median <- grepl("median_norm_gcv", tolower(method_id), fixed = TRUE) ||
        any(grepl("median", tokens, fixed = TRUE))
      y_candidates <- if (prefer_median) {
        c("median.norm", "mean.norm", "trimmed.mean.norm", "gcv.norm", "gcv", "value")
      } else {
        c("gcv", "gcv.norm", "median.norm", "mean.norm", "trimmed.mean.norm", "value")
      }
      y_idx <- pick_col(y_candidates)
      if (!is.finite(y_idx)) {
        return("")
      }

      xx <- suppressWarnings(as.numeric(tbl[[x_idx]]))
      yy <- suppressWarnings(as.numeric(tbl[[y_idx]]))
      keep <- is.finite(xx) & is.finite(yy)
      if (sum(keep) < 2L) {
        return("")
      }
      xx <- xx[keep]
      yy <- yy[keep]
      ord <- order(xx)
      xx <- xx[ord]
      yy <- yy[ord]

      cache_root <- as.character(cache_dir %||% "")
      if (!nzchar(cache_root)) {
        cache_root <- file.path(tempdir(), "gflowui_optimal_k_cache")
      }
      dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

      base_id <- sanitize_token_id(tools::file_path_sans_ext(basename(csv_path)), fallback = "optimal_k")
      mid <- sanitize_token_id(as.character(method_id %||% "criterion"), fallback = "criterion")
      out_pdf <- file.path(cache_root, sprintf("%s_%s.pdf", base_id, mid))

      csv_mtime <- tryCatch(file.info(csv_path)$mtime, error = function(e) as.POSIXct(NA))
      pdf_mtime <- tryCatch(file.info(out_pdf)$mtime, error = function(e) as.POSIXct(NA))
      if (file.exists(out_pdf) && is.finite(as.numeric(pdf_mtime)) && is.finite(as.numeric(csv_mtime)) &&
          as.numeric(pdf_mtime) >= as.numeric(csv_mtime)) {
        return(normalizePath(out_pdf, mustWork = TRUE))
      }

      title_txt <- sprintf("%s vs k", names(tbl)[[y_idx]])
      ylab_txt <- names(tbl)[[y_idx]]
      xlab_txt <- names(tbl)[[x_idx]]
      ok <- tryCatch(
        {
          grDevices::pdf(file = out_pdf, width = 8.5, height = 5.5, useDingbats = FALSE)
          on.exit(grDevices::dev.off(), add = TRUE)
          graphics::plot(
            xx, yy,
            type = "b", pch = 16, col = "#1f78b4", lwd = 1.6,
            xlab = xlab_txt, ylab = ylab_txt, main = title_txt
          )
          if (length(unique(xx)) > 1L) {
            k_best <- xx[[which.min(yy)]]
            graphics::abline(v = k_best, lty = 2, col = "#d95f02")
          }
          TRUE
        },
        error = function(e) FALSE
      )
      if (!isTRUE(ok) || !file.exists(out_pdf)) {
        return("")
      }
      normalizePath(out_pdf, mustWork = TRUE)
    }

    if (ext %in% c("csv", "tsv", "txt")) {
      gen_pdf <- generate_pdf_from_csv(pp)
      if (nzchar(gen_pdf)) {
        return(gen_pdf)
      }
    }

    pp
  }

  open_external_path <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp)) {
      return(FALSE)
    }
    if (grepl("^https?://", pp, ignore.case = TRUE)) {
      utils::browseURL(pp)
      return(TRUE)
    }
    if (!file.exists(pp)) {
      return(FALSE)
    }
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      status <- tryCatch(system2("open", pp), error = function(e) 1L)
      return(is.numeric(status) && as.integer(status) == 0L)
    }
    utils::browseURL(pp)
    TRUE
  }

  list(
    fmt_k_values = fmt_k_values,
    compact_values = compact_values,
    default_vertex_layout_for_graph = default_vertex_layout_for_graph,
    normalize_paths = normalize_paths,
    summarize_graph_assets = summarize_graph_assets,
    summarize_condexp_assets = summarize_condexp_assets,
    summarize_endpoint_assets = summarize_endpoint_assets,
    build_html_table = build_html_table,
    set_run_monitor_note = set_run_monitor_note,
    sanitize_token_id = sanitize_token_id,
    parse_k_values_text = parse_k_values_text,
    graph_set_choices = graph_set_choices,
    graph_set_k_values = graph_set_k_values,
    graph_k_choices = graph_k_choices,
    collect_outcomes_from_condexp = collect_outcomes_from_condexp,
    current_reference_info = current_reference_info,
    resolve_graph_selection = resolve_graph_selection,
    graph_set_by_id = graph_set_by_id,
    infer_data_type_label = infer_data_type_label,
    infer_feature_count = infer_feature_count,
    infer_sample_count = infer_sample_count,
    infer_graph_dims_from_project_metadata = infer_graph_dims_from_project_metadata,
    graph_data_type_choices = graph_data_type_choices,
    graph_alias_tokens = graph_alias_tokens,
    infer_optimal_method_id = infer_optimal_method_id,
    optimal_method_label = optimal_method_label,
    discover_optimal_k_methods = discover_optimal_k_methods,
    resolve_optimal_k_display_path = resolve_optimal_k_display_path,
    open_external_path = open_external_path
  )
}
