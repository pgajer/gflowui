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

    full_summary <- read_csv(file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "summary.across.criteria.csv"))
    if (is.data.frame(full_summary) && nrow(full_summary) > 0L && sid %in% c("all", "asv", "shared_all_asv")) {
      out$n_samples <- first_pos_int(full_summary$n.samples)
      out$n_features <- first_pos_int(full_summary$graph.features %||% full_summary$n.features.in.criterion)
      if (is.finite(out$n_samples) || is.finite(out$n_features)) {
        return(out)
      }
    }

    run_meta <- tryCatch(
      readRDS(file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "run.metadata.rds")),
      error = function(e) NULL
    )
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
    switch(
      as.character(id %||% ""),
      median_norm_gcv = "median norm-GCV",
      response_gcv = "response GCV",
      edit_distance = "edit distance",
      mixing = "mixing score",
      connectivity = "connectivity summary",
      criterion = "criterion summary",
      as.character(id %||% "criterion")
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
    ord <- order(
      match(ids, c("median_norm_gcv", "response_gcv", "edit_distance", "mixing", "connectivity", "criterion"), nomatch = 100L),
      labs
    )
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

  resolve_optimal_k_display_path <- function(path, set_tokens = character(0)) {
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
    ext <- tolower(tools::file_ext(pp))
    if (identical(ext, "pdf")) {
      return(normalizePath(pp, mustWork = TRUE))
    }

    dd <- dirname(pp)
    pdfs <- list.files(dd, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
    if (length(pdfs) < 1L) {
      return(normalizePath(pp, mustWork = TRUE))
    }
    pdfs <- normalizePath(pdfs[file.exists(pdfs)], mustWork = TRUE)
    if (length(pdfs) < 1L) {
      return(normalizePath(pp, mustWork = TRUE))
    }

    base <- tolower(tools::file_path_sans_ext(basename(pp)))
    tokens <- unique(tolower(c(base, as.character(set_tokens %||% character(0)))))
    tokens <- tokens[nzchar(tokens)]

    score_pdf <- function(one) {
      low <- tolower(one)
      sc <- 0
      for (tok in tokens) {
        if (grepl(tok, low, fixed = TRUE)) {
          sc <- sc + 2
        }
      }
      if (grepl("gcv", low, fixed = TRUE)) {
        sc <- sc + 1
      }
      sc
    }

    scores <- vapply(pdfs, score_pdf, numeric(1))
    pick <- pdfs[which.max(scores)]
    if (!nzchar(pick)) {
      pick <- pdfs[1]
    }
    pick
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
