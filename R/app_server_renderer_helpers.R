gflowui_make_server_renderer_helpers <- function(rv, current_reference_info) {
  if (!is.function(current_reference_info)) {
    stop("current_reference_info must be a function.", call. = FALSE)
  }

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

  normalize_paths <- function(paths) {
    x <- as.character(paths %||% character(0))
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  }

  sanitize_token_id <- function(x, fallback = "asset") {
    id <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(x %||% "")))
    id <- gsub("^_+|_+$", "", id)
    if (!nzchar(id)) {
      id <- fallback
    }
    id
  }
  normalize_coord_matrix <- function(coords) {
    mat <- as.matrix(coords)
    if (nrow(mat) < 1L) {
      return(matrix(numeric(0), ncol = 3L))
    }
    if (ncol(mat) < 3L) {
      mat <- cbind(mat, matrix(0, nrow = nrow(mat), ncol = 3L - ncol(mat)))
    }
    mat <- mat[, seq_len(3L), drop = FALSE]

    for (jj in seq_len(3L)) {
      v <- suppressWarnings(as.numeric(mat[, jj]))
      v[!is.finite(v)] <- NA_real_
      if (all(is.na(v))) {
        v <- rep(0, length(v))
      } else {
        mu <- mean(v, na.rm = TRUE)
        sdv <- stats::sd(v, na.rm = TRUE)
        if (!is.finite(sdv) || sdv < 1e-10) {
          sdv <- max(abs(v - mu), na.rm = TRUE)
        }
        if (!is.finite(sdv) || sdv < 1e-10) {
          sdv <- 1
        }
        v <- (v - mu) / sdv
        v[!is.finite(v)] <- 0
      }
      mat[, jj] <- v
    }
    mat
  }

  adj_to_edge_matrix <- function(adj_list) {
    nn <- length(adj_list)
    if (nn < 2L) {
      return(matrix(integer(0), ncol = 2L))
    }

    pairs <- lapply(seq_len(nn), function(ii) {
      nb <- suppressWarnings(as.integer(adj_list[[ii]] %||% integer(0)))
      nb <- nb[is.finite(nb) & nb > ii & nb <= nn]
      if (length(nb) < 1L) {
        return(NULL)
      }
      cbind(rep.int(ii, length(nb)), nb)
    })

    edges <- do.call(rbind, pairs)
    if (is.null(edges)) {
      matrix(integer(0), ncol = 2L)
    } else {
      edges
    }
  }

  extract_graph_collection <- function(graph_obj) {
    if (!is.list(graph_obj)) {
      return(NULL)
    }

    xg <- graph_obj$X.graphs
    if (is.null(xg)) {
      xg <- graph_obj
    }

    if (is.list(xg) && is.list(xg$geom_pruned_graphs) && length(xg$geom_pruned_graphs) > 0L) {
      kv <- suppressWarnings(as.integer(graph_obj$k.values %||% graph_obj$k.reported %||% xg$k.values %||% xg$k.reported))
      return(list(graphs = xg$geom_pruned_graphs, k_values = kv))
    }

    if (is.list(xg) && length(xg) == 1L && is.list(xg[[1]]) && length(xg[[1]]) > 0L &&
        is.list(xg[[1]][[1]]) && !is.null(xg[[1]][[1]]$adj_list)) {
      kv <- suppressWarnings(as.integer(graph_obj$k.reported %||% graph_obj$k.values %||% graph_obj$k.requested))
      return(list(graphs = xg[[1]], k_values = kv))
    }

    if (is.list(xg) && length(xg) > 0L && is.list(xg[[1]]) && !is.null(xg[[1]]$adj_list)) {
      kv <- suppressWarnings(as.integer(graph_obj$k.values %||% graph_obj$k.reported))
      return(list(graphs = xg, k_values = kv))
    }

    NULL
  }

  select_graph_for_k <- function(collection, target_k = NA_integer_) {
    if (is.null(collection) || !is.list(collection$graphs) || length(collection$graphs) < 1L) {
      return(NULL)
    }

    graphs <- collection$graphs
    kk <- suppressWarnings(as.integer(collection$k_values))
    target <- scalar_int(target_k, default = NA_integer_)
    idx <- 1L
    k_actual <- NA_integer_

    if (length(kk) == length(graphs) && any(is.finite(kk))) {
      if (is.finite(target) && any(kk == target, na.rm = TRUE)) {
        idx <- which(kk == target)[1]
      } else if (is.finite(target)) {
        dist <- abs(kk - target)
        dist[!is.finite(dist)] <- Inf
        idx <- which.min(dist)
        if (!is.finite(dist[idx])) {
          idx <- 1L
        }
      } else {
        idx <- which(is.finite(kk))[1]
        if (length(idx) < 1L || is.na(idx)) {
          idx <- 1L
        }
      }
      k_actual <- suppressWarnings(as.integer(kk[idx]))
    } else {
      if (is.finite(target) && target >= 1L && target <= length(graphs)) {
        idx <- as.integer(target)
      }
      k_actual <- scalar_int(target, default = NA_integer_)
    }

    g <- graphs[[idx]]
    if (!is.list(g) || is.null(g$adj_list)) {
      return(NULL)
    }

    list(
      graph = g,
      index = idx,
      k_actual = k_actual
    )
  }

  resolve_reference_spec <- function(
      manifest,
      preferred_set_id = NA_character_,
      preferred_k = NA_integer_) {
    if (!is.list(manifest)) {
      return(NULL)
    }
    graph_sets <- if (is.list(manifest$graph_sets)) manifest$graph_sets else list()
    if (length(graph_sets) < 1L) {
      return(NULL)
    }

    ref <- current_reference_info(manifest)
    set_id <- scalar_chr(preferred_set_id %||% "", default = "")
    if (!nzchar(set_id)) {
      set_id <- scalar_chr(ref$set_id %||% "", default = "")
    }
    if (!nzchar(set_id)) {
      set_id <- scalar_chr(manifest$defaults$graph_set_id %||% "", default = "")
    }

    ids <- vapply(graph_sets, function(gs) as.character(gs$id %||% ""), character(1))
    idx <- match(set_id, ids)
    if (length(idx) < 1L || is.na(idx[[1]])) {
      idx <- 1L
    } else {
      idx <- idx[[1]]
    }
    gs <- graph_sets[[idx]]

    k_ref <- scalar_int(preferred_k, default = NA_integer_)
    if (!is.finite(k_ref)) {
      k_ref <- scalar_int(ref$k, default = NA_integer_)
    }
    if (!is.finite(k_ref)) {
      k_ref <- scalar_int(gs$selected_k, default = NA_integer_)
    }
    if (!is.finite(k_ref)) {
      kvals <- suppressWarnings(as.integer(gs$k_values %||% integer(0)))
      kvals <- kvals[is.finite(kvals)]
      if (length(kvals) > 0L) {
        k_ref <- kvals[1]
      }
    }

    list(
      graph_set = gs,
      set_id = as.character(gs$id %||% ids[idx]),
      set_label = as.character(gs$label %||% gs$id %||% "graph_set"),
      k_ref = scalar_int(k_ref, default = NA_integer_),
      reference = ref
    )
  }

  find_fit_file_for_k <- function(fit_files, k_use) {
    files <- normalize_paths(fit_files)
    if (length(files) < 1L || !is.finite(suppressWarnings(as.integer(k_use)))) {
      return(character(0))
    }
    kk <- as.integer(k_use)
    pat <- sprintf("k0*%d\\.rds$", kk)
    out <- files[grepl(pat, basename(files), perl = TRUE)]
    if (length(out) > 0L) {
      return(out)
    }

    file_k <- .k_from_filename_vector(files)
    if (length(file_k) < 1L) {
      return(character(0))
    }
    nearest <- file_k[which.min(abs(file_k - kk))]
    pat2 <- sprintf("k0*%d\\.rds$", nearest)
    files[grepl(pat2, basename(files), perl = TRUE)]
  }

  extract_lcc_index <- function(adj_list) {
    if (!is.list(adj_list) || length(adj_list) < 1L) {
      return(integer(0))
    }
    comp <- tryCatch(
      gflow::graph.connected.components(adj_list),
      error = function(e) NULL
    )
    comp <- suppressWarnings(as.integer(comp))
    if (length(comp) != length(adj_list)) {
      return(integer(0))
    }
    tab <- table(comp)
    if (length(tab) < 1L) {
      return(integer(0))
    }
    as.integer(which(comp == as.integer(names(which.max(tab)))))
  }

  expand_lcc_to_full <- function(values, reference_adj_list, n_vertices) {
    vv <- suppressWarnings(as.numeric(values))
    if (length(vv) == n_vertices) {
      return(vv)
    }
    if (!is.list(reference_adj_list) || length(reference_adj_list) != n_vertices) {
      return(vv)
    }
    idx <- extract_lcc_index(reference_adj_list)
    if (length(idx) != length(vv)) {
      return(vv)
    }
    out <- rep(NA_real_, n_vertices)
    out[idx] <- vv
    out
  }

  collect_reference_metadata_sources <- function(manifest, graph_set, n_vertices) {
    out <- list()
    if (!is.list(manifest) || !is.list(graph_set) || n_vertices < 1L) {
      return(out)
    }

    color_assets <- if (is.list(graph_set$color_assets)) graph_set$color_assets else list()
    preferred <- c("CST", "subCST")
    preferred <- c(preferred, as.character(color_assets$preferred_order %||% character(0)))
    cols_hint <- as.character(color_assets$vector_columns %||% character(0))
    cols_hint <- cols_hint[nzchar(cols_hint)]
    if (length(cols_hint) > 0L) {
      preferred <- unique(c(preferred, cols_hint))
    }
    preferred <- unique(preferred[nzchar(preferred)])

    labels_map <- character(0)
    labels_raw <- color_assets$labels
    if (is.list(labels_raw)) {
      labels_raw <- unlist(labels_raw, recursive = TRUE, use.names = TRUE)
    }
    if (is.character(labels_raw) && !is.null(names(labels_raw)) && length(labels_raw) > 0L) {
      keep <- nzchar(names(labels_raw)) & nzchar(as.character(labels_raw))
      labels_map <- as.character(labels_raw[keep])
      names(labels_map) <- tolower(as.character(names(labels_raw)[keep]))
    }

    metadata_object <- scalar_chr(color_assets$metadata_object %||% "mt.asv", default = "mt.asv")

    root <- scalar_chr(manifest$project_root %||% "", default = "")
    candidates <- c(
      as.character(color_assets$metadata_file %||% ""),
      if (nzchar(root)) file.path(root, "data", "S_asv.rda") else ""
    )
    candidates <- unique(candidates[nzchar(candidates)])
    if (length(candidates) < 1L) {
      return(out)
    }

    for (path in candidates) {
      if (!file.exists(path)) {
        next
      }
      env <- new.env(parent = emptyenv())
      ok <- tryCatch({
        load(path, envir = env)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        next
      }
      object_candidates <- unique(c(metadata_object, "mt.asv", "mt"))
      mt <- NULL
      for (obj_name in object_candidates) {
        if (!exists(obj_name, envir = env, inherits = FALSE)) {
          next
        }
        candidate <- get(obj_name, envir = env, inherits = FALSE)
        if (is.data.frame(candidate) && nrow(candidate) == n_vertices) {
          mt <- candidate
          break
        }
      }
      if (!is.data.frame(mt) || nrow(mt) != n_vertices) {
        next
      }

      cn <- names(mt)
      canon <- function(nm) {
        idx <- match(tolower(nm), tolower(cn))
        idx <- idx[is.finite(idx)]
        if (length(idx) < 1L) return("")
        as.character(cn[idx[[1]]])
      }
      use_cols <- unique(vapply(preferred, canon, character(1)))
      use_cols <- use_cols[nzchar(use_cols)]
      if (length(use_cols) < 1L) {
        next
      }

      add_one <- function(key, label, values, type = c("numeric", "categorical")) {
        type <- match.arg(type)
        vv <- values
        if (length(vv) != n_vertices || all(is.na(vv))) {
          return(invisible(NULL))
        }
        kk <- sanitize_token_id(key, fallback = "meta")
        while (kk %in% names(out)) {
          kk <- sprintf("%s_%d", kk, length(out) + 1L)
        }
        out[[kk]] <<- list(key = kk, label = label, type = type, values = vv)
        invisible(NULL)
      }

      pretty_label <- function(col_name) {
        low <- tolower(col_name)
        if (low %in% names(labels_map)) {
          return(labels_map[[low]])
        }
        if (identical(low, "cst")) {
          return("CST")
        }
        if (identical(low, "subcst")) {
          return("subCST")
        }
        txt <- gsub("_+", " ", as.character(col_name))
        paste(toupper(substr(txt, 1L, 1L)), substr(txt, 2L, nchar(txt)), sep = "")
      }

      for (col in use_cols) {
        vv_raw <- mt[[col]]
        label <- pretty_label(col)
        if (is.factor(vv_raw) || is.character(vv_raw) || is.logical(vv_raw)) {
          add_one(col, label, as.character(vv_raw), type = "categorical")
        } else {
          vv_num <- suppressWarnings(as.numeric(vv_raw))
          if (length(vv_num) == n_vertices) {
            add_one(col, label, vv_num, type = "numeric")
          }
        }
      }
      if (length(out) > 0L) {
        break
      }
    }

    out
  }

  collect_reference_condexp_sources <- function(manifest, set_id, k_use, n_vertices, reference_adj_list = NULL) {
    sources <- list()
    spectral_coords <- NULL
    spectral_best <- NULL
    spectral_best_score <- -Inf
    spectral_fallback <- NULL

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
        label = as.character(label),
        type = type,
        values = vv
      )
      invisible(NULL)
    }

    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    set_id_chr <- tolower(as.character(set_id %||% ""))
    alias <- unique(c(
      set_id_chr,
      sub("^top", "hv", set_id_chr),
      sub("^hv", "top", set_id_chr),
      sub("^asv[_-]?", "", set_id_chr),
      sub("^shared_", "", set_id_chr)
    ))
    if (set_id_chr %in% c("all", "asv", "shared_all_asv", "full", "asvfull")) {
      alias <- unique(c(alias, "all", "asv", "shared_all_asv", "full", "asvfull"))
    }
    alias <- alias[nzchar(alias)]

    pretty_outcome_label <- function(x) {
      xx <- as.character(x %||% "")
      xx <- xx[nzchar(xx)]
      if (length(xx) < 1L) {
        return("")
      }
      txt <- xx[[1]]
      txt <- gsub("[^A-Za-z0-9]+", "_", txt)
      txt <- gsub("^_+|_+$", "", txt)
      if (!nzchar(txt)) {
        return("")
      }
      low <- tolower(txt)
      if (low %in% c("ibs", "ibd", "vag_odor")) {
        return(toupper(low))
      }
      txt <- gsub("_+", " ", txt)
      paste(toupper(substr(txt, 1L, 1L)), substr(txt, 2L, nchar(txt)), sep = "")
    }

    as_numeric_vector <- function(x) {
      xx <- x
      if (is.logical(xx)) {
        xx <- as.numeric(xx)
      } else if (is.factor(xx)) {
        xx <- as.character(xx)
      }
      suppressWarnings(as.numeric(xx))
    }

    binary_outcome_rate <- function(y) {
      yy <- as_numeric_vector(y)
      yy <- yy[is.finite(yy)]
      if (length(yy) < 1L) {
        return(NA_real_)
      }
      uniq <- sort(unique(yy))
      if (length(uniq) < 1L || length(uniq) > 2L) {
        return(NA_real_)
      }
      if (!all(uniq %in% c(0, 1))) {
        return(NA_real_)
      }
      mu <- mean(yy, na.rm = TRUE)
      if (!is.finite(mu) || mu <= 0) {
        return(NA_real_)
      }
      mu
    }

    add_relative_condexp_source <- function(key_base, outcome_label, yhat, yobs) {
      yhat_num <- as_numeric_vector(yhat)
      yobs_num <- as_numeric_vector(yobs)
      if (length(yhat_num) != n_vertices || length(yobs_num) != n_vertices) {
        return(invisible(NULL))
      }
      mu <- binary_outcome_rate(yobs_num)
      if (!is.finite(mu) || mu <= 0) {
        return(invisible(NULL))
      }
      add_source(
        key = sprintf("%s_rel_yhat", key_base),
        label = sprintf("%s rel.y.hat", as.character(outcome_label)),
        values = yhat_num / mu,
        type = "numeric"
      )
      invisible(NULL)
    }

    for (cs in condexp_sets) {
      cs_id <- as.character(cs$id %||% "condexp")
      cs_outcomes <- as.character(cs$outcomes %||% character(0))
      cs_outcomes <- cs_outcomes[nzchar(cs_outcomes)]
      cs_outcome_label <- pretty_outcome_label(if (length(cs_outcomes) > 0L) cs_outcomes[[1]] else cs_id)
      if (!nzchar(cs_outcome_label)) {
        cs_outcome_label <- cs_id
      }

      if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        for (fr in cs$family_runs) {
          fam <- as.character(fr$family %||% "family")
          fam_key <- tolower(fam)
          fam_alias <- unique(c(
            fam_key,
            sub("^top", "hv", fam_key),
            sub("^hv", "top", fam_key),
            sub("^asv[_-]?", "", fam_key),
            sub("^shared_", "", fam_key)
          ))
          if (fam_key %in% c("all", "asv", "shared_all_asv", "full", "asvfull")) {
            fam_alias <- unique(c(fam_alias, "all", "asv", "shared_all_asv", "full", "asvfull"))
          }
          fam_alias <- fam_alias[nzchar(fam_alias)]
          fam_match <- (fam_key %in% alias) || (length(intersect(alias, fam_alias)) > 0L)
          if (!isTRUE(fam_match)) {
            next
          }

          cand <- find_fit_file_for_k(fr$fit_files, k_use = k_use)
          if (length(cand) < 1L) {
            next
          }

          score <- vapply(cand, function(pp) {
            sum(vapply(alias, function(tok) nzchar(tok) && grepl(tok, pp, fixed = TRUE), logical(1)))
          }, numeric(1))
          path <- cand[which.max(score)]

          fit <- tryCatch(readRDS(path), error = function(e) NULL)
          if (!is.list(fit)) {
            next
          }

          yhat <- expand_lcc_to_full(
            values = fit$fitted.values %||% numeric(0),
            reference_adj_list = reference_adj_list,
            n_vertices = n_vertices
          )
          yobs <- expand_lcc_to_full(
            values = fit$y %||% numeric(0),
            reference_adj_list = reference_adj_list,
            n_vertices = n_vertices
          )
          src_base_key <- sprintf("condexp_%s_%s", cs_id, fam)
          if (length(yhat) == n_vertices) {
            add_source(
              key = sprintf("%s_yhat", src_base_key),
              label = sprintf("%s CondExp", cs_outcome_label),
              values = yhat,
              type = "numeric"
            )
          }
          if (length(yobs) == n_vertices) {
            add_source(
              key = sprintf("%s_yobs", src_base_key),
              label = sprintf("Observed %s", cs_outcome_label),
              values = yobs,
              type = "numeric"
            )
          }
          add_relative_condexp_source(
            key_base = src_base_key,
            outcome_label = cs_outcome_label,
            yhat = yhat,
            yobs = yobs
          )

          if (is.list(fit$spectral) && is.matrix(fit$spectral$eigenvectors)) {
            ev <- fit$spectral$eigenvectors
            if (nrow(ev) == n_vertices && ncol(ev) >= 3L) {
              cols <- if (ncol(ev) >= 4L) 2:4 else 1:3
              cand_coords <- ev[, cols, drop = FALSE]
              if (is.null(spectral_fallback)) {
                spectral_fallback <- cand_coords
              }

              score <- 0
              if (length(alias) > 0L && length(fam_alias) > 0L) {
                score <- score + 5L * length(intersect(alias, fam_alias))
              }
              if (fam_key %in% alias) {
                score <- score + 10L
              }
              if (score > spectral_best_score) {
                spectral_best <- cand_coords
                spectral_best_score <- score
              }
            }
          }
        }
      }

      long_file <- as.character(cs$long_table_file %||% "")
      if (nzchar(long_file) && file.exists(long_file)) {
        tbl <- tryCatch(readRDS(long_file), error = function(e) NULL)
        if (is.data.frame(tbl) && nrow(tbl) > 0L && all(c("outcome", "y_fitted") %in% names(tbl))) {
          if ("k" %in% names(tbl) && is.finite(suppressWarnings(as.integer(k_use)))) {
            tbl <- tbl[suppressWarnings(as.integer(tbl$k)) == as.integer(k_use), , drop = FALSE]
          }
          outs <- unique(as.character(tbl$outcome))
          for (oo in outs) {
            dd <- tbl[as.character(tbl$outcome) == oo, , drop = FALSE]
            if (nrow(dd) != n_vertices) {
              next
            }
            oo_label <- pretty_outcome_label(oo)
            src_base_key <- sprintf("long_%s_%s", cs_id, oo)
            yhat <- suppressWarnings(as.numeric(dd$y_fitted))
            yobs <- if ("y_observed" %in% names(dd)) suppressWarnings(as.numeric(dd$y_observed)) else numeric(0)
            add_source(
              key = sprintf("%s_yhat", src_base_key),
              label = sprintf("%s CondExp", oo_label),
              values = yhat,
              type = "numeric"
            )
            if ("y_observed" %in% names(dd)) {
              add_source(
                key = sprintf("%s_yobs", src_base_key),
                label = sprintf("Observed %s", oo_label),
                values = yobs,
                type = "numeric"
              )
            }
            add_relative_condexp_source(
              key_base = src_base_key,
              outcome_label = oo_label,
              yhat = yhat,
              yobs = yobs
            )
          }
        }
      }
    }

    if (!is.null(spectral_best)) {
      spectral_coords <- spectral_best
    } else if (!is.null(spectral_fallback)) {
      spectral_coords <- spectral_fallback
    }

    list(
      sources = sources,
      spectral_coords = spectral_coords
    )
  }

  collect_reference_endpoint_sources <- function(manifest, k_use, n_vertices, reference_adj_list = NULL) {
    sources <- list()
    if (!is.list(manifest) || n_vertices < 1L) {
      return(sources)
    }

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
        label = as.character(label),
        type = type,
        values = vv
      )
      invisible(NULL)
    }

    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    if (length(endpoint_runs) < 1L) {
      return(sources)
    }

    default_ep <- tolower(scalar_chr(manifest$defaults$endpoint_run_id %||% "", default = ""))
    if (nzchar(default_ep)) {
      ids <- tolower(vapply(endpoint_runs, function(ep) scalar_chr(ep$id %||% "", default = ""), character(1)))
      hit <- match(default_ep, ids)
      if (is.finite(hit) && hit >= 1L && hit <= length(endpoint_runs)) {
        endpoint_runs <- c(endpoint_runs[hit], endpoint_runs[-hit])
      }
    }

    k_int <- suppressWarnings(as.integer(k_use))
    if (!is.finite(k_int)) {
      k_int <- NA_integer_
    }

    get_named <- function(lst, candidates) {
      if (!is.list(lst) || length(candidates) < 1L) {
        return(NULL)
      }
      nms <- names(lst)
      if (is.null(nms) || length(nms) < 1L) {
        return(NULL)
      }
      nms_low <- tolower(nms)
      for (cand in candidates) {
        cc <- as.character(cand %||% "")
        if (!nzchar(cc)) {
          next
        }
        idx <- match(tolower(cc), nms_low)
        if (is.finite(idx) && idx >= 1L && idx <= length(lst)) {
          return(lst[[idx]])
        }
      }
      NULL
    }

    expand_to_full <- function(values, obj = NULL) {
      vv <- suppressWarnings(as.numeric(values))
      if (length(vv) == n_vertices) {
        return(vv)
      }

      idx_global <- suppressWarnings(as.integer(c(
        get_named(obj, c(
          "lcc.index.global", "lcc_index_global",
          "vertex.global", "vertex_global",
          "index.global", "index_global"
        )),
        if (is.list(obj$lcc)) {
          get_named(obj$lcc, c(
            "lcc.index.global", "lcc_index_global",
            "vertex.global", "vertex_global",
            "index.global", "index_global"
          ))
        } else {
          integer(0)
        }
      )))
      idx_global <- idx_global[is.finite(idx_global) & idx_global >= 1L & idx_global <= n_vertices]

      if (length(idx_global) == length(vv) && length(vv) > 0L) {
        out <- rep(NA_real_, n_vertices)
        out[idx_global] <- vv
        return(out)
      }

      if (is.list(reference_adj_list) && length(reference_adj_list) == n_vertices) {
        out <- expand_lcc_to_full(
          values = vv,
          reference_adj_list = reference_adj_list,
          n_vertices = n_vertices
        )
        if (length(out) == n_vertices) {
          return(out)
        }
      }

      vv
    }

    pick_run_bundle <- function(ep_run) {
      per_k_files <- normalize_paths(ep_run$per_k_bundles %||% character(0))
      per_k_files <- per_k_files[file.exists(per_k_files)]
      bundle_file <- scalar_chr(ep_run$bundle_file %||% "", default = "")

      if (length(per_k_files) > 0L && is.finite(k_int)) {
        cand <- find_fit_file_for_k(per_k_files, k_use = k_int)
        if (length(cand) > 0L) {
          return(cand[[1]])
        }

        parse_file_k <- function(path) {
          mm <- regexec("k0*([0-9]+)", basename(path), perl = TRUE)
          rr <- regmatches(basename(path), mm)[[1]]
          if (length(rr) >= 2L && nzchar(rr[[2]])) {
            val <- suppressWarnings(as.integer(rr[[2]]))
            if (is.finite(val) && val > 0L) {
              return(val)
            }
          }
          NA_integer_
        }
        file_k <- suppressWarnings(as.integer(vapply(per_k_files, parse_file_k, integer(1))))
        keep <- is.finite(file_k)
        if (any(keep)) {
          files_use <- per_k_files[keep]
          k_use <- file_k[keep]
          exact <- which(k_use == k_int)
          if (length(exact) > 0L) {
            return(files_use[[exact[[1]]]])
          }
          near <- which.min(abs(k_use - k_int))
          if (is.finite(near) && near >= 1L && near <= length(files_use)) {
            return(files_use[[near]])
          }
        }
      }

      if (nzchar(bundle_file) && file.exists(bundle_file)) {
        return(normalizePath(bundle_file, mustWork = TRUE))
      }

      if (length(per_k_files) > 0L) {
        return(per_k_files[[1]])
      }

      ""
    }

    have_condexp <- FALSE
    have_raw <- FALSE

    for (ep in endpoint_runs) {
      bundle_path <- pick_run_bundle(ep)
      if (!nzchar(bundle_path)) {
        next
      }
      obj <- tryCatch(readRDS(bundle_path), error = function(e) NULL)
      if (!is.list(obj)) {
        next
      }

      if (!have_condexp) {
        fit_vals <- get_named(
          obj,
          c(
            "fitted.evenness", "fitted_evenness",
            "evenness.hat", "evenness_hat",
            "fitted.values", "fitted_values",
            "fitted.evenness.lcc", "fitted_evenness_lcc"
          )
        )
        if (is.null(fit_vals) && is.list(obj$fit)) {
          fit_vals <- get_named(
            obj$fit,
            c("fitted.values", "fitted_values", "fitted.evenness", "fitted_evenness")
          )
        }
        fit_full <- expand_to_full(fit_vals, obj = obj)
        if (length(fit_full) == n_vertices && !all(is.na(fit_full))) {
          add_source(
            key = "endpoint_evenness_condexp",
            label = "Evenness CondExp",
            values = fit_full,
            type = "numeric"
          )
          have_condexp <- TRUE
        }
      }

      if (!have_raw) {
        raw_vals <- get_named(
          obj,
          c(
            "sample.evenness.raw", "sample_evenness_raw",
            "sample.evenness", "sample_evenness",
            "evenness.raw", "evenness"
          )
        )
        if (is.null(raw_vals) && is.list(obj$fit)) {
          raw_vals <- get_named(obj$fit, c("y", "y_observed", "observed"))
        }
        raw_full <- expand_to_full(raw_vals, obj = obj)
        if (length(raw_full) == n_vertices && !all(is.na(raw_full))) {
          add_source(
            key = "endpoint_evenness",
            label = "Evenness",
            values = raw_full,
            type = "numeric"
          )
          have_raw <- TRUE
        }
      }

      if (have_condexp && have_raw) {
        break
      }
    }

    sources
  }

  compute_reference_layout <- function(adj_list, cache_key, spectral_coords = NULL) {
    cache <- rv$reference.layout.cache %||% list()
    if (cache_key %in% names(cache)) {
      return(cache[[cache_key]])
    }

    nn <- length(adj_list)
    coords <- NULL

    if (!is.null(spectral_coords)) {
      coords <- normalize_coord_matrix(spectral_coords)
    }

    if (is.null(coords) && requireNamespace("igraph", quietly = TRUE) && nn > 1L && nn <= 7000L) {
      edges <- adj_to_edge_matrix(adj_list)
      if (nrow(edges) > 0L) {
        coords <- tryCatch(
          {
            g <- igraph::graph_from_edgelist(edges, directed = FALSE)
            igraph::layout_with_fr(g, dim = 3)
          },
          error = function(e) NULL
        )
      }
    }

    if (is.null(coords)) {
      idx <- seq_len(max(1L, nn))
      deg <- lengths(adj_list)
      if (length(deg) < nn) {
        deg <- rep(0, nn)
      }
      deg <- suppressWarnings(as.numeric(deg))
      deg[!is.finite(deg)] <- 0
      dmin <- min(deg)
      dmax <- max(deg)
      span <- dmax - dmin
      dscaled <- if (is.finite(span) && span > 1e-10) (deg - dmin) / span else rep(0, nn)

      theta <- (idx - 1) / max(1, nn) * 2 * pi
      radius <- 1 + 0.7 * dscaled
      z <- if (nn > 1L) (rank(deg, ties.method = "average") - 1) / (nn - 1) * 2 - 1 else 0
      coords <- cbind(
        x = radius * cos(theta),
        y = radius * sin(theta),
        z = z
      )
    }

    coords <- normalize_coord_matrix(coords)
    cache[[cache_key]] <- coords
    rv$reference.layout.cache <- cache
    coords
  }

  normalize_token_key <- function(x) {
    tolower(gsub("[^a-z0-9]+", "_", as.character(x %||% "")))
  }

  normalize_size_label <- function(x) {
    txt <- tolower(as.character(x %||% ""))
    txt <- gsub("_", ".", txt)
    if (!nzchar(txt)) {
      return("1x")
    }
    if (grepl("^[0-9]+(?:\\.[0-9]+)?x$", txt, perl = TRUE)) {
      return(txt)
    }
    if (grepl("^[0-9]+(?:\\.[0-9]+)?$", txt, perl = TRUE)) {
      return(sprintf("%sx", txt))
    }
    mm <- regexec("([0-9]+(?:\\.[0-9]+)?)x", txt, perl = TRUE)
    rr <- regmatches(txt, mm)[[1]]
    if (length(rr) >= 2L && nzchar(rr[[2]])) {
      return(sprintf("%sx", rr[[2]]))
    }
    "1x"
  }

  parse_size_numeric <- function(x, default = 1) {
    val <- suppressWarnings(as.numeric(gsub("[^0-9.]+", "", as.character(x %||% ""))))
    if (!is.finite(val) || val <= 0) {
      return(as.numeric(default))
    }
    val
  }

  size_label_to_manifest_tag <- function(size_label) {
    num <- parse_size_numeric(size_label, default = 1)
    tag <- sprintf("s%0.2f", num)
    gsub("\\.", "p", tag)
  }

  project_layout_manifest_matrix <- function(project_root, spec, size_label = "1x") {
    root <- as.character(project_root %||% "")
    if (!nzchar(root) || identical(root, "NA")) {
      return(NULL)
    }
    root <- tryCatch(normalizePath(path.expand(root), mustWork = TRUE), error = function(e) "")
    if (!nzchar(root)) {
      return(NULL)
    }

    set_id <- tolower(as.character(spec$set_id %||% ""))
    k_ref <- suppressWarnings(as.integer(spec$k_ref))
    req_size <- parse_size_numeric(size_label, default = 1)
    req_size_tag <- tolower(size_label_to_manifest_tag(size_label))

    set_alias <- unique(c(
      set_id,
      sub("^asv[_-]?", "", set_id),
      sub("^shared_", "", set_id),
      if (set_id %in% c("shared_all_asv", "asv")) "all" else character(0)
    ))
    set_alias <- set_alias[nzchar(set_alias)]

    read_one_manifest <- function(path) {
      if (!file.exists(path)) {
        return(NULL)
      }
      tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
        return(NULL)
      }
      if ("status" %in% names(tbl)) {
        tbl <- tbl[tolower(as.character(tbl$status)) %in% c("written", "ok", ""), , drop = FALSE]
      }
      if (nrow(tbl) < 1L) {
        return(NULL)
      }

      if ("set.tag" %in% names(tbl) && length(set_alias) > 0L) {
        set_col <- tolower(as.character(tbl$set.tag))
        tbl <- tbl[set_col %in% set_alias, , drop = FALSE]
      }
      if (nrow(tbl) < 1L) {
        return(NULL)
      }

      if (is.finite(k_ref) && "k" %in% names(tbl)) {
        k_col <- suppressWarnings(as.integer(tbl$k))
        tbl_k <- tbl[k_col == k_ref, , drop = FALSE]
        if (nrow(tbl_k) > 0L) {
          tbl <- tbl_k
        }
      }

      if ("size.tag" %in% names(tbl)) {
        tag_col <- tolower(as.character(tbl$size.tag))
        tbl_tag <- tbl[tag_col == req_size_tag, , drop = FALSE]
        if (nrow(tbl_tag) > 0L) {
          tbl <- tbl_tag
        }
      } else if ("sphere.scale" %in% names(tbl)) {
        ss <- suppressWarnings(as.numeric(tbl$sphere.scale))
        if (any(is.finite(ss))) {
          idx <- which.min(abs(ss - req_size))
          if (length(idx) > 0L && is.finite(idx[[1]])) {
            target <- ss[[idx[[1]]]]
            tbl <- tbl[abs(ss - target) < 1e-9, , drop = FALSE]
          }
        }
      }

      layout_col <- if ("layout.file" %in% names(tbl)) {
        "layout.file"
      } else if ("layout_file" %in% names(tbl)) {
        "layout_file"
      } else {
        ""
      }
      if (!nzchar(layout_col)) {
        return(NULL)
      }

      lf <- as.character(tbl[[layout_col]] %||% character(0))
      lf <- lf[!is.na(lf) & nzchar(lf)]
      lf <- lf[file.exists(lf)]
      if (length(lf) < 1L) {
        return(NULL)
      }

      for (path_one in unique(lf)) {
        mat <- tryCatch(readRDS(path_one), error = function(e) NULL)
        if (is.data.frame(mat)) {
          mat <- as.matrix(mat)
        } else {
          mat <- suppressWarnings(as.matrix(mat))
        }
        if (!is.matrix(mat) || nrow(mat) < 1L || ncol(mat) < 3L) {
          next
        }
        num <- suppressWarnings(matrix(
          as.numeric(mat),
          nrow = nrow(mat),
          ncol = ncol(mat)
        ))
        if (!is.matrix(num) || nrow(num) < 1L || ncol(num) < 3L) {
          next
        }
        if (!any(is.finite(num))) {
          next
        }
        num[!is.finite(num)] <- 0
        return(num[, seq_len(3L), drop = FALSE])
      }

      NULL
    }

    manifest_paths <- c(
      file.path(root, "results", "asv_hv_k_gcv_sweep", "asv_layouts_html_manifest.csv"),
      file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "asvfull_layouts_html_manifest.csv")
    )

    for (mp in manifest_paths) {
      out <- read_one_manifest(mp)
      if (is.matrix(out) && nrow(out) > 0L && ncol(out) >= 3L) {
        return(out)
      }
    }

    NULL
  }

  grip_layout_matrix_for_graph_set <- function(graph_set, k_ref = NA_integer_) {
    if (!is.list(graph_set)) {
      return(NULL)
    }

    raw <- graph_set$layout_assets$grip_layouts
    if (!is.list(raw) || length(raw) < 1L) {
      return(NULL)
    }

    entries <- lapply(raw, function(one) {
      if (!is.list(one)) {
        return(NULL)
      }
      path <- as.character(one$path %||% "")
      if (!nzchar(path) || !file.exists(path)) {
        return(NULL)
      }
      kk <- suppressWarnings(as.integer(one$k))
      if (!is.finite(kk)) {
        return(NULL)
      }
      list(path = normalizePath(path, mustWork = TRUE), k = kk)
    })
    entries <- Filter(Negate(is.null), entries)
    if (length(entries) < 1L) {
      return(NULL)
    }

    k_use <- suppressWarnings(as.integer(k_ref))
    idx_order <- seq_along(entries)
    if (is.finite(k_use)) {
      kvals <- vapply(entries, function(one) suppressWarnings(as.integer(one$k)), integer(1))
      if (any(kvals == k_use, na.rm = TRUE)) {
        idx_order <- which(kvals == k_use)
      } else {
        idx_order <- order(abs(kvals - k_use), kvals)
      }
    }

    for (ii in idx_order) {
      mat <- tryCatch(readRDS(entries[[ii]]$path), error = function(e) NULL)
      if (is.data.frame(mat)) {
        mat <- as.matrix(mat)
      } else {
        mat <- suppressWarnings(as.matrix(mat))
      }
      if (!is.matrix(mat) || nrow(mat) < 1L || ncol(mat) < 3L) {
        next
      }

      num <- suppressWarnings(matrix(
        as.numeric(mat),
        nrow = nrow(mat),
        ncol = ncol(mat)
      ))
      if (!is.matrix(num) || nrow(num) < 1L || ncol(num) < 3L) {
        next
      }
      if (!any(is.finite(num))) {
        next
      }
      num[!is.finite(num)] <- 0
      return(num[, seq_len(3L), drop = FALSE])
    }

    NULL
  }

  list(
    normalize_coord_matrix = normalize_coord_matrix,
    adj_to_edge_matrix = adj_to_edge_matrix,
    extract_graph_collection = extract_graph_collection,
    select_graph_for_k = select_graph_for_k,
    resolve_reference_spec = resolve_reference_spec,
    find_fit_file_for_k = find_fit_file_for_k,
    collect_reference_metadata_sources = collect_reference_metadata_sources,
    collect_reference_condexp_sources = collect_reference_condexp_sources,
    collect_reference_endpoint_sources = collect_reference_endpoint_sources,
    compute_reference_layout = compute_reference_layout,
    project_layout_manifest_matrix = project_layout_manifest_matrix,
    grip_layout_matrix_for_graph_set = grip_layout_matrix_for_graph_set
  )
}
