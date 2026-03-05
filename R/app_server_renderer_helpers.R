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

  collect_reference_condexp_sources <- function(manifest, set_id, k_use, n_vertices) {
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

    for (cs in condexp_sets) {
      cs_id <- as.character(cs$id %||% "condexp")

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

          yhat <- suppressWarnings(as.numeric(fit$fitted.values %||% numeric(0)))
          if (length(yhat) == n_vertices) {
            add_source(
              key = sprintf("condexp_%s_%s_yhat", cs_id, fam),
              label = sprintf("CondExp %s/%s y.hat", cs_id, fam),
              values = yhat,
              type = "numeric"
            )
          }

          yobs <- suppressWarnings(as.numeric(fit$y %||% numeric(0)))
          if (length(yobs) == n_vertices) {
            add_source(
              key = sprintf("condexp_%s_%s_yobs", cs_id, fam),
              label = sprintf("Observed %s/%s y", cs_id, fam),
              values = yobs,
              type = "numeric"
            )
          }

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
            add_source(
              key = sprintf("long_%s_%s_yhat", cs_id, oo),
              label = sprintf("CondExp %s %s y.hat", cs_id, oo),
              values = suppressWarnings(as.numeric(dd$y_fitted)),
              type = "numeric"
            )
            if ("y_observed" %in% names(dd)) {
              add_source(
                key = sprintf("long_%s_%s_yobs", cs_id, oo),
                label = sprintf("Observed %s %s y", cs_id, oo),
                values = suppressWarnings(as.numeric(dd$y_observed)),
                type = "numeric"
              )
            }
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

  string_hash_token <- function(x) {
    chars <- utf8ToInt(enc2utf8(as.character(x %||% "")))
    if (length(chars) < 1L) {
      return("0")
    }
    h <- 0
    for (ii in seq_along(chars)) {
      h <- (h * 131 + chars[[ii]]) %% 2147483647
    }
    sprintf("%x", as.integer(h))
  }

  local_html_resource_url <- function(path) {
    pp <- as.character(path %||% "")
    if (!nzchar(pp)) {
      return("")
    }
    if (grepl("^https?://", pp, ignore.case = TRUE)) {
      return(pp)
    }

    file_path <- normalizePath(path.expand(pp), mustWork = TRUE)
    dir_path <- dirname(file_path)
    file_name <- basename(file_path)

    alias_base <- sprintf("gfhtml_%s", string_hash_token(dir_path))
    map <- rv$html.resource.map %||% list()
    alias <- alias_base
    suffix <- 1L
    repeat {
      existing <- map[[alias]]
      if (is.null(existing)) {
        shiny::addResourcePath(alias, dir_path)
        map[[alias]] <- dir_path
        break
      }
      if (identical(normalizePath(existing, mustWork = FALSE), dir_path)) {
        break
      }
      suffix <- suffix + 1L
      alias <- sprintf("%s_%d", alias_base, suffix)
    }
    rv$html.resource.map <- map

    paste0("/", alias, "/", utils::URLencode(file_name, reserved = TRUE))
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

  layout_variant_paths <- function(
      graph_set,
      requested_renderer = "html",
      vertex_mode = "sphere",
      size_label = "1x",
      color_label = "",
      k_ref = NA_integer_) {
    if (!is.list(graph_set)) {
      return(character(0))
    }
    variants <- graph_set$layout_assets$variants
    if (!is.list(variants) || length(variants) < 1L) {
      return(character(0))
    }

    req_renderer <- tolower(as.character(requested_renderer %||% "html"))
    req_vertex <- tolower(as.character(vertex_mode %||% "sphere"))
    req_size <- normalize_size_label(size_label)
    req_color <- normalize_token_key(color_label)
    k_use <- suppressWarnings(as.integer(k_ref))

    vals <- lapply(variants, function(vv) {
      path <- as.character(vv$path %||% "")
      if (!nzchar(path)) {
        return(NULL)
      }
      exists <- grepl("^https?://", path, ignore.case = TRUE) || file.exists(path)
      if (!exists) {
        return(NULL)
      }
      list(
        path = if (grepl("^https?://", path, ignore.case = TRUE)) path else normalizePath(path, mustWork = TRUE),
        renderer = tolower(as.character(vv$renderer %||% "html")),
        vertex_layout = tolower(as.character(vv$vertex_layout %||% "sphere")),
        vertex_size = normalize_size_label(vv$vertex_size %||% "1x"),
        color_by = normalize_token_key(vv$color_by %||% ""),
        k = suppressWarnings(as.integer(vv$k))
      )
    })
    vals <- Filter(Negate(is.null), vals)
    if (length(vals) < 1L) {
      return(character(0))
    }

    score_one <- function(one) {
      sc <- 0
      if (identical(one$renderer, req_renderer)) {
        sc <- sc + 18
      } else if (identical(one$renderer, "html")) {
        sc <- sc + 4
      }
      if (identical(one$vertex_layout, req_vertex)) {
        sc <- sc + 8
      }
      if (identical(one$vertex_size, req_size)) {
        sc <- sc + 7
      }
      if (nzchar(req_color) && nzchar(one$color_by)) {
        if (identical(one$color_by, req_color) || grepl(one$color_by, req_color, fixed = TRUE) ||
            grepl(req_color, one$color_by, fixed = TRUE)) {
          sc <- sc + 6
        }
      } else if (!nzchar(one$color_by)) {
        sc <- sc + 1
      }
      if (is.finite(k_use) && is.finite(one$k)) {
        if (identical(one$k, k_use)) {
          sc <- sc + 9
        } else {
          sc <- sc - min(5, abs(one$k - k_use))
        }
      }
      if (is.finite(k_use) && grepl(sprintf("k0*%d", k_use), basename(one$path), perl = TRUE)) {
        sc <- sc + 3
      }
      sc
    }

    scores <- vapply(vals, score_one, numeric(1))
    paths <- vapply(vals, function(x) x$path, character(1))
    ord <- order(-scores, nchar(paths), paths)
    unique(paths[ord])
  }

  project_layout_manifest_candidates <- function(project_root, spec, size_label = "1x") {
    root <- as.character(project_root %||% "")
    if (!nzchar(root) || identical(root, "NA")) {
      return(character(0))
    }
    root <- tryCatch(normalizePath(path.expand(root), mustWork = TRUE), error = function(e) "")
    if (!nzchar(root)) {
      return(character(0))
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
        return(character(0))
      }
      tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.data.frame(tbl) || nrow(tbl) < 1L) {
        return(character(0))
      }
      if ("status" %in% names(tbl)) {
        tbl <- tbl[tolower(as.character(tbl$status)) %in% c("written", "ok", ""), , drop = FALSE]
      }
      if (nrow(tbl) < 1L) {
        return(character(0))
      }

      if ("set.tag" %in% names(tbl) && length(set_alias) > 0L) {
        set_col <- tolower(as.character(tbl$set.tag))
        tbl <- tbl[set_col %in% set_alias, , drop = FALSE]
      }
      if (nrow(tbl) < 1L) {
        return(character(0))
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

      file_col <- if ("file.no.gray" %in% names(tbl)) "file.no.gray" else "file"
      ff <- as.character(tbl[[file_col]] %||% character(0))
      ff <- ff[!is.na(ff) & nzchar(ff)]
      ff <- ff[file.exists(ff)]
      if (length(ff) < 1L) {
        return(character(0))
      }
      unique(normalizePath(ff, mustWork = TRUE))
    }

    files <- c(
      read_one_manifest(file.path(root, "results", "asv_hv_k_gcv_sweep", "asv_layouts_html_manifest.csv")),
      read_one_manifest(file.path(root, "results", "asv_full_graph_hv_criteria_k_selection", "asvfull_layouts_html_manifest.csv"))
    )
    unique(files)
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

  discover_reference_html_candidates <- function(
      manifest,
      spec,
      color_label = "",
      extra_tokens = character(0),
      requested_renderer = "html",
      vertex_mode = "sphere",
      size_label = "1x") {
    if (!is.list(manifest) || !is.list(spec) || !is.list(spec$graph_set)) {
      return(character(0))
    }

    key <- paste(
      rv$project.id %||% "project",
      spec$set_id %||% "set",
      spec$k_ref %||% "k",
      as.character(color_label %||% ""),
      as.character(requested_renderer %||% "html"),
      as.character(vertex_mode %||% "sphere"),
      as.character(size_label %||% "1x"),
      paste(as.character(extra_tokens %||% character(0)), collapse = ","),
      sep = "|"
    )
    cache <- rv$reference.html.cache %||% list()
    if (key %in% names(cache)) {
      return(cache[[key]])
    }

    add_paths <- function(cur, x) {
      vals <- normalize_paths(x)
      if (length(vals) < 1L) {
        return(cur)
      }
      unique(c(cur, vals))
    }
    add_dirs <- function(cur, x) {
      vals <- as.character(x %||% character(0))
      vals <- vals[nzchar(vals)]
      vals <- vals[file.exists(vals)]
      vals <- vals[file.info(vals)$isdir %in% TRUE]
      if (length(vals) < 1L) {
        return(cur)
      }
      vals <- normalizePath(vals, mustWork = TRUE)
      unique(c(cur, vals))
    }

    candidates <- character(0)
    dirs <- character(0)
    gs <- spec$graph_set
    k_ref <- suppressWarnings(as.integer(spec$k_ref))

    explicit_from_variants <- add_paths(
      character(0),
      layout_variant_paths(
        graph_set = gs,
        requested_renderer = requested_renderer,
        vertex_mode = vertex_mode,
        size_label = size_label,
        color_label = color_label,
        k_ref = k_ref
      )
    )
    if (length(explicit_from_variants) > 0L) {
      cache[[key]] <- explicit_from_variants
      rv$reference.html.cache <- cache
      return(explicit_from_variants)
    }

    explicit_from_project_manifest <- project_layout_manifest_candidates(
      project_root = manifest$project_root %||% "",
      spec = spec,
      size_label = size_label
    )
    if (length(explicit_from_project_manifest) > 0L) {
      cache[[key]] <- explicit_from_project_manifest
      rv$reference.html.cache <- cache
      return(explicit_from_project_manifest)
    }

    candidates <- add_paths(candidates, explicit_from_variants)
    candidates <- add_paths(candidates, c(gs$html_file, gs$html_files, gs$html_candidates))
    if (nzchar(as.character(gs$graph_file %||% "")) && file.exists(gs$graph_file)) {
      dirs <- add_dirs(dirs, dirname(gs$graph_file))
    }

    endpoint_runs <- if (is.list(manifest$endpoint_runs)) manifest$endpoint_runs else list()
    for (ep in endpoint_runs) {
      kvals <- suppressWarnings(as.integer(ep$k_values %||% integer(0)))
      if (is.finite(k_ref) && length(kvals) > 0L && !(k_ref %in% kvals)) {
        next
      }
      dirs <- add_dirs(dirs, c(ep$run_dir, dirname(as.character(ep$summary_csv %||% ""))))
      candidates <- add_paths(candidates, c(ep$html_file, ep$html_files, ep$index_html))
    }

    condexp_sets <- if (is.list(manifest$condexp_sets)) manifest$condexp_sets else list()
    for (cs in condexp_sets) {
      dirs <- add_dirs(dirs, c(cs$run_dir, dirname(as.character(cs$summary_file %||% ""))))
      candidates <- add_paths(candidates, c(cs$html_file, cs$html_files))
      if (is.list(cs$family_runs) && length(cs$family_runs) > 0L) {
        for (fr in cs$family_runs) {
          dirs <- add_dirs(dirs, c(fr$fits_dir, dirname(as.character(fr$summary_file %||% ""))))
          candidates <- add_paths(candidates, c(fr$html_file, fr$html_files))
        }
      }
    }

    scan_html <- function(dd, k_use = NA_integer_) {
      dd <- normalizePath(dd, mustWork = TRUE)
      patt <- if (is.finite(k_use)) sprintf("k0*%d.*\\.html?$", as.integer(k_use)) else "\\.html?$"
      ff <- list.files(
        dd,
        recursive = TRUE,
        full.names = TRUE,
        pattern = patt,
        ignore.case = TRUE
      )
      if (length(ff) < 1L) {
        ff <- list.files(
          dd,
          recursive = TRUE,
          full.names = TRUE,
          pattern = "\\.html?$",
          ignore.case = TRUE
        )
      }
      if (length(ff) > 300L) {
        ff <- ff[seq_len(300L)]
      }
      ff[file.exists(ff)]
    }

    for (dd in dirs) {
      ff <- tryCatch(scan_html(dd, k_use = k_ref), error = function(e) character(0))
      candidates <- add_paths(candidates, ff)
    }

    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) < 1L) {
      cache[[key]] <- character(0)
      rv$reference.html.cache <- cache
      return(character(0))
    }

    tokens <- unique(tolower(c(
      as.character(spec$set_id %||% ""),
      as.character(spec$set_label %||% ""),
      as.character(color_label %||% ""),
      sub("^top", "hv", as.character(spec$set_id %||% "")),
      as.character(manifest$defaults$condexp_set_id %||% ""),
      as.character(extra_tokens %||% character(0)),
      "cst", "y_hat", "vag_odor"
    )))
    tokens <- tokens[nzchar(tokens)]

    score_one <- function(pp) {
      base <- tolower(basename(pp))
      full <- tolower(pp)
      sc <- 0
      for (tok in tokens) {
        if (grepl(tok, base, fixed = TRUE)) {
          sc <- sc + 4
        } else if (grepl(tok, full, fixed = TRUE)) {
          sc <- sc + 2
        }
      }
      if (is.finite(k_ref) && grepl(sprintf("k0*%d", as.integer(k_ref)), base, perl = TRUE)) {
        sc <- sc + 5
      }
      if (grepl("index\\.html?$", base, ignore.case = TRUE)) {
        sc <- sc + 1
      }
      if (grepl("no_gray", base, fixed = TRUE)) {
        sc <- sc - 1
      }
      sc
    }

    scores <- vapply(candidates, score_one, numeric(1))
    ord <- order(-scores, nchar(candidates), candidates)
    out <- unique(candidates[ord])
    if (length(out) > 120L) {
      out <- out[seq_len(120L)]
    }

    cache[[key]] <- out
    rv$reference.html.cache <- cache
    out
  }

  html_candidate_choices <- function(paths, project_root = NA_character_) {
    pp <- normalize_paths(paths)
    if (length(pp) < 1L) {
      return(c())
    }

    root_norm <- suppressWarnings(normalizePath(project_root, mustWork = FALSE))
    labels <- vapply(pp, function(one) {
      base <- basename(one)
      parent <- basename(dirname(one))
      rel <- one
      if (nzchar(root_norm) && !identical(root_norm, "NA")) {
        rel_try <- tryCatch(
          substring(normalizePath(one, mustWork = TRUE), nchar(root_norm) + 2L),
          error = function(e) one
        )
        if (nzchar(rel_try) && !identical(rel_try, one)) {
          rel <- rel_try
        }
      }
      if (nzchar(parent) && !identical(parent, ".") && !identical(parent, "/")) {
        sprintf("%s [%s] - %s", base, parent, rel)
      } else {
        sprintf("%s - %s", base, rel)
      }
    }, character(1))

    dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    if (any(dup)) {
      labels[dup] <- sprintf("%s (%d)", labels[dup], seq_len(sum(dup)))
    }

    stats::setNames(pp, labels)
  }


  list(
    normalize_coord_matrix = normalize_coord_matrix,
    adj_to_edge_matrix = adj_to_edge_matrix,
    extract_graph_collection = extract_graph_collection,
    select_graph_for_k = select_graph_for_k,
    resolve_reference_spec = resolve_reference_spec,
    find_fit_file_for_k = find_fit_file_for_k,
    collect_reference_condexp_sources = collect_reference_condexp_sources,
    compute_reference_layout = compute_reference_layout,
    string_hash_token = string_hash_token,
    local_html_resource_url = local_html_resource_url,
    discover_reference_html_candidates = discover_reference_html_candidates,
    html_candidate_choices = html_candidate_choices,
    project_layout_manifest_matrix = project_layout_manifest_matrix,
    grip_layout_matrix_for_graph_set = grip_layout_matrix_for_graph_set
  )
}
