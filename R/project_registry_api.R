`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.gflowui_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

gflowui_registry_columns <- function() {
  c(
    "id",
    "label",
    "origin",
    "has_graphs",
    "has_condexp",
    "has_endpoints",
    "project_root",
    "manifest_file",
    "created_at",
    "updated_at"
  )
}

gflowui_default_registry <- function() {
  data.frame(
    id = character(0),
    label = character(0),
    origin = character(0),
    has_graphs = logical(0),
    has_condexp = logical(0),
    has_endpoints = logical(0),
    project_root = character(0),
    manifest_file = character(0),
    created_at = character(0),
    updated_at = character(0),
    stringsAsFactors = FALSE
  )
}

gflowui_projects_data_dir <- function() {
  custom <- getOption("gflowui.projects_data_dir", NULL)
  if (!is.null(custom) && is.character(custom) && nzchar(custom[1])) {
    return(normalizePath(path.expand(custom[1]), mustWork = FALSE))
  }

  file.path(tools::R_user_dir("gflowui", which = "data"), "projects")
}

gflowui_registry_path <- function() {
  file.path(gflowui_projects_data_dir(), "registry.rds")
}

gflowui_manifests_dir <- function() {
  file.path(gflowui_projects_data_dir(), "manifests")
}

gflowui_manifest_path <- function(project_id) {
  file.path(gflowui_manifests_dir(), sprintf("%s.manifest.rds", as.character(project_id)))
}

gflowui_sanitize_registry <- function(x) {
  cols <- gflowui_registry_columns()

  if (!is.data.frame(x) || nrow(x) < 1L) {
    return(gflowui_default_registry())
  }

  missing_cols <- setdiff(cols, names(x))
  if (length(missing_cols) > 0L) {
    for (cc in missing_cols) {
      x[[cc]] <- NA
    }
  }

  out <- x[, cols, drop = FALSE]
  out$id <- as.character(out$id)
  out$label <- as.character(out$label)
  out$origin <- as.character(out$origin)
  out$has_graphs <- as.logical(out$has_graphs)
  out$has_condexp <- as.logical(out$has_condexp)
  out$has_endpoints <- as.logical(out$has_endpoints)
  out$project_root <- as.character(out$project_root)
  out$manifest_file <- as.character(out$manifest_file)
  out$created_at <- as.character(out$created_at)
  out$updated_at <- as.character(out$updated_at)

  out <- out[nzchar(out$id) & nzchar(out$label), , drop = FALSE]
  out <- out[!duplicated(out$id), , drop = FALSE]

  out$has_graphs[is.na(out$has_graphs)] <- FALSE
  out$has_condexp[is.na(out$has_condexp)] <- FALSE
  out$has_endpoints[is.na(out$has_endpoints)] <- FALSE
  out$origin[!nzchar(out$origin)] <- "registered"

  rownames(out) <- NULL
  out
}

gflowui_load_registry <- function() {
  path <- gflowui_registry_path()
  if (!file.exists(path)) {
    return(gflowui_default_registry())
  }

  loaded <- tryCatch(readRDS(path), error = function(e) NULL)
  gflowui_sanitize_registry(loaded)
}

gflowui_save_registry <- function(registry_df) {
  reg <- gflowui_sanitize_registry(registry_df)
  path <- gflowui_registry_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp <- tempfile(pattern = "registry-", tmpdir = dirname(path), fileext = ".rds")
  tryCatch(
    {
      saveRDS(reg, file = tmp)
      moved <- file.rename(tmp, path)
      if (!moved) {
        moved <- file.copy(tmp, path, overwrite = TRUE)
      }
      if (!moved) {
        stop("Could not write registry file.")
      }
    },
    error = function(e) {
      warning(
        sprintf("gflowui: failed to persist project registry: %s", conditionMessage(e)),
        call. = FALSE
      )
    },
    finally = {
      if (file.exists(tmp)) {
        unlink(tmp, force = TRUE)
      }
    }
  )

  invisible(reg)
}

gflowui_make_project_id <- function(label, existing_ids = character(0)) {
  base <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(label %||% "project")))
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) {
    base <- "project"
  }

  candidate <- base
  suffix <- 2L
  while (candidate %in% existing_ids) {
    candidate <- sprintf("%s_%d", base, suffix)
    suffix <- suffix + 1L
  }
  candidate
}

gflowui_registry_entry <- function(
    id,
    label,
    origin = "registered",
    has_graphs = FALSE,
    has_condexp = FALSE,
    has_endpoints = FALSE,
    project_root = NA_character_,
    manifest_file = NA_character_,
    created_at = .gflowui_now(),
    updated_at = .gflowui_now()) {
  data.frame(
    id = as.character(id),
    label = as.character(label),
    origin = as.character(origin),
    has_graphs = isTRUE(has_graphs),
    has_condexp = isTRUE(has_condexp),
    has_endpoints = isTRUE(has_endpoints),
    project_root = as.character(project_root %||% NA_character_),
    manifest_file = as.character(manifest_file %||% NA_character_),
    created_at = as.character(created_at %||% .gflowui_now()),
    updated_at = as.character(updated_at %||% .gflowui_now()),
    stringsAsFactors = FALSE
  )
}

gflowui_upsert_registry_row <- function(registry_df, entry_df, overwrite = TRUE) {
  reg <- gflowui_sanitize_registry(registry_df)
  ent <- gflowui_sanitize_registry(entry_df)
  if (nrow(ent) != 1L) {
    stop("entry_df must contain exactly one registry row.", call. = FALSE)
  }

  idx <- match(ent$id[1], reg$id)
  if (!is.na(idx) && !isTRUE(overwrite)) {
    stop(sprintf("Project id '%s' already exists. Use overwrite = TRUE.", ent$id[1]), call. = FALSE)
  }

  if (is.na(idx)) {
    reg <- rbind(reg, ent)
  } else {
    ent$created_at <- reg$created_at[idx]
    reg[idx, ] <- ent
  }

  rownames(reg) <- NULL
  reg
}

gflowui_read_manifest <- function(path) {
  if (!is.character(path) || !nzchar(path[1]) || !file.exists(path[1])) {
    return(NULL)
  }
  tryCatch(readRDS(path[1]), error = function(e) NULL)
}

gflowui_write_manifest <- function(manifest, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(manifest, file = path)
  invisible(path)
}

.read_csv_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

.extract_int_values <- function(x) {
  x <- as.integer(x)
  x <- x[is.finite(x)]
  sort(unique(x))
}

.k_from_column <- function(path, column_name = "k") {
  tbl <- .read_csv_if_exists(path)
  if (is.null(tbl) || !(column_name %in% names(tbl))) {
    return(integer(0))
  }
  .extract_int_values(tbl[[column_name]])
}

.k_from_filename_vector <- function(paths, pattern = "k([0-9]+)") {
  if (length(paths) < 1L) {
    return(integer(0))
  }
  b <- basename(paths)
  out <- rep(NA_integer_, length(b))
  m <- regexec(pattern, b, perl = TRUE)
  mm <- regmatches(b, m)
  for (ii in seq_along(mm)) {
    if (length(mm[[ii]]) >= 2L) {
      out[ii] <- as.integer(mm[[ii]][2])
    }
  }
  .extract_int_values(out)
}

.as_scalar_pos_int <- function(x) {
  vals <- suppressWarnings(as.integer(x))
  vals <- vals[is.finite(vals) & vals > 0L]
  if (length(vals) < 1L) {
    return(NA_integer_)
  }
  vals[[1]]
}

.as_scalar_chr <- function(x, default = "") {
  vals <- as.character(x)
  vals <- vals[!is.na(vals)]
  if (length(vals) < 1L || !nzchar(vals[[1]])) {
    return(as.character(default)[[1]])
  }
  vals[[1]]
}

.is_url_path <- function(x) {
  grepl("^https?://", as.character(x %||% ""), ignore.case = TRUE)
}

.normalize_path_or_url <- function(path) {
  pp <- .as_scalar_chr(path, default = "")
  if (!nzchar(pp)) {
    return("")
  }
  if (.is_url_path(pp)) {
    return(pp)
  }
  tryCatch(normalizePath(path.expand(pp), mustWork = TRUE), error = function(e) "")
}

.sanitize_variant_id <- function(x, fallback = "variant") {
  id <- tolower(gsub("[^a-zA-Z0-9]+", "_", .as_scalar_chr(x, default = "")))
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) {
    id <- fallback
  }
  id
}

.normalize_size_label <- function(x, default = "1x") {
  txt <- tolower(.as_scalar_chr(x, default = ""))
  if (!nzchar(txt)) {
    return(default)
  }
  txt <- gsub("_", ".", txt)
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
  default
}

.infer_variant_k <- function(path) {
  low <- tolower(basename(.as_scalar_chr(path, default = "")))
  mm <- regexec("k0*([0-9]+)", low, perl = TRUE)
  rr <- regmatches(low, mm)[[1]]
  if (length(rr) >= 2L && nzchar(rr[[2]])) {
    val <- suppressWarnings(as.integer(rr[[2]]))
    if (is.finite(val) && val > 0L) {
      return(val)
    }
  }
  NA_integer_
}

.normalize_grip_layout_entry <- function(layout, fallback_id = "grip_layout") {
  one <- layout
  if (is.character(one)) {
    one <- list(path = one[[1]])
  }
  if (!is.list(one)) {
    return(NULL)
  }

  path_norm <- .normalize_path_or_url(one$path %||% one$layout_file %||% one$rds_file %||% "")
  if (!nzchar(path_norm)) {
    return(NULL)
  }

  k_val <- .as_scalar_pos_int(one$k %||% one$k_value %||% .infer_variant_k(path_norm))
  if (!is.finite(k_val)) {
    return(NULL)
  }

  id <- .sanitize_variant_id(one$id %||% sprintf("k%02d", k_val), fallback = fallback_id)
  label <- .as_scalar_chr(one$label, default = sprintf("k=%d grip.layout", k_val))
  source <- .as_scalar_chr(one$source, default = "grip.layout")

  list(
    id = id,
    label = label,
    k = k_val,
    path = path_norm,
    source = source
  )
}

.normalize_grip_layout_entries <- function(layouts) {
  raw <- layouts
  if (is.data.frame(raw) && nrow(raw) > 0L) {
    rows <- vector("list", nrow(raw))
    for (ii in seq_len(nrow(raw))) {
      rows[[ii]] <- as.list(raw[ii, , drop = FALSE])
    }
    raw <- rows
  } else if (is.character(raw)) {
    raw <- as.list(raw)
  } else if (!is.list(raw)) {
    raw <- list()
  }

  out <- list()
  seen_ids <- character(0)
  seen_path <- character(0)
  seen_k <- integer(0)

  if (length(raw) > 0L) {
    for (ii in seq_along(raw)) {
      one <- .normalize_grip_layout_entry(raw[[ii]], fallback_id = sprintf("grip_layout_%d", ii))
      if (is.null(one)) {
        next
      }
      if (one$path %in% seen_path || one$k %in% seen_k) {
        next
      }
      while (one$id %in% seen_ids) {
        one$id <- sprintf("%s_%d", one$id, length(out) + 1L)
      }
      seen_ids <- c(seen_ids, one$id)
      seen_path <- c(seen_path, one$path)
      seen_k <- c(seen_k, one$k)
      out[[one$id]] <- one
    }
  }

  out
}

.discover_grip_layout_entries <- function(graph_file, set_id = "", k_values = integer(0), max_files = 4000L) {
  gp <- .normalize_path_or_url(graph_file)
  if (!nzchar(gp) || .is_url_path(gp) || !file.exists(gp)) {
    return(list())
  }

  sid <- tolower(.as_scalar_chr(set_id, default = ""))
  alias <- unique(c(
    sid,
    sub("^top", "hv", sid),
    sub("^hv", "top", sid),
    sub("^asv[_-]?", "", sid),
    sub("^shared_", "", sid)
  ))
  if (sid %in% c("all", "asv", "shared_all_asv", "full", "asvfull")) {
    alias <- unique(c(alias, "all", "asv", "asvfull", "shared_all_asv", "full"))
  }
  alias <- alias[nzchar(alias)]

  base_dir <- dirname(gp)
  parent_dir <- dirname(base_dir)
  sibling_layout_roots <- character(0)
  if (dir.exists(parent_dir)) {
    sibling_dirs <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)
    sibling_layout_roots <- sibling_dirs[grepl("^(graph3d_html|layouts?_3d)", basename(sibling_dirs), ignore.case = TRUE)]
  }

  alias_layout_roots <- unlist(lapply(alias, function(tok) {
    if (!nzchar(tok)) {
      return(character(0))
    }
    c(
      file.path(parent_dir, tok),
      file.path(parent_dir, tok, "layouts_3d_rds"),
      file.path(parent_dir, tok, "layouts_3d"),
      file.path(parent_dir, tok, "layout_3d_rds")
    )
  }), use.names = FALSE)

  search_dirs <- unique(c(
    base_dir,
    file.path(base_dir, "layouts_3d_rds"),
    file.path(base_dir, "layouts_3d"),
    file.path(base_dir, "layout_3d_rds"),
    alias_layout_roots,
    file.path(parent_dir, "graph3d_html"),
    file.path(parent_dir, "layouts_3d_rds"),
    file.path(parent_dir, "layouts_3d"),
    sibling_layout_roots
  ))
  search_dirs <- search_dirs[file.exists(search_dirs) & dir.exists(search_dirs)]
  if (length(search_dirs) < 1L) {
    return(list())
  }

  files <- character(0)
  for (dd in search_dirs) {
    ff <- list.files(
      dd,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "layout3d\\.rds$",
      ignore.case = TRUE
    )
    if (length(ff) > 0L) {
      files <- c(files, ff)
    }
  }
  files <- unique(files[file.exists(files)])
  if (length(files) < 1L) {
    return(list())
  }
  if (length(files) > max_files) {
    files <- files[seq_len(max_files)]
  }

  k_use <- suppressWarnings(as.integer(k_values))
  k_use <- k_use[is.finite(k_use)]

  score_one <- function(path) {
    low <- tolower(path)
    base <- tolower(basename(path))
    sc <- 0
    for (tok in alias) {
      if (!nzchar(tok)) {
        next
      }
      if (grepl(tok, base, fixed = TRUE)) {
        sc <- sc + 4
      } else if (grepl(tok, low, fixed = TRUE)) {
        sc <- sc + 2
      }
    }
    if (grepl("layout3d", base, fixed = TRUE)) {
      sc <- sc + 2
    }
    if (grepl("graph3d_html", low, fixed = TRUE)) {
      sc <- sc + 3
    }
    if (grepl("layouts_3d", low, fixed = TRUE)) {
      sc <- sc + 2
    }
    sc
  }

  k_for_file <- rep.int(NA_integer_, length(files))
  mm <- regexec("k0*([0-9]+)", basename(files), perl = TRUE)
  rr <- regmatches(basename(files), mm)
  for (ii in seq_along(rr)) {
    if (length(rr[[ii]]) >= 2L) {
      val <- suppressWarnings(as.integer(rr[[ii]][2]))
      if (is.finite(val) && val > 0L) {
        k_for_file[[ii]] <- val
      }
    }
  }

  keep <- is.finite(k_for_file)
  if (!any(keep)) {
    return(list())
  }

  tbl <- data.frame(
    path = files[keep],
    k = as.integer(k_for_file[keep]),
    score = vapply(files[keep], score_one, numeric(1)),
    stringsAsFactors = FALSE
  )
  if (length(k_use) > 0L && any(tbl$k %in% k_use)) {
    tbl <- tbl[tbl$k %in% k_use, , drop = FALSE]
  }
  if (nrow(tbl) < 1L) {
    return(list())
  }

  ord <- order(tbl$k, -tbl$score, nchar(tbl$path), tbl$path)
  tbl <- tbl[ord, , drop = FALSE]
  tbl <- tbl[!duplicated(tbl$k), , drop = FALSE]

  .normalize_grip_layout_entries(lapply(seq_len(nrow(tbl)), function(ii) {
    list(
      k = tbl$k[[ii]],
      path = tbl$path[[ii]],
      source = "grip.layout"
    )
  }))
}

.normalize_grip_layout_params <- function(params) {
  pp <- if (is.list(params)) params else list()
  if (length(pp) < 1L) {
    return(list())
  }

  as_int <- function(x) {
    val <- suppressWarnings(as.integer(x))
    val <- val[is.finite(val)]
    if (length(val) < 1L) return(NULL)
    val[[1]]
  }
  as_num <- function(x) {
    val <- suppressWarnings(as.numeric(x))
    val <- val[is.finite(val)]
    if (length(val) < 1L) return(NULL)
    val[[1]]
  }

  out <- list()

  dim_v <- as_int(pp$dim)
  if (!is.null(dim_v) && dim_v >= 2L) out$dim <- dim_v
  rounds_v <- as_int(pp$rounds)
  if (!is.null(rounds_v) && rounds_v > 0L) out$rounds <- rounds_v
  final_rounds_v <- as_int(pp$final_rounds)
  if (!is.null(final_rounds_v) && final_rounds_v > 0L) out$final_rounds <- final_rounds_v
  num_init_v <- as_int(pp$num_init)
  if (!is.null(num_init_v) && num_init_v > 0L) out$num_init <- num_init_v
  num_nbrs_v <- as_int(pp$num_nbrs)
  if (!is.null(num_nbrs_v) && num_nbrs_v > 0L) out$num_nbrs <- num_nbrs_v

  r_v <- as_num(pp$r)
  if (!is.null(r_v) && r_v > 0) out$r <- r_v
  s_v <- as_num(pp$s)
  if (!is.null(s_v) && s_v > 0) out$s <- s_v
  tinit_v <- as_num(pp$tinit_factor)
  if (!is.null(tinit_v) && tinit_v > 0) out$tinit_factor <- tinit_v

  seed_v <- as_int(pp$seed)
  if (!is.null(seed_v)) out$seed <- seed_v

  out
}

.normalize_layout_assets <- function(layout_assets) {
  la <- if (is.list(layout_assets)) layout_assets else list()
  presets <- if (is.list(la$presets)) la$presets else list()

  renderer <- tolower(.as_scalar_chr(presets$renderer, default = "rglwidget"))
  if (identical(renderer, "rgl")) {
    renderer <- "rglwidget"
  }
  if (!(renderer %in% c("plotly", "rglwidget"))) {
    renderer <- "rglwidget"
  }
  presets$renderer <- renderer

  vertex_layout <- tolower(.as_scalar_chr(presets$vertex_layout, default = "point"))
  if (!(vertex_layout %in% c("sphere", "point"))) {
    vertex_layout <- "point"
  }
  presets$vertex_layout <- vertex_layout

  vertex_size <- .as_scalar_chr(presets$vertex_size, default = "1x")
  if (!nzchar(vertex_size)) {
    vertex_size <- "1x"
  }
  presets$vertex_size <- vertex_size

  color_by <- .as_scalar_chr(presets$color_by, default = "vertex_degree")
  if (!nzchar(color_by)) {
    color_by <- "vertex_degree"
  }
  presets$color_by <- color_by

  vertex_color <- tolower(.as_scalar_chr(presets$vertex_color, default = "#111827"))
  if (!nzchar(vertex_color)) {
    vertex_color <- "#111827"
  }
  presets$vertex_color <- vertex_color

  grip_raw <- la$grip_layouts %||% la$layout3d_files %||% la$layout_files %||% list()
  grip_layouts <- .normalize_grip_layout_entries(grip_raw)
  grip_params <- .normalize_grip_layout_params(la$grip_layout_params %||% la$grip_layout_args %||% list())

  la$presets <- presets
  la$variants <- list()
  la$grip_layouts <- grip_layouts
  la$grip_layout_params <- grip_params
  la
}

.normalize_color_assets <- function(color_assets) {
  ca <- if (is.list(color_assets)) color_assets else list()

  metadata_file <- .normalize_path_or_url(ca$metadata_file %||% ca$data_file %||% "")
  ca$metadata_file <- metadata_file
  ca$metadata_object <- .as_scalar_chr(ca$metadata_object, default = "mt.asv")

  cols <- as.character(ca$vector_columns %||% ca$columns %||% character(0))
  cols <- unique(cols[nzchar(cols)])
  ca$vector_columns <- cols

  preferred <- as.character(ca$preferred_order %||% cols)
  preferred <- unique(preferred[nzchar(preferred)])
  ca$preferred_order <- preferred

  labels <- character(0)
  labels_in <- ca$labels
  if (is.list(labels_in)) {
    labels_in <- unlist(labels_in, recursive = TRUE, use.names = TRUE)
  }
  if (is.character(labels_in) && length(labels_in) > 0L) {
    if (is.null(names(labels_in))) {
      labels <- as.character(labels_in)
      if (length(cols) > 0L) {
        names(labels) <- cols[seq_len(min(length(cols), length(labels)))]
      }
    } else {
      keep <- nzchar(names(labels_in)) & nzchar(as.character(labels_in))
      labels <- as.character(labels_in[keep])
      names(labels) <- as.character(names(labels_in)[keep])
    }
  }
  ca$labels <- labels
  ca
}

gflowui_normalize_graph_set_manifest <- function(graph_set) {
  if (!is.list(graph_set)) {
    return(list())
  }

  out <- graph_set
  out$id <- .as_scalar_chr(out$id, default = "")
  out$label <- .as_scalar_chr(out$label, default = out$id)

  out$data_type_id <- .as_scalar_chr(out$data_type_id, default = out$id)
  label_default <- if (nzchar(out$label)) out$label else out$data_type_id
  out$data_type_label <- .as_scalar_chr(out$data_type_label, default = label_default)

  out$n_samples <- .as_scalar_pos_int(out$n_samples)
  out$n_features <- .as_scalar_pos_int(out$n_features)

  if (!is.list(out$optimal_k_artifacts)) {
    out$optimal_k_artifacts <- list()
  }
  if (length(out$optimal_k_artifacts) > 0L) {
    keep <- vapply(out$optimal_k_artifacts, function(x) nzchar(.as_scalar_chr(x, default = "")), logical(1))
    out$optimal_k_artifacts <- out$optimal_k_artifacts[keep]
    if (length(out$optimal_k_artifacts) > 0L) {
      vals <- lapply(out$optimal_k_artifacts, function(x) .as_scalar_chr(x, default = ""))
      nms <- names(vals)
      if (is.null(nms)) {
        nms <- rep("", length(vals))
      }
      nms[!nzchar(nms)] <- sprintf("criterion_%d", which(!nzchar(nms)))
      names(vals) <- nms
      out$optimal_k_artifacts <- vals
    }
  }

  out$layout_assets <- .normalize_layout_assets(out$layout_assets)
  out$color_assets <- .normalize_color_assets(out$color_assets)

  existing_grip <- if (is.list(out$layout_assets$grip_layouts)) out$layout_assets$grip_layouts else list()
  existing_grip <- .normalize_grip_layout_entries(existing_grip)
  existing_k <- if (length(existing_grip) > 0L) {
    suppressWarnings(as.integer(vapply(existing_grip, function(x) x$k, integer(1))))
  } else {
    integer(0)
  }
  existing_path <- if (length(existing_grip) > 0L) {
    vapply(existing_grip, function(x) .as_scalar_chr(x$path, default = ""), character(1))
  } else {
    character(0)
  }

  legacy_grip_paths <- c(
    out$layout3d_file,
    out$layout3d_files,
    out$layout_file,
    out$layout_files,
    out$layout_assets$layout3d_file,
    out$layout_assets$layout3d_files,
    out$layout_assets$layout_file,
    out$layout_assets$layout_files
  )
  inferred_grip <- .normalize_grip_layout_entries(legacy_grip_paths)
  discovered_grip <- .discover_grip_layout_entries(
    graph_file = out$graph_file,
    set_id = out$id,
    k_values = out$k_values
  )

  for (one in c(inferred_grip, discovered_grip)) {
    if (!is.list(one) || is.null(one$k) || is.null(one$path)) {
      next
    }
    k_one <- suppressWarnings(as.integer(one$k))
    path_one <- .as_scalar_chr(one$path, default = "")
    if (!is.finite(k_one) || !nzchar(path_one)) {
      next
    }
    if (k_one %in% existing_k || path_one %in% existing_path) {
      next
    }
    id_use <- one$id
    while (id_use %in% names(existing_grip)) {
      id_use <- sprintf("%s_%d", one$id, length(existing_grip) + 1L)
    }
    one$id <- id_use
    existing_grip[[id_use]] <- one
    existing_k <- c(existing_k, k_one)
    existing_path <- c(existing_path, path_one)
  }

  out$layout_assets$grip_layouts <- existing_grip

  params_use <- .normalize_grip_layout_params(out$layout_assets$grip_layout_params %||% out$grip_layout_params %||% list())
  out$layout_assets$grip_layout_params <- params_use
  out
}

gflowui_normalize_graph_sets_manifest <- function(graph_sets) {
  if (!is.list(graph_sets) || length(graph_sets) < 1L) {
    return(list())
  }
  lapply(graph_sets, gflowui_normalize_graph_set_manifest)
}

.resolve_profile <- function(profile, project_root) {
  profile <- match.arg(
    profile,
    choices = c("auto", "symptoms_restart", "agp_restart", "custom")
  )

  if (!identical(profile, "auto")) {
    return(profile)
  }

  root_name <- basename(normalizePath(project_root, mustWork = FALSE))
  if (identical(root_name, "symptoms")) {
    return("symptoms_restart")
  }
  if (identical(root_name, "AGP")) {
    return("agp_restart")
  }
  "custom"
}

.discover_symptoms_artifacts <- function(project_root) {
  results_root <- file.path(project_root, "results")
  metadata_file <- file.path(project_root, "data", "S_asv.rda")
  metadata_file_norm <- if (file.exists(metadata_file)) normalizePath(metadata_file, mustWork = TRUE) else ""
  symptoms_color_assets <- list(
    metadata_file = metadata_file_norm,
    metadata_object = "mt.asv",
    vector_columns = c("CST", "subCST"),
    preferred_order = c("CST", "subCST"),
    labels = c(CST = "CST", subCST = "subCST")
  )
  hv_summary_file <- file.path(results_root, "asv_hv_k_gcv_sweep", "summary.across.feature.sets.csv")
  full_summary_file <- file.path(results_root, "asv_full_graph_hv_criteria_k_selection", "summary.across.criteria.csv")
  full_meta_file <- file.path(results_root, "asv_full_graph_hv_criteria_k_selection", "run.metadata.rds")

  hv_summary_tbl <- .read_csv_if_exists(hv_summary_file)
  full_summary_tbl <- .read_csv_if_exists(full_summary_file)
  full_meta <- if (file.exists(full_meta_file)) {
    tryCatch(readRDS(full_meta_file), error = function(e) NULL)
  } else {
    NULL
  }

  graph_specs <- list(
    list(
      id = "top20",
      label = "ASV HV20",
      graph_file = file.path(results_root, "asv_hv_k_gcv_sweep", "top20", "iknn.selection.rds"),
      k_source = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "vag_odor_gcv_by_k.csv")
    ),
    list(
      id = "top30",
      label = "ASV HV30",
      graph_file = file.path(results_root, "asv_hv_k_gcv_sweep", "top30", "iknn.selection.rds"),
      k_source = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv30", "vag_odor_gcv_by_k.csv")
    ),
    list(
      id = "top50",
      label = "ASV HV50",
      graph_file = file.path(results_root, "asv_hv_k_gcv_sweep", "top50", "iknn.selection.rds"),
      k_source = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv50", "vag_odor_gcv_by_k.csv")
    ),
    list(
      id = "all",
      label = "ASV Full Graph",
      graph_file = file.path(results_root, "asv_full_graph_hv_criteria_k_selection", "asv.full.iknn.selection.rds"),
      k_source = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "all", "vag_odor_gcv_by_k.csv")
    )
  )

  hv_fig_dir <- file.path(results_root, "asv_hv_k_gcv_sweep", "figures")
  full_criteria_dir <- file.path(results_root, "asv_full_graph_hv_criteria_k_selection")
  full_fig_dir <- file.path(full_criteria_dir, "figures")
  vag_fig_dir <- file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "figures")

  graph_sets <- list()
  for (sp in graph_specs) {
    if (!file.exists(sp$graph_file)) {
      next
    }
    k_vals <- .k_from_column(sp$k_source, column_name = "k")
    n_features <- suppressWarnings(as.integer(sub("^top([0-9]+)$", "\\1", sp$id, perl = TRUE)))
    if (!is.finite(n_features)) {
      n_features <- NA_integer_
    }
    n_samples <- NA_integer_

    if (is.data.frame(hv_summary_tbl) && nrow(hv_summary_tbl) > 0L && "set.tag" %in% names(hv_summary_tbl)) {
      row <- hv_summary_tbl[tolower(as.character(hv_summary_tbl$set.tag)) == tolower(sp$id), , drop = FALSE]
      if (nrow(row) > 0L) {
        s <- suppressWarnings(as.integer(row$n.samples))
        s <- s[is.finite(s) & s > 0L]
        if (length(s) > 0L) {
          n_samples <- s[[1]]
        }

        f <- suppressWarnings(as.integer(row$n.features))
        f <- f[is.finite(f) & f > 0L]
        if (length(f) > 0L) {
          n_features <- f[[1]]
        }
      }
    }

    if (identical(sp$id, "all") && is.data.frame(full_summary_tbl) && nrow(full_summary_tbl) > 0L) {
      s <- suppressWarnings(as.integer(full_summary_tbl$n.samples))
      s <- s[is.finite(s) & s > 0L]
      if (length(s) > 0L) {
        n_samples <- s[[1]]
      }

      gf <- suppressWarnings(as.integer(full_summary_tbl$graph.features))
      gf <- gf[is.finite(gf) & gf > 0L]
      if (length(gf) > 0L) {
        n_features <- gf[[1]]
      }
    }

    if (is.list(full_meta)) {
      if (!is.finite(n_samples)) {
        s <- suppressWarnings(as.integer(full_meta$asv.samples))
        s <- s[is.finite(s) & s > 0L]
        if (length(s) > 0L) {
          n_samples <- s[[1]]
        }
      }
      if (!is.finite(n_features)) {
        f <- suppressWarnings(as.integer(full_meta$asv.features))
        f <- f[is.finite(f) & f > 0L]
        if (length(f) > 0L) {
          n_features <- f[[1]]
        }
      }
    }

    optimal_artifacts <- list()
    add_artifact <- function(name, path) {
      if (file.exists(path)) {
        optimal_artifacts[[name]] <<- normalizePath(path, mustWork = TRUE)
      }
      invisible(NULL)
    }

    if (identical(sp$id, "all")) {
      add_artifact("median_norm_gcv", file.path(full_fig_dir, "criterion_hv20_hv30_hv50_mean_median_vs_k.pdf"))
      add_artifact("median_norm_gcv_summary", file.path(full_criteria_dir, "summary.across.criteria.csv"))
      add_artifact("median_norm_gcv_hv20", file.path(full_fig_dir, "criterion.hv20_mean_median_vs_k.pdf"))
      add_artifact("median_norm_gcv_hv30", file.path(full_fig_dir, "criterion.hv30_mean_median_vs_k.pdf"))
      add_artifact("median_norm_gcv_hv50", file.path(full_fig_dir, "criterion.hv50_mean_median_vs_k.pdf"))
      add_artifact("response_gcv", file.path(vag_fig_dir, "all_vag_odor_gcv_vs_k.pdf"))
      if (!("response_gcv" %in% names(optimal_artifacts))) {
        add_artifact("response_gcv", sp$k_source)
      }
    } else {
      set_tag <- as.character(sp$id)
      fam_tag <- sub("^top", "hv", set_tag)

      add_artifact("median_norm_gcv", file.path(hv_fig_dir, sprintf("%s_mean_median_vs_k.pdf", set_tag)))
      add_artifact("median_norm_gcv_summary", file.path(results_root, "asv_hv_k_gcv_sweep", set_tag, "k.distribution.summary.csv"))
      if (!("median_norm_gcv" %in% names(optimal_artifacts))) {
        add_artifact("median_norm_gcv", file.path(results_root, "asv_hv_k_gcv_sweep", set_tag, "k.distribution.summary.csv"))
      }

      add_artifact("response_gcv", file.path(vag_fig_dir, sprintf("%s_vag_odor_gcv_vs_k.pdf", fam_tag)))
      if (!("response_gcv" %in% names(optimal_artifacts))) {
        add_artifact("response_gcv", sp$k_source)
      }
    }

    graph_sets[[length(graph_sets) + 1L]] <- gflowui_normalize_graph_set_manifest(list(
      id = sp$id,
      label = sp$label,
      data_type_id = sp$id,
      data_type_label = if (identical(sp$id, "all")) "ASV" else sprintf("ASV-top%d", n_features),
      graph_file = normalizePath(sp$graph_file, mustWork = TRUE),
      k_values = k_vals,
      n_samples = n_samples,
      n_features = n_features,
      optimal_k_artifacts = optimal_artifacts,
      color_assets = symptoms_color_assets
    ))
  }

  condexp_sets <- list()
  fam_info <- list(
    hv20 = list(
      summary = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "vag_odor_gcv_by_k.csv"),
      fits = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv20", "fits")
    ),
    hv30 = list(
      summary = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv30", "vag_odor_gcv_by_k.csv"),
      fits = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv30", "fits")
    ),
    hv50 = list(
      summary = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv50", "vag_odor_gcv_by_k.csv"),
      fits = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "hv50", "fits")
    ),
    all = list(
      summary = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "all", "vag_odor_gcv_by_k.csv"),
      fits = file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "all", "fits")
    )
  )

  fam_rows <- list()
  for (nm in names(fam_info)) {
    summary_file <- fam_info[[nm]]$summary
    fits_dir <- fam_info[[nm]]$fits
    if (!file.exists(summary_file) || !dir.exists(fits_dir)) {
      next
    }

    fit_files <- list.files(
      fits_dir,
      pattern = "^vag_odor_fit_k[0-9]+\\.rds$",
      full.names = TRUE
    )
    fit_files <- fit_files[file.exists(fit_files)]

    fam_rows[[length(fam_rows) + 1L]] <- list(
      family = nm,
      summary_file = normalizePath(summary_file, mustWork = TRUE),
      fits_dir = normalizePath(fits_dir, mustWork = TRUE),
      fit_files = normalizePath(fit_files, mustWork = TRUE),
      k_values = .k_from_column(summary_file, column_name = "k")
    )
  }

  if (length(fam_rows) > 0L) {
    k_union <- .extract_int_values(unlist(lapply(fam_rows, function(x) x$k_values), use.names = FALSE))
    condexp_sets[[1L]] <- list(
      id = "vag_odor_binary",
      label = "VAG_ODOR Conditional Expectation",
      type = "fit_files",
      outcomes = c("vag_odor"),
      family_runs = fam_rows,
      summary_file = if (file.exists(file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "vag_odor_gcv_all_families.csv"))) {
        normalizePath(file.path(results_root, "vag_odor_asv_graph_gcv_sweep", "vag_odor_gcv_all_families.csv"), mustWork = TRUE)
      } else {
        NA_character_
      },
      k_values = k_union
    )
  }

  endpoint_runs <- list()
  endpoint_dir <- file.path(results_root, "asv_full_graph_evenness_endpoints_k05")
  endpoint_bundle <- file.path(endpoint_dir, "evenness.endpoints.k05.bundle.rds")
  endpoint_summary <- file.path(endpoint_dir, "evenness.endpoint.summary.k05.csv")
  endpoint_labels <- file.path(endpoint_dir, "evenness.endpoint.labels.k05.csv")

  if (dir.exists(endpoint_dir) &&
      (file.exists(endpoint_bundle) || file.exists(endpoint_summary) || file.exists(endpoint_labels))) {
    k_vals <- .k_from_column(endpoint_summary, column_name = "k")
    endpoint_runs[[1L]] <- list(
      id = "evenness_k05",
      label = "Evenness Endpoints (k=5)",
      run_dir = normalizePath(endpoint_dir, mustWork = TRUE),
      bundle_file = if (file.exists(endpoint_bundle)) normalizePath(endpoint_bundle, mustWork = TRUE) else NA_character_,
      summary_csv = if (file.exists(endpoint_summary)) normalizePath(endpoint_summary, mustWork = TRUE) else NA_character_,
      labels_csv = if (file.exists(endpoint_labels)) normalizePath(endpoint_labels, mustWork = TRUE) else NA_character_,
      k_values = k_vals,
      method = "evenness_minima"
    )
  }

  list(
    profile = "symptoms_restart",
    project_root = normalizePath(project_root, mustWork = TRUE),
    graph_sets = graph_sets,
    condexp_sets = condexp_sets,
    endpoint_runs = endpoint_runs,
    defaults = list(
      graph_set_id = if (any(vapply(graph_sets, function(x) identical(x$id, "all"), logical(1)))) "all" else {
        if (length(graph_sets) > 0L) graph_sets[[1L]]$id else NA_character_
      },
      condexp_set_id = if (length(condexp_sets) > 0L) condexp_sets[[1L]]$id else NA_character_,
      endpoint_run_id = if (length(endpoint_runs) > 0L) endpoint_runs[[1L]]$id else NA_character_
    )
  )
}

.discover_agp_artifacts <- function(project_root) {
  base <- file.path(project_root, "results", "asv_hv_k_gcv_sweep")
  run_meta_file <- file.path(base, "run.metadata.rds")
  run_meta <- if (file.exists(run_meta_file)) {
    tryCatch(readRDS(run_meta_file), error = function(e) NULL)
  } else {
    NULL
  }
  run_meta_samples <- suppressWarnings(as.integer(run_meta$asv.samples %||% run_meta$sample_set.count %||% NA_integer_))
  run_meta_samples <- run_meta_samples[is.finite(run_meta_samples) & run_meta_samples > 0L]
  run_meta_features <- suppressWarnings(as.integer(run_meta$asv.features %||% NA_integer_))
  run_meta_features <- run_meta_features[is.finite(run_meta_features) & run_meta_features > 0L]

  graph_sets <- list()
  shared_graph_file <- file.path(base, "shared_graphs_all_asv", "iknn.selection.rds")
  if (file.exists(shared_graph_file)) {
    k_status <- file.path(base, "top20", "k.status.csv")
    k_vals <- .k_from_column(file.path(base, "top20", "k.status.csv"), column_name = "k.requested")
    optimal_artifacts <- list()
    if (file.exists(k_status)) {
      optimal_artifacts$response_gcv <- normalizePath(k_status, mustWork = TRUE)
    }
    graph_sets[[length(graph_sets) + 1L]] <- gflowui_normalize_graph_set_manifest(list(
      id = "shared_all_asv",
      label = "Shared All-ASV Graph Family",
      data_type_id = "shared_all_asv",
      data_type_label = "ASV",
      graph_file = normalizePath(shared_graph_file, mustWork = TRUE),
      k_values = k_vals,
      n_samples = if (length(run_meta_samples) > 0L) run_meta_samples[[1]] else NA_integer_,
      n_features = if (length(run_meta_features) > 0L) run_meta_features[[1]] else NA_integer_,
      optimal_k_artifacts = optimal_artifacts
    ))
  }

  sens_dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  sens_dirs <- sens_dirs[grepl("^k_sensitivity_", basename(sens_dirs))]
  for (dd in sens_dirs) {
    sens_file <- file.path(dd, "iknn.selection.sensitivity.rds")
    if (!file.exists(sens_file)) {
      next
    }
    k_vals <- .k_from_filename_vector(strsplit(basename(dd), "_", fixed = TRUE)[[1]])
    graph_sets[[length(graph_sets) + 1L]] <- gflowui_normalize_graph_set_manifest(list(
      id = basename(dd),
      label = sprintf("Sensitivity Bundle (%s)", basename(dd)),
      data_type_id = basename(dd),
      data_type_label = sprintf("Sensitivity (%s)", basename(dd)),
      graph_file = normalizePath(sens_file, mustWork = TRUE),
      k_values = k_vals
    ))
  }

  condexp_sets <- list()
  bench_dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  bench_dirs <- bench_dirs[grepl("^ibs_ibd_benchmark", basename(bench_dirs))]
  for (dd in bench_dirs) {
    long_file <- file.path(dd, "ibs_ibd_conditional_expectation.long.rds")
    gcv_file <- file.path(dd, "ibs_ibd_gcv_summary.csv")
    if (!file.exists(long_file)) {
      next
    }

    k_vals <- integer(0)
    outcomes <- character(0)
    gcv_tbl <- .read_csv_if_exists(gcv_file)
    if (!is.null(gcv_tbl)) {
      if ("k" %in% names(gcv_tbl)) {
        k_vals <- .extract_int_values(gcv_tbl$k)
      }
      if ("outcome" %in% names(gcv_tbl)) {
        outcomes <- sort(unique(as.character(gcv_tbl$outcome)))
      }
    }
    if ((length(k_vals) < 1L || length(outcomes) < 1L) && file.exists(long_file)) {
      long_tbl <- tryCatch(readRDS(long_file), error = function(e) NULL)
      if (is.data.frame(long_tbl) && nrow(long_tbl) > 0L) {
        if (length(k_vals) < 1L && "k" %in% names(long_tbl)) {
          k_vals <- .extract_int_values(long_tbl$k)
        }
        if (length(outcomes) < 1L && "outcome" %in% names(long_tbl)) {
          outcomes <- sort(unique(as.character(long_tbl$outcome)))
        }
      }
    }

    condexp_sets[[length(condexp_sets) + 1L]] <- list(
      id = basename(dd),
      label = sprintf("IBS/IBD Benchmark (%s)", basename(dd)),
      type = "long_table",
      run_dir = normalizePath(dd, mustWork = TRUE),
      long_table_file = normalizePath(long_file, mustWork = TRUE),
      gcv_summary_file = if (file.exists(gcv_file)) normalizePath(gcv_file, mustWork = TRUE) else NA_character_,
      outcomes = outcomes,
      k_values = k_vals
    )
  }

  endpoint_runs <- list()
  ep_dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  ep_dirs <- ep_dirs[grepl("^evenness_endpoints_", basename(ep_dirs))]
  for (dd in ep_dirs) {
    summary_file <- file.path(dd, "evenness_endpoint_summary.csv")
    labels_file <- file.path(dd, "evenness_endpoint_labels.csv")
    per_k_dir <- file.path(dd, "per_k")
    per_k_files <- list.files(
      per_k_dir,
      pattern = "^evenness_k[0-9]+_endpoints\\.rds$",
      full.names = TRUE
    )

    has_any <- file.exists(summary_file) || file.exists(labels_file) || length(per_k_files) > 0L
    if (!has_any) {
      next
    }

    k_vals <- .k_from_column(summary_file, column_name = "k")
    if (length(k_vals) < 1L) {
      k_vals <- .k_from_filename_vector(per_k_files)
    }

    methods <- character(0)
    summary_tbl <- .read_csv_if_exists(summary_file)
    if (!is.null(summary_tbl) && "endpoint.method" %in% names(summary_tbl)) {
      methods <- sort(unique(as.character(summary_tbl$endpoint.method)))
    }

    endpoint_runs[[length(endpoint_runs) + 1L]] <- list(
      id = basename(dd),
      label = sprintf("Evenness Endpoints (%s)", basename(dd)),
      run_dir = normalizePath(dd, mustWork = TRUE),
      summary_csv = if (file.exists(summary_file)) normalizePath(summary_file, mustWork = TRUE) else NA_character_,
      labels_csv = if (file.exists(labels_file)) normalizePath(labels_file, mustWork = TRUE) else NA_character_,
      run_metadata = if (file.exists(file.path(dd, "run.metadata.rds"))) normalizePath(file.path(dd, "run.metadata.rds"), mustWork = TRUE) else NA_character_,
      per_k_bundles = if (length(per_k_files) > 0L) normalizePath(per_k_files, mustWork = TRUE) else character(0),
      k_values = k_vals,
      methods = methods
    )
  }

  pick_default <- function(ids, preferred) {
    if (length(ids) < 1L) {
      return(NA_character_)
    }
    for (pp in preferred) {
      if (pp %in% ids) {
        return(pp)
      }
    }
    ids[1]
  }

  graph_ids <- vapply(graph_sets, function(x) x$id, character(1))
  condexp_ids <- vapply(condexp_sets, function(x) x$id, character(1))
  endpoint_ids <- vapply(endpoint_runs, function(x) x$id, character(1))

  list(
    profile = "agp_restart",
    project_root = normalizePath(project_root, mustWork = TRUE),
    graph_sets = graph_sets,
    condexp_sets = condexp_sets,
    endpoint_runs = endpoint_runs,
    defaults = list(
      graph_set_id = pick_default(graph_ids, c("shared_all_asv")),
      condexp_set_id = pick_default(condexp_ids, c("ibs_ibd_benchmark_k071230")),
      endpoint_run_id = pick_default(endpoint_ids, c("evenness_endpoints_k07", "evenness_endpoints_k071230_tuned"))
    )
  )
}

#' Discover Existing Project Artifacts
#'
#' Scans an existing analysis project directory and returns graph, conditional
#' expectation, and endpoint artifacts that can be registered in `gflowui`.
#'
#' @param project_root Path to the external analysis project root.
#' @param profile Discovery profile. Use `"auto"` to infer from project folder
#'   name (`symptoms`, `AGP`), or set one of `"symptoms_restart"`,
#'   `"agp_restart"`, or `"custom"`.
#'
#' @return A list with `graph_sets`, `condexp_sets`, `endpoint_runs`, and
#'   suggested `defaults`.
#' @export
#'
#' @examples
#' \dontrun{
#' discover_project_artifacts("~/current_projects/symptoms", profile = "symptoms_restart")
#' }
discover_project_artifacts <- function(
    project_root,
    profile = c("auto", "symptoms_restart", "agp_restart", "custom")) {
  if (!is.character(project_root) || !nzchar(project_root[1])) {
    stop("project_root must be a non-empty string.", call. = FALSE)
  }
  root <- normalizePath(path.expand(project_root[1]), mustWork = TRUE)
  profile_use <- .resolve_profile(profile = profile[1], project_root = root)

  if (identical(profile_use, "symptoms_restart")) {
    return(.discover_symptoms_artifacts(root))
  }
  if (identical(profile_use, "agp_restart")) {
    return(.discover_agp_artifacts(root))
  }

  list(
    profile = "custom",
    project_root = root,
    graph_sets = list(),
    condexp_sets = list(),
    endpoint_runs = list(),
    defaults = list(
      graph_set_id = NA_character_,
      condexp_set_id = NA_character_,
      endpoint_run_id = NA_character_
    )
  )
}

#' Register A Project For gflowui
#'
#' Creates or updates a `gflowui` project registry entry with a persisted
#' project manifest so the project appears in the app `Projects` dropdown.
#'
#' @param project_root Path to the external analysis project root.
#' @param project_id Optional project identifier. When `NULL`, one is generated
#'   from `project_name`.
#' @param project_name Display name in the UI.
#' @param profile Discovery profile (`"auto"`, `"symptoms_restart"`,
#'   `"agp_restart"`, `"custom"`).
#' @param graph_sets Optional explicit graph-set list to store in manifest.
#' @param condexp_sets Optional explicit conditional-expectation set list.
#' @param endpoint_runs Optional explicit endpoint run list.
#' @param defaults Optional list overriding discovered defaults.
#' @param scan_results Whether to run discovery from on-disk outputs.
#' @param overwrite Whether to overwrite an existing project with same id.
#'
#' @return Invisibly returns a list with `project_id`, `manifest_file`, and
#'   the manifest object.
#' @export
#'
#' @examples
#' \dontrun{
#' register_project(
#'   project_root = "~/current_projects/AGP",
#'   profile = "agp_restart",
#'   project_name = "AGP Restart"
#' )
#' }
register_project <- function(
    project_root,
    project_id = NULL,
    project_name = NULL,
    profile = c("auto", "symptoms_restart", "agp_restart", "custom"),
    graph_sets = NULL,
    condexp_sets = NULL,
    endpoint_runs = NULL,
    defaults = list(),
    scan_results = TRUE,
    overwrite = FALSE) {
  if (!is.character(project_root) || !nzchar(project_root[1])) {
    stop("project_root must be a non-empty string.", call. = FALSE)
  }
  root <- normalizePath(path.expand(project_root[1]), mustWork = TRUE)

  reg <- gflowui_load_registry()
  existing_ids <- reg$id

  if (is.null(project_name) || !nzchar(as.character(project_name[1]))) {
    project_name <- basename(root)
  }
  project_name <- as.character(project_name[1])

  if (is.null(project_id) || !nzchar(as.character(project_id[1]))) {
    project_id <- gflowui_make_project_id(project_name, existing_ids = existing_ids)
  } else {
    project_id <- as.character(project_id[1])
  }

  idx_existing <- match(project_id, reg$id)
  if (!is.na(idx_existing) && !isTRUE(overwrite)) {
    stop(sprintf("Project id '%s' already exists. Use overwrite = TRUE.", project_id), call. = FALSE)
  }

  discovered <- list(
    profile = .resolve_profile(profile = profile[1], project_root = root),
    project_root = root,
    graph_sets = list(),
    condexp_sets = list(),
    endpoint_runs = list(),
    defaults = list()
  )

  if (isTRUE(scan_results)) {
    discovered <- discover_project_artifacts(project_root = root, profile = profile[1])
  }

  graph_sets_use <- if (!is.null(graph_sets)) graph_sets else discovered$graph_sets
  condexp_sets_use <- if (!is.null(condexp_sets)) condexp_sets else discovered$condexp_sets
  endpoint_runs_use <- if (!is.null(endpoint_runs)) endpoint_runs else discovered$endpoint_runs
  graph_sets_use <- gflowui_normalize_graph_sets_manifest(graph_sets_use)

  defaults_use <- discovered$defaults
  if (length(defaults) > 0L) {
    defaults_use[names(defaults)] <- defaults
  }

  created_at <- .gflowui_now()
  if (!is.na(idx_existing)) {
    created_at <- as.character(reg$created_at[idx_existing] %||% .gflowui_now())
  }

  manifest <- list(
    version = "1",
    project_id = project_id,
    project_name = project_name,
    profile = as.character(discovered$profile %||% "custom"),
    project_root = root,
    created_at = created_at,
    updated_at = .gflowui_now(),
    graph_sets = graph_sets_use,
    condexp_sets = condexp_sets_use,
    endpoint_runs = endpoint_runs_use,
    defaults = defaults_use
  )

  manifest_file <- gflowui_manifest_path(project_id)
  gflowui_write_manifest(manifest, manifest_file)

  entry <- gflowui_registry_entry(
    id = project_id,
    label = project_name,
    origin = sprintf("registered:%s", as.character(discovered$profile %||% "custom")),
    has_graphs = length(graph_sets_use) > 0L,
    has_condexp = length(condexp_sets_use) > 0L,
    has_endpoints = length(endpoint_runs_use) > 0L,
    project_root = root,
    manifest_file = normalizePath(manifest_file, mustWork = FALSE),
    created_at = created_at,
    updated_at = .gflowui_now()
  )

  reg_updated <- gflowui_upsert_registry_row(reg, entry, overwrite = TRUE)
  gflowui_save_registry(reg_updated)

  invisible(list(
    project_id = project_id,
    manifest_file = normalizePath(manifest_file, mustWork = FALSE),
    manifest = manifest
  ))
}

#' List Registered gflowui Projects
#'
#' @param include_manifests When `TRUE`, also returns parsed project manifests.
#'
#' @return A data frame of registered projects, or a list with `registry` and
#'   `manifests` when `include_manifests = TRUE`.
#' @export
#'
#' @examples
#' list_projects()
list_projects <- function(include_manifests = FALSE) {
  reg <- gflowui_load_registry()

  if (!isTRUE(include_manifests)) {
    return(reg)
  }

  manifests <- lapply(reg$manifest_file, gflowui_read_manifest)
  names(manifests) <- reg$id

  list(
    registry = reg,
    manifests = manifests
  )
}

#' Unregister A gflowui Project
#'
#' @param project_id Project id to remove from registry.
#' @param delete_manifest Whether to delete the stored manifest file.
#'
#' @return Invisibly returns `TRUE` when removed and `FALSE` when no matching
#'   project id exists.
#' @export
#'
#' @examples
#' \dontrun{unregister_project("agp_restart")}
unregister_project <- function(project_id, delete_manifest = TRUE) {
  if (!is.character(project_id) || !nzchar(project_id[1])) {
    stop("project_id must be a non-empty string.", call. = FALSE)
  }
  id <- as.character(project_id[1])

  reg <- gflowui_load_registry()
  idx <- match(id, reg$id)
  if (is.na(idx)) {
    return(invisible(FALSE))
  }

  manifest_file <- reg$manifest_file[idx]
  reg <- reg[-idx, , drop = FALSE]
  gflowui_save_registry(reg)

  if (isTRUE(delete_manifest) && is.character(manifest_file) && nzchar(manifest_file) && file.exists(manifest_file)) {
    unlink(manifest_file, force = TRUE)
  }

  invisible(TRUE)
}
