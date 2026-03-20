args_all <- commandArgs(trailingOnly = FALSE)
args_trailing <- commandArgs(trailingOnly = TRUE)
file_arg <- "--file="
script_arg <- args_all[startsWith(args_all, file_arg)]
if (length(script_arg) < 1L) {
  stop("This script must be run with Rscript.", call. = FALSE)
}

script_path <- normalizePath(sub(file_arg, "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args_trailing[startsWith(args_trailing, prefix)]
  if (length(hit) < 1L) {
    return(default)
  }
  sub(prefix, "", hit[[1L]], fixed = TRUE)
}

project_id <- as.character(arg_value("project-id", "prjna896865"))
project_name <- as.character(arg_value("project-name", "PRJNA896865"))

source_root_default <- "/Users/pgajer/current_projects/vaginal_microbiome/outputs/amplicon_lanes/v4/projects/PRJNA896865"
source_root <- normalizePath(
  arg_value("source-root", source_root_default),
  mustWork = TRUE
)
project_root_default <- file.path(
  dirname(dirname(source_root)),
  "gflowui_projects",
  project_id
)
project_root <- normalizePath(
  path.expand(arg_value("project-root", project_root_default)),
  mustWork = FALSE
)

dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

read_tsv <- function(path) {
  utils::read.delim(
    path,
    sep = "\t",
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_tsv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    x,
    file = path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = "NA"
  )
  invisible(path)
}

assert_that <- function(ok, msg) {
  if (!isTRUE(ok)) {
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (
    is.null(x) ||
    length(x) < 1L ||
    (is.character(x) && length(x) > 0L && !nzchar(x[[1L]]))
  ) {
    y
  } else {
    x
  }
}

first_string <- function(x, default = "") {
  vals <- as.character(x)
  vals <- vals[!is.na(vals)]
  if (length(vals) < 1L || !nzchar(vals[[1L]])) {
    return(as.character(default)[[1L]])
  }
  vals[[1L]]
}

first_non_missing <- function(x, default = NA_character_) {
  vals <- as.character(x)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) < 1L) {
    return(as.character(default)[[1L]])
  }
  vals[[1L]]
}

first_integer <- function(x, default = NA_integer_) {
  vals <- suppressWarnings(as.integer(x))
  vals <- vals[is.finite(vals)]
  if (length(vals) < 1L) {
    return(as.integer(default)[[1L]])
  }
  vals[[1L]]
}

first_logical <- function(x, default = FALSE) {
  vals_chr <- trimws(tolower(as.character(x)))
  vals_chr <- vals_chr[!is.na(vals_chr) & nzchar(vals_chr)]
  if (length(vals_chr) < 1L) {
    return(isTRUE(default))
  }
  if (vals_chr[[1L]] %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }
  if (vals_chr[[1L]] %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }
  isTRUE(default)
}

format_taxon_label <- function(x) {
  y <- as.character(x)
  y[is.na(y) | !nzchar(y)] <- NA_character_
  keep <- !is.na(y)
  if (!any(keep)) {
    return(y)
  }

  y[keep] <- gsub("_", " ", y[keep], fixed = TRUE)
  y[keep] <- sub("^Ca Lachnocurva ", "Ca. Lachnocurva ", y[keep])
  y[keep] <- sub("^Lactobacillus ", "L. ", y[keep])
  y[keep] <- sub("^Streptococcus ", "Strep. ", y[keep])
  y[keep] <- sub("^Gardnerella ", "Gard. ", y[keep])
  y[keep] <- sub("^Megasphaera ", "Mega. ", y[keep])
  y[keep] <- sub("^Fannyhessea ", "F. ", y[keep])
  y[keep] <- sub("^Ureaplasma ", "U. ", y[keep])
  y[keep] <- sub("^Shuttleworthia ", "Shuttle. ", y[keep])
  y
}

choose_display_taxon <- function(df) {
  out <- rep(NA_character_, nrow(df))
  for (nm in c("consensus_species", "speciateIT_label", "silva_species", "silva_genus")) {
    vals <- as.character(df[[nm]])
    vals[!nzchar(vals)] <- NA_character_
    fill <- is.na(out) & !is.na(vals)
    if (any(fill)) {
      out[fill] <- format_taxon_label(vals[fill])
    }
  }
  out
}

connected_component_count <- function(adj_list) {
  n <- length(adj_list)
  if (n < 1L) {
    return(0L)
  }
  seen <- rep(FALSE, n)
  n_comp <- 0L
  for (start in seq_len(n)) {
    if (seen[[start]]) {
      next
    }
    n_comp <- n_comp + 1L
    queue <- start
    seen[[start]] <- TRUE
    idx <- 1L
    while (idx <= length(queue)) {
      vv <- queue[[idx]]
      idx <- idx + 1L
      nb <- suppressWarnings(as.integer(adj_list[[vv]]))
      nb <- nb[is.finite(nb) & nb >= 1L & nb <= n]
      nb <- nb[!seen[nb]]
      if (length(nb) > 0L) {
        seen[nb] <- TRUE
        queue <- c(queue, nb)
      }
    }
  }
  as.integer(n_comp)
}

safe_bool_string <- function(x) {
  if (isTRUE(first_logical(x))) "yes" else "no"
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

graph_edge_keys_from_adj_list <- function(adj_list) {
  out <- character(0)
  for (ii in seq_along(adj_list)) {
    nb <- suppressWarnings(as.integer(adj_list[[ii]]))
    nb <- nb[is.finite(nb) & nb > ii]
    if (length(nb) > 0L) {
      out <- c(out, sprintf("%d__%d", ii, nb))
    }
  }
  sort(unique(out))
}

read_embedding_matrix <- function(embedding_df, sample_ids, set_id) {
  if ("sample_id" %in% names(embedding_df)) {
    assert_that(
      identical(as.character(embedding_df$sample_id), as.character(sample_ids)),
      sprintf("Embedding sample_id column is not aligned to retained samples for %s.", set_id)
    )
  }

  embedding_num_cols <- names(embedding_df)[vapply(embedding_df, is.numeric, logical(1))]
  assert_that(
    length(embedding_num_cols) >= 1L,
    sprintf("No numeric embedding columns were found for %s.", set_id)
  )

  emb_mat <- as.matrix(embedding_df[, embedding_num_cols, drop = FALSE])
  storage.mode(emb_mat) <- "double"
  rownames(emb_mat) <- as.character(sample_ids)
  emb_mat
}

build_graph_family_from_embedding <- function(embedding_mat, k_values, selected_k, source_edges_df, selected_graph_path, set_id) {
  if (!requireNamespace("gflow", quietly = TRUE)) {
    stop("Package 'gflow' is required to rebuild graph families.", call. = FALSE)
  }

  source_keys <- sort(unique(as.character(source_edges_df$undirected_key)))
  family_graphs <- vector("list", length(k_values))
  metrics_rows <- vector("list", length(k_values))

  selected_graph_source <- readRDS(selected_graph_path)
  source_adj_list <- selected_graph_source$pruned_adj_list %||% selected_graph_source$adj_list
  assert_that(
    is.list(source_adj_list) && length(source_adj_list) == nrow(embedding_mat),
    sprintf("Selected graph object did not contain a usable adjacency list for %s.", set_id)
  )
  source_graph_keys <- graph_edge_keys_from_adj_list(source_adj_list)
  assert_that(
    identical(source_graph_keys, source_keys),
    sprintf("Selected graph RDS and graph_edges.tsv disagree for %s.", set_id)
  )

  for (ii in seq_along(k_values)) {
    k_use <- as.integer(k_values[[ii]])
    graph_raw <- gflow::create.single.iknn.graph(embedding_mat, k = k_use, verbose = FALSE)
    adj_use <- graph_raw$pruned_adj_list %||% graph_raw$adj_list
    wt_use <- graph_raw$pruned_weight_list %||% graph_raw$weight_list
    if (!is.list(adj_use) || length(adj_use) != nrow(embedding_mat)) {
      stop(sprintf("Graph reconstruction returned an invalid adjacency list for %s @ k=%d.", set_id, k_use), call. = FALSE)
    }
    if (!is.list(wt_use) || length(wt_use) != length(adj_use)) {
      wt_use <- lapply(adj_use, function(nb) rep(1, length(nb)))
    }

    edge_keys <- graph_edge_keys_from_adj_list(adj_use)
    edge_count <- as.integer(length(edge_keys))
    component_count <- connected_component_count(adj_use)

    if (identical(k_use, as.integer(selected_k))) {
      assert_that(
        identical(edge_keys, source_keys),
        sprintf("Rebuilt selected graph does not match authoritative edges for %s.", set_id)
      )
    }

    family_graphs[[ii]] <- list(adj_list = adj_use, weight_list = wt_use)
    metrics_rows[[ii]] <- data.frame(
      k = k_use,
      graph_edge_count = edge_count,
      graph_component_count = component_count,
      stringsAsFactors = FALSE
    )
  }

  graph_obj <- list(
    X.graphs = list(
      geom_pruned_graphs = lapply(family_graphs, function(one) {
        list(adj_list = one$adj_list, weight_list = one$weight_list)
      }),
      k.values = as.integer(k_values),
      k.reported = as.integer(k_values)
    ),
    k.values = as.integer(k_values),
    k.reported = as.integer(k_values),
    selected.k = as.integer(selected_k),
    selected.k.source = "median_norm_gcv_top20",
    graph.metrics.by.k = do.call(rbind, metrics_rows)
  )

  list(
    graph_obj = graph_obj,
    family_graphs = family_graphs,
    metrics_by_k = do.call(rbind, metrics_rows)
  )
}

write_grip_layout_files <- function(family_graphs, k_values, asset_dir, set_id, sample_ids) {
  if (!requireNamespace("grip", quietly = TRUE)) {
    stop("Package 'grip' is required to compute graph layouts.", call. = FALSE)
  }

  params <- default_grip_layout_params()
  layouts <- list()

  for (ii in seq_along(k_values)) {
    one <- family_graphs[[ii]]
    k_use <- as.integer(k_values[[ii]])
    coords <- do.call(
      grip::grip.layout,
      c(list(adj_list = one$adj_list, weight_list = one$weight_list), params)
    )
    coords <- suppressWarnings(as.matrix(coords))
    if (!is.matrix(coords) || ncol(coords) < 3L) {
      stop(sprintf("grip.layout() did not return a usable 3D matrix for %s @ k=%d.", set_id, k_use), call. = FALSE)
    }
    coords <- suppressWarnings(matrix(as.numeric(coords), nrow = nrow(coords), ncol = ncol(coords)))
    coords <- coords[, seq_len(3L), drop = FALSE]
    coords[!is.finite(coords)] <- 0
    rownames(coords) <- as.character(sample_ids)
    colnames(coords) <- c("x", "y", "z")

    layout_path <- file.path(asset_dir, sprintf("layout_k%02d.rds", k_use))
    saveRDS(coords, file = layout_path, compress = "xz")
    layouts[[sprintf("k%02d", k_use)]] <- list(
      id = sprintf("k%02d", k_use),
      label = sprintf("grip.layout k=%d", k_use),
      k = k_use,
      path = normalizePath(layout_path, mustWork = TRUE),
      source = "grip.layout"
    )
  }

  list(layouts = layouts, params = params)
}

now_txt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

screen_labels <- c(
  ge_1pct = ">=1% of samples",
  ge_2.5pct = ">=2.5% of samples",
  ge_5pct = ">=5% of samples"
)

screen_id_tokens <- c(
  ge_1pct = "ge_1pct",
  "ge_2.5pct" = "ge_2p5pct",
  ge_5pct = "ge_5pct"
)

representation_labels <- c(
  relative_abundance_pca = "Relative abundance + PCA",
  hellinger_pca = "Hellinger + PCA",
  rclr_rpca = "rCLR + RPCA"
)

default_graph_set_id <- "ge_2p5pct_relative_abundance_pca"

orientation_summary <- read_tsv(file.path(source_root, "orientation_canonicalization_summary.tsv"))
screen_summary <- read_tsv(file.path(source_root, "screen_retention_summary.tsv"))
representation_qc_summary <- read_tsv(file.path(source_root, "representation_qc_summary.tsv"))
graph_quality_summary <- read_tsv(file.path(source_root, "graph_quality_summary.tsv"))
sample_annotation <- read_tsv(file.path(source_root, "sample_annotation_join.tsv"))
detection_manifest <- read_tsv(file.path(source_root, "PRJNA896865_asv_detection_manifest.tsv"))
taxonomy_working <- read_tsv(file.path(source_root, "orientation_canonical_taxonomy_working.tsv"))

dcst_dir <- file.path(source_root, "dominance_csts_2026-03-19")
dcst_depth1_rare <- read_tsv(file.path(dcst_dir, "prjna896865_dcst_assignments_rare.tsv"))
dcst_depth1_absorb <- read_tsv(file.path(dcst_dir, "prjna896865_dcst_assignments_absorb.tsv"))
dcst_depth2_rare <- read_tsv(file.path(dcst_dir, "prjna896865_depth2_dcst_assignments_rare.tsv"))
dcst_depth2_absorb <- read_tsv(file.path(dcst_dir, "prjna896865_depth2_dcst_assignments_absorb.tsv"))
dcst_summary <- read_tsv(file.path(dcst_dir, "prjna896865_dcst_summary.tsv"))
dcst_depth2_summary <- read_tsv(file.path(dcst_dir, "prjna896865_depth2_dcst_summary.tsv"))

detection_manifest$display_taxon <- choose_display_taxon(detection_manifest)
ord_detection <- order(
  -suppressWarnings(as.numeric(detection_manifest$total_reads)),
  -suppressWarnings(as.numeric(detection_manifest$detection_prevalence)),
  as.character(detection_manifest$canonical_asv_id)
)
detection_manifest <- detection_manifest[ord_detection, , drop = FALSE]
rownames(detection_manifest) <- NULL
detection_manifest$asv_rank <- seq_len(nrow(detection_manifest))
detection_manifest$asv_id <- sprintf("asv_%d", detection_manifest$asv_rank)
taxon_tab <- table(detection_manifest$display_taxon, useNA = "no")
detection_manifest$taxon_group_size <- ifelse(
  is.na(detection_manifest$display_taxon),
  NA_integer_,
  as.integer(taxon_tab[match(detection_manifest$display_taxon, names(taxon_tab))])
)
detection_manifest$taxon_asv_label <- ifelse(
  !is.na(detection_manifest$taxon_group_size) & detection_manifest$taxon_group_size > 1L,
  paste(detection_manifest$display_taxon, detection_manifest$asv_rank),
  detection_manifest$display_taxon
)

canonical_lookup <- detection_manifest[
  ,
  c(
    "canonical_asv_id",
    "asv_id",
    "asv_rank",
    "display_taxon",
    "taxon_group_size",
    "taxon_asv_label"
  )
]
rownames(canonical_lookup) <- canonical_lookup$canonical_asv_id

report_taxon_label <- function(canonical_id) {
  key <- first_string(canonical_id, default = "")
  if (!nzchar(key) || !(key %in% rownames(canonical_lookup))) {
    return(key)
  }
  hit <- canonical_lookup[key, , drop = FALSE]
  lbl <- first_string(hit$taxon_asv_label, default = key)
  if (!nzchar(lbl)) {
    key
  } else {
    lbl
  }
}

report_cell_label <- function(label_vec) {
  vapply(label_vec, function(one) {
    if (is.na(one) || !nzchar(as.character(one))) {
      return(NA_character_)
    }
    parts <- strsplit(as.character(one), "__", fixed = TRUE)[[1L]]
    mapped <- vapply(parts, function(part) {
      if (identical(part, "RARE_DOMINANT")) {
        return("Rare")
      }
      report_taxon_label(part)
    }, character(1L))
    paste(mapped, collapse = " > ")
  }, character(1L))
}

source_artifact_rows <- data.frame(
  artifact_id = c(
    "report_pdf",
    "report_tex",
    "run_summary",
    "screen_retention_summary",
    "representation_qc_summary",
    "graph_quality_summary",
    "sample_annotation_join",
    "orientation_summary",
    "detection_manifest",
    "dcst_summary",
    "dcst_depth2_summary"
  ),
  label = c(
    "PDF report",
    "LaTeX report",
    "Run summary",
    "Screen retention summary",
    "Representation QC summary",
    "Graph quality summary",
    "Sample annotation join",
    "Orientation summary",
    "ASV detection manifest",
    "Depth-1 DCST summary",
    "Depth-2 DCST summary"
  ),
  path = c(
    file.path(source_root, "PRJNA896865_iknn_preprocessing_report_2026-03-19.pdf"),
    file.path(source_root, "PRJNA896865_iknn_preprocessing_report_2026-03-19.tex"),
    file.path(source_root, "PRJNA896865_run_summary.md"),
    file.path(source_root, "screen_retention_summary.tsv"),
    file.path(source_root, "representation_qc_summary.tsv"),
    file.path(source_root, "graph_quality_summary.tsv"),
    file.path(source_root, "sample_annotation_join.tsv"),
    file.path(source_root, "orientation_canonicalization_summary.tsv"),
    file.path(source_root, "PRJNA896865_asv_detection_manifest.tsv"),
    file.path(dcst_dir, "prjna896865_dcst_summary.tsv"),
    file.path(dcst_dir, "prjna896865_depth2_dcst_summary.tsv")
  ),
  stringsAsFactors = FALSE
)

source_artifact_rows$path <- normalizePath(source_artifact_rows$path, mustWork = TRUE)

artifact_inventory_path <- file.path(project_root, "metadata", "source_artifacts.tsv")
write_tsv(source_artifact_rows, artifact_inventory_path)

detection_manifest_pretty <- detection_manifest
detection_manifest_pretty_path <- file.path(project_root, "metadata", "asv_detection_manifest_with_labels.tsv")
write_tsv(detection_manifest_pretty, detection_manifest_pretty_path)

screen_feature_manifest_paths <- list()
graph_variant_rows <- list()
graph_sets <- list()

for (screen_name in names(screen_labels)) {
  feature_source <- file.path(
    source_root,
    "stage_single_project",
    screen_name,
    "filtered_feature_manifest.tsv"
  )
  feature_manifest <- read_tsv(feature_source)
  feature_manifest_pretty <- merge(
    feature_manifest,
    canonical_lookup[, c("canonical_asv_id", "asv_id", "asv_rank", "display_taxon", "taxon_asv_label"), drop = FALSE],
    by = "canonical_asv_id",
    all.x = TRUE,
    sort = FALSE
  )
  feature_manifest_pretty <- feature_manifest_pretty[
    order(feature_manifest_pretty$asv_rank, feature_manifest_pretty$canonical_asv_id),
    ,
    drop = FALSE
  ]
  feature_manifest_path <- file.path(
    project_root,
    "data",
    "screens",
    screen_id_tokens[[screen_name]],
    "feature_manifest_with_labels.tsv"
  )
  write_tsv(feature_manifest_pretty, feature_manifest_path)
  screen_feature_manifest_paths[[screen_name]] <- normalizePath(feature_manifest_path, mustWork = TRUE)
}

similarity_cols <- grep("_sim$", names(sample_annotation), value = TRUE)

for (screen_name in names(screen_labels)) {
  for (representation in names(representation_labels)) {
    set_id <- sprintf("%s_%s", screen_id_tokens[[screen_name]], representation)
    set_label <- sprintf("%s / %s", screen_labels[[screen_name]], representation_labels[[representation]])
    branch_root <- file.path(source_root, "stage_single_project", screen_name, representation)
    asset_dir <- file.path(project_root, "data", "graph_sets", set_id)
    dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

    rep_qc <- read_tsv(file.path(branch_root, "representation_qc.tsv"))
    latent_embedding_name <- if ("latent_embedding_artifact" %in% names(rep_qc)) {
      first_non_missing(as.character(rep_qc$latent_embedding_artifact), default = "embedding.tsv")
    } else {
      "embedding.tsv"
    }
    graph_layout_name <- if ("graph_layout_artifact" %in% names(rep_qc)) {
      first_non_missing(as.character(rep_qc$graph_layout_artifact), default = "embedding.tsv")
    } else {
      "embedding.tsv"
    }

    samples_retained <- read_tsv(file.path(branch_root, "samples_retained_after_usability.tsv"))
    latent_embedding <- read_tsv(file.path(branch_root, latent_embedding_name))
    graph_layout <- read_tsv(file.path(branch_root, graph_layout_name))
    graph_edges <- read_tsv(file.path(branch_root, "graph_edges.tsv"))
    norm_gcv_top20 <- read_tsv(file.path(branch_root, "median_norm_gcv_top20.tsv"))
    norm_gcv_top30 <- read_tsv(file.path(branch_root, "median_norm_gcv_top30.tsv"))

    graph_row <- graph_quality_summary[
      graph_quality_summary$screen_name == screen_name &
        graph_quality_summary$representation == representation,
      ,
      drop = FALSE
    ]
    assert_that(nrow(graph_row) == 1L, sprintf("Expected one graph quality row for %s.", set_id))

    assert_that(
      nrow(samples_retained) == nrow(latent_embedding),
      sprintf("Latent embedding/sample row count mismatch for %s.", set_id)
    )
    assert_that(
      nrow(samples_retained) == nrow(graph_layout),
      sprintf("Graph layout/sample row count mismatch for %s.", set_id)
    )
    assert_that(
      nrow(graph_edges) == first_integer(graph_row$graph_edge_count),
      sprintf("Edge count mismatch for %s.", set_id)
    )
    assert_that(
      first_integer(graph_row$selected_k) == first_integer(unique(graph_edges$selected_k)),
      sprintf("Selected k mismatch for %s.", set_id)
    )

    top20_k <- first_integer(norm_gcv_top20$k[which.min(suppressWarnings(as.numeric(norm_gcv_top20$median_norm_gcv)))])
    top30_k <- first_integer(norm_gcv_top30$k[which.min(suppressWarnings(as.numeric(norm_gcv_top30$median_norm_gcv)))])
    assert_that(
      top20_k == first_integer(graph_row$selected_k),
      sprintf("Top20 norm-GCV minimum mismatch for %s.", set_id)
    )
    assert_that(
      top30_k == first_integer(graph_row$selected_k_top30),
      sprintf("Top30 norm-GCV minimum mismatch for %s.", set_id)
    )
    assert_that(
      nrow(samples_retained) == first_integer(rep_qc$samples_output),
      sprintf("Sample count mismatch between retained-sample table and QC for %s.", set_id)
    )
    assert_that(
      first_integer(rep_qc$features_output) == first_integer(graph_row$feature_count_for_graph),
      sprintf("Feature count mismatch between QC and graph summary for %s.", set_id)
    )

    samples_retained$graph_index <- seq_len(nrow(samples_retained))

    sample_match <- match(samples_retained$sample_id, sample_annotation$sample_id)
    assert_that(all(is.finite(sample_match)), sprintf("Sample annotation join failed for %s.", set_id))
    branch_meta <- sample_annotation[sample_match, c("sample_id", "CST", "subCST", "score", similarity_cols), drop = FALSE]
    assert_that(
      identical(as.character(branch_meta$sample_id), as.character(samples_retained$sample_id)),
      sprintf("Sample annotation order mismatch for %s.", set_id)
    )

    depth1_rare_match <- match(samples_retained$sample_id, dcst_depth1_rare$sample_id)
    depth1_absorb_match <- match(samples_retained$sample_id, dcst_depth1_absorb$sample_id)
    depth2_rare_match <- match(samples_retained$sample_id, dcst_depth2_rare$sample_id)
    depth2_absorb_match <- match(samples_retained$sample_id, dcst_depth2_absorb$sample_id)
    assert_that(all(is.finite(depth1_rare_match)), sprintf("Depth-1 rare DCST join failed for %s.", set_id))
    assert_that(all(is.finite(depth1_absorb_match)), sprintf("Depth-1 absorb DCST join failed for %s.", set_id))
    assert_that(all(is.finite(depth2_rare_match)), sprintf("Depth-2 rare DCST join failed for %s.", set_id))
    assert_that(all(is.finite(depth2_absorb_match)), sprintf("Depth-2 absorb DCST join failed for %s.", set_id))

    depth1_rare_branch <- dcst_depth1_rare[depth1_rare_match, , drop = FALSE]
    depth1_absorb_branch <- dcst_depth1_absorb[depth1_absorb_match, , drop = FALSE]
    depth2_rare_branch <- dcst_depth2_rare[depth2_rare_match, , drop = FALSE]
    depth2_absorb_branch <- dcst_depth2_absorb[depth2_absorb_match, , drop = FALSE]

    branch_meta <- data.frame(
      graph_index = samples_retained$graph_index,
      sample_id = as.character(samples_retained$sample_id),
      screen = rep(screen_name, nrow(samples_retained)),
      representation = rep(representation, nrow(samples_retained)),
      original_reads = suppressWarnings(as.numeric(samples_retained$original_reads)),
      retained_reads = suppressWarnings(as.numeric(samples_retained$retained_reads)),
      retained_read_fraction = suppressWarnings(as.numeric(samples_retained$retained_read_fraction)),
      retained_nonzero_asvs = suppressWarnings(as.numeric(samples_retained$retained_nonzero_asvs)),
      usable = vapply(samples_retained$usable, first_logical, logical(1L)),
      CST = as.character(branch_meta$CST),
      subCST = as.character(branch_meta$subCST),
      score = suppressWarnings(as.numeric(branch_meta$score)),
      stringsAsFactors = FALSE
    )
    rownames(branch_meta) <- branch_meta$sample_id

    for (cc in similarity_cols) {
      branch_meta[[cc]] <- suppressWarnings(as.numeric(sample_annotation[[cc]][sample_match]))
    }

    branch_meta$dcst_depth1_rare_raw <- as.character(depth1_rare_branch$dcst_label)
    branch_meta$dcst_depth1_rare_display <- report_cell_label(depth1_rare_branch$dcst_label)
    branch_meta$dcst_depth1_absorb_raw <- as.character(depth1_absorb_branch$dcst_label)
    branch_meta$dcst_depth1_absorb_display <- report_cell_label(depth1_absorb_branch$dcst_label)
    branch_meta$dcst_depth2_rare_raw <- as.character(depth2_rare_branch$depth2_label)
    branch_meta$dcst_depth2_rare_display <- report_cell_label(depth2_rare_branch$depth2_label)
    branch_meta$dcst_depth2_absorb_raw <- as.character(depth2_absorb_branch$depth2_label)
    branch_meta$dcst_depth2_absorb_display <- report_cell_label(depth2_absorb_branch$depth2_label)
    branch_meta$dcst_raw_dominant_asv <- as.character(depth1_rare_branch$raw_dominant_asv_id)
    branch_meta$dcst_raw_dominant_display <- report_cell_label(depth1_rare_branch$raw_dominant_asv_id)

    metadata_file <- file.path(asset_dir, "metadata.rda")
    mt.asv <- branch_meta
    save(mt.asv, file = metadata_file, compress = "xz")

    sample_index_file <- file.path(asset_dir, "graph_index_to_sample.tsv")
    write_tsv(
      branch_meta[, c("graph_index", "sample_id", "original_reads", "retained_reads", "retained_read_fraction", "retained_nonzero_asvs"), drop = FALSE],
      sample_index_file
    )

    selected_k <- first_integer(graph_row$selected_k)
    k_values <- sort(unique(suppressWarnings(as.integer(norm_gcv_top20$k))))
    k_values <- k_values[is.finite(k_values)]
    assert_that(length(k_values) >= 1L, sprintf("No k values were found for %s.", set_id))
    assert_that(
      identical(k_values, sort(unique(suppressWarnings(as.integer(norm_gcv_top30$k))))),
      sprintf("Top20/top30 k grids disagree for %s.", set_id)
    )
    assert_that(
      identical(range(k_values), c(first_integer(graph_row$k_min), first_integer(graph_row$k_max))),
      sprintf("k range mismatch between norm-GCV summaries and graph summary for %s.", set_id)
    )
    assert_that(
      selected_k %in% k_values,
      sprintf("Selected k is not in the available k grid for %s.", set_id)
    )

    embedding_mat <- read_embedding_matrix(
      embedding_df = latent_embedding,
      sample_ids = samples_retained$sample_id,
      set_id = set_id
    )

    selected_graph_path <- file.path(branch_root, sprintf("selected_graph_k%02d.rds", selected_k))
    graph_family <- build_graph_family_from_embedding(
      embedding_mat = embedding_mat,
      k_values = k_values,
      selected_k = selected_k,
      source_edges_df = graph_edges,
      selected_graph_path = selected_graph_path,
      set_id = set_id
    )

    metrics_by_k <- graph_family$metrics_by_k
    selected_metrics <- metrics_by_k[metrics_by_k$k == selected_k, , drop = FALSE]
    assert_that(
      nrow(selected_metrics) == 1L,
      sprintf("Could not isolate selected-k graph metrics for %s.", set_id)
    )
    component_count <- first_integer(selected_metrics$graph_component_count)
    assert_that(
      component_count == first_integer(graph_row$graph_component_count),
      sprintf("Component count mismatch for %s.", set_id)
    )
    assert_that(
      first_integer(selected_metrics$graph_edge_count) == nrow(graph_edges),
      sprintf("Selected graph edge count mismatch for %s.", set_id)
    )

    selected_graph_index <- match(selected_k, k_values)
    assert_that(
      length(selected_graph_index) == 1L && is.finite(selected_graph_index),
      sprintf("Could not resolve the selected graph index for %s.", set_id)
    )
    selected_graph_entry <- graph_family$family_graphs[[selected_graph_index]]

    graph_obj <- graph_family$graph_obj
    graph_obj$project <- project_id
    graph_obj$screen_name <- screen_name
    graph_obj$representation <- representation
    graph_obj$vertex_ids <- branch_meta$sample_id
    graph_obj$vertex_labels <- branch_meta$sample_id
    graph_obj$n.vertices <- nrow(branch_meta)
    graph_obj$selected.k.index <- as.integer(selected_graph_index)
    graph_obj$selected.graph <- list(
      adj_list = selected_graph_entry$adj_list,
      weight_list = selected_graph_entry$weight_list
    )
    graph_obj$adj.list <- selected_graph_entry$adj_list
    graph_obj$weight.list <- selected_graph_entry$weight_list
    graph_obj$graph.summary <- list(
      graph_edge_count = first_integer(selected_metrics$graph_edge_count),
      graph_component_count = component_count
    )

    graph_file <- file.path(asset_dir, "graph.rds")
    saveRDS(graph_obj, file = graph_file, compress = "xz")

    graph_metrics_file <- file.path(asset_dir, "graph_metrics_by_k.tsv")
    write_tsv(metrics_by_k, graph_metrics_file)

    layout_bundle <- write_grip_layout_files(
      family_graphs = graph_family$family_graphs,
      k_values = k_values,
      asset_dir = asset_dir,
      set_id = set_id,
      sample_ids = branch_meta$sample_id
    )
    selected_layout_id <- sprintf("k%02d", selected_k)
    layout_file <- layout_bundle$layouts[[selected_layout_id]]$path
    assert_that(
      nzchar(as.character(layout_file %||% "")),
      sprintf("Could not resolve the selected layout file for %s.", set_id)
    )

    color_columns <- c(
      "CST",
      "subCST",
      "score",
      similarity_cols,
      "dcst_depth1_rare_display",
      "dcst_depth1_absorb_display",
      "dcst_depth2_rare_display",
      "dcst_depth2_absorb_display",
      "original_reads",
      "retained_reads",
      "retained_read_fraction",
      "retained_nonzero_asvs",
      "usable"
    )
    color_labels <- c(
      CST = "VALENCIA CST",
      subCST = "VALENCIA subCST",
      score = "VALENCIA score",
      dcst_depth1_rare_display = "DCST depth-1 (rare)",
      dcst_depth1_absorb_display = "DCST depth-1 (absorb)",
      dcst_depth2_rare_display = "DCST depth-2 (rare)",
      dcst_depth2_absorb_display = "DCST depth-2 (absorb)",
      original_reads = "Original reads",
      retained_reads = "Retained reads",
      retained_read_fraction = "Retained read fraction",
      retained_nonzero_asvs = "Retained nonzero ASVs",
      usable = "Usable"
    )
    similarity_labels <- stats::setNames(
      paste0("VALENCIA ", gsub("_", " ", similarity_cols, fixed = TRUE)),
      similarity_cols
    )
    color_labels <- c(color_labels, similarity_labels)

    graph_variant_file <- file.path(asset_dir, "graph_variant_metadata.tsv")
    graph_variant_df <- data.frame(
      metric = c(
        "screen",
        "representation",
        "selected_k_top20",
        "selected_k_top30",
        "selected_k_stable_top20_top30",
        "available_k_values",
        "sample_count_for_graph",
        "feature_count_for_graph",
        "graph_edge_count",
        "graph_component_count"
      ),
      value = c(
        screen_name,
        representation,
        first_integer(graph_row$selected_k),
        first_integer(graph_row$selected_k_top30),
        safe_bool_string(first_logical(graph_row$selected_k_stable_top20_top30)),
        paste(k_values, collapse = ","),
        nrow(branch_meta),
        first_integer(graph_row$feature_count_for_graph),
        nrow(graph_edges),
        component_count
      ),
      stringsAsFactors = FALSE
    )
    write_tsv(graph_variant_df, graph_variant_file)

    graph_variant_rows[[length(graph_variant_rows) + 1L]] <- data.frame(
      graph_set_id = set_id,
      label = set_label,
      screen_name = screen_name,
      screen_label = screen_labels[[screen_name]],
      representation = representation,
      representation_label = representation_labels[[representation]],
      selected_k = first_integer(graph_row$selected_k),
      selected_k_top30 = first_integer(graph_row$selected_k_top30),
      selected_k_stable_top20_top30 = first_logical(graph_row$selected_k_stable_top20_top30),
      sample_count_for_graph = nrow(branch_meta),
      feature_count_for_graph = first_integer(graph_row$feature_count_for_graph),
      graph_edge_count = nrow(graph_edges),
      graph_component_count = component_count,
      graph_file = normalizePath(graph_file, mustWork = TRUE),
      layout_file = normalizePath(layout_file, mustWork = TRUE),
      graph_metrics_by_k_file = normalizePath(graph_metrics_file, mustWork = TRUE),
      metadata_file = normalizePath(metadata_file, mustWork = TRUE),
      feature_manifest_file = screen_feature_manifest_paths[[screen_name]],
      sample_index_file = normalizePath(sample_index_file, mustWork = TRUE),
      stringsAsFactors = FALSE
    )

    graph_sets[[length(graph_sets) + 1L]] <- list(
      id = set_id,
      label = set_label,
      data_type_id = set_id,
      data_type_label = set_label,
      graph_file = normalizePath(graph_file, mustWork = TRUE),
      k_values = as.integer(k_values),
      n_samples = nrow(branch_meta),
      n_features = first_integer(graph_row$feature_count_for_graph),
      optimal_k_artifacts = list(
        median_norm_gcv_top20_summary = normalizePath(file.path(branch_root, "median_norm_gcv_top20.tsv"), mustWork = TRUE),
        median_norm_gcv_top30_summary = normalizePath(file.path(branch_root, "median_norm_gcv_top30.tsv"), mustWork = TRUE),
        representation_qc = normalizePath(file.path(branch_root, "representation_qc.tsv"), mustWork = TRUE)
      ),
      layout_assets = list(
        presets = list(
          renderer = "plotly",
          vertex_layout = "point",
          vertex_size = "1x",
          color_by = "CST",
          component = "all"
        ),
        grip_layouts = layout_bundle$layouts,
        grip_layout_params = layout_bundle$params
      ),
      color_assets = list(
        metadata_file = normalizePath(metadata_file, mustWork = TRUE),
        metadata_object = "mt.asv",
        vector_columns = color_columns,
        preferred_order = c(
          "CST",
          "subCST",
          "dcst_depth1_rare_display",
          "dcst_depth1_absorb_display",
          "dcst_depth2_rare_display",
          "dcst_depth2_absorb_display",
          "score"
        ),
        labels = color_labels
      ),
      screen_name = screen_name,
      screen_label = screen_labels[[screen_name]],
      representation = representation,
      representation_label = representation_labels[[representation]],
      selected_k = first_integer(graph_row$selected_k),
      selected_k_top30 = first_integer(graph_row$selected_k_top30),
      selected_k_stable_top20_top30 = first_logical(graph_row$selected_k_stable_top20_top30),
      graph_edge_count = nrow(graph_edges),
      graph_component_count = component_count,
      graph_metrics_by_k_file = normalizePath(graph_metrics_file, mustWork = TRUE),
      feature_manifest_file = screen_feature_manifest_paths[[screen_name]],
      sample_index_file = normalizePath(sample_index_file, mustWork = TRUE),
      graph_variant_metadata_file = normalizePath(graph_variant_file, mustWork = TRUE),
      source_artifacts = list(
        graph_edges = normalizePath(file.path(branch_root, "graph_edges.tsv"), mustWork = TRUE),
        latent_embedding = normalizePath(file.path(branch_root, latent_embedding_name), mustWork = TRUE),
        graph_layout = normalizePath(file.path(branch_root, graph_layout_name), mustWork = TRUE),
        selected_graph = normalizePath(selected_graph_path, mustWork = TRUE),
        samples_retained_after_usability = normalizePath(file.path(branch_root, "samples_retained_after_usability.tsv"), mustWork = TRUE),
        representation_qc = normalizePath(file.path(branch_root, "representation_qc.tsv"), mustWork = TRUE),
        median_norm_gcv_top20 = normalizePath(file.path(branch_root, "median_norm_gcv_top20.tsv"), mustWork = TRUE),
        median_norm_gcv_top30 = normalizePath(file.path(branch_root, "median_norm_gcv_top30.tsv"), mustWork = TRUE)
      ),
      selection_method = "precomputed_norm_gcv_top20",
      source = "prjna896865_adapter",
      updated_at = now_txt
    )
  }
}

graph_variant_summary <- do.call(rbind, graph_variant_rows)
rownames(graph_variant_summary) <- NULL
graph_variant_summary_path <- file.path(project_root, "metadata", "graph_variant_summary.tsv")
write_tsv(graph_variant_summary, graph_variant_summary_path)

overview_summary <- data.frame(
  Metric = c(
    "Project",
    "Amplicon",
    "Samples",
    "Raw ASVs",
    "Canonical ASVs",
    "DCST depth-1",
    "DCST depth-2",
    "Graph variants",
    "Adapter root"
  ),
  Value = c(
    project_name,
    "16S V4",
    as.character(first_integer(screen_summary$samples_retained[screen_summary$screen_name == "ge_1pct"])),
    as.character(first_integer(orientation_summary$release_asv_count)),
    as.character(first_integer(orientation_summary$canonical_asv_count)),
    "rare and absorb (n0 = 50)",
    "rare and absorb (refinement factor = 2)",
    as.character(nrow(graph_variant_summary)),
    normalizePath(project_root, mustWork = TRUE)
  ),
  stringsAsFactors = FALSE
)
overview_summary_path <- file.path(project_root, "metadata", "overview_summary.tsv")
write_tsv(overview_summary, overview_summary_path)

readme_lines <- c(
  "# PRJNA896865 gflowui adapter project",
  "",
  "This directory contains derived import assets for `gflowui` built from the authoritative",
  "PRJNA896865 preprocessing outputs under:",
  "",
  sprintf("- `%s`", normalizePath(source_root, mustWork = TRUE)),
  "",
  "Nothing in the source directory is modified. The adapter writes:",
  "",
  "- branch-specific graph-family RDS files for k = 3:20 rebuilt from the saved representation embeddings",
  "- branch-specific grip.layout() RDS files for each available k",
  "- branch-specific metadata `.rda` files for VALENCIA and DCST overlays",
  "- readable feature manifests and graph/sample index tables",
  "- overview and graph-variant summary tables for the app",
  "",
  "Rebuild with:",
  "",
  sprintf("```r\nRscript %s\n```", normalizePath(script_path, mustWork = TRUE))
)
readme_path <- file.path(project_root, "README.md")
writeLines(readme_lines, con = readme_path, useBytes = TRUE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required to register the project.", call. = FALSE)
}

pkgload::load_all(repo_root, export_all = FALSE, quiet = TRUE)

doc_sets <- lapply(seq_len(nrow(source_artifact_rows)), function(ii) {
  list(
    id = as.character(source_artifact_rows$artifact_id[[ii]]),
    label = as.character(source_artifact_rows$label[[ii]]),
    path = as.character(source_artifact_rows$path[[ii]])
  )
})

project_spec <- gflowui::build_project_spec_iknn_3x3(
  project_root = normalizePath(project_root, mustWork = TRUE),
  graph_sets = graph_sets,
  doc_sets = doc_sets,
  defaults = list(
    graph_set_id = default_graph_set_id,
    reference_graph_set_id = default_graph_set_id,
    reference_k = first_integer(
      graph_variant_summary$selected_k[graph_variant_summary$graph_set_id == default_graph_set_id]
    ),
    reference_reason = "median norm-GCV top20"
  ),
  metadata = list(
    overview = list(
      summary_table = overview_summary,
      artifact_paths = stats::setNames(
        as.list(c(
          source_artifact_rows$path,
          graph_variant_summary = normalizePath(graph_variant_summary_path, mustWork = TRUE),
          overview_summary = normalizePath(overview_summary_path, mustWork = TRUE),
          artifact_inventory = normalizePath(artifact_inventory_path, mustWork = TRUE),
          detection_manifest_with_labels = normalizePath(detection_manifest_pretty_path, mustWork = TRUE)
        )),
        c(
          source_artifact_rows$artifact_id,
          "graph_variant_summary",
          "overview_summary",
          "artifact_inventory",
          "detection_manifest_with_labels"
        )
      ),
      artifact_labels = c(
        report_pdf = "PDF report",
        report_tex = "LaTeX report",
        run_summary = "Run summary",
        screen_retention_summary = "Screen retention summary",
        representation_qc_summary = "Representation QC summary",
        graph_quality_summary = "Graph quality summary",
        sample_annotation_join = "Sample annotation join",
        orientation_summary = "Orientation summary",
        detection_manifest = "ASV detection manifest",
        dcst_summary = "Depth-1 DCST summary",
        dcst_depth2_summary = "Depth-2 DCST summary",
        graph_variant_summary = "Graph variant summary",
        overview_summary = "Overview summary",
        artifact_inventory = "Artifact inventory",
        detection_manifest_with_labels = "ASV detection manifest (readable labels)"
      ),
      source_root = normalizePath(source_root, mustWork = TRUE),
      adapter_root = normalizePath(project_root, mustWork = TRUE),
      generated_at = now_txt
    ),
    registration = list(
      builder_script = normalizePath(script_path, mustWork = TRUE),
      registration_method = "build_project_spec_iknn_3x3",
      profile = "iknn_3x3"
    )
  )
)

reg_result <- gflowui::register_project(
  project_root = normalizePath(project_root, mustWork = TRUE),
  project_id = project_id,
  project_name = project_name,
  profile = "iknn_3x3",
  project_spec = project_spec,
  scan_results = FALSE,
  overwrite = TRUE
)

manifest <- reg_result$manifest
manifest_snapshot_path <- file.path(project_root, "metadata", "registered_manifest_snapshot.rds")
saveRDS(manifest, file = manifest_snapshot_path)

message("Created PRJNA896865 gflowui project")
message(sprintf("Source root: %s", normalizePath(source_root, mustWork = TRUE)))
message(sprintf("Project root: %s", normalizePath(project_root, mustWork = TRUE)))
message(sprintf("Manifest file: %s", reg_result$manifest_file))
message(sprintf("Graph sets: %d", length(manifest$graph_sets %||% list())))
message(sprintf("Profile: %s", as.character(manifest$profile %||% "")))
