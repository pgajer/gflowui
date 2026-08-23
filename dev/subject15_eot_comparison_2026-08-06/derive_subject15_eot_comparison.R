#!/usr/bin/env Rscript

args <- commandArgs(FALSE)
file.arg <- args[grepl("^--file=", args)]
script.path <- normalizePath(
  sub("^--file=", "", file.arg[[1L]]),
  mustWork = TRUE
)
output.dir <- dirname(script.path)
gflowui.root <- normalizePath(file.path(output.dir, "..", ".."), mustWork = TRUE)
study.root <- normalizePath(
  "/Users/pgajer/current_projects/vaginal_community_trajectory_types",
  mustWork = TRUE
)

asset.path <- file.path(
  study.root,
  "analysis_output",
  "hmp_subject15_k03_eta_basin_path_estimand_corrected_20260731",
  "objects",
  "subject15_k03_eta_basin_path.rds"
)
visits.path <- file.path(
  study.root,
  "analysis_output",
  "hmp_phase3",
  "gflowui_project",
  "data",
  "subjects",
  "hmp_subject_samples.tsv"
)
fixture.path <- file.path(
  gflowui.root,
  "tests",
  "testthat",
  "fixtures",
  "basin_merge_tree_subject15_maxima.csv"
)
provenance.path <- file.path(
  gflowui.root,
  "tests",
  "testthat",
  "fixtures",
  "basin_merge_tree_subject15_maxima_provenance.csv"
)
graph.path <- file.path(
  study.root,
  "analysis_output",
  "hmp_subject15_k03_gflowui_project_20260727",
  "data",
  "graph",
  "symmetric_knn_k03.rds"
)

required <- c(
  asset.path,
  visits.path,
  fixture.path,
  provenance.path,
  graph.path
)
if (any(!file.exists(required))) {
  stop(
    "Missing required Subject 15 source assets: ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

asset <- readRDS(asset.path)
visits <- utils::read.delim(
  visits.path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fixture <- utils::read.csv(
  fixture.path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fixture.provenance <- utils::read.csv(
  provenance.path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
graph.asset <- readRDS(graph.path)
graph <- graph.asset$X.graphs$k03

field.id <- "HMP_S15_K03_ETA_04"
visits <- visits[
  as.character(visits$subject_id) == "15",
  ,
  drop = FALSE
]
visits <- visits[order(visits$time_idx), , drop = FALSE]
assignments <- asset$assignments[
  asset$assignments$field.id == field.id,
  ,
  drop = FALSE
]
raw.basins <- asset$raw.basins[
  asset$raw.basins$field.id == field.id,
  ,
  drop = FALSE
]

fixture <- fixture[
  order(-fixture$primary_support_mass, fixture$extremum_vertex),
  ,
  drop = FALSE
]
fixture$basin_label <- paste0("M", seq_len(nrow(fixture)))
label.by.basin.id <- stats::setNames(
  fixture$basin_label,
  fixture$trajectory_basin_id
)
label.by.peak.vertex <- stats::setNames(
  fixture$basin_label,
  as.character(fixture$extremum_vertex)
)
fixture$parent_basin_label <- unname(
  label.by.basin.id[fixture$parent_canonical_branch_id]
)

if (
  nrow(visits) != 70L ||
  anyDuplicated(visits$vertex) ||
  anyDuplicated(visits$time_idx) ||
  !identical(as.integer(visits$time_idx), sort(as.integer(visits$time_idx))) ||
  nrow(assignments) != 6529L ||
  nrow(raw.basins) != 352L ||
  anyDuplicated(assignments$point.id) ||
  anyDuplicated(raw.basins$raw.basin.id) ||
  any(assignments$assignment.status != "assigned") ||
  nrow(fixture) != 352L ||
  anyDuplicated(fixture$extremum_vertex)
) {
  stop("Subject 15 visit, assignment, or maximum-basin grain validation failed.")
}

peak.by.raw.basin <- stats::setNames(
  as.character(raw.basins$peak.vertex.id),
  raw.basins$raw.basin.id
)
assignment.index <- match(
  as.character(visits$vertex),
  assignments$point.id
)
if (anyNA(assignment.index)) {
  stop("At least one Subject 15 visit is absent from the field assignment table.")
}

visits$internal_raw_basin_id <- assignments$raw.basin.id[assignment.index]
visits$root_vertex <- unname(
  peak.by.raw.basin[visits$internal_raw_basin_id]
)
visits$basin_label <- unname(
  label.by.peak.vertex[visits$root_vertex]
)
if (anyNA(visits$root_vertex) || anyNA(visits$basin_label)) {
  stop("The archived raw-basin IDs could not be mapped to current basin labels.")
}

# Midpoint exposure weights approximate the time represented by each sampled
# state on the continuous observed interval [first time, last time]. They are a
# descriptive sensitivity measure, not an equilibrium or EOT estimator.
observation.time <- as.numeric(visits$time_idx)
midpoint.weight <- numeric(length(observation.time))
midpoint.weight[[1L]] <- (
  observation.time[[2L]] - observation.time[[1L]]
) / 2
midpoint.weight[[length(observation.time)]] <- (
  observation.time[[length(observation.time)]] -
    observation.time[[length(observation.time) - 1L]]
) / 2
midpoint.weight[2L:(length(observation.time) - 1L)] <- (
  observation.time[3L:length(observation.time)] -
    observation.time[1L:(length(observation.time) - 2L)]
) / 2
visits$midpoint_time_weight_days <- midpoint.weight

run.encoding <- rle(visits$basin_label)
run.table <- data.frame(
  basin_label = run.encoding$values,
  run_length = run.encoding$lengths,
  stringsAsFactors = FALSE
)

summarize.list <- function(values) {
  if (!length(values)) {
    return("")
  }
  paste(values, collapse = ";")
}

profile.values <- function(values) {
  if (!length(values)) {
    return("")
  }
  counts <- sort(table(values), decreasing = TRUE)
  paste0(names(counts), ":", as.integer(counts), collapse = ";")
}

labels <- fixture$basin_label
visit.count <- as.integer(table(factor(visits$basin_label, levels = labels)))
time.weight <- tapply(
  visits$midpoint_time_weight_days,
  factor(visits$basin_label, levels = labels),
  sum
)
time.weight[is.na(time.weight)] <- 0

run.count <- vapply(
  labels,
  function(label) sum(run.table$basin_label == label),
  integer(1)
)
max.run.length <- vapply(
  labels,
  function(label) {
    value <- run.table$run_length[run.table$basin_label == label]
    if (length(value)) max(value) else 0L
  },
  integer(1)
)
visit.vertices <- vapply(
  labels,
  function(label) summarize.list(visits$vertex[visits$basin_label == label]),
  character(1)
)
visit.times <- vapply(
  labels,
  function(label) summarize.list(visits$time_idx[visits$basin_label == label]),
  character(1)
)
state.profile <- vapply(
  labels,
  function(label) profile.values(visits$state[visits$basin_label == label]),
  character(1)
)
cst.profile <- vapply(
  labels,
  function(label) profile.values(visits$cst_norm[visits$basin_label == label]),
  character(1)
)

positive.rank <- function(value) {
  result <- rep(NA_integer_, length(value))
  positive <- value > 0
  result[positive] <- as.integer(
    rank(-value[positive], ties.method = "min")
  )
  result
}

support.class <- cut(
  visit.count,
  breaks = c(-Inf, 0, 1, 4, Inf),
  labels = c("no direct visit", "one direct visit", "2-4 direct visits", "5+ direct visits"),
  right = TRUE
)

comparison <- data.frame(
  basin_label = labels,
  extremum_vertex = as.integer(fixture$extremum_vertex),
  parent_basin_label = fixture$parent_basin_label,
  component_survivor = as.logical(fixture$is_component_survivor),
  density_mass_rank = seq_len(nrow(fixture)),
  density_mass = as.numeric(fixture$primary_support_mass),
  graph_support_vertices = as.integer(fixture$primary_support_size),
  graph_support_rank = as.integer(
    rank(-fixture$primary_support_size, ties.method = "min")
  ),
  peak_value = as.numeric(fixture$peak_value),
  peak_value_rank = as.integer(
    rank(-fixture$peak_value, ties.method = "min")
  ),
  prominence = as.numeric(fixture$canonical_prominence),
  prominence_rank = as.integer(
    rank(-fixture$canonical_prominence, ties.method = "min")
  ),
  merge_level = pmax(
    0,
    as.numeric(fixture$peak_value) -
      as.numeric(fixture$canonical_prominence)
  ),
  subject_visit_count = visit.count,
  subject_visit_rank = positive.rank(visit.count),
  subject_visit_share = visit.count / nrow(visits),
  observed_run_count = run.count,
  max_observed_run_length = max.run.length,
  observed_return_count = pmax(run.count - 1L, 0L),
  midpoint_time_weight_days = as.numeric(time.weight),
  midpoint_time_weight_rank = positive.rank(as.numeric(time.weight)),
  midpoint_time_weight_share = as.numeric(time.weight) / sum(midpoint.weight),
  density_mass_minus_visit_share =
    as.numeric(fixture$primary_support_mass) - visit.count / nrow(visits),
  density_mass_minus_midpoint_time_share =
    as.numeric(fixture$primary_support_mass) -
      as.numeric(time.weight) / sum(midpoint.weight),
  empirical_visit_support_class = as.character(support.class),
  has_direct_subject_visit = visit.count > 0L,
  singleton_graph_basin = fixture$primary_support_size == 1L,
  illustrative_eligible_min_2_visits = visit.count >= 2L,
  illustrative_eligible_min_3_visits = visit.count >= 3L,
  illustrative_eligible_min_5_visits = visit.count >= 5L,
  direct_visit_vertices = visit.vertices,
  direct_visit_time_indices = visit.times,
  observed_state_profile = state.profile,
  observed_cst_profile = cst.profile,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

visits.output <- visits[
  ,
  c(
    "time_idx", "week", "day", "vertex", "sample_id", "state", "cst_norm",
    "basin_label", "root_vertex", "midpoint_time_weight_days",
    "internal_raw_basin_id"
  ),
  drop = FALSE
]

m17.vertex <- 1589L
m17.neighbors <- as.integer(graph$adj_list[[m17.vertex]])
m17.edge.lengths <- as.numeric(graph$weight_list[[m17.vertex]])
maximum.assignment <- assignments[
  match(as.character(seq_len(6529L)), assignments$point.id),
  ,
  drop = FALSE
]
selected.next <- suppressWarnings(as.integer(maximum.assignment$next.vertex.id))
selected.root <- suppressWarnings(as.integer(maximum.assignment$root.vertex.id))
selected.edge.length <- function(source, target) {
  if (is.na(target)) {
    return(NA_real_)
  }
  neighbor.index <- match(target, graph$adj_list[[source]])
  if (is.na(neighbor.index)) {
    stop("Selected next vertex is not adjacent to its source.")
  }
  as.numeric(graph$weight_list[[source]][[neighbor.index]])
}
m17.local.flow <- data.frame(
  source_vertex = m17.neighbors,
  source_field = as.numeric(
    asset$probability.mass[m17.neighbors, 4L]
  ),
  selected_next_vertex = selected.next[m17.neighbors],
  selected_edge_length = vapply(
    m17.neighbors,
    function(vertex) {
      selected.edge.length(vertex, selected.next[[vertex]])
    },
    numeric(1)
  ),
  edge_length_to_m17 = m17.edge.lengths,
  selected_root_vertex = selected.root[m17.neighbors],
  selected_root_basin = unname(
    label.by.peak.vertex[as.character(selected.root[m17.neighbors])]
  ),
  flows_to_m17 = selected.root[m17.neighbors] == m17.vertex,
  stringsAsFactors = FALSE
)

thresholds <- c(1L, 2L, 3L, 5L, 10L)
sensitivity <- data.frame(
  minimum_direct_visits = thresholds,
  eligible_basin_count = vapply(
    thresholds,
    function(threshold) sum(comparison$subject_visit_count >= threshold),
    integer(1)
  ),
  visits_covered = vapply(
    thresholds,
    function(threshold) {
      sum(comparison$subject_visit_count[
        comparison$subject_visit_count >= threshold
      ])
    },
    integer(1)
  ),
  visit_share_covered = vapply(
    thresholds,
    function(threshold) {
      sum(comparison$subject_visit_share[
        comparison$subject_visit_count >= threshold
      ])
    },
    numeric(1)
  ),
  midpoint_days_covered = vapply(
    thresholds,
    function(threshold) {
      sum(comparison$midpoint_time_weight_days[
        comparison$subject_visit_count >= threshold
      ])
    },
    numeric(1)
  ),
  midpoint_time_share_covered = vapply(
    thresholds,
    function(threshold) {
      sum(comparison$midpoint_time_weight_share[
        comparison$subject_visit_count >= threshold
      ])
    },
    numeric(1)
  ),
  density_mass_covered = vapply(
    thresholds,
    function(threshold) {
      sum(comparison$density_mass[
        comparison$subject_visit_count >= threshold
      ])
    },
    numeric(1)
  ),
  stringsAsFactors = FALSE
)

if (
  sum(comparison$graph_support_vertices) != 6529L ||
  abs(sum(comparison$density_mass) - 1) > 1e-10 ||
  sum(comparison$subject_visit_count) != 70L ||
  abs(sum(comparison$midpoint_time_weight_days) - 71) > 1e-12 ||
  !identical(
    comparison$basin_label[comparison$has_direct_subject_visit],
    paste0("M", seq_len(17L))
  )
) {
  stop("Subject 15 comparison-table conservation or alignment checks failed.")
}

utils::write.csv(
  comparison,
  file.path(output.dir, "subject15_maximum_basin_eot_comparison.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  visits.output,
  file.path(output.dir, "subject15_visit_basin_assignments.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  sensitivity,
  file.path(output.dir, "subject15_eot_visit_threshold_sensitivity.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  m17.local.flow,
  file.path(output.dir, "subject15_m17_local_flow.csv"),
  row.names = FALSE,
  na = ""
)

summary <- list(
  generated_at = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S %Z",
    tz = "America/New_York"
  ),
  field_id = field.id,
  eta = asset$selected$eta,
  subject_visit_count = nrow(visits),
  observed_time_span_days = sum(midpoint.weight),
  maximum_basin_count = nrow(comparison),
  directly_visited_basin_count = sum(comparison$has_direct_subject_visit),
  directly_visited_labels = comparison$basin_label[
    comparison$has_direct_subject_visit
  ],
  top_17_density_mass = sum(comparison$density_mass[seq_len(17L)]),
  density_mass_visit_share_total_variation = 0.5 * sum(abs(
    comparison$density_mass - comparison$subject_visit_share
  )),
  density_mass_midpoint_share_total_variation = 0.5 * sum(abs(
    comparison$density_mass - comparison$midpoint_time_weight_share
  )),
  spearman_mass_vs_visit_count = unname(stats::cor(
    comparison$density_mass[seq_len(17L)],
    comparison$subject_visit_count[seq_len(17L)],
    method = "spearman"
  )),
  spearman_graph_support_vs_visit_count = unname(stats::cor(
    comparison$graph_support_vertices[seq_len(17L)],
    comparison$subject_visit_count[seq_len(17L)],
    method = "spearman"
  )),
  m17 = as.list(comparison[
    comparison$basin_label == "M17",
    c(
      "extremum_vertex", "density_mass", "graph_support_vertices",
      "subject_visit_count", "midpoint_time_weight_days",
      "direct_visit_time_indices"
    ),
    drop = FALSE
  ]),
  source_files = list(
    basin_fixture = fixture.path,
    basin_fixture_provenance = provenance.path,
    scale_path_asset = asset.path,
    visit_metadata = visits.path,
    graph_asset = graph.path
  ),
  fixture_provenance = as.list(fixture.provenance[1L, , drop = FALSE])
)
saveRDS(
  summary,
  file.path(output.dir, "subject15_eot_comparison_summary.rds"),
  compress = "xz"
)

cat(sprintf(
  paste0(
    "Wrote %d maximum basins and %d visit assignments. ",
    "%d basins have direct Subject 15 visits; midpoint exposure spans %.1f days.\n"
  ),
  nrow(comparison),
  nrow(visits.output),
  sum(comparison$has_direct_subject_visit),
  sum(comparison$midpoint_time_weight_days)
))
