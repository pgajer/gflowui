#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
zip.path <- if (length(args) >= 1L) args[[1L]] else
  file.path(
    Sys.getenv("HOME"), "current_projects",
    "vaginal_community_trajectory_types", "data",
    paste0(
      "gflowui_basin_hmp-subject15-k03-heat-basin-path_",
      "eod-subject-15-graph-heat-kernel-time-index-4_",
      "20260730_041807_022ec9d11e.zip"
    )
  )
topology.path <- if (length(args) >= 2L) args[[2L]] else
  file.path(
    Sys.getenv("HOME"), "current_projects",
    "vaginal_community_trajectory_types", "analysis_output",
    "hmp_subject15_basin_topology_stability_20260730", "objects",
    "topology_stability.rds"
  )

script.arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script.path <- normalizePath(
  sub("^--file=", "", script.arg[[1L]]),
  mustWork = TRUE
)
repo.root <- normalizePath(file.path(dirname(script.path), "..", ".."))
fixture.dir <- file.path(repo.root, "tests", "testthat", "fixtures")
dir.create(fixture.dir, recursive = TRUE, showWarnings = FALSE)

expected <- c(
  zip = "15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275",
  topology = "afb7863d761932e31f4f1816f95b496db16fc58028663f26cb036ec6aa1af000"
)
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("The digest package is required to verify source assets.")
}
observed <- c(
  zip = digest::digest(
    file = zip.path, algo = "sha256", serialize = FALSE
  ),
  topology = digest::digest(
    file = topology.path, algo = "sha256", serialize = FALSE
  )
)
if (!identical(observed, expected)) {
  stop("Source digest mismatch; refusing to regenerate the pinned fixture.")
}

characteristics <- utils::read.csv(
  unz(zip.path, "basin_characteristics.csv"),
  stringsAsFactors = FALSE
)
mapping <- utils::read.csv(
  unz(zip.path, "basin_internal_mapping.csv"),
  stringsAsFactors = FALSE
)
analysis <- readRDS(topology.path)
modes <- analysis$merge.tree.modes

maximum <- characteristics[
  characteristics$extremum_type == "maximum", , drop = FALSE
]
maximum.mapping <- mapping[
  mapping$extremum_basin %in% maximum$extremum_basin, , drop = FALSE
]
maximum.mapping <- maximum.mapping[
  match(maximum$extremum_basin, maximum.mapping$extremum_basin),
  ,
  drop = FALSE
]
mode.index <- match(maximum$extremum_vertex, modes$peak.vertex)

if (nrow(maximum) != 352L ||
    anyDuplicated(maximum$extremum_vertex) ||
    anyDuplicated(maximum.mapping$extremum_vertex) ||
    anyNA(mode.index) ||
    !identical(maximum$extremum_vertex, maximum.mapping$extremum_vertex)) {
  stop("The trajectory-flow to canonical-branch mapping is not one-to-one.")
}

canonical.id <- function(vertex) {
  ifelse(
    is.na(vertex),
    NA_character_,
    sprintf("basin_max_v%08d", as.integer(vertex))
  )
}

fixture <- data.frame(
  direction = "max",
  component = 1L,
  trajectory_basin_id = maximum.mapping$basin_id,
  canonical_branch_id = canonical.id(maximum$extremum_vertex),
  extremum_vertex = maximum$extremum_vertex,
  parent_canonical_branch_id = canonical.id(modes$parent.peak[mode.index]),
  is_component_survivor = modes$is.global[mode.index],
  primary_support_mass = maximum$mass,
  primary_support_size = maximum$support,
  peak_value = maximum$extremum_value,
  canonical_prominence = modes$persistence[mode.index],
  stringsAsFactors = FALSE
)
fixture <- fixture[order(fixture$canonical_branch_id), , drop = FALSE]

if (!identical(fixture$trajectory_basin_id, fixture$canonical_branch_id) ||
    sum(fixture$is_component_survivor) != 1L ||
    any(!fixture$parent_canonical_branch_id[
      !fixture$is_component_survivor
    ] %in% fixture$canonical_branch_id) ||
    max(abs(
      maximum$prominence -
        modes$persistence[match(maximum$extremum_vertex, modes$peak.vertex)]
    )) > 1e-14) {
  stop("Fixture identity, ancestry, or prominence validation failed.")
}

provenance <- data.frame(
  fixture_schema = "gflowui_basin_merge_tree_adaptive_fixture/1",
  upstream_repository_commit =
    "4615555547f3f406e79436c308d28fd78985b64e",
  gflowui_baseline_commit =
    "925ed84bb6d4ab70efc0b7ebf5bc21979ee0c670",
  gflow_baseline_commit =
    "24a671c4927df6ab6e5ac10361aecfd87cfaa0cb",
  source_zip_sha256 = unname(observed[["zip"]]),
  topology_rds_sha256 = unname(observed[["topology"]]),
  trajectory_construction_fingerprint =
    "022ec9d11e0b21926623e24e5ac324ad02df62fa689defaead29e5f895deea18",
  canonical_analysis_fingerprint =
    "d383158eb5dce7d360f32f9b7b2c4de54143136928c581945eee76cadf4056af",
  graph_fingerprint =
    "c0872d45a94b66aeb58689f49c1e59e61ad0dfd82de984c4b30c453cbab5d052",
  field_fingerprint =
    "32b46729ef18eb0b3f6f38a6fde1d67e914788d2614e4ac3bd84c669c8dc0ee5",
  proposal_mass_measure = "trajectory_flow.primary.support.mass",
  support_measure = "trajectory_flow.primary.support.size",
  peak_measure = "selected_field.value_at_extremum",
  prominence_measure = "superlevel_merge_tree.persistence",
  derivation_script =
    "dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R",
  stringsAsFactors = FALSE
)

fixture.mass.sum <- sum(
  sort(fixture$primary_support_mass, decreasing = TRUE)
)
numeric.columns <- c(
  "primary_support_mass", "peak_value", "canonical_prominence"
)
fixture[numeric.columns] <- lapply(
  fixture[numeric.columns],
  function(value) sprintf("%.17g", value)
)
utils::write.csv(
  fixture,
  file.path(
    fixture.dir, "basin_merge_tree_subject15_maxima.csv"
  ),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  provenance,
  file.path(
    fixture.dir, "basin_merge_tree_subject15_maxima_provenance.csv"
  ),
  row.names = FALSE
)

cat(sprintf(
  "Wrote %d mapped maximum branches; mass sum %.17g.\n",
  nrow(fixture), fixture.mass.sum
))
