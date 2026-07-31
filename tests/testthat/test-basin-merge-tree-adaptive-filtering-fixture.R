reference_subject15_auto_proposal <- function(
    fixture,
    coverage_target = 0.99,
    strong_gap_decades = 3,
    minimum_core_branches = 3L,
    core_branch_budget = 50L,
    final_render_budget = 80L,
    sentinel_top_n = 10L) {
  positive <- fixture$primary_support_mass > 0
  ranked <- fixture[positive, , drop = FALSE]
  ranked <- ranked[
    order(
      -ranked$primary_support_mass,
      ranked$canonical_branch_id,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]

  group_number <- cumsum(c(
    TRUE,
    ranked$primary_support_mass[-1L] !=
      ranked$primary_support_mass[-nrow(ranked)]
  ))
  group_rows <- split(seq_len(nrow(ranked)), group_number)
  group_endpoints <- unname(vapply(group_rows, max, integer(1)))
  group_masses <- unname(vapply(
    group_rows,
    function(rows) ranked$primary_support_mass[[rows[[1L]]]],
    numeric(1)
  ))
  tie_groups <- lapply(
    group_rows,
    function(rows) ranked$canonical_branch_id[rows]
  )

  denominator <- sum(ranked$primary_support_mass)
  coverage <- vapply(
    group_endpoints,
    function(endpoint) {
      sum(ranked$primary_support_mass[seq_len(endpoint)]) / denominator
    },
    numeric(1)
  )
  j_coverage <- group_endpoints[which(coverage >= coverage_target)[[1L]]]
  minimum_count <- min(minimum_core_branches, nrow(ranked))
  j_minimum <- group_endpoints[
    which(group_endpoints >= minimum_count)[[1L]]
  ]
  j_required <- max(j_coverage, j_minimum)

  eligible_boundaries <- group_endpoints[
    group_endpoints >= j_required &
      group_endpoints <= core_branch_budget &
      group_endpoints < nrow(ranked)
  ]
  eligible_gaps <- vapply(
    eligible_boundaries,
    function(endpoint) {
      log10(ranked$primary_support_mass[[endpoint]]) -
        log10(ranked$primary_support_mass[[endpoint + 1L]])
    },
    numeric(1)
  )
  qualifying <- eligible_boundaries[
    eligible_gaps >= strong_gap_decades
  ]
  if (!length(qualifying)) {
    stop("Subject 15 reference fixture did not produce a strong gap.")
  }
  boundary <- qualifying[[1L]]
  core_ids <- ranked$canonical_branch_id[seq_len(boundary)]

  top_n_with_ties <- function(value, id, n) {
    if (n == 0L || !length(value)) return(character())
    ranked_index <- order(-value, id, method = "radix")
    cutoff_index <- min(n, length(ranked_index))
    cutoff <- value[ranked_index[[cutoff_index]]]
    sort(id[value >= cutoff])
  }
  sentinel_ids <- unique(c(
    fixture$canonical_branch_id[fixture$is_component_survivor],
    top_n_with_ties(
      fixture$peak_value,
      fixture$canonical_branch_id,
      sentinel_top_n
    ),
    top_n_with_ties(
      fixture$canonical_prominence,
      fixture$canonical_branch_id,
      sentinel_top_n
    ),
    top_n_with_ties(
      fixture$primary_support_size,
      fixture$canonical_branch_id,
      sentinel_top_n
    )
  ))
  preclosure_ids <- unique(c(core_ids, sentinel_ids))
  parent <- setNames(
    fixture$parent_canonical_branch_id,
    fixture$canonical_branch_id
  )
  final_ids <- preclosure_ids
  repeat {
    ancestor <- unname(parent[final_ids])
    expanded <- unique(c(final_ids, ancestor[!is.na(ancestor)]))
    if (length(expanded) == length(final_ids)) break
    final_ids <- expanded
  }

  list(
    status = "strong_gap",
    final_status = if (length(core_ids) > final_render_budget) {
      "core_overflow"
    } else {
      "renderable"
    },
    tie_groups = tie_groups,
    group_masses = group_masses,
    denominator = denominator,
    j_coverage = j_coverage,
    j_minimum = j_minimum,
    eligible_boundaries = eligible_boundaries,
    eligible_gaps = eligible_gaps,
    boundary = boundary,
    core_ids = core_ids,
    sentinel_only_ids = setdiff(sentinel_ids, core_ids),
    ancestor_only_ids = setdiff(final_ids, preclosure_ids),
    final_ids = sort(final_ids),
    coverage = sum(
      ranked$primary_support_mass[seq_len(boundary)]
    ) / denominator
  )
}

test_that("Subject 15 adaptive-filtering fixture is complete and pinned", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  provenance <- utils::read.csv(
    test_path(
      "fixtures",
      "basin_merge_tree_subject15_maxima_provenance.csv"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(fixture), 352L)
  expect_true(all(fixture$direction == "max"))
  expect_true(all(fixture$component == 1L))
  expect_identical(anyDuplicated(fixture$trajectory_basin_id), 0L)
  expect_identical(anyDuplicated(fixture$canonical_branch_id), 0L)
  expect_identical(
    fixture$trajectory_basin_id,
    fixture$canonical_branch_id
  )
  expect_equal(sum(fixture$is_component_survivor), 1L)
  expect_true(all(
    fixture$parent_canonical_branch_id[
      !fixture$is_component_survivor
    ] %in% fixture$canonical_branch_id
  ))
  expect_equal(
    provenance$fixture_schema,
    "gflowui_basin_merge_tree_adaptive_fixture/1"
  )
  expect_equal(
    provenance$source_zip_sha256,
    "15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275"
  )
})

test_that("Subject 15 fixture preserves the raw rank-17 evidence", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  mass <- sort(fixture$primary_support_mass, decreasing = TRUE)
  gap <- log10(mass[-length(mass)]) - log10(mass[-1L])

  expect_equal(sum(mass), 1.0000000000000087, tolerance = 3e-16)
  expect_equal(mass[[17L]], 0.0122134243817115)
  expect_equal(mass[[18L]], 1.40305377913392e-15)
  expect_equal(which.max(gap), 17L)
  expect_equal(gap[[17L]], 12.9397631299771, tolerance = 1e-12)
  expect_equal(sum(mass[seq_len(17L)]), 0.99999999999992595)
  expect_equal(
    sum(mass[seq_len(17L)]) / sum(mass),
    0.99999999999991729
  )
})

test_that("Subject 15 fixture reproduces the revision-3 bounded proposal", {
  fixture <- utils::read.csv(
    test_path("fixtures", "basin_merge_tree_subject15_maxima.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  proposal <- reference_subject15_auto_proposal(fixture)
  expected_ids <- sort(c(
    "basin_max_v00001598",
    "basin_max_v00001628",
    "basin_max_v00001635",
    "basin_max_v00001575",
    "basin_max_v00001641",
    "basin_max_v00001578",
    "basin_max_v00001609",
    "basin_max_v00001603",
    "basin_max_v00001622",
    "basin_max_v00001590",
    "basin_max_v00001614",
    "basin_max_v00001621",
    "basin_max_v00001638",
    "basin_max_v00001574",
    "basin_max_v00001618",
    "basin_max_v00001640",
    "basin_max_v00001589"
  ))

  expect_identical(proposal$status, "strong_gap")
  expect_identical(proposal$final_status, "renderable")
  expect_equal(length(proposal$tie_groups), 352L)
  expect_true(all(lengths(proposal$tie_groups) == 1L))
  expect_true(all(diff(proposal$group_masses) < 0))
  expect_equal(proposal$denominator, 1.0000000000000087,
               tolerance = 3e-16)
  expect_identical(proposal$j_coverage, 17L)
  expect_identical(proposal$j_minimum, 3L)
  expect_identical(proposal$eligible_boundaries, 17:50)
  expect_equal(
    proposal$eligible_gaps[[1L]],
    12.9397631299771,
    tolerance = 1e-12
  )
  expect_identical(proposal$boundary, 17L)
  expect_setequal(proposal$core_ids, expected_ids)
  expect_identical(proposal$sentinel_only_ids, character())
  expect_identical(proposal$ancestor_only_ids, character())
  expect_identical(proposal$final_ids, expected_ids)
  expect_equal(proposal$coverage, 0.99999999999991729)
})
