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
    core_outcome = "strong_gap",
    core_warnings = character(),
    render_outcome = if (length(core_ids) > final_render_budget) {
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

reference_manual_core <- function(
    mass,
    id,
    mode = c("minimum_mass", "top_k", "complete"),
    minimum_mass = NULL,
    top_k = NULL) {
  mode <- match.arg(mode)
  stopifnot(
    length(mass) == length(id),
    all(is.finite(mass)),
    all(mass >= 0)
  )
  ranked <- order(-mass, id, method = "radix")
  ranked_mass <- mass[ranked]
  ranked_id <- id[ranked]
  group_number <- cumsum(c(
    TRUE,
    ranked_mass[-1L] != ranked_mass[-length(ranked_mass)]
  ))
  group_rows <- split(seq_along(ranked_mass), group_number)
  endpoints <- unname(vapply(group_rows, max, integer(1)))

  if (mode == "complete") {
    return(list(
      core_outcome = "complete",
      core_warnings = character(),
      core_ids = sort(id)
    ))
  }
  if (mode == "minimum_mass") {
    stopifnot(
      length(minimum_mass) == 1L,
      is.finite(minimum_mass),
      minimum_mass >= 0
    )
    keep <- which(ranked_mass >= minimum_mass)
    return(list(
      core_outcome = if (length(keep)) {
        "minimum_mass"
      } else {
        "threshold_empty"
      },
      core_warnings = character(),
      core_ids = sort(ranked_id[keep])
    ))
  }

  stopifnot(
    length(top_k) == 1L,
    top_k == as.integer(top_k),
    top_k >= 1L,
    top_k <= length(ranked_id)
  )
  endpoint <- endpoints[which(endpoints >= top_k)[[1L]]]
  list(
    core_outcome = "top_k",
    core_warnings = if (endpoint > top_k) {
      "tie_overflow"
    } else {
      character()
    },
    core_ids = sort(ranked_id[seq_len(endpoint)])
  )
}

reference_proposal_state <- function(
    mode = c("auto", "cumulative", "minimum_mass", "top_k", "complete"),
    identity = "current",
    source = "valid",
    mapping = "valid",
    mass = "valid",
    settings = "valid") {
  mode <- match.arg(mode)
  ordinary_outcome <- c(
    auto = "strong_gap",
    cumulative = "coverage",
    minimum_mass = "minimum_mass",
    top_k = "top_k",
    complete = "complete"
  )[[mode]]
  result <- function(core_outcome, render_outcome) {
    list(
      identity_validation = identity,
      source_validation = source,
      mapping_validation = mapping,
      mass_validation = mass,
      settings_validation = settings,
      core_outcome = core_outcome,
      render_outcome = render_outcome
    )
  }
  if (identity == "stale") {
    return(result(NULL, "stale"))
  }
  if (source != "valid" || mapping != "valid" || settings != "valid") {
    return(result(NULL, "unavailable"))
  }
  if (mode != "complete" && mass != "valid") {
    return(result(NULL, "unavailable"))
  }
  result(ordinary_outcome, "renderable")
}

reference_activate_mode <- function(
    state,
    mode = c("auto", "cumulative", "minimum_mass", "top_k", "complete"),
    component_branch_count) {
  mode <- match.arg(mode)
  stopifnot(
    length(component_branch_count) == 1L,
    component_branch_count >= 1L
  )
  if (is.null(state$values)) {
    state$values <- list()
  }
  if (is.null(state$activated)) {
    state$activated <- character()
  }

  if (mode == "top_k" && !"top_k" %in% state$activated) {
    state$values$top_k <- min(10L, component_branch_count)
    state$activated <- union(state$activated, "top_k")
  }
  if (mode == "minimum_mass" && !"minimum_mass" %in% state$activated) {
    state$values$minimum_mass <- 0
    state$activated <- union(state$activated, "minimum_mass")
  }

  valid <- TRUE
  if (mode == "top_k") {
    value <- state$values$top_k
    valid <- length(value) == 1L &&
      is.numeric(value) &&
      !is.na(value) &&
      is.finite(value) &&
      value == as.integer(value) &&
      value >= 1L &&
      value <= component_branch_count
  }
  if (mode == "minimum_mass") {
    value <- state$values$minimum_mass
    valid <- length(value) == 1L &&
      is.numeric(value) &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 0
  }

  list(
    state = state,
    settings_validation = if (valid) "valid" else "settings_invalid"
  )
}

reference_ranking_gate <- function(
    mass,
    support,
    peak,
    prominence,
    mode = c("auto", "complete")) {
  mode <- match.arg(mode)
  n <- length(mass)
  valid_finite <- function(x) {
    is.numeric(x) && length(x) == n && all(is.finite(x))
  }
  mass_status <- if (!valid_finite(mass) || any(mass < 0)) {
    "mass_invalid"
  } else if (!any(mass > 0)) {
    "mass_unavailable"
  } else {
    "valid"
  }
  support_status <- if (
    !valid_finite(support) ||
      any(support < 0) ||
      any(support != floor(support))
  ) {
    "support_invalid"
  } else {
    "valid"
  }
  peak_status <- if (!valid_finite(peak)) {
    "peak_invalid"
  } else {
    "valid"
  }
  prominence_status <- if (
    !valid_finite(prominence) ||
      any(prominence < 0)
  ) {
    "prominence_invalid"
  } else {
    "valid"
  }
  validation <- list(
    trajectory_flow_mass = mass_status,
    trajectory_flow_support = support_status,
    source_peak = peak_status,
    canonical_prominence = prominence_status
  )
  nonmass_blocked <- any(c(
    support_status != "valid",
    peak_status != "valid",
    prominence_status != "valid"
  ))
  mass_blocked <- mode != "complete" && mass_status != "valid"
  blocked <- nonmass_blocked || mass_blocked
  ids <- paste0("b", seq_len(n))

  list(
    source_validation = if (peak_status == "valid") {
      "valid"
    } else {
      "source_invalid"
    },
    ranking_measure_validation = validation,
    core_outcome = if (blocked) {
      NULL
    } else if (mode == "complete") {
      "complete"
    } else {
      "strong_gap"
    },
    sentinel_ids = if (blocked) character() else ids,
    label_ids = if (blocked) character() else ids,
    final_ids = if (blocked) character() else ids,
    render_outcome = if (blocked) "unavailable" else "renderable",
    mass_views_available = mass_status == "valid"
  )
}

reference_canonical_text <- function(x) {
  scalar_text <- function(value, prefix) {
    value <- enc2utf8(as.character(value))
    paste0(prefix, nchar(value, type = "bytes"), ":", value, ";")
  }
  if (is.null(x)) {
    return("N;")
  }
  if (is.list(x)) {
    if (!is.null(names(x))) {
      ordered_names <- sort(enc2utf8(names(x)), method = "radix")
      parts <- vapply(
        ordered_names,
        function(name) {
          paste0(
            scalar_text(name, "K"),
            reference_canonical_text(x[[name]])
          )
        },
        character(1)
      )
      return(paste0(
        "M",
        length(parts),
        "{",
        paste0(parts, collapse = ""),
        "}"
      ))
    }
    parts <- vapply(x, reference_canonical_text, character(1))
    return(paste0(
      "L",
      length(parts),
      "[",
      paste0(parts, collapse = ""),
      "]"
    ))
  }
  if (is.character(x)) {
    parts <- vapply(x, scalar_text, character(1), prefix = "S")
    return(paste0("C", length(parts), "[", paste0(parts, collapse = ""), "]"))
  }
  if (is.logical(x)) {
    parts <- ifelse(
      is.na(x),
      "B:NA;",
      ifelse(x, "B:1;", "B:0;")
    )
    return(paste0("B", length(parts), "[", paste0(parts, collapse = ""), "]"))
  }
  if (is.integer(x)) {
    parts <- ifelse(is.na(x), "I:NA;", paste0("I:", x, ";"))
    return(paste0("I", length(parts), "[", paste0(parts, collapse = ""), "]"))
  }
  if (is.numeric(x)) {
    encode_double <- function(value) {
      if (is.na(value) && !is.nan(value)) return("D:NA;")
      if (is.nan(value)) return("D:NAN;")
      if (is.infinite(value)) {
        return(if (value > 0) "D:INF;" else "D:-INF;")
      }
      if (value == 0) value <- 0
      paste0("D:", sprintf("%a", value), ";")
    }
    parts <- vapply(x, encode_double, character(1))
    return(paste0("D", length(parts), "[", paste0(parts, collapse = ""), "]"))
  }
  stop("Unsupported reference fingerprint type.")
}

reference_sha256 <- function(x) {
  digest::digest(
    reference_canonical_text(x),
    algo = "sha256",
    serialize = FALSE
  )
}

reference_context_fingerprint <- function(context) {
  reference_sha256(list(
    schema = "gflowui_basin_merge_tree_context/1",
    context = context
  ))
}

reference_attempt_fingerprint <- function(context_fingerprint, input_values) {
  reference_sha256(list(
    schema = "gflowui_basin_merge_tree_active_attempt/1",
    context_fingerprint = context_fingerprint,
    input_values = input_values
  ))
}

reference_proposal_fingerprint <- function(proposal) {
  content <- proposal
  content$proposal_fingerprint <- NULL
  content$creation_time <- NULL
  reference_sha256(list(
    schema = "gflowui_basin_merge_tree_display_proposal_content/1",
    proposal = content
  ))
}

reference_mass_derived <- function(
    mass_state = c("valid", "mass_unavailable", "mass_invalid"),
    branch_ids,
    mass = NULL) {
  mass_state <- match.arg(mass_state)
  branch_ids <- sort(branch_ids)
  if (mass_state == "mass_invalid") {
    return(list(
      available = FALSE,
      unavailable_reason = "mass_invalid",
      positive_groups = NULL,
      all_mass_groups = NULL,
      denominator = NULL,
      positive_count = NULL,
      zero_count = NULL,
      core_coverage = NULL,
      final_coverage = NULL
    ))
  }
  if (mass_state == "mass_unavailable") {
    return(list(
      available = FALSE,
      unavailable_reason = "mass_unavailable",
      positive_groups = list(),
      all_mass_groups = list(list(mass = 0, ids = branch_ids)),
      denominator = 0,
      positive_count = 0L,
      zero_count = length(branch_ids),
      core_coverage = NULL,
      final_coverage = NULL
    ))
  }

  stopifnot(
    length(mass) == length(branch_ids),
    all(is.finite(mass)),
    all(mass >= 0),
    any(mass > 0)
  )
  names(mass) <- branch_ids
  grouped <- split(branch_ids, mass[branch_ids])
  group_values <- as.numeric(names(grouped))
  group_order <- order(-group_values)
  all_groups <- lapply(
    group_order,
    function(index) {
      list(
        mass = group_values[[index]],
        ids = sort(grouped[[index]])
      )
    }
  )
  positive_groups <- Filter(
    function(group) group$mass > 0,
    all_groups
  )
  denominator <- sum(mass[mass > 0])
  list(
    available = TRUE,
    unavailable_reason = NULL,
    positive_groups = positive_groups,
    all_mass_groups = all_groups,
    denominator = denominator,
    positive_count = sum(mass > 0),
    zero_count = sum(mass == 0),
    core_coverage = sum(mass) / denominator,
    final_coverage = sum(mass) / denominator
  )
}

reference_view_proposal <- function(
    context,
    input_values,
    final_ids,
    render_outcome = "renderable",
    mass_state = c("valid", "mass_unavailable", "mass_invalid"),
    mass = NULL,
    creation_time = "2026-08-01T12:00:00-04:00") {
  mass_state <- match.arg(mass_state)
  final_ids <- sort(final_ids)
  if (mass_state == "valid" && is.null(mass)) {
    mass <- rep(1 / length(final_ids), length(final_ids))
  }
  mass_derived <- reference_mass_derived(
    mass_state,
    final_ids,
    mass
  )
  label_contributions <- list(
    trajectory_flow_mass = if (mass_derived$available) {
      final_ids[[1L]]
    } else {
      character()
    },
    source_peak = final_ids[[1L]],
    canonical_prominence = final_ids[[min(2L, length(final_ids))]],
    trajectory_flow_support = final_ids[[min(3L, length(final_ids))]],
    component_survivor = final_ids[[1L]],
    selected_or_pinned = character()
  )
  proposal <- list(
    schema = "gflowui_basin_merge_tree_display_proposal/3",
    context_fingerprint = reference_context_fingerprint(context),
    proposal_fingerprint = NULL,
    creation_time = creation_time,
    context_fields = context,
    algorithm = "adaptive_initial_filtering",
    algorithm_version = 6L,
    input_values = input_values,
    validation = list(
      identity = "current",
      source = "valid",
      mapping = "valid",
      ranking_measure = list(
        trajectory_flow_mass = mass_state,
        trajectory_flow_support = "valid",
        source_peak = "valid",
        canonical_prominence = "valid"
      ),
      settings = "valid"
    ),
    mass_derived = mass_derived,
    label_contributions = label_contributions,
    label_omission_reasons = if (mass_derived$available) {
      character()
    } else {
      paste0("trajectory_flow_mass:", mass_state)
    },
    core_outcome = if (
      identical(input_values$filter_mode, "none")
    ) {
      "complete"
    } else {
      "top_k"
    },
    core_ids = final_ids,
    final_ids = final_ids,
    label_ids = sort(unique(unlist(
      label_contributions,
      use.names = FALSE
    ))),
    render_outcome = render_outcome
  )
  proposal$proposal_fingerprint <-
    reference_proposal_fingerprint(proposal)
  proposal
}

reference_validate_proposal <- function(
    proposal,
    expected_context_fingerprint = NULL) {
  valid <- identical(
    proposal$context_fingerprint,
    reference_context_fingerprint(proposal$context_fields)
  ) &&
    identical(
      proposal$proposal_fingerprint,
      reference_proposal_fingerprint(proposal)
    )
  if (
    !is.null(expected_context_fingerprint) &&
      !identical(
        proposal$context_fingerprint,
        expected_context_fingerprint
      )
  ) {
    valid <- FALSE
  }
  if (!valid) stop("fingerprint_invalid")
  TRUE
}

reference_view_transition <- function(
    state = NULL,
    context_fingerprint,
    attempt_fingerprint,
    input_values,
    validation = c(
      "valid",
      "settings_invalid",
      "source_invalid",
      "mapping_invalid",
      "mass_invalid",
      "mass_unavailable",
      "support_invalid",
      "peak_invalid",
      "prominence_invalid",
      "stale"
    ),
    proposal = NULL) {
  validation <- match.arg(validation)
  validation_record <- list(
    identity = "current",
    source = "valid",
    mapping = "valid",
    ranking_measure = list(
      trajectory_flow_mass = "valid",
      trajectory_flow_support = "valid",
      source_peak = "valid",
      canonical_prominence = "valid"
    ),
    settings = "valid"
  )
  if (validation == "stale") {
    validation_record$identity <- "stale"
  } else if (validation == "source_invalid") {
    validation_record$source <- "source_invalid"
  } else if (validation == "mapping_invalid") {
    validation_record$mapping <- "mapping_invalid"
  } else if (validation == "mass_invalid") {
    validation_record$ranking_measure$trajectory_flow_mass <-
      "mass_invalid"
  } else if (validation == "mass_unavailable") {
    validation_record$ranking_measure$trajectory_flow_mass <-
      "mass_unavailable"
  } else if (validation == "support_invalid") {
    validation_record$ranking_measure$trajectory_flow_support <-
      "support_invalid"
  } else if (validation == "peak_invalid") {
    validation_record$source <- "source_invalid"
    validation_record$ranking_measure$source_peak <- "peak_invalid"
  } else if (validation == "prominence_invalid") {
    validation_record$ranking_measure$canonical_prominence <-
      "prominence_invalid"
  } else if (validation == "settings_invalid") {
    validation_record$settings <- "settings_invalid"
  }
  same_context <- !is.null(state) &&
    identical(state$context_fingerprint, context_fingerprint)
  previous <- if (same_context) state$display_proposal else NULL
  filter_mode <- input_values$filter_mode
  mass_only_none <- identical(filter_mode, "none") &&
    validation %in% c("mass_invalid", "mass_unavailable")
  proposal_created <- validation == "valid" || mass_only_none
  active_attempt <- list(
    fingerprint = attempt_fingerprint,
    input_values = input_values,
    validation = validation_record,
    outcome = if (proposal_created) {
      "proposal_created"
    } else if (validation == "stale") {
      "stale"
    } else {
      "blocked"
    },
    render_outcome = if (proposal_created) {
      NULL
    } else if (validation == "stale") {
      "stale"
    } else {
      "unavailable"
    }
  )

  if (proposal_created) {
    stopifnot(
      !is.null(proposal),
      identical(proposal$context_fingerprint, context_fingerprint)
    )
    reference_validate_proposal(proposal, context_fingerprint)
    display_source <- "current"
    display_proposal <- proposal
  } else if (
    validation == "settings_invalid" &&
      !is.null(previous)
  ) {
    display_source <- "retained_last_valid"
    display_proposal <- previous
  } else {
    display_source <- "none"
    display_proposal <- NULL
  }

  list(
    schema = "gflowui_basin_merge_tree_view_state/1",
    context_fingerprint = context_fingerprint,
    active_attempt = active_attempt,
    display_source = display_source,
    display_proposal_fingerprint = if (is.null(display_proposal)) {
      NULL
    } else {
      display_proposal$proposal_fingerprint
    },
    display_proposal = display_proposal
  )
}

reference_validate_view_state <- function(state) {
  valid <- identical(
    state$active_attempt$fingerprint,
    reference_attempt_fingerprint(
      state$context_fingerprint,
      state$active_attempt$input_values
    )
  )
  if (!is.null(state$display_proposal)) {
    valid <- valid &&
      identical(
        state$display_proposal_fingerprint,
        state$display_proposal$proposal_fingerprint
      )
    if (valid) {
      valid <- isTRUE(reference_validate_proposal(
        state$display_proposal,
        state$context_fingerprint
      ))
    }
  } else {
    valid <- valid && is.null(state$display_proposal_fingerprint)
  }
  if (!valid) stop("fingerprint_invalid")
  TRUE
}

reference_complete_tree_action <- function(
    state,
    action = c("filter_none", "show_all", "open_complete_viewer"),
    complete_proposal) {
  action <- match.arg(action)
  if (action == "open_complete_viewer") {
    state$viewer_open <- TRUE
    return(state)
  }
  reference_validate_proposal(
    complete_proposal,
    state$context_fingerprint
  )
  state$filter_mode <- "none"
  state$recomputed <- TRUE
  state$active_attempt_fingerprint <- reference_attempt_fingerprint(
    state$context_fingerprint,
    complete_proposal$input_values
  )
  state$active_input_values <- complete_proposal$input_values
  state$active_attempt_outcome <- "proposal_created"
  state$active_attempt_render_outcome <- NULL
  state$display_source <- "current"
  state$display_proposal_fingerprint <-
    complete_proposal$proposal_fingerprint
  state$display_proposal <- complete_proposal
  state$core_outcome <- complete_proposal$core_outcome
  state
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
  expect_true(all(is.finite(fixture$peak_value)))
  expect_true(all(is.finite(fixture$primary_support_size)))
  expect_true(all(fixture$primary_support_size >= 0))
  expect_true(all(
    fixture$primary_support_size ==
      floor(fixture$primary_support_size)
  ))
  expect_true(all(is.finite(fixture$canonical_prominence)))
  expect_true(all(fixture$canonical_prominence >= 0))
  expect_true(all(
    fixture$canonical_prominence[fixture$is_component_survivor] > 0
  ))
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

test_that("Subject 15 fixture reproduces the revision-5 bounded proposal", {
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

  expect_identical(proposal$core_outcome, "strong_gap")
  expect_identical(proposal$core_warnings, character())
  expect_identical(proposal$render_outcome, "renderable")
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

test_that("manual mass modes preserve raw scale and the complete zero tie", {
  mass <- c(a = 0.6, b = 0.4, c = 0, d = 0)
  id <- names(mass)

  top_three <- reference_manual_core(
    mass, id, mode = "top_k", top_k = 3L
  )
  expect_identical(top_three$core_outcome, "top_k")
  expect_identical(top_three$core_warnings, "tie_overflow")
  expect_identical(top_three$core_ids, c("a", "b", "c", "d"))

  top_two <- reference_manual_core(
    mass, id, mode = "top_k", top_k = 2L
  )
  expect_identical(top_two$core_outcome, "top_k")
  expect_identical(top_two$core_warnings, character())
  expect_identical(top_two$core_ids, c("a", "b"))

  minimum_zero <- reference_manual_core(
    mass, id, mode = "minimum_mass", minimum_mass = 0
  )
  expect_identical(minimum_zero$core_outcome, "minimum_mass")
  expect_identical(minimum_zero$core_ids, c("a", "b", "c", "d"))

  raw_scale <- reference_manual_core(
    c(a = 0.4, b = 0.3),
    c("a", "b"),
    mode = "minimum_mass",
    minimum_mass = 0.5
  )
  expect_identical(raw_scale$core_outcome, "threshold_empty")
  expect_identical(raw_scale$core_ids, character())

  complete <- reference_manual_core(mass, id, mode = "complete")
  expect_identical(complete$core_outcome, "complete")
  expect_identical(complete$core_ids, c("a", "b", "c", "d"))
})

test_that("manual settings initialize, retain, and validate only when active", {
  state <- list()
  top_first <- reference_activate_mode(state, "top_k", 4L)
  expect_identical(top_first$settings_validation, "valid")
  expect_identical(top_first$state$values$top_k, 4L)

  top_first$state$values$top_k <- 3L
  minimum_first <- reference_activate_mode(
    top_first$state, "minimum_mass", 4L
  )
  expect_identical(minimum_first$settings_validation, "valid")
  expect_identical(minimum_first$state$values$minimum_mass, 0)
  expect_identical(minimum_first$state$values$top_k, 3L)

  minimum_first$state$values$minimum_mass <- 0.25
  top_again <- reference_activate_mode(
    minimum_first$state, "top_k", 4L
  )
  expect_identical(top_again$settings_validation, "valid")
  expect_identical(top_again$state$values$top_k, 3L)
  expect_identical(top_again$state$values$minimum_mass, 0.25)

  top_again$state$values$top_k <- 2.5
  inactive_bad_top <- reference_activate_mode(
    top_again$state, "minimum_mass", 4L
  )
  expect_identical(inactive_bad_top$settings_validation, "valid")
  active_bad_top <- reference_activate_mode(
    top_again$state, "top_k", 4L
  )
  expect_identical(active_bad_top$settings_validation, "settings_invalid")
})

test_that("proposal state separates validation, mass, core, and rendering", {
  mass_modes <- c("auto", "cumulative", "minimum_mass", "top_k")
  ordinary <- c(
    auto = "strong_gap",
    cumulative = "coverage",
    minimum_mass = "minimum_mass",
    top_k = "top_k",
    complete = "complete"
  )

  for (mode in names(ordinary)) {
    valid <- reference_proposal_state(mode)
    expect_identical(valid$core_outcome, ordinary[[mode]])
    expect_identical(valid$render_outcome, "renderable")
  }

  for (mass_state in c("mass_invalid", "mass_unavailable")) {
    for (mode in mass_modes) {
      blocked <- reference_proposal_state(mode, mass = mass_state)
      expect_identical(blocked$mass_validation, mass_state)
      expect_null(blocked$core_outcome)
      expect_identical(blocked$render_outcome, "unavailable")
    }
    complete <- reference_proposal_state("complete", mass = mass_state)
    expect_identical(complete$mass_validation, mass_state)
    expect_identical(complete$core_outcome, "complete")
    expect_identical(complete$render_outcome, "renderable")
  }

  blocking_states <- list(
    source = "source_invalid",
    mapping = "mapping_invalid",
    settings = "settings_invalid"
  )
  for (field in names(blocking_states)) {
    for (mode in names(ordinary)) {
      args <- list(mode = mode)
      args[[field]] <- blocking_states[[field]]
      blocked <- do.call(reference_proposal_state, args)
      expect_null(blocked$core_outcome)
      expect_identical(blocked$render_outcome, "unavailable")
    }
  }

  for (mode in names(ordinary)) {
    stale <- reference_proposal_state(mode, identity = "stale")
    expect_identical(stale$identity_validation, "stale")
    expect_null(stale$core_outcome)
    expect_identical(stale$render_outcome, "stale")
  }
})

test_that("all ranking measures have exact validation domains", {
  mass <- c(0.5, 0.3, 0.2)
  support <- c(0, 2, 3)
  peak <- c(3, 2, 1)
  prominence <- c(0, 0.2, 1)

  valid <- reference_ranking_gate(
    mass, support, peak, prominence, mode = "auto"
  )
  expect_true(all(
    unlist(valid$ranking_measure_validation, use.names = FALSE) == "valid"
  ))
  expect_identical(valid$render_outcome, "renderable")
  expect_identical(valid$final_ids, c("b1", "b2", "b3"))

  invalid_support <- list(
    c(0, 2),
    c(0, NA, 3),
    c(0, Inf, 3),
    c(0, -1, 3),
    c(0, 1.5, 3)
  )
  for (value in invalid_support) {
    result <- reference_ranking_gate(
      mass, value, peak, prominence, mode = "complete"
    )
    expect_identical(
      result$ranking_measure_validation$trajectory_flow_support,
      "support_invalid"
    )
    expect_null(result$core_outcome)
    expect_identical(result$sentinel_ids, character())
    expect_identical(result$label_ids, character())
    expect_identical(result$final_ids, character())
    expect_identical(result$render_outcome, "unavailable")
  }

  invalid_prominence <- list(
    c(0, 0.2),
    c(0, NA, 1),
    c(0, Inf, 1),
    c(0, -0.1, 1)
  )
  for (value in invalid_prominence) {
    result <- reference_ranking_gate(
      mass, support, peak, value, mode = "complete"
    )
    expect_identical(
      result$ranking_measure_validation$canonical_prominence,
      "prominence_invalid"
    )
    expect_null(result$core_outcome)
    expect_identical(result$final_ids, character())
    expect_identical(result$render_outcome, "unavailable")
  }

  for (value in list(c(3, 2), c(3, NA, 1), c(3, Inf, 1))) {
    result <- reference_ranking_gate(
      mass, support, value, prominence, mode = "complete"
    )
    expect_identical(
      result$ranking_measure_validation$source_peak,
      "peak_invalid"
    )
    expect_identical(result$source_validation, "source_invalid")
    expect_null(result$core_outcome)
    expect_identical(result$final_ids, character())
    expect_identical(result$render_outcome, "unavailable")
  }

  mass_invalid_none <- reference_ranking_gate(
    c(0.5, NA, 0.5),
    support,
    peak,
    prominence,
    mode = "complete"
  )
  expect_identical(
    mass_invalid_none$ranking_measure_validation$trajectory_flow_mass,
    "mass_invalid"
  )
  expect_identical(mass_invalid_none$core_outcome, "complete")
  expect_false(mass_invalid_none$mass_views_available)

  mass_unavailable_none <- reference_ranking_gate(
    c(0, 0, 0),
    support,
    peak,
    prominence,
    mode = "complete"
  )
  expect_identical(
    mass_unavailable_none$ranking_measure_validation$trajectory_flow_mass,
    "mass_unavailable"
  )
  expect_identical(mass_unavailable_none$core_outcome, "complete")
  expect_false(mass_unavailable_none$mass_views_available)
})

test_that("view state keeps invalid attempts separate from retained proposals", {
  context <- list(
    project = "project-1",
    subject = "subject-15",
    graph = "k03",
    topology_fingerprint = "topology-1",
    vertex_map_fingerprint = "vertices-1",
    selected_field = "occupation",
    selected_field_fingerprint = "field-1",
    selected_source = "density",
    selected_source_fingerprint = "source-1",
    estimate = "graph_heat",
    trajectory_construction_fingerprint = "trajectory-1",
    canonical_construction_fingerprint = "canonical-1",
    direction = "max",
    component = 1L
  )
  context_fingerprint <- reference_context_fingerprint(context)
  inputs_one <- list(filter_mode = "top_k", top_k = 3L)
  proposal_one <- reference_view_proposal(
    context,
    inputs_one,
    c("b1", "b2", "b3")
  )
  current <- reference_view_transition(
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      inputs_one
    ),
    input_values = proposal_one$input_values,
    validation = "valid",
    proposal = proposal_one
  )
  expect_identical(current$display_source, "current")
  expect_identical(current$active_attempt$outcome, "proposal_created")
  expect_null(current$active_attempt$render_outcome)
  expect_identical(
    current$display_proposal_fingerprint,
    proposal_one$proposal_fingerprint
  )
  expect_identical(current$display_proposal, proposal_one)
  expect_true(reference_validate_view_state(current))

  invalid_inputs <- list(filter_mode = "top_k", top_k = 2.5)
  retained <- reference_view_transition(
    current,
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      invalid_inputs
    ),
    input_values = invalid_inputs,
    validation = "settings_invalid"
  )
  expect_identical(retained$display_source, "retained_last_valid")
  expect_identical(retained$active_attempt$outcome, "blocked")
  expect_identical(
    retained$active_attempt$validation$settings,
    "settings_invalid"
  )
  expect_identical(
    retained$active_attempt$render_outcome,
    "unavailable"
  )
  expect_identical(retained$active_attempt$input_values$top_k, 2.5)
  expect_false(any(
    c("core_ids", "final_ids") %in% names(retained$active_attempt)
  ))
  expect_identical(retained$display_proposal, proposal_one)
  expect_identical(
    retained$display_proposal$input_values$top_k,
    3L
  )
  expect_identical(
    unserialize(serialize(retained, NULL)),
    retained
  )
  expect_true(reference_validate_view_state(retained))

  inputs_two <- list(filter_mode = "top_k", top_k = 2L)
  proposal_two <- reference_view_proposal(
    context,
    inputs_two,
    c("b1", "b2"),
    render_outcome = "core_overflow"
  )
  recovered <- reference_view_transition(
    retained,
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      inputs_two
    ),
    input_values = proposal_two$input_values,
    validation = "valid",
    proposal = proposal_two
  )
  expect_identical(recovered$display_source, "current")
  expect_identical(recovered$display_proposal, proposal_two)
  expect_identical(
    recovered$display_proposal_fingerprint,
    proposal_two$proposal_fingerprint
  )
  expect_true(reference_validate_view_state(recovered))

  initial_invalid <- reference_view_transition(
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      invalid_inputs
    ),
    input_values = invalid_inputs,
    validation = "settings_invalid"
  )
  expect_identical(initial_invalid$display_source, "none")
  expect_null(initial_invalid$display_proposal_fingerprint)
  expect_null(initial_invalid$display_proposal)

  for (blocking in c(
    "source_invalid",
    "mapping_invalid",
    "mass_invalid",
    "mass_unavailable",
    "support_invalid",
    "peak_invalid",
    "prominence_invalid",
    "stale"
  )) {
    blocking_inputs <- proposal_two$input_values
    cleared <- reference_view_transition(
      recovered,
      context_fingerprint = context_fingerprint,
      attempt_fingerprint = reference_attempt_fingerprint(
        context_fingerprint,
        blocking_inputs
      ),
      input_values = blocking_inputs,
      validation = blocking
    )
    expect_identical(
      cleared$active_attempt$fingerprint,
      reference_attempt_fingerprint(
        context_fingerprint,
        blocking_inputs
      )
    )
    expect_true(any(
      unlist(
        cleared$active_attempt$validation,
        use.names = FALSE
      ) != "valid"
    ))
    expect_identical(
      cleared$active_attempt$render_outcome,
      if (blocking == "stale") "stale" else "unavailable"
    )
    expect_identical(cleared$display_source, "none")
    expect_null(cleared$display_proposal_fingerprint)
    expect_null(cleared$display_proposal)
    expect_true(reference_validate_view_state(cleared))
  }

  context_two <- context
  context_two$component <- 2L
  context_two_fingerprint <- reference_context_fingerprint(context_two)
  changed_context <- reference_view_transition(
    recovered,
    context_fingerprint = context_two_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_two_fingerprint,
      invalid_inputs
    ),
    input_values = invalid_inputs,
    validation = "settings_invalid"
  )
  expect_identical(
    changed_context$context_fingerprint,
    context_two_fingerprint
  )
  expect_identical(changed_context$display_source, "none")
  expect_null(changed_context$display_proposal)
  expect_true(reference_validate_view_state(changed_context))
})

test_that("Filter None installs typed mass-failure proposals end to end", {
  context <- list(
    project = "project-1",
    subject = "subject-15",
    graph = "k03",
    topology_fingerprint = "topology-1",
    vertex_map_fingerprint = "vertices-1",
    selected_field = "occupation",
    selected_field_fingerprint = "field-1",
    selected_source = "density",
    selected_source_fingerprint = "source-1",
    estimate = "graph_heat",
    trajectory_construction_fingerprint = "trajectory-1",
    canonical_construction_fingerprint = "canonical-1",
    direction = "max",
    component = 1L
  )
  context_fingerprint <- reference_context_fingerprint(context)
  branch_ids <- c("b1", "b2", "b3", "b4")
  inputs <- list(filter_mode = "none")

  for (mass_state in c("mass_invalid", "mass_unavailable")) {
    proposal <- reference_view_proposal(
      context,
      inputs,
      branch_ids,
      mass_state = mass_state,
      render_outcome = "core_overflow"
    )
    view <- reference_view_transition(
      context_fingerprint = context_fingerprint,
      attempt_fingerprint = reference_attempt_fingerprint(
        context_fingerprint,
        inputs
      ),
      input_values = inputs,
      validation = mass_state,
      proposal = proposal
    )

    expect_false(proposal$mass_derived$available)
    expect_identical(
      proposal$mass_derived$unavailable_reason,
      mass_state
    )
    expect_identical(proposal$core_outcome, "complete")
    expect_identical(proposal$core_ids, sort(branch_ids))
    expect_identical(proposal$final_ids, sort(branch_ids))
    expect_identical(proposal$render_outcome, "core_overflow")
    expect_identical(
      proposal$label_contributions$trajectory_flow_mass,
      character()
    )
    expect_true(length(proposal$label_ids) > 0L)
    expect_identical(
      proposal$label_omission_reasons,
      paste0("trajectory_flow_mass:", mass_state)
    )
    expect_null(proposal$mass_derived$core_coverage)
    expect_null(proposal$mass_derived$final_coverage)

    if (mass_state == "mass_invalid") {
      expect_null(proposal$mass_derived$positive_groups)
      expect_null(proposal$mass_derived$all_mass_groups)
      expect_null(proposal$mass_derived$denominator)
      expect_null(proposal$mass_derived$positive_count)
      expect_null(proposal$mass_derived$zero_count)
    } else {
      expect_identical(
        proposal$mass_derived$positive_groups,
        list()
      )
      expect_length(proposal$mass_derived$all_mass_groups, 1L)
      expect_identical(
        proposal$mass_derived$all_mass_groups[[1L]]$mass,
        0
      )
      expect_identical(
        proposal$mass_derived$all_mass_groups[[1L]]$ids,
        sort(branch_ids)
      )
      expect_identical(proposal$mass_derived$denominator, 0)
      expect_identical(proposal$mass_derived$positive_count, 0L)
      expect_identical(
        proposal$mass_derived$zero_count,
        length(branch_ids)
      )
    }

    expect_identical(view$active_attempt$outcome, "proposal_created")
    expect_null(view$active_attempt$render_outcome)
    expect_identical(view$display_source, "current")
    expect_identical(view$display_proposal, proposal)
    round_trip <- unserialize(serialize(view, NULL))
    expect_identical(round_trip, view)
    expect_true(reference_validate_view_state(round_trip))
  }

  mass_mode_inputs <- list(filter_mode = "top_k", top_k = 2L)
  blocked <- reference_view_transition(
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      mass_mode_inputs
    ),
    input_values = mass_mode_inputs,
    validation = "mass_invalid"
  )
  expect_identical(blocked$active_attempt$outcome, "blocked")
  expect_identical(blocked$display_source, "none")
  expect_null(blocked$display_proposal)

  old_context <- context
  old_context$trajectory_construction_fingerprint <- "trajectory-old"
  old_context_fingerprint <- reference_context_fingerprint(old_context)
  old_inputs <- list(filter_mode = "top_k", top_k = 2L)
  old_proposal <- reference_view_proposal(
    old_context,
    old_inputs,
    c("b1", "b2")
  )
  old_view <- reference_view_transition(
    context_fingerprint = old_context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      old_context_fingerprint,
      old_inputs
    ),
    input_values = old_inputs,
    validation = "valid",
    proposal = old_proposal
  )
  replacement <- reference_view_proposal(
    context,
    inputs,
    branch_ids,
    mass_state = "mass_invalid"
  )
  replaced_after_mass_change <- reference_view_transition(
    old_view,
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      inputs
    ),
    input_values = inputs,
    validation = "mass_invalid",
    proposal = replacement
  )
  expect_identical(
    replaced_after_mass_change$display_source,
    "current"
  )
  expect_identical(
    replaced_after_mass_change$display_proposal,
    replacement
  )
  expect_false(identical(
    replaced_after_mass_change$display_proposal_fingerprint,
    old_proposal$proposal_fingerprint
  ))
})

test_that("fingerprints are deterministic and reject inconsistent state", {
  context <- list(
    project = "project-1",
    subject = "subject-15",
    graph = "k03",
    topology_fingerprint = "topology-1",
    vertex_map_fingerprint = "vertices-1",
    selected_field = "occupation",
    selected_field_fingerprint = "field-1",
    selected_source = "density",
    selected_source_fingerprint = "source-1",
    estimate = "graph_heat",
    trajectory_construction_fingerprint = "trajectory-1",
    canonical_construction_fingerprint = "canonical-1",
    direction = "max",
    component = 1L
  )
  reordered_context <- context[rev(names(context))]
  expect_identical(
    reference_context_fingerprint(context),
    reference_context_fingerprint(reordered_context)
  )

  inputs <- list(filter_mode = "none", labels = "important")
  reordered_inputs <- inputs[rev(names(inputs))]
  context_fingerprint <- reference_context_fingerprint(context)
  expect_identical(
    reference_attempt_fingerprint(context_fingerprint, inputs),
    reference_attempt_fingerprint(
      context_fingerprint,
      reordered_inputs
    )
  )

  proposal <- reference_view_proposal(
    context,
    inputs,
    c("b1", "b2", "b3"),
    creation_time = "2026-08-01T12:00:00-04:00"
  )
  timestamp_only <- proposal
  timestamp_only$creation_time <- "2026-08-01T13:00:00-04:00"
  expect_identical(
    reference_proposal_fingerprint(proposal),
    reference_proposal_fingerprint(timestamp_only)
  )
  expect_true(reference_validate_proposal(proposal, context_fingerprint))

  tampered <- proposal
  tampered$final_ids <- c("b1", "b2")
  expect_error(
    reference_validate_proposal(tampered, context_fingerprint),
    "fingerprint_invalid"
  )

  wrong_context <- proposal
  wrong_context$context_fields$component <- 2L
  expect_error(
    reference_validate_proposal(wrong_context, context_fingerprint),
    "fingerprint_invalid"
  )

  context_two <- context
  context_two$component <- 2L
  internally_valid_wrong_context <- reference_view_proposal(
    context_two,
    inputs,
    c("b1", "b2", "b3")
  )
  expect_error(
    reference_validate_proposal(
      internally_valid_wrong_context,
      context_fingerprint
    ),
    "fingerprint_invalid"
  )

  view <- reference_view_transition(
    context_fingerprint = context_fingerprint,
    attempt_fingerprint = reference_attempt_fingerprint(
      context_fingerprint,
      inputs
    ),
    input_values = inputs,
    validation = "valid",
    proposal = proposal
  )
  expect_true(reference_validate_view_state(view))

  corrupted_view <- unserialize(serialize(view, NULL))
  corrupted_view$display_proposal_fingerprint <-
    paste0(
      if (startsWith(
        corrupted_view$display_proposal_fingerprint,
        "0"
      )) "1" else "0",
      substring(
      corrupted_view$display_proposal_fingerprint,
      2L
      )
    )
  expect_error(
    reference_validate_view_state(corrupted_view),
    "fingerprint_invalid"
  )

  corrupted_attempt <- unserialize(serialize(view, NULL))
  corrupted_attempt$active_attempt$input_values$labels <- "all"
  expect_error(
    reference_validate_view_state(corrupted_attempt),
    "fingerprint_invalid"
  )
})

test_that("complete-tree controls have distinct persistent and viewer actions", {
  context <- list(
    project = "project-1",
    subject = "subject-15",
    graph = "k03",
    topology_fingerprint = "topology-1",
    vertex_map_fingerprint = "vertices-1",
    selected_field = "occupation",
    selected_field_fingerprint = "field-1",
    selected_source = "density",
    selected_source_fingerprint = "source-1",
    estimate = "graph_heat",
    trajectory_construction_fingerprint = "trajectory-1",
    canonical_construction_fingerprint = "canonical-1",
    direction = "max",
    component = 1L
  )
  context_fingerprint <- reference_context_fingerprint(context)
  for (render_outcome in c("renderable", "core_overflow")) {
    complete_proposal <- reference_view_proposal(
      context,
      list(filter_mode = "none"),
      c("b1", "b2", "b3"),
      render_outcome = render_outcome
    )
    state <- list(
      context_fingerprint = context_fingerprint,
      filter_mode = "auto",
      manual_settings = list(top_k = 3L, minimum_mass = 0.1),
      selected_ids = c("b2"),
      active_attempt_fingerprint = "attempt-1",
      display_source = "retained_last_valid",
      display_proposal_fingerprint = "proposal-1",
      display_proposal = NULL,
      render_outcome = render_outcome,
      viewer_open = FALSE,
      recomputed = FALSE
    )

    viewer <- reference_complete_tree_action(
      state, "open_complete_viewer", complete_proposal
    )
    expected_viewer <- state
    expected_viewer$viewer_open <- TRUE
    expect_identical(viewer, expected_viewer)

    show_all <- reference_complete_tree_action(
      state, "show_all", complete_proposal
    )
    filter_none <- reference_complete_tree_action(
      state, "filter_none", complete_proposal
    )
    expect_identical(show_all, filter_none)
    expect_identical(show_all$filter_mode, "none")
    expect_identical(show_all$core_outcome, "complete")
    expect_identical(
      show_all$active_input_values,
      complete_proposal$input_values
    )
    expect_identical(
      show_all$active_attempt_fingerprint,
      reference_attempt_fingerprint(
        context_fingerprint,
        complete_proposal$input_values
      )
    )
    expect_identical(
      show_all$active_attempt_outcome,
      "proposal_created"
    )
    expect_null(show_all$active_attempt_render_outcome)
    expect_true(show_all$recomputed)
    expect_identical(show_all$manual_settings, state$manual_settings)
    expect_identical(show_all$selected_ids, state$selected_ids)
    expect_identical(show_all$display_source, "current")
    expect_identical(
      show_all$display_proposal_fingerprint,
      complete_proposal$proposal_fingerprint
    )
    expect_identical(show_all$display_proposal, complete_proposal)
    expect_false(show_all$viewer_open)
  }
})
