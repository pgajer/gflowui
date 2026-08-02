.gflowui_basin_minimum_core_branches <- 3L

gflowui_basin_default_controls <- function(component.size) {
  if (!is.numeric(component.size) ||
      length(component.size) != 1L ||
      is.na(component.size) ||
      component.size != floor(component.size) ||
      component.size < 1 ||
      component.size > .Machine$integer.max) {
    .gflowui_basin_stop(
      "'component.size' must be a positive supported whole number.",
      "gflowui_basin_settings_error"
    )
  }
  list(
    filter.mode = "auto",
    coverage.target = 0.99,
    strong.gap.decades = 3,
    core.branch.budget = 50L,
    final.render.budget = 80L,
    sentinel.top.n = 10L,
    peak.sentinel.enabled = TRUE,
    prominence.sentinel.enabled = TRUE,
    support.sentinel.enabled = TRUE,
    top.k = as.integer(min(10, component.size)),
    minimum.mass = 0,
    important.label.n = 6L,
    label.mode = "important"
  )
}

.gflowui_basin_validate_integer <- function(value,
                                             field,
                                             minimum = 0L,
                                             maximum = .Machine$integer.max) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= minimum &&
    value <= maximum &&
    value <= .Machine$integer.max
  if (!valid) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "'%s' must be a whole number from %s through %s.",
        field,
        format(minimum, scientific = FALSE),
        format(maximum, scientific = FALSE)
      )
    ))
  }
  list(valid = TRUE, value = as.integer(value))
}

.gflowui_basin_validate_number <- function(value,
                                            field,
                                            minimum = -Inf,
                                            maximum = Inf,
                                            minimum.open = FALSE) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value <= maximum &&
    if (minimum.open) value > minimum else value >= minimum
  if (!valid) {
    return(list(
      valid = FALSE,
      message = sprintf("'%s' is outside its supported domain.", field)
    ))
  }
  list(valid = TRUE, value = as.numeric(value))
}

.gflowui_basin_validate_toggle <- function(value, field) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    return(list(
      valid = FALSE,
      message = sprintf("'%s' must be TRUE or FALSE.", field)
    ))
  }
  list(valid = TRUE, value = value)
}

gflowui_basin_validate_controls <- function(controls, component.size) {
  modes <- c(
    "auto",
    "cumulative_mass",
    "minimum_mass",
    "top_k",
    "none"
  )
  if (!is.list(controls) ||
      !.gflowui_basin_scalar_string(controls$filter.mode) ||
      !controls$filter.mode %in% modes) {
    return(list(
      valid = FALSE,
      messages = "'filter.mode' is unsupported.",
      accepted.parameters = NULL,
      presentation = NULL
    ))
  }
  size.check <- .gflowui_basin_validate_integer(
    component.size,
    "component.size",
    minimum = 1L
  )
  if (!size.check$valid) {
    return(list(
      valid = FALSE,
      messages = size.check$message,
      accepted.parameters = NULL,
      presentation = NULL
    ))
  }
  component.size <- size.check$value
  checks <- list(
    final.render.budget = .gflowui_basin_validate_integer(
      controls$final.render.budget,
      "final.render.budget",
      minimum = 1L
    ),
    sentinel.top.n = .gflowui_basin_validate_integer(
      controls$sentinel.top.n,
      "sentinel.top.n",
      minimum = 0L
    ),
    peak.sentinel.enabled = .gflowui_basin_validate_toggle(
      controls$peak.sentinel.enabled,
      "peak.sentinel.enabled"
    ),
    prominence.sentinel.enabled = .gflowui_basin_validate_toggle(
      controls$prominence.sentinel.enabled,
      "prominence.sentinel.enabled"
    ),
    support.sentinel.enabled = .gflowui_basin_validate_toggle(
      controls$support.sentinel.enabled,
      "support.sentinel.enabled"
    )
  )
  mode <- controls$filter.mode
  if (mode %in% c("auto", "cumulative_mass")) {
    checks$coverage.target <- .gflowui_basin_validate_number(
      controls$coverage.target,
      "coverage.target",
      minimum = 0,
      maximum = 1,
      minimum.open = TRUE
    )
    checks$core.branch.budget <- .gflowui_basin_validate_integer(
      controls$core.branch.budget,
      "core.branch.budget",
      minimum = .gflowui_basin_minimum_core_branches
    )
  }
  if (mode == "auto") {
    checks$strong.gap.decades <- .gflowui_basin_validate_number(
      controls$strong.gap.decades,
      "strong.gap.decades",
      minimum = 0
    )
  }
  if (mode == "minimum_mass") {
    checks$minimum.mass <- .gflowui_basin_validate_number(
      controls$minimum.mass,
      "minimum.mass",
      minimum = 0
    )
  }
  if (mode == "top_k") {
    checks$top.k <- .gflowui_basin_validate_integer(
      controls$top.k,
      "top.k",
      minimum = 1L,
      maximum = component.size
    )
  }
  invalid <- names(checks)[
    !vapply(checks, `[[`, logical(1), "valid")
  ]
  presentation.checks <- list(
    important.label.n = .gflowui_basin_validate_integer(
      controls$important.label.n,
      "important.label.n",
      minimum = 0L
    )
  )
  label.valid <- .gflowui_basin_scalar_string(controls$label.mode) &&
    controls$label.mode %in%
      c("important", "selected", "displayed", "none", "all")
  if (!label.valid) {
    presentation.checks$label.mode <- list(
      valid = FALSE,
      message = "'label.mode' is unsupported."
    )
  } else {
    presentation.checks$label.mode <- list(
      valid = TRUE,
      value = controls$label.mode
    )
  }
  presentation.invalid <- names(presentation.checks)[
    !vapply(presentation.checks, `[[`, logical(1), "valid")
  ]
  presentation <- list(
    valid = !length(presentation.invalid),
    messages = if (length(presentation.invalid)) {
      unname(vapply(
        presentation.checks[presentation.invalid],
        `[[`,
        character(1),
        "message"
      ))
    } else {
      character()
    },
    important.label.n = if (!"important.label.n" %in%
        presentation.invalid) {
      presentation.checks$important.label.n$value
    } else {
      NULL
    },
    label.mode = if (!"label.mode" %in% presentation.invalid) {
      presentation.checks$label.mode$value
    } else {
      NULL
    }
  )
  if (length(invalid)) {
    return(list(
      valid = FALSE,
      messages = unname(vapply(
        checks[invalid],
        `[[`,
        character(1),
        "message"
      )),
      accepted.parameters = NULL,
      presentation = presentation
    ))
  }
  common.names <- c(
    "final.render.budget",
    "sentinel.top.n",
    "peak.sentinel.enabled",
    "prominence.sentinel.enabled",
    "support.sentinel.enabled"
  )
  mode.names <- switch(
    mode,
    auto = c(
      "coverage.target",
      "strong.gap.decades",
      "core.branch.budget"
    ),
    cumulative_mass = c("coverage.target", "core.branch.budget"),
    minimum_mass = "minimum.mass",
    top_k = "top.k",
    none = character()
  )
  accepted <- list(filter.mode = mode)
  for (name in c(common.names, mode.names)) {
    accepted[[name]] <- checks[[name]]$value
  }
  list(
    valid = TRUE,
    messages = character(),
    accepted.parameters = accepted,
    presentation = presentation
  )
}

.gflowui_basin_mass_groups <- function(ids, mass, positive.only = FALSE) {
  keep <- if (positive.only) mass > 0 else rep(TRUE, length(mass))
  ids <- ids[keep]
  mass <- mass[keep]
  if (!length(ids)) {
    return(list(
      ranked.ids = character(),
      ranked.mass = numeric(),
      groups = list(),
      endpoints = integer(),
      starts = integer(),
      group.mass = numeric()
    ))
  }
  order.index <- order(-mass, ids, method = "radix")
  ranked.ids <- ids[order.index]
  ranked.mass <- mass[order.index]
  group.number <- cumsum(c(
    TRUE,
    ranked.mass[-1L] != ranked.mass[-length(ranked.mass)]
  ))
  rows <- split(seq_along(ranked.mass), group.number)
  list(
    ranked.ids = ranked.ids,
    ranked.mass = ranked.mass,
    groups = unname(lapply(rows, function(index) ranked.ids[index])),
    endpoints = unname(vapply(rows, max, integer(1))),
    starts = unname(vapply(rows, min, integer(1))),
    group.mass = unname(vapply(
      rows,
      function(index) ranked.mass[[index[[1L]]]],
      numeric(1)
    ))
  )
}

.gflowui_basin_group_coverage <- function(groups) {
  if (!length(groups$ranked.mass)) {
    return(list(denominator = 0, cumulative = numeric()))
  }
  denominator <- .gflowui_basin_fixed_sum(groups$ranked.mass)
  running <- 0
  cumulative <- numeric(length(groups$endpoints))
  previous <- 0L
  for (index in seq_along(groups$endpoints)) {
    endpoint <- groups$endpoints[[index]]
    rows <- seq.int(previous + 1L, endpoint)
    running <- running +
      .gflowui_basin_fixed_sum(groups$ranked.mass[rows])
    cumulative[[index]] <- running / denominator
    previous <- endpoint
  }
  list(denominator = denominator, cumulative = cumulative)
}

.gflowui_basin_core_record <- function(ids,
                                       outcome,
                                       warnings = character(),
                                       boundary = NULL,
                                       gap.decades = NULL,
                                       informational.cutoff = NULL) {
  list(
    ids = sort(unique(as.character(ids)), method = "radix"),
    outcome = outcome,
    warnings = unique(as.character(warnings)),
    boundary = boundary,
    gap.decades = gap.decades,
    informational.cutoff = informational.cutoff
  )
}

.gflowui_basin_budget_endpoint <- function(groups, budget) {
  straddling <- which(
    groups$starts <= budget &
      groups$endpoints > budget
  )
  if (length(straddling)) {
    return(list(
      endpoint = groups$endpoints[[straddling[[1L]]]],
      straddling = TRUE
    ))
  }
  within <- groups$endpoints[groups$endpoints <= budget]
  list(
    endpoint = if (length(within)) max(within) else groups$endpoints[[1L]],
    straddling = FALSE
  )
}

.gflowui_basin_auto_core <- function(ids, mass, parameters) {
  groups <- .gflowui_basin_mass_groups(ids, mass, positive.only = TRUE)
  coverage <- .gflowui_basin_group_coverage(groups)
  n.positive <- length(groups$ranked.ids)
  if (n.positive == 1L) {
    return(.gflowui_basin_core_record(
      groups$ranked.ids,
      "single_positive",
      boundary = 1L
    ))
  }
  j.coverage.index <- which(
    coverage$cumulative >= parameters$coverage.target
  )[[1L]]
  j.coverage <- groups$endpoints[[j.coverage.index]]
  minimum.count <- min(
    .gflowui_basin_minimum_core_branches,
    n.positive
  )
  j.minimum <- groups$endpoints[
    which(groups$endpoints >= minimum.count)[[1L]]
  ]
  j.required <- max(j.coverage, j.minimum)
  budget <- parameters$core.branch.budget
  if (j.required > budget) {
    capped <- .gflowui_basin_budget_endpoint(groups, budget)
    group.index <- match(capped$endpoint, groups$endpoints)
    reaches <- coverage$cumulative[[group.index]] >=
      parameters$coverage.target
    return(.gflowui_basin_core_record(
      groups$ranked.ids[seq_len(capped$endpoint)],
      if (reaches) "coverage" else "coverage_capped",
      warnings = if (capped$straddling) {
        "tie_overflow"
      } else {
        character()
      },
      boundary = as.integer(capped$endpoint)
    ))
  }
  eligible <- groups$endpoints[
    groups$endpoints >= j.required &
      groups$endpoints <= budget &
      groups$endpoints < n.positive
  ]
  for (endpoint in eligible) {
    gap <- log10(groups$ranked.mass[[endpoint]]) -
      log10(groups$ranked.mass[[endpoint + 1L]])
    if (gap >= parameters$strong.gap.decades) {
      cutoff <- 10^mean(log10(c(
        groups$ranked.mass[[endpoint]],
        groups$ranked.mass[[endpoint + 1L]]
      )))
      return(.gflowui_basin_core_record(
        groups$ranked.ids[seq_len(endpoint)],
        "strong_gap",
        boundary = as.integer(endpoint),
        gap.decades = gap,
        informational.cutoff = cutoff
      ))
    }
  }
  .gflowui_basin_core_record(
    groups$ranked.ids[seq_len(j.required)],
    "coverage",
    boundary = as.integer(j.required)
  )
}

.gflowui_basin_cumulative_core <- function(ids, mass, parameters) {
  groups <- .gflowui_basin_mass_groups(ids, mass, positive.only = TRUE)
  coverage <- .gflowui_basin_group_coverage(groups)
  index <- which(coverage$cumulative >= parameters$coverage.target)[[1L]]
  j.coverage <- groups$endpoints[[index]]
  budget <- parameters$core.branch.budget
  if (j.coverage <= budget) {
    return(.gflowui_basin_core_record(
      groups$ranked.ids[seq_len(j.coverage)],
      "coverage",
      boundary = as.integer(j.coverage)
    ))
  }
  capped <- .gflowui_basin_budget_endpoint(groups, budget)
  group.index <- match(capped$endpoint, groups$endpoints)
  reaches <- coverage$cumulative[[group.index]] >=
    parameters$coverage.target
  .gflowui_basin_core_record(
    groups$ranked.ids[seq_len(capped$endpoint)],
    if (reaches) "coverage" else "coverage_capped",
    warnings = if (capped$straddling) {
      "tie_overflow"
    } else {
      character()
    },
    boundary = as.integer(capped$endpoint)
  )
}

.gflowui_basin_manual_core <- function(ids, mass, parameters) {
  mode <- parameters$filter.mode
  if (mode == "none") {
    return(.gflowui_basin_core_record(ids, "complete"))
  }
  groups <- .gflowui_basin_mass_groups(ids, mass, positive.only = FALSE)
  if (mode == "minimum_mass") {
    keep <- groups$ranked.mass >= parameters$minimum.mass
    return(.gflowui_basin_core_record(
      groups$ranked.ids[keep],
      if (any(keep)) "minimum_mass" else "threshold_empty"
    ))
  }
  endpoint <- groups$endpoints[
    which(groups$endpoints >= parameters$top.k)[[1L]]
  ]
  .gflowui_basin_core_record(
    groups$ranked.ids[seq_len(endpoint)],
    "top_k",
    warnings = if (endpoint > parameters$top.k) {
      "tie_overflow"
    } else {
      character()
    },
    boundary = as.integer(endpoint)
  )
}

.gflowui_basin_top_with_ties <- function(ids, values, count) {
  if (!length(ids) || count == 0L) {
    return(character())
  }
  ranked <- order(-values, ids, method = "radix")
  cutoff.index <- min(count, length(ranked))
  cutoff <- values[ranked[[cutoff.index]]]
  sort(ids[values >= cutoff], method = "radix")
}

.gflowui_basin_add_reason <- function(reasons, ids, reason) {
  for (id in ids) {
    reasons[[id]] <- unique(c(reasons[[id]], reason))
  }
  reasons
}

.gflowui_basin_sentinels <- function(component.data,
                                     core.ids,
                                     pinned.ids,
                                     parameters) {
  ids <- component.data$basin.id
  roots <- ids[is.na(component.data$parent.basin.id)]
  reasons <- list()
  reasons <- .gflowui_basin_add_reason(reasons, pinned.ids, "pinned")
  reasons <- .gflowui_basin_add_reason(
    reasons,
    roots,
    "component_survivor"
  )
  families <- list(
    peak = if (parameters$peak.sentinel.enabled) {
      .gflowui_basin_top_with_ties(
        ids,
        component.data$peak.value,
        parameters$sentinel.top.n
      )
    } else {
      character()
    },
    prominence = if (parameters$prominence.sentinel.enabled) {
      .gflowui_basin_top_with_ties(
        ids,
        component.data$persistence,
        parameters$sentinel.top.n
      )
    } else {
      character()
    },
    support = if (parameters$support.sentinel.enabled) {
      .gflowui_basin_top_with_ties(
        ids,
        component.data$trajectory.flow.support,
        parameters$sentinel.top.n
      )
    } else {
      character()
    }
  )
  for (family in names(families)) {
    reasons <- .gflowui_basin_add_reason(
      reasons,
      families[[family]],
      family
    )
  }
  sentinel.ids <- sort(unique(names(reasons)), method = "radix")
  reasons <- reasons[sentinel.ids]
  list(ids = sentinel.ids, reasons = reasons, families = families)
}

.gflowui_basin_blocked_attempt <- function(reason,
                                           context,
                                           attempt.id,
                                           messages = character()) {
  structure(
    list(
      status = "blocked",
      reason = reason,
      messages = as.character(messages),
      bundle.id = context$bundle.id,
      context.generation = context$context.generation,
      attempt.id = as.integer(attempt.id),
      proposal = NULL
    ),
    class = c("gflowui_basin_attempt_result", "list")
  )
}

.gflowui_basin_attempt_id <- function(attempt.id) {
  checked <- .gflowui_basin_validate_integer(
    attempt.id,
    "attempt.id",
    minimum = 1L
  )
  if (!checked$valid) {
    .gflowui_basin_stop(
      checked$message,
      "gflowui_basin_attempt_error"
    )
  }
  checked$value
}

.gflowui_basin_validate_context <- function(context, bundle) {
  .gflowui_basin_assert_bundle(bundle)
  if (!is.list(context) ||
      !identical(context$bundle.id, bundle$bundle.id) ||
      !identical(context$direction, "max") ||
      !is.numeric(context$component) ||
      length(context$component) != 1L) {
    return(FALSE)
  }
  expected <- gflowui_basin_context(
    bundle,
    context.generation = context$context.generation,
    component = context$component
  )
  identical(context$context.key, expected$context.key)
}

.gflowui_basin_assert_proposal_postconditions <- function(
    proposal,
    bundle,
    component.data,
    preclosure.ids,
    layout) {
  .gflowui_basin_assert_pair(proposal, bundle)
  sorted.unique <- function(ids) {
    identical(
      ids,
      sort(unique(as.character(ids)), method = "radix")
    )
  }
  id.fields <- list(
    component = proposal$component$ids,
    pinned = proposal$pinned.ids,
    core = proposal$core$ids,
    sentinel = proposal$sentinels$ids,
    ancestor = proposal$ancestor.only.ids,
    final = proposal$final.ids
  )
  if (!all(vapply(id.fields, sorted.unique, logical(1))) ||
      length(setdiff(
        unlist(id.fields[-1L], use.names = FALSE),
        proposal$component$ids
      )) ||
      length(setdiff(proposal$core$ids, proposal$final.ids)) ||
      !identical(
        proposal$final.ids,
        sort(unique(c(
          preclosure.ids,
          proposal$ancestor.only.ids
        )), method = "radix")
      ) ||
      !identical(
        proposal$final.ids,
        sort(as.character(layout$branches$basin.id), method = "radix")
      )) {
    .gflowui_basin_stop(
      "Proposal ID-set postconditions failed.",
      "gflowui_basin_constructor_error"
    )
  }
  reasons <- proposal$sentinels$reasons
  allowed.reasons <- c(
    "pinned", "component_survivor", "peak", "prominence", "support"
  )
  reason.values <- unlist(reasons, use.names = FALSE)
  if (!identical(
      sort(names(reasons), method = "radix"),
      proposal$sentinels$ids
  ) ||
      any(!lengths(reasons)) ||
      any(!reason.values %in% allowed.reasons) ||
      (!proposal$accepted.parameters$peak.sentinel.enabled &&
        "peak" %in% reason.values) ||
      (!proposal$accepted.parameters$prominence.sentinel.enabled &&
        "prominence" %in% reason.values) ||
      (!proposal$accepted.parameters$support.sentinel.enabled &&
        "support" %in% reason.values)) {
    .gflowui_basin_stop(
      "Proposal sentinel-reason postconditions failed.",
      "gflowui_basin_constructor_error"
    )
  }
  if (proposal$mass.status == "valid") {
    groups <- .gflowui_basin_mass_groups(
      component.data$basin.id,
      component.data$trajectory.flow.mass,
      positive.only = proposal$accepted.parameters$filter.mode %in%
        c("auto", "cumulative_mass")
    )
    split.tie <- vapply(groups$groups, function(group.ids) {
      selected <- group.ids %in% proposal$core$ids
      any(selected) && !all(selected)
    }, logical(1))
    if (any(split.tie)) {
      .gflowui_basin_stop(
        "Proposal core membership split an exact mass tie.",
        "gflowui_basin_constructor_error"
      )
    }
  }
  budget <- proposal$accepted.parameters$final.render.budget
  expected.outcome <- if (length(proposal$core$ids) > budget) {
    "core_overflow"
  } else if (length(preclosure.ids) > budget) {
    "sentinel_overflow"
  } else if (length(proposal$final.ids) > budget) {
    "closure_overflow"
  } else {
    "renderable"
  }
  if (!identical(proposal$render.outcome, expected.outcome)) {
    .gflowui_basin_stop(
      "Proposal render-outcome postconditions failed.",
      "gflowui_basin_constructor_error"
    )
  }
  invisible(TRUE)
}

gflowui_basin_construct_proposal <- function(
    context,
    bundle,
    controls,
    pinned.ids = character(),
    attempt.id = 1L,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  attempt.id <- .gflowui_basin_attempt_id(attempt.id)
  if (!.gflowui_basin_validate_context(context, bundle)) {
    return(.gflowui_basin_blocked_attempt(
      "stale",
      context,
      attempt.id,
      "The context and scientific bundle identities differ."
    ))
  }
  data <- gflowui_basin_bundle_snapshot(bundle)
  component.data <- data$canonical[
    data$canonical$component == context$component,
    ,
    drop = FALSE
  ]
  component.ids <- component.data$basin.id
  validation <- data$validation
  blocking <- c(
    source = validation$source,
    mapping = validation$mapping,
    support = validation$trajectory_flow_support,
    peak = validation$source_peak,
    prominence = validation$canonical_prominence
  )
  blocking <- blocking[blocking != "valid"]
  if (length(blocking)) {
    return(.gflowui_basin_blocked_attempt(
      unname(blocking[[1L]]),
      context,
      attempt.id,
      sprintf("%s: %s", names(blocking), blocking)
    ))
  }
  settings <- gflowui_basin_validate_controls(
    controls,
    nrow(component.data)
  )
  if (!settings$valid) {
    return(.gflowui_basin_blocked_attempt(
      "settings_invalid",
      context,
      attempt.id,
      settings$messages
    ))
  }
  parameters <- settings$accepted.parameters
  mass.status <- validation$trajectory_flow_mass
  if (parameters$filter.mode != "none" && mass.status != "valid") {
    return(.gflowui_basin_blocked_attempt(
      mass.status,
      context,
      attempt.id,
      sprintf("trajectory_flow_mass: %s", mass.status)
    ))
  }
  if (!is.character(pinned.ids) ||
      anyNA(pinned.ids) ||
      anyDuplicated(pinned.ids) ||
      !all(pinned.ids %in% component.ids)) {
    return(.gflowui_basin_blocked_attempt(
      "pins_invalid",
      context,
      attempt.id,
      "Pinned IDs must be unique canonical IDs in the selected component."
    ))
  }
  pinned.ids <- sort(pinned.ids, method = "radix")
  mass <- component.data$trajectory.flow.mass
  core <- if (parameters$filter.mode == "auto") {
    .gflowui_basin_auto_core(component.ids, mass, parameters)
  } else if (parameters$filter.mode == "cumulative_mass") {
    .gflowui_basin_cumulative_core(component.ids, mass, parameters)
  } else {
    .gflowui_basin_manual_core(component.ids, mass, parameters)
  }
  sentinels <- .gflowui_basin_sentinels(
    component.data,
    core$ids,
    pinned.ids,
    parameters
  )
  preclosure.ids <- sort(
    unique(c(core$ids, sentinels$ids)),
    method = "radix"
  )
  if (!is.function(layout.accessor)) {
    return(.gflowui_basin_blocked_attempt(
      "layout_invalid",
      context,
      attempt.id,
      "The Phase 1 layout accessor is unavailable."
    ))
  }
  layout <- tryCatch(
    layout.accessor(
      data$canonical.tree,
      direction = "max",
      component = context$component,
      basin.ids = preclosure.ids,
      close.ancestors = TRUE
    ),
    error = identity
  )
  if (inherits(layout, "error")) {
    return(.gflowui_basin_blocked_attempt(
      "layout_invalid",
      context,
      attempt.id,
      conditionMessage(layout)
    ))
  }
  final.ids <- sort(layout$branches$basin.id, method = "radix")
  ancestor.only.ids <- sort(
    setdiff(final.ids, preclosure.ids),
    method = "radix"
  )
  budget <- parameters$final.render.budget
  render.outcome <- if (length(core$ids) > budget) {
    "core_overflow"
  } else if (length(preclosure.ids) > budget) {
    "sentinel_overflow"
  } else if (length(final.ids) > budget) {
    "closure_overflow"
  } else {
    "renderable"
  }
  proposal <- structure(
    list(
      context.key = context$context.key,
      bundle.id = bundle$bundle.id,
      context.generation = context$context.generation,
      attempt.id = attempt.id,
      accepted.parameters = parameters,
      component = list(
        id = context$component,
        ids = sort(component.ids, method = "radix"),
        selection.rule = context$selection.rule,
        fallback.reason = context$fallback.reason
      ),
      pinned.ids = pinned.ids,
      mass.status = mass.status,
      core = core,
      sentinels = list(
        ids = sentinels$ids,
        reasons = sentinels$reasons
      ),
      ancestor.only.ids = ancestor.only.ids,
      final.ids = final.ids,
      render.outcome = render.outcome
    ),
    class = c("basin_display_proposal", "list")
  )
  postconditions <- tryCatch(
    .gflowui_basin_assert_proposal_postconditions(
      proposal,
      bundle,
      component.data,
      preclosure.ids,
      layout
    ),
    error = identity
  )
  if (inherits(postconditions, "error")) {
    return(.gflowui_basin_blocked_attempt(
      "construction_failed",
      context,
      attempt.id,
      conditionMessage(postconditions)
    ))
  }
  structure(
    list(
      status = "proposal_created",
      reason = NULL,
      messages = character(),
      bundle.id = bundle$bundle.id,
      context.generation = context$context.generation,
      attempt.id = attempt.id,
      proposal = proposal
    ),
    class = c("gflowui_basin_attempt_result", "list")
  )
}

.gflowui_basin_assert_pair <- function(proposal, bundle) {
  .gflowui_basin_assert_bundle(bundle)
  if (!inherits(proposal, "basin_display_proposal") ||
      !identical(proposal$bundle.id, bundle$bundle.id)) {
    .gflowui_basin_stop(
      "The proposal and scientific bundle identities differ.",
      "gflowui_basin_stale_error"
    )
  }
  data <- gflowui_basin_bundle_snapshot(bundle)
  if (!proposal$component$id %in% data$component.ids ||
      !all(proposal$final.ids %in% proposal$component$ids)) {
    .gflowui_basin_stop(
      "The proposal does not belong to the bundle component.",
      "gflowui_basin_stale_error"
    )
  }
  data
}

.gflowui_basin_proposal_component <- function(proposal, bundle) {
  data <- .gflowui_basin_assert_pair(proposal, bundle)
  data$canonical[
    data$canonical$component == proposal$component$id,
    ,
    drop = FALSE
  ]
}

.gflowui_basin_selected_mass <- function(component.data, selected.ids) {
  groups <- .gflowui_basin_mass_groups(
    component.data$basin.id,
    component.data$trajectory.flow.mass,
    positive.only = TRUE
  )
  keep <- groups$ranked.ids %in% selected.ids
  .gflowui_basin_fixed_sum(groups$ranked.mass[keep])
}

gflowui_basin_derive_mass <- function(proposal, bundle) {
  component.data <- .gflowui_basin_proposal_component(proposal, bundle)
  if (proposal$mass.status != "valid") {
    return(list(
      available = FALSE,
      unavailable.reason = proposal$mass.status,
      positive.groups = NULL,
      all.mass.groups = NULL,
      denominator = NULL,
      positive.count = NULL,
      zero.count = NULL,
      core.coverage = NULL,
      final.coverage = NULL
    ))
  }
  positive <- .gflowui_basin_mass_groups(
    component.data$basin.id,
    component.data$trajectory.flow.mass,
    positive.only = TRUE
  )
  all.mass <- .gflowui_basin_mass_groups(
    component.data$basin.id,
    component.data$trajectory.flow.mass,
    positive.only = FALSE
  )
  denominator <- .gflowui_basin_fixed_sum(positive$ranked.mass)
  list(
    available = TRUE,
    unavailable.reason = NULL,
    positive.groups = positive,
    all.mass.groups = all.mass,
    denominator = denominator,
    positive.count = length(positive$ranked.ids),
    zero.count = sum(component.data$trajectory.flow.mass == 0),
    core.coverage =
      .gflowui_basin_selected_mass(component.data, proposal$core$ids) /
        denominator,
    final.coverage =
      .gflowui_basin_selected_mass(component.data, proposal$final.ids) /
        denominator
  )
}

gflowui_basin_derive_counts <- function(proposal, bundle) {
  .gflowui_basin_assert_pair(proposal, bundle)
  precedence <- c(
    "pinned",
    "component_survivor",
    "peak",
    "prominence",
    "support"
  )
  reasons <- proposal$sentinels$reasons
  sentinel.only.ids <- setdiff(
    proposal$sentinels$ids,
    proposal$core$ids
  )
  sentinel.only.reasons <- reasons[sentinel.only.ids]
  primary <- vapply(sentinel.only.reasons, function(value) {
    precedence[precedence %in% value][[1L]]
  }, character(1))
  list(
    component = length(proposal$component$ids),
    core = length(proposal$core$ids),
    sentinel = length(proposal$sentinels$ids),
    sentinel.only = length(sentinel.only.ids),
    ancestor.only = length(proposal$ancestor.only.ids),
    final = length(proposal$final.ids),
    reason.counts = table(factor(
      unlist(reasons, use.names = FALSE),
      levels = precedence
    )),
    primary.reason.counts = table(factor(
      primary,
      levels = precedence
    )),
    primary.reasons = primary
  )
}

gflowui_basin_derive_diagnostics <- function(proposal, bundle) {
  component.data <- .gflowui_basin_proposal_component(proposal, bundle)
  mass <- gflowui_basin_derive_mass(proposal, bundle)
  if (!mass$available) {
    return(list(
      available = FALSE,
      unavailable.reason = mass$unavailable.reason,
      log10.mass = NULL,
      ranked = NULL,
      cumulative = NULL,
      zero.count = NULL
    ))
  }
  list(
    available = TRUE,
    unavailable.reason = NULL,
    log10.mass = log10(mass$positive.groups$ranked.mass),
    ranked = data.frame(
      rank = seq_along(mass$positive.groups$ranked.ids),
      basin.id = mass$positive.groups$ranked.ids,
      log10.mass = log10(mass$positive.groups$ranked.mass),
      stringsAsFactors = FALSE
    ),
    cumulative = .gflowui_basin_group_coverage(
      mass$positive.groups
    )$cumulative,
    zero.count = sum(component.data$trajectory.flow.mass == 0)
  )
}

gflowui_basin_validate_presentation <- function(important.label.n,
                                                label.mode) {
  count <- .gflowui_basin_validate_integer(
    important.label.n,
    "important.label.n",
    minimum = 0L
  )
  mode.valid <- .gflowui_basin_scalar_string(label.mode) &&
    label.mode %in% c(
      "important",
      "selected",
      "displayed",
      "none",
      "all"
    )
  if (!count$valid || !mode.valid) {
    return(list(
      valid = FALSE,
      messages = c(
        if (!count$valid) count$message else character(),
        if (!mode.valid) "'label.mode' is unsupported." else character()
      )
    ))
  }
  list(
    valid = TRUE,
    important.label.n = count$value,
    label.mode = label.mode,
    messages = character()
  )
}

gflowui_basin_derive_labels <- function(
    proposal,
    bundle,
    important.label.n = 6L,
    label.mode = "important",
    selected.ids = character()) {
  presentation <- gflowui_basin_validate_presentation(
    important.label.n,
    label.mode
  )
  if (!presentation$valid) {
    .gflowui_basin_stop(
      paste(presentation$messages, collapse = " "),
      "gflowui_basin_presentation_error"
    )
  }
  component.data <- .gflowui_basin_proposal_component(proposal, bundle)
  displayed <- proposal$final.ids
  selected.displayed <- intersect(selected.ids, displayed)
  selected.hidden <- setdiff(
    intersect(selected.ids, proposal$component$ids),
    displayed
  )
  if (label.mode == "none") {
    return(list(
      ids = character(),
      contributions = list(),
      omissions = character(),
      warning = NULL,
      selected.hidden = selected.hidden
    ))
  }
  if (label.mode %in% c("displayed", "all")) {
    return(list(
      ids = displayed,
      contributions = list(displayed = length(displayed)),
      omissions = character(),
      warning = if (label.mode == "all") {
        "All displayed branch labels may be crowded."
      } else {
        NULL
      },
      selected.hidden = selected.hidden
    ))
  }
  if (label.mode == "selected") {
    return(list(
      ids = sort(selected.displayed, method = "radix"),
      contributions = list(selected = length(selected.displayed)),
      omissions = if (length(selected.hidden)) {
        "Selected hidden branches are not part of the static layout."
      } else {
        character()
      },
      warning = NULL,
      selected.hidden = selected.hidden
    ))
  }
  displayed.data <- component.data[
    match(displayed, component.data$basin.id),
    ,
    drop = FALSE
  ]
  count <- presentation$important.label.n
  contributions <- list(
    trajectory_flow_mass = if (proposal$mass.status == "valid") {
      .gflowui_basin_top_with_ties(
        displayed.data$basin.id,
        displayed.data$trajectory.flow.mass,
        count
      )
    } else {
      character()
    },
    peak = .gflowui_basin_top_with_ties(
      displayed.data$basin.id,
      displayed.data$peak.value,
      count
    ),
    prominence = .gflowui_basin_top_with_ties(
      displayed.data$basin.id,
      displayed.data$persistence,
      count
    ),
    support = .gflowui_basin_top_with_ties(
      displayed.data$basin.id,
      displayed.data$trajectory.flow.support,
      count
    ),
    component_survivor = displayed.data$basin.id[
      is.na(displayed.data$parent.basin.id)
    ],
    pinned = intersect(proposal$pinned.ids, displayed),
    selected = selected.displayed
  )
  list(
    ids = sort(unique(unlist(contributions)), method = "radix"),
    contributions = lapply(contributions, length),
    omissions = c(
      if (proposal$mass.status != "valid") {
        sprintf(
          "Trajectory-flow mass labels unavailable: %s.",
          proposal$mass.status
        )
      } else {
        character()
      },
      if (length(selected.hidden)) {
        "Selected hidden branches are not part of the static layout."
      } else {
        character()
      }
    ),
    warning = NULL,
    selected.hidden = selected.hidden
  )
}

gflowui_basin_derive_layout <- function(
    proposal,
    bundle,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  data <- .gflowui_basin_assert_pair(proposal, bundle)
  layout.accessor(
    data$canonical.tree,
    direction = "max",
    component = proposal$component$id,
    basin.ids = proposal$final.ids,
    close.ancestors = FALSE
  )
}

gflowui_basin_complete_layout <- function(bundle, component = NULL) {
  .gflowui_basin_assert_bundle(bundle)
  data <- gflowui_basin_bundle_snapshot(bundle)
  if (is.null(component)) {
    component <- data$component.selection$id
  }
  gflow::get.basin.merge.tree.layout(
    data$canonical.tree,
    direction = "max",
    component = component
  )
}

gflowui_basin_derive_status <- function(proposal, bundle) {
  counts <- gflowui_basin_derive_counts(proposal, bundle)
  mass <- gflowui_basin_derive_mass(proposal, bundle)
  list(
    text = sprintf(
      paste(
        "%d of %d maximum basins in component %s;",
        "core %s; render %s."
      ),
      counts$final,
      counts$component,
      proposal$component$id,
      proposal$core$outcome,
      proposal$render.outcome
    ),
    coverage = mass$final.coverage,
    warnings = proposal$core$warnings,
    mass.owner = "trajectory-flow primary.support.mass"
  )
}

.gflowui_basin_recipe_fields <- function(mode) {
  c(
    "recipe.version",
    "filter.mode",
    "final.render.budget",
    "sentinel.top.n",
    "peak.sentinel.enabled",
    "prominence.sentinel.enabled",
    "support.sentinel.enabled",
    switch(
      mode,
      auto = c(
        "coverage.target",
        "strong.gap.decades",
        "core.branch.budget"
      ),
      cumulative_mass = c("coverage.target", "core.branch.budget"),
      minimum_mass = "minimum.mass",
      top_k = "top.k",
      none = character(),
      character()
    ),
    "important.label.n",
    "label.mode"
  )
}

gflowui_basin_recipe <- function(controls, component.size) {
  validated <- gflowui_basin_validate_controls(controls, component.size)
  if (!validated$valid || !isTRUE(validated$presentation$valid)) {
    .gflowui_basin_stop(
      paste(
        c(validated$messages, validated$presentation$messages),
        collapse = " "
      ),
      "gflowui_basin_recipe_error"
    )
  }
  values <- c(
    list(recipe.version = 1L),
    validated$accepted.parameters,
    validated$presentation[c("important.label.n", "label.mode")]
  )
  values[.gflowui_basin_recipe_fields(controls$filter.mode)]
}

.gflowui_basin_recipe_from_transport <- function(recipe) {
  if (!is.list(recipe)) {
    return(recipe)
  }
  integer.fields <- c(
    "recipe.version",
    "final.render.budget",
    "sentinel.top.n",
    "core.branch.budget",
    "top.k",
    "important.label.n"
  )
  for (name in intersect(integer.fields, names(recipe))) {
    value <- recipe[[name]]
    if (is.numeric(value) &&
        length(value) == 1L &&
        is.finite(value) &&
        value == floor(value)) {
      recipe[[name]] <- as.integer(value)
    }
  }
  recipe
}

.gflowui_basin_recipe_runtime <- function(recipe,
                                          bundle,
                                          context.generation = 1L) {
  .gflowui_basin_assert_bundle(bundle)
  if (!is.list(recipe) ||
      !identical(recipe$recipe.version, 1L) ||
      !.gflowui_basin_scalar_string(recipe$filter.mode)) {
    .gflowui_basin_stop(
      "The basin-analysis recipe version or filter mode is unsupported.",
      "gflowui_basin_recipe_error"
    )
  }
  expected <- .gflowui_basin_recipe_fields(recipe$filter.mode)
  if (!length(expected) ||
      !identical(sort(names(recipe)), sort(expected))) {
    .gflowui_basin_stop(
      "The basin-analysis recipe contains unknown or missing features.",
      "gflowui_basin_recipe_error"
    )
  }
  context <- gflowui_basin_context(
    bundle,
    context.generation = context.generation
  )
  component.size <- sum(
    gflowui_basin_bundle_snapshot(bundle)$canonical$component ==
      context$component
  )
  controls <- gflowui_basin_default_controls(component.size)
  for (name in setdiff(names(recipe), "recipe.version")) {
    controls[[name]] <- recipe[[name]]
  }
  validated <- gflowui_basin_validate_controls(controls, component.size)
  if (!validated$valid || !isTRUE(validated$presentation$valid)) {
    .gflowui_basin_stop(
      paste(
        c(validated$messages, validated$presentation$messages),
        collapse = " "
      ),
      "gflowui_basin_recipe_error"
    )
  }
  list(
    context = context,
    controls = controls,
    presentation = validated$presentation[
      c("important.label.n", "label.mode")
    ]
  )
}

gflowui_basin_restore_recipe <- function(
    recipe,
    bundle,
    context.generation = 1L,
    attempt.id = 1L) {
  runtime <- .gflowui_basin_recipe_runtime(
    recipe,
    bundle,
    context.generation = context.generation
  )
  list(
    context = runtime$context,
    controls = runtime$controls,
    presentation = runtime$presentation[
      c("important.label.n", "label.mode")
    ],
    attempt = gflowui_basin_construct_proposal(
      runtime$context,
      bundle,
      runtime$controls,
      pinned.ids = character(),
      attempt.id = attempt.id
    )
  )
}
