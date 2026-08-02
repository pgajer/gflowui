.gflowui_basin_state_stop <- function(message) {
  .gflowui_basin_stop(message, "gflowui_basin_state_error")
}

.gflowui_basin_state_display_source <- function(state) {
  if (!is.null(state$current.proposal)) {
    "current"
  } else if (!is.null(state$retained.last.valid.proposal)) {
    "retained_last_valid"
  } else {
    "none"
  }
}

.gflowui_basin_component_size <- function(bundle, context) {
  data <- gflowui_basin_bundle_snapshot(bundle)
  sum(data$canonical$component == context$component)
}

.gflowui_basin_attempt_validation <- function(state) {
  data <- gflowui_basin_bundle_snapshot(state$bundle)
  context.valid <- .gflowui_basin_validate_context(
    state$context,
    state$bundle
  )
  settings <- gflowui_basin_validate_controls(
    state$controls,
    .gflowui_basin_component_size(state$bundle, state$context)
  )
  validation <- list(
    identity = if (context.valid) "current" else "stale",
    bundle = if (identical(state$bundle.id, state$bundle$bundle.id)) {
      "valid"
    } else {
      "bundle_mismatch"
    },
    source = data$validation$source,
    mapping = data$validation$mapping,
    ranking.measure = list(
      trajectory_flow_mass =
        data$validation$trajectory_flow_mass,
      trajectory_flow_support =
        data$validation$trajectory_flow_support,
      source_peak = data$validation$source_peak,
      canonical_prominence =
        data$validation$canonical_prominence
    ),
    proposal.settings = if (settings$valid) {
      "valid"
    } else {
      "settings_invalid"
    }
  )
  if (!context.valid) {
    return(list(
      valid = FALSE,
      scientific.invalid = TRUE,
      reason = "stale",
      messages = "The runtime context is stale.",
      validation = validation
    ))
  }
  if (!identical(validation$bundle, "valid")) {
    return(list(
      valid = FALSE,
      scientific.invalid = TRUE,
      reason = validation$bundle,
      messages = "The runtime bundle identity is invalid.",
      validation = validation
    ))
  }
  scientific <- c(
    source = validation$source,
    mapping = validation$mapping,
    support =
      validation$ranking.measure$trajectory_flow_support,
    peak = validation$ranking.measure$source_peak,
    prominence =
      validation$ranking.measure$canonical_prominence
  )
  invalid.scientific <- scientific[scientific != "valid"]
  if (length(invalid.scientific)) {
    return(list(
      valid = FALSE,
      scientific.invalid = TRUE,
      reason = unname(invalid.scientific[[1L]]),
      messages = sprintf(
        "%s: %s",
        names(invalid.scientific)[[1L]],
        invalid.scientific[[1L]]
      ),
      validation = validation
    ))
  }
  if (!settings$valid) {
    return(list(
      valid = FALSE,
      scientific.invalid = FALSE,
      reason = "settings_invalid",
      messages = settings$messages,
      validation = validation
    ))
  }
  mass.status <-
    validation$ranking.measure$trajectory_flow_mass
  if (settings$accepted.parameters$filter.mode != "none" &&
      mass.status != "valid") {
    return(list(
      valid = FALSE,
      scientific.invalid = TRUE,
      reason = mass.status,
      messages = sprintf(
        "trajectory_flow_mass: %s",
        mass.status
      ),
      validation = validation
    ))
  }
  data <- data$canonical[
    data$canonical$component == state$context$component,
    ,
    drop = FALSE
  ]
  pins.valid <- is.character(state$pinned.ids) &&
    !anyNA(state$pinned.ids) &&
    !anyDuplicated(state$pinned.ids) &&
    all(state$pinned.ids %in% data$basin.id)
  if (!pins.valid) {
    return(list(
      valid = FALSE,
      scientific.invalid = FALSE,
      reason = "pins_invalid",
      messages = paste(
        "Pinned IDs must be unique canonical IDs in the",
        "selected component."
      ),
      validation = validation
    ))
  }
  list(
    valid = TRUE,
    scientific.invalid = FALSE,
    reason = NULL,
    messages = character(),
    validation = validation
  )
}

.gflowui_basin_assert_runtime_state <- function(state) {
  required <- c(
    "bundle.id",
    "context.generation",
    "next.attempt.id",
    "active.attempt",
    "current.proposal",
    "retained.last.valid.proposal",
    "pinned.ids",
    "selected.ids",
    "display.source",
    "bundle",
    "context",
    "controls",
    "presentation",
    "caches",
    "pending.work"
  )
  if (!is.list(state) ||
      !all(required %in% names(state))) {
    .gflowui_basin_state_stop(
      "A complete basin-analysis runtime state is required."
    )
  }
  .gflowui_basin_assert_bundle(state$bundle)
  next.id <- .gflowui_basin_validate_integer(
    state$next.attempt.id,
    "next.attempt.id",
    minimum = 1L
  )
  valid <- identical(state$bundle.id, state$bundle$bundle.id) &&
    identical(
      state$context.generation,
      state$context$context.generation
    ) &&
    .gflowui_basin_validate_context(state$context, state$bundle) &&
    next.id$valid &&
    is.character(state$pinned.ids) &&
    !anyNA(state$pinned.ids) &&
    !anyDuplicated(state$pinned.ids) &&
    is.character(state$selected.ids) &&
    !anyNA(state$selected.ids) &&
    !anyDuplicated(state$selected.ids) &&
    is.list(state$controls) &&
    is.list(state$presentation) &&
    is.list(state$caches) &&
    !(is.null(state$current.proposal) == FALSE &&
      is.null(state$retained.last.valid.proposal) == FALSE) &&
    identical(
      state$display.source,
      .gflowui_basin_state_display_source(state)
    )
  if (!valid) {
    .gflowui_basin_state_stop(
      "The basin-analysis runtime state violates its ownership contract."
    )
  }
  if (!is.null(state$active.attempt)) {
    active <- state$active.attempt
    active.valid <- is.list(active) &&
      identical(active$bundle.id, state$bundle.id) &&
      identical(
        active$context.generation,
        state$context.generation
      ) &&
      is.integer(active$attempt.id) &&
      length(active$attempt.id) == 1L &&
      active$attempt.id < state$next.attempt.id &&
      active$outcome %in% c(
        "pending",
        "proposal_created",
        "blocked",
        "construction_failed",
        "stale"
      )
    if (!active.valid) {
      .gflowui_basin_state_stop(
        "The active basin-analysis attempt is invalid."
      )
    }
  }
  if (!is.null(state$pending.work)) {
    pending <- state$pending.work
    pending.valid <- !is.null(state$active.attempt) &&
      identical(state$active.attempt$outcome, "pending") &&
      identical(pending$bundle.id, state$active.attempt$bundle.id) &&
      identical(
        pending$context.generation,
        state$active.attempt$context.generation
      ) &&
      identical(
        pending$attempt.id,
        state$active.attempt$attempt.id
      )
    if (!pending.valid) {
      .gflowui_basin_state_stop(
        "Pending work does not belong to the active attempt."
      )
    }
  }
  invisible(TRUE)
}

.gflowui_basin_allocate_attempt <- function(state, cause) {
  if (state$next.attempt.id >= .Machine$integer.max) {
    .gflowui_basin_state_stop(
      "The basin-analysis attempt counter is exhausted."
    )
  }
  attempt.id <- as.integer(state$next.attempt.id)
  state$next.attempt.id <- as.integer(attempt.id + 1L)
  state$active.attempt <- list(
    bundle.id = state$bundle.id,
    context.generation = state$context.generation,
    attempt.id = attempt.id,
    cause = cause,
    validation = NULL,
    outcome = "pending",
    reason = NULL,
    messages = character()
  )
  state["pending.work"] <- list(NULL)
  state
}

.gflowui_basin_move_current_to_retained <- function(state) {
  if (!is.null(state$current.proposal)) {
    state$retained.last.valid.proposal <- state$current.proposal
    state["current.proposal"] <- list(NULL)
  }
  state$display.source <- .gflowui_basin_state_display_source(state)
  state
}

.gflowui_basin_clear_scientific_display <- function(state) {
  state["current.proposal"] <- list(NULL)
  state["retained.last.valid.proposal"] <- list(NULL)
  state$pinned.ids <- character()
  state$selected.ids <- character()
  state$caches <- list()
  state["pending.work"] <- list(NULL)
  state$display.source <- "none"
  state
}

.gflowui_basin_advance_generation <- function(state,
                                               bundle = state$bundle,
                                               component = NULL) {
  generation <- state$context.generation + 1
  if (generation > .Machine$integer.max) {
    .gflowui_basin_state_stop(
      "The basin-analysis context generation is exhausted."
    )
  }
  state$bundle <- bundle
  state$bundle.id <- bundle$bundle.id
  state$context <- gflowui_basin_context(
    bundle,
    context.generation = generation,
    component = component
  )
  state$context.generation <- state$context$context.generation
  if (!is.null(state$active.attempt)) {
    state$active.attempt$bundle.id <- state$bundle.id
    state$active.attempt$context.generation <-
      state$context.generation
  }
  .gflowui_basin_clear_scientific_display(state)
}

.gflowui_basin_block_active <- function(state,
                                        validation,
                                        reason,
                                        messages) {
  state$active.attempt$validation <- validation
  state$active.attempt$outcome <- "blocked"
  state$active.attempt$reason <- reason
  state$active.attempt$messages <- as.character(messages)
  state["pending.work"] <- list(NULL)
  state$display.source <- .gflowui_basin_state_display_source(state)
  state
}

.gflowui_basin_prepare_active <- function(state,
                                          advance.on.scientific = TRUE) {
  checked <- .gflowui_basin_attempt_validation(state)
  if (!checked$valid &&
      checked$scientific.invalid &&
      advance.on.scientific) {
    component <- state$context$component
    state <- .gflowui_basin_advance_generation(
      state,
      component = component
    )
    checked <- .gflowui_basin_attempt_validation(state)
  }
  if (!checked$valid) {
    return(.gflowui_basin_block_active(
      state,
      checked$validation,
      checked$reason,
      checked$messages
    ))
  }
  state$active.attempt$validation <- checked$validation
  state$active.attempt$outcome <- "pending"
  state$pending.work <- list(
    bundle.id = state$bundle.id,
    context.generation = state$context.generation,
    attempt.id = state$active.attempt$attempt.id,
    bundle = state$bundle,
    context = state$context,
    controls = .gflowui_basin_copy(state$controls),
    pinned.ids = state$pinned.ids
  )
  state$display.source <- .gflowui_basin_state_display_source(state)
  state
}

.gflowui_basin_start_same_context <- function(state, cause) {
  state <- .gflowui_basin_allocate_attempt(state, cause)
  state <- .gflowui_basin_move_current_to_retained(state)
  .gflowui_basin_prepare_active(state)
}

.gflowui_basin_start_context <- function(state,
                                         cause,
                                         bundle = state$bundle,
                                         component = NULL) {
  state <- .gflowui_basin_allocate_attempt(state, cause)
  state <- .gflowui_basin_advance_generation(
    state,
    bundle = bundle,
    component = component
  )
  .gflowui_basin_prepare_active(
    state,
    advance.on.scientific = FALSE
  )
}

.gflowui_basin_mode_fields <- function(mode) {
  switch(
    mode,
    auto = c(
      "coverage.target",
      "strong.gap.decades",
      "core.branch.budget"
    ),
    cumulative_mass = c(
      "coverage.target",
      "core.branch.budget"
    ),
    minimum_mass = "minimum.mass",
    top_k = "top.k",
    none = character(),
    character()
  )
}

.gflowui_basin_control_effect <- function(name, controls) {
  if (name %in% c("important.label.n", "label.mode")) {
    return("presentation")
  }
  if (name == "filter.mode" ||
      name %in% c(
        "final.render.budget",
        "sentinel.top.n",
        "peak.sentinel.enabled",
        "prominence.sentinel.enabled",
        "support.sentinel.enabled"
      ) ||
      name %in% .gflowui_basin_mode_fields(controls$filter.mode)) {
    return("proposal")
  }
  if (name %in% c(
    "coverage.target",
    "strong.gap.decades",
    "core.branch.budget",
    "minimum.mass",
    "top.k"
  )) {
    return("inactive")
  }
  "unknown"
}

.gflowui_basin_apply_control <- function(state, event) {
  if (!.gflowui_basin_scalar_string(event$name)) {
    .gflowui_basin_state_stop(
      "A basin-analysis control name is required."
    )
  }
  controls <- state$controls
  controls[[event$name]] <- event$value
  effect <- .gflowui_basin_control_effect(event$name, controls)
  if (effect == "unknown") {
    .gflowui_basin_state_stop(
      sprintf("Unknown basin-analysis control: %s.", event$name)
    )
  }
  if (effect == "presentation") {
    validated <- gflowui_basin_validate_controls(
      controls,
      .gflowui_basin_component_size(state$bundle, state$context)
    )
    if (isTRUE(validated$presentation$valid)) {
      state$controls <- controls
      state$presentation[
        c("important.label.n", "label.mode")
      ] <- validated$presentation[
        c("important.label.n", "label.mode")
      ]
    }
    return(state)
  }
  state$controls <- controls
  if (effect == "inactive") {
    return(state)
  }
  .gflowui_basin_start_same_context(
    state,
    sprintf("control:%s", event$name)
  )
}

.gflowui_basin_apply_pin <- function(state, event, remove = FALSE) {
  state <- .gflowui_basin_allocate_attempt(
    state,
    if (remove) "unpin" else "pin"
  )
  state <- .gflowui_basin_move_current_to_retained(state)
  if (!.gflowui_basin_scalar_string(event$id)) {
    validation <- .gflowui_basin_attempt_validation(state)$validation
    return(.gflowui_basin_block_active(
      state,
      validation,
      "pins_invalid",
      "A scalar canonical basin ID is required."
    ))
  }
  state$pinned.ids <- if (remove) {
    setdiff(state$pinned.ids, event$id)
  } else {
    sort(unique(c(state$pinned.ids, event$id)), method = "radix")
  }
  .gflowui_basin_prepare_active(state)
}

.gflowui_basin_apply_selection <- function(state, event) {
  ids <- event$ids
  data <- gflowui_basin_bundle_snapshot(state$bundle)$canonical
  component.ids <- data$basin.id[
    data$component == state$context$component
  ]
  if (!is.character(ids) ||
      anyNA(ids) ||
      anyDuplicated(ids) ||
      !all(ids %in% component.ids)) {
    return(state)
  }
  state$selected.ids <- sort(ids, method = "radix")
  state
}

.gflowui_basin_apply_recipe <- function(state, event) {
  state <- .gflowui_basin_allocate_attempt(state, "recipe_restore")
  state <- .gflowui_basin_move_current_to_retained(state)
  restored <- tryCatch(
    .gflowui_basin_recipe_runtime(
      event$recipe,
      state$bundle,
      context.generation = state$context.generation
    ),
    error = identity
  )
  if (inherits(restored, "error")) {
    validation <- .gflowui_basin_attempt_validation(state)$validation
    validation$proposal.settings <- "settings_invalid"
    return(.gflowui_basin_block_active(
      state,
      validation,
      "recipe_invalid",
      conditionMessage(restored)
    ))
  }
  component.changed <- !identical(
    restored$context$component,
    state$context$component
  )
  state$controls <- restored$controls
  state$presentation[
    c("important.label.n", "label.mode")
  ] <- restored$presentation
  state$pinned.ids <- character()
  state$selected.ids <- character()
  if (component.changed) {
    state <- .gflowui_basin_advance_generation(state)
  } else {
    state$context <- restored$context
  }
  .gflowui_basin_prepare_active(
    state,
    advance.on.scientific = !component.changed
  )
}

.gflowui_basin_install_result <- function(state, result) {
  active <- state$active.attempt
  matches <- is.list(result) &&
    !is.null(active) &&
    identical(active$outcome, "pending") &&
    !is.null(state$pending.work) &&
    identical(result$bundle.id, active$bundle.id) &&
    identical(
      result$context.generation,
      active$context.generation
    ) &&
    identical(result$attempt.id, active$attempt.id)
  if (!matches) {
    return(state)
  }
  state["pending.work"] <- list(NULL)
  successful <- identical(result$status, "proposal_created") &&
    inherits(result$proposal, "basin_display_proposal") &&
    identical(result$proposal$bundle.id, active$bundle.id) &&
    identical(
      result$proposal$context.generation,
      active$context.generation
    ) &&
    identical(result$proposal$attempt.id, active$attempt.id)
  if (successful) {
    state$current.proposal <-
      .gflowui_basin_copy(result$proposal)
    state["retained.last.valid.proposal"] <- list(NULL)
    state$caches <- list()
    state$active.attempt$outcome <- "proposal_created"
    state$active.attempt["reason"] <- list(NULL)
    state$active.attempt$messages <- character()
    state$display.source <- "current"
    return(state)
  }
  state["current.proposal"] <- list(NULL)
  state$active.attempt$outcome <- "construction_failed"
  state$active.attempt$reason <- if (
    .gflowui_basin_scalar_string(result$reason)
  ) {
    result$reason
  } else {
    "construction_failed"
  }
  state$active.attempt$messages <- as.character(result$messages)
  state$display.source <- .gflowui_basin_state_display_source(state)
  state
}

gflowui_basin_state_event <- function(type, ...) {
  if (!.gflowui_basin_scalar_string(type)) {
    .gflowui_basin_state_stop(
      "A basin-analysis event type is required."
    )
  }
  structure(
    c(list(type = type), list(...)),
    class = c("gflowui_basin_state_event", "list")
  )
}

gflowui_basin_reduce_state <- function(state, event) {
  .gflowui_basin_assert_runtime_state(state)
  if (!is.list(event) ||
      !.gflowui_basin_scalar_string(event$type)) {
    .gflowui_basin_state_stop(
      "A typed basin-analysis state event is required."
    )
  }
  next.state <- state
  if (event$type == "recompute") {
    next.state <- .gflowui_basin_start_same_context(
      next.state,
      "recompute"
    )
  } else if (event$type == "control_change") {
    next.state <- .gflowui_basin_apply_control(next.state, event)
  } else if (event$type == "pin") {
    next.state <- .gflowui_basin_apply_pin(next.state, event)
  } else if (event$type == "unpin") {
    next.state <- .gflowui_basin_apply_pin(
      next.state,
      event,
      remove = TRUE
    )
  } else if (event$type == "bundle_change") {
    .gflowui_basin_assert_bundle(event$bundle)
    next.state <- .gflowui_basin_start_context(
      next.state,
      "bundle_change",
      bundle = event$bundle
    )
  } else if (event$type == "component_change") {
    next.state <- .gflowui_basin_start_context(
      next.state,
      "component_change",
      component = event$component
    )
  } else if (event$type == "selection_change") {
    next.state <- .gflowui_basin_apply_selection(
      next.state,
      event
    )
  } else if (event$type == "diagnostic_visibility") {
    if (is.logical(event$visible) &&
        length(event$visible) == 1L &&
        !is.na(event$visible)) {
      next.state$presentation$diagnostics.visible <- event$visible
    }
  } else if (event$type == "open_viewer") {
    next.state <- next.state
  } else if (event$type == "recipe_restore") {
    next.state <- .gflowui_basin_apply_recipe(next.state, event)
  } else if (event$type == "result") {
    next.state <- .gflowui_basin_install_result(
      next.state,
      event$result
    )
  } else {
    .gflowui_basin_state_stop(
      sprintf("Unsupported basin-analysis event: %s.", event$type)
    )
  }
  next.state$display.source <-
    .gflowui_basin_state_display_source(next.state)
  .gflowui_basin_assert_runtime_state(next.state)
  next.state
}

gflowui_basin_execute_pending <- function(
    pending.work,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  required <- c(
    "bundle.id",
    "context.generation",
    "attempt.id",
    "bundle",
    "context",
    "controls",
    "pinned.ids"
  )
  if (!is.list(pending.work) ||
      !all(required %in% names(pending.work)) ||
      !identical(
        pending.work$bundle.id,
        pending.work$bundle$bundle.id
      )) {
    .gflowui_basin_state_stop(
      "A complete pending basin-analysis attempt is required."
    )
  }
  gflowui_basin_construct_proposal(
    pending.work$context,
    pending.work$bundle,
    pending.work$controls,
    pinned.ids = pending.work$pinned.ids,
    attempt.id = pending.work$attempt.id,
    layout.accessor = layout.accessor
  )
}

gflowui_basin_displayed_proposal <- function(state) {
  .gflowui_basin_assert_runtime_state(state)
  proposal <- if (state$display.source == "current") {
    state$current.proposal
  } else if (state$display.source == "retained_last_valid") {
    state$retained.last.valid.proposal
  } else {
    NULL
  }
  if (is.null(proposal)) NULL else .gflowui_basin_copy(proposal)
}
