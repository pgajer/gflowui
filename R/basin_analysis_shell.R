.gflowui_basin_shell_stop <- function(message,
                                      class = "gflowui_basin_shell_error") {
  .gflowui_basin_stop(message, class)
}

.gflowui_basin_shell_string <- function(value, fallback) {
  value <- as.character(value %||% "")
  if (length(value) == 1L && !is.na(value) && nzchar(value)) {
    enc2utf8(value)
  } else {
    enc2utf8(as.character(fallback))
  }
}

.gflowui_basin_shell_identity <- function(result, request) {
  construction <- request$construction_identity
  record <- construction$record
  graph <- record$graph
  build.id <- .gflowui_basin_shell_string(
    record$gflow.build.id,
    result$build_identity$build.id %||% "gflow-build-unavailable"
  )
  source.id <- .gflowui_basin_shell_string(
    request$alignment$source.id,
    request$source$key %||% "source-unavailable"
  )
  construction.fingerprint <- .gflowui_basin_shell_string(
    construction$fingerprint,
    "construction-unavailable"
  )
  list(
    project = .gflowui_basin_shell_string(
      record$project.id,
      "project-unavailable"
    ),
    subject = source.id,
    graph = .gflowui_basin_shell_string(
      graph$graph.id,
      graph$graph.fingerprint %||% "graph-unavailable"
    ),
    topology = .gflowui_basin_shell_string(
      graph$graph.fingerprint,
      "topology-unavailable"
    ),
    vertex_map = .gflowui_basin_shell_string(
      graph$vertex.id.fingerprint,
      "vertex-map-unavailable"
    ),
    field = .gflowui_basin_shell_string(
      record$field.fingerprint,
      "field-unavailable"
    ),
    source = .gflowui_basin_shell_string(
      record$source.key,
      source.id
    ),
    estimate = .gflowui_basin_shell_string(
      construction.fingerprint,
      request$source_fingerprint %||% source.id
    ),
    trajectory_construction = gflowui_basin_sha256(list(
      construction.fingerprint = construction.fingerprint,
      build.id = build.id,
      method = record$construction
    )),
    canonical_tree_construction = gflowui_basin_sha256(list(
      construction.fingerprint = construction.fingerprint,
      build.id = build.id,
      method = "superlevel_merge_tree",
      direction = "max"
    ))
  )
}

gflowui_basin_bundle_from_overlay <- function(result, request) {
  required.result <- c("basin", "prominence_complex")
  required.request <- c(
    "source",
    "vertex_id",
    "alignment",
    "source_fingerprint",
    "construction_identity"
  )
  if (!is.list(result) ||
      !all(required.result %in% names(result)) ||
      !is.list(request) ||
      !all(required.request %in% names(request)) ||
      !is.list(request$source) ||
      !is.list(request$source$graph)) {
    .gflowui_basin_shell_stop(
      "A complete canonical construction and source request are required.",
      "gflowui_basin_shell_input_error"
    )
  }
  if (!inherits(result$basin, "basin_complex") ||
      !identical(as.character(result$basin$status %||% ""), "ok")) {
    .gflowui_basin_shell_stop(
      "The trajectory-flow basin construction is malformed.",
      "gflowui_basin_shell_input_error"
    )
  }
  tree <- tryCatch(
    gflow::get.basin.merge.tree(result$prominence_complex),
    error = identity
  )
  if (inherits(tree, "error")) {
    .gflowui_basin_shell_stop(
      sprintf(
        "The canonical maximum merge tree is unavailable: %s",
        conditionMessage(tree)
      ),
      "gflowui_basin_shell_input_error"
    )
  }
  trajectory <- result$basin$basin.table
  required.trajectory <- c(
    "basin.id",
    "type",
    "extremum.vertex",
    "primary.support.mass",
    "primary.support.size"
  )
  if (!is.data.frame(trajectory) ||
      !all(required.trajectory %in% names(trajectory))) {
    .gflowui_basin_shell_stop(
      "The trajectory-flow basin table is incomplete.",
      "gflowui_basin_shell_input_error"
    )
  }
  if (!is.character(trajectory$basin.id) ||
      anyNA(trajectory$basin.id) ||
      any(!nzchar(trajectory$basin.id)) ||
      !is.character(trajectory$type) ||
      anyNA(trajectory$type) ||
      !is.numeric(trajectory$extremum.vertex) ||
      anyNA(trajectory$extremum.vertex) ||
      any(!is.finite(trajectory$extremum.vertex)) ||
      any(trajectory$extremum.vertex !=
        floor(trajectory$extremum.vertex)) ||
      !is.numeric(trajectory$primary.support.mass) ||
      !is.numeric(trajectory$primary.support.size)) {
    .gflowui_basin_shell_stop(
      "The trajectory-flow mapping and ranking columns are malformed.",
      "gflowui_basin_shell_input_error"
    )
  }
  source.values <- request$source$values
  vertex.ids <- request$vertex_id
  if (!is.numeric(source.values) ||
      anyNA(source.values) ||
      any(!is.finite(source.values)) ||
      !is.character(vertex.ids) ||
      length(vertex.ids) != length(source.values) ||
      anyNA(vertex.ids) ||
      any(!nzchar(vertex.ids)) ||
      anyDuplicated(vertex.ids)) {
    .gflowui_basin_shell_stop(
      "The reviewed source vector or vertex mapping is malformed.",
      "gflowui_basin_shell_input_error"
    )
  }
  trajectory <- trajectory[
    trajectory$type == "max",
    ,
    drop = FALSE
  ]
  component <- tree$graph.input$validation$component
  extrema <- as.integer(trajectory$extremum.vertex)
  if (!length(extrema) ||
      anyNA(extrema) ||
      any(extrema < 1L | extrema > length(component))) {
    .gflowui_basin_shell_stop(
      "Trajectory extrema do not align with canonical graph components.",
      "gflowui_basin_shell_input_error"
    )
  }
  trajectory.table <- data.frame(
    trajectory.basin.id = trajectory$basin.id,
    direction = trajectory$type,
    component = as.integer(component[extrema]),
    extremum.vertex = extrema,
    primary.support.mass = trajectory$primary.support.mass,
    primary.support.size = trajectory$primary.support.size,
    stringsAsFactors = FALSE
  )
  gflowui_basin_new_scientific_bundle(
    graph = request$source$graph$adj_list,
    vertex.ids = vertex.ids,
    source.values = stats::setNames(source.values, vertex.ids),
    identity = .gflowui_basin_shell_identity(result, request),
    trajectory.table = trajectory.table,
    canonical.tree = tree,
    direction = "max"
  )
}

gflowui_basin_try_bundle_from_overlay <- function(result, request) {
  bundle <- tryCatch(
    gflowui_basin_bundle_from_overlay(result, request),
    error = identity
  )
  if (inherits(bundle, "error")) {
    condition.class <- class(bundle)
    expected.input.error <- any(grepl(
      "^gflowui_basin_(shell_input|bundle|policy|mapping|tree|source)",
      condition.class
    ))
    return(list(
      ok = FALSE,
      bundle = NULL,
      diagnostic = list(
        kind = if (expected.input.error) {
          "bundle_input_invalid"
        } else {
          "internal_bundle_assembly_failure"
        },
        message = conditionMessage(bundle),
        condition.class = condition.class
      )
    ))
  }
  list(
    ok = TRUE,
    bundle = bundle,
    diagnostic = NULL
  )
}

gflowui_basin_async_job <- function(state,
                                    session.id,
                                    construction.fingerprint = "") {
  .gflowui_basin_assert_runtime_state(state)
  if (!.gflowui_basin_scalar_string(session.id) ||
      is.null(state$pending.work) ||
      is.null(state$active.attempt) ||
      !identical(state$active.attempt$outcome, "pending")) {
    .gflowui_basin_shell_stop(
      "A pending attempt and scalar session identity are required."
    )
  }
  keys <- list(
    bundle.id = state$active.attempt$bundle.id,
    context.generation = state$active.attempt$context.generation,
    attempt.id = state$active.attempt$attempt.id
  )
  structure(
    list(
      job.id = digest::digest(
        c(
          list(session.id = session.id),
          keys,
          list(construction.fingerprint = construction.fingerprint)
        ),
        algo = "sha256",
        serialize = TRUE
      ),
      session.id = session.id,
      construction.fingerprint =
        as.character(construction.fingerprint %||% ""),
      bundle.id = keys$bundle.id,
      context.generation = keys$context.generation,
      attempt.id = keys$attempt.id,
      pending.work = .gflowui_basin_copy(state$pending.work)
    ),
    class = c("gflowui_basin_async_job", "list")
  )
}

.gflowui_basin_async_failure_result <- function(job,
                                                reason,
                                                messages) {
  structure(
    list(
      status = "construction_failed",
      reason = reason,
      messages = as.character(messages),
      bundle.id = job$bundle.id,
      context.generation = job$context.generation,
      attempt.id = job$attempt.id,
      proposal = NULL
    ),
    class = c("gflowui_basin_attempt_result", "list")
  )
}

gflowui_basin_async_completion <- function(job,
                                           result,
                                           diagnostic = NULL) {
  if (!inherits(job, "gflowui_basin_async_job")) {
    .gflowui_basin_shell_stop("A typed basin-analysis job is required.")
  }
  structure(
    list(
      job.id = job$job.id,
      session.id = job$session.id,
      construction.fingerprint = job$construction.fingerprint,
      result = result,
      diagnostic = diagnostic
    ),
    class = c("gflowui_basin_async_completion", "list")
  )
}

gflowui_basin_execute_async_job <- function(job) {
  if (!inherits(job, "gflowui_basin_async_job")) {
    .gflowui_basin_shell_stop("A typed basin-analysis job is required.")
  }
  result <- tryCatch(
    gflowui_basin_execute_pending(job$pending.work),
    error = identity
  )
  diagnostic <- NULL
  if (inherits(result, "error")) {
    diagnostic <- list(
      kind = "internal_execution_failure",
      message = conditionMessage(result),
      condition.class = class(result)
    )
    result <- .gflowui_basin_async_failure_result(
      job,
      "construction_failed",
      conditionMessage(result)
    )
  }
  gflowui_basin_async_completion(job, result, diagnostic)
}

gflowui_basin_install_async_completion <- function(
    state,
    completion,
    session.id,
    session.active = TRUE,
    construction.fingerprint = NULL) {
  completion.diagnostic <- if (is.list(completion)) {
    completion$diagnostic %||% NULL
  } else {
    NULL
  }
  unchanged <- function(disposition) {
    list(
      state = state,
      disposition = disposition,
      installed = FALSE,
      diagnostic = completion.diagnostic
    )
  }
  if (!isTRUE(session.active)) {
    return(unchanged("session_closed"))
  }
  if (!inherits(completion, "gflowui_basin_async_completion") ||
      !.gflowui_basin_scalar_string(session.id) ||
      !identical(completion$session.id, session.id)) {
    return(unchanged("stale_session"))
  }
  if (is.null(state)) {
    return(unchanged("state_invalidated"))
  }
  if (!is.null(construction.fingerprint) &&
      !identical(
        as.character(completion$construction.fingerprint),
        as.character(construction.fingerprint)
      )) {
    return(unchanged("stale_construction"))
  }
  .gflowui_basin_assert_runtime_state(state)
  active <- state$active.attempt
  result <- completion$result
  matches <- is.list(active) &&
    identical(active$outcome, "pending") &&
    is.list(result) &&
    identical(result$bundle.id, active$bundle.id) &&
    identical(
      result$context.generation,
      active$context.generation
    ) &&
    identical(result$attempt.id, active$attempt.id)
  if (!matches) {
    return(unchanged("stale_attempt"))
  }
  next.state <- gflowui_basin_reduce_state(
    state,
    gflowui_basin_state_event("result", result = result)
  )
  list(
    state = next.state,
    disposition = if (
      identical(next.state$active.attempt$outcome, "proposal_created")
    ) {
      "proposal_installed"
    } else {
      "construction_failed"
    },
    installed = TRUE,
    diagnostic = completion$diagnostic %||% NULL
  )
}

gflowui_basin_start_bundle_attempt <- function(
    state,
    bundle,
    session.id,
    construction.fingerprint = "",
    context.generation = 1L,
    reducer = gflowui_basin_reduce_state) {
  checked <- tryCatch(
    .gflowui_basin_assert_bundle(bundle),
    error = identity
  )
  if (inherits(checked, "error")) {
    invalidated <- if (is.null(state)) {
      list(state = NULL, error = NULL)
    } else {
      tryCatch(
        list(
          state = reducer(
            state,
            gflowui_basin_state_event(
              "bundle_change",
              bundle = bundle
            )
          ),
          error = NULL
        ),
        error = function(error) {
          list(state = state, error = error)
        }
      )
    }
    if (!is.null(invalidated$error)) {
      return(list(
        state = invalidated$state,
        job = NULL,
        diagnostic = list(
          kind = "internal_transition_failure",
          message = conditionMessage(invalidated$error),
          condition.class = class(invalidated$error)
        )
      ))
    }
    return(list(
      state = invalidated$state,
      job = NULL,
      diagnostic = list(
        kind = "bundle_input_invalid",
        message = conditionMessage(checked),
        condition.class = class(checked)
      )
    ))
  }
  candidate <- tryCatch(
    {
      next.state <- if (is.null(state)) {
        initialized <- gflowui_basin_new_runtime_state(
          bundle,
          context.generation = context.generation
        )
        reducer(
          initialized,
          gflowui_basin_state_event("recompute")
        )
      } else {
        reducer(
          state,
          gflowui_basin_state_event(
            "bundle_change",
            bundle = bundle
          )
        )
      }
      list(state = next.state, error = NULL)
    },
    error = function(error) {
      list(state = state, error = error)
    }
  )
  if (!is.null(candidate$error)) {
    return(list(
      state = candidate$state,
      job = NULL,
      diagnostic = list(
        kind = "internal_transition_failure",
        message = conditionMessage(candidate$error),
        condition.class = class(candidate$error)
      )
    ))
  }
  next.state <- candidate$state
  active <- next.state$active.attempt
  if (identical(active$outcome, "blocked") &&
      identical(active$reason, "bundle_invalid")) {
    return(list(
      state = next.state,
      job = NULL,
      diagnostic = list(
        kind = "internal_transition_failure",
        message = paste(active$messages, collapse = " "),
        condition.class = "gflowui_basin_state_error"
      )
    ))
  }
  job <- if (identical(active$outcome, "pending")) {
    gflowui_basin_async_job(
      next.state,
      session.id,
      construction.fingerprint
    )
  } else {
    NULL
  }
  list(
    state = next.state,
    job = job,
    diagnostic = if (is.null(job)) {
      list(
        kind = "scientific_blocked",
        message = paste(active$messages, collapse = " "),
        condition.class = active$reason
      )
    } else {
      NULL
    }
  )
}

gflowui_basin_launch_async_job <- function(job,
                                           callback,
                                           delay = 0) {
  if (!inherits(job, "gflowui_basin_async_job") ||
      !is.function(callback)) {
    .gflowui_basin_shell_stop(
      "A typed job and completion callback are required."
    )
  }
  delay <- suppressWarnings(as.numeric(delay))
  if (length(delay) != 1L ||
      is.na(delay) ||
      !is.finite(delay) ||
      delay < 0) {
    .gflowui_basin_shell_stop(
      "The asynchronous launch delay must be a finite nonnegative scalar."
    )
  }
  later::later(
    function() callback(gflowui_basin_execute_async_job(job)),
    delay = delay
  )
  invisible(job$job.id)
}

gflowui_basin_default_plot_specs <- function(
    construction.fingerprint,
    first_id = 1L) {
  fingerprint <- .gflowui_basin_shell_string(
    construction.fingerprint,
    "construction-unavailable"
  )
  first.id <- .gflowui_basin_validate_integer(
    first_id,
    "first.id",
    minimum = 1L
  )
  if (!first.id$valid ||
      first.id$value >= .Machine$integer.max) {
    .gflowui_basin_shell_stop(
      "Two supported default plot IDs are required."
    )
  }
  features <- list(
    c("extremum_value_rank", "support_rank"),
    c("extremum_value_rank", "mass_rank")
  )
  lapply(seq_along(features), function(index) {
    list(
      id = as.integer(first.id$value + index - 1L),
      kind = "scatter",
      features = features[[index]],
      scope = "all",
      type = "max",
      x_scale = "log10",
      y_scale = "log10",
      construction_fingerprint = fingerprint,
      seeded.default = TRUE
    )
  })
}

gflowui_basin_seed_default_plots <- function(
    existing,
    seeded.fingerprints,
    construction.fingerprint,
    next.id = 0L) {
  existing <- if (is.list(existing)) existing else list()
  seeded.fingerprints <- unique(as.character(
    seeded.fingerprints %||% character()
  ))
  fingerprint <- .gflowui_basin_shell_string(
    construction.fingerprint,
    "construction-unavailable"
  )
  if (fingerprint %in% seeded.fingerprints) {
    return(list(
      specs = existing,
      seeded.fingerprints = seeded.fingerprints,
      next.id = as.integer(next.id),
      added = 0L
    ))
  }
  first.id <- suppressWarnings(as.integer(next.id)) + 1L
  defaults <- gflowui_basin_default_plot_specs(
    fingerprint,
    first_id = first.id
  )
  list(
    specs = c(existing, defaults),
    seeded.fingerprints = c(seeded.fingerprints, fingerprint),
    next.id = max(vapply(defaults, `[[`, integer(1), "id")),
    added = 2L
  )
}

gflowui_basin_analysis_shell_summary <- function(state) {
  .gflowui_basin_assert_runtime_state(state)
  data <- gflowui_basin_bundle_snapshot(state$bundle)
  proposal <- gflowui_basin_displayed_proposal(state)
  component <- state$context$component
  component.rows <- data$canonical$component == component
  list(
    outcome = as.character(
      state$active.attempt$outcome %||% "not_started"
    ),
    reason = as.character(state$active.attempt$reason %||% ""),
    messages = as.character(
      state$active.attempt$messages %||% character()
    ),
    display.source = state$display.source,
    component = as.integer(component),
    component.count = as.integer(length(data$component.ids)),
    component.maximum.count = as.integer(sum(component.rows)),
    final.count = if (is.list(proposal)) {
      as.integer(length(proposal$final.ids))
    } else {
      0L
    },
    filter.mode = if (is.list(proposal)) {
      as.character(proposal$accepted.parameters$filter.mode)
    } else {
      as.character(state$controls$filter.mode %||% "")
    }
  )
}
