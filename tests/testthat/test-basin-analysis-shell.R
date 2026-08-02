phase4_overlay_fixture <- function(suffix = "base") {
  adjacency <- list(
    2L,
    c(1L, 3L, 4L),
    2L,
    c(2L, 5L, 6L),
    4L,
    4L
  )
  edge.lengths <- lapply(
    adjacency,
    function(neighbors) rep(1, length(neighbors))
  )
  field <- c(5, 1, 4, 0, 3, 2)
  vertex.ids <- paste0("v", seq_along(field))
  vertex.mass <- rep(1 / length(field), length(field))
  graph.identity <- gflowui:::gflowui_basin_graph_identity(
    adj_list = adjacency,
    edge_length_list = edge.lengths,
    vertex_id = vertex.ids,
    graph_id = paste0("phase4-graph-", suffix),
    graph_k = 2L
  )
  build.identity <- gflow::get.gflow.build.identity()
  construction.record <- list(
    schema = "gflowui_basin_construction_identity/2",
    project.id = paste0("phase4-project-", suffix),
    graph.set.id = "phase4-set",
    graph = graph.identity,
    source.key = "occupation_density_active",
    source.fingerprint = paste0("source-", suffix),
    field.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(field),
    mass.fingerprint =
      gflowui:::gflowui_basin_field_fingerprint(vertex.mass),
    mass.provenance = list(kind = "test"),
    alignment.validation = list(status = "validated"),
    construction = list(
      method = "trajectory_flow",
      direction = "both"
    ),
    gflow.build.id = build.identity$build.id,
    gflow.runtime.id = build.identity$runtime$id
  )
  construction <- list(
    record = construction.record,
    fingerprint = gflowui:::gflowui_basin_sha256(
      construction.record
    )
  )
  basin <- gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "trajectory_flow",
    direction = "both",
    vertex.mass = vertex.mass,
    method.params = list(
      edge.length.quantile.thld = 1,
      store.trajectories = FALSE
    ),
    vertex.id = vertex.ids
  )
  prominence <- gflow::create.basin.complex(
    adjacency,
    edge.lengths,
    field,
    method = "superlevel_merge_tree",
    direction = "both",
    vertex.mass = vertex.mass,
    vertex.id = vertex.ids
  )
  list(
    result = list(
      basin = basin,
      prominence_complex = prominence,
      build_identity = build.identity
    ),
    request = list(
      source = list(
        key = "occupation_density_active",
        label = paste("Phase 4", suffix),
        values = field,
        graph = list(
          adj_list = adjacency,
          weight_list = edge.lengths
        )
      ),
      vertex_id = vertex.ids,
      alignment = list(
        source.id = paste0("subject-", suffix)
      ),
      source_fingerprint = paste0("source-", suffix),
      construction_identity = construction
    )
  )
}

phase4_bundle <- function(suffix = "base") {
  fixture <- phase4_overlay_fixture(suffix)
  gflowui:::gflowui_basin_bundle_from_overlay(
    fixture$result,
    fixture$request
  )
}

phase4_completion <- function(job) {
  gflowui:::gflowui_basin_execute_async_job(job)
}

test_that("overlay assembly creates the complete immutable Phase 2 bundle", {
  fixture <- phase4_overlay_fixture("assembly")
  bundle <- gflowui:::gflowui_basin_bundle_from_overlay(
    fixture$result,
    fixture$request
  )
  snapshot <- gflowui:::gflowui_basin_bundle_snapshot(bundle)

  expect_s3_class(bundle, "gflowui_basin_scientific_bundle")
  expect_true(environmentIsLocked(bundle))
  expect_identical(
    names(snapshot$identity),
    gflowui:::.gflowui_basin_required_identities()
  )
  expect_identical(
    snapshot$identity$estimate,
    fixture$request$construction_identity$fingerprint
  )
  expect_identical(snapshot$direction, "max")
  expect_identical(snapshot$validation$source, "valid")
  expect_identical(snapshot$validation$mapping, "valid")
  expect_true(all(vapply(
    snapshot$identity,
    gflowui:::.gflowui_basin_scalar_string,
    logical(1)
  )))
  expect_setequal(
    snapshot$canonical$basin.id,
    snapshot$trajectory.table$trajectory.basin.id
  )
})

test_that("overlay assembly rejects malformed mapping columns without coercion", {
  fixture <- phase4_overlay_fixture("malformed")
  mutations <- list(
    type_key = function(table) {
      table$basin.id <- factor(table$basin.id)
      table
    },
    character_vertex = function(table) {
      table$extremum.vertex <- as.character(table$extremum.vertex)
      table
    },
    fractional_vertex = function(table) {
      table$extremum.vertex[[1L]] <- 1.5
      table
    },
    nonfinite_vertex = function(table) {
      table$extremum.vertex[[1L]] <- Inf
      table
    },
    character_mass = function(table) {
      table$primary.support.mass <-
        as.character(table$primary.support.mass)
      table
    },
    character_support = function(table) {
      table$primary.support.size <-
        as.character(table$primary.support.size)
      table
    }
  )

  for (name in names(mutations)) {
    malformed <- unserialize(serialize(fixture$result, NULL))
    malformed$basin$basin.table <- mutations[[name]](
      malformed$basin$basin.table
    )
    checked <- gflowui:::gflowui_basin_try_bundle_from_overlay(
      malformed,
      fixture$request
    )
    expect_false(checked$ok, info = name)
    expect_null(checked$bundle, info = name)
    expect_identical(
      checked$diagnostic$kind,
      "bundle_input_invalid",
      info = name
    )
  }

  missing <- gflowui:::gflowui_basin_try_bundle_from_overlay(
    NULL,
    NULL
  )
  expect_false(missing$ok)
  expect_identical(
    missing$diagnostic$kind,
    "bundle_input_invalid"
  )

  malformed.source <- fixture$request
  malformed.source$source$values <-
    as.character(malformed.source$source$values)
  checked.source <-
    gflowui:::gflowui_basin_try_bundle_from_overlay(
      fixture$result,
      malformed.source
    )
  expect_false(checked.source$ok)
  expect_identical(
    checked.source$diagnostic$kind,
    "bundle_input_invalid"
  )
})

test_that("Basin Analysis default workspace is exact and construction-scoped", {
  specs <- gflowui:::gflowui_basin_default_plot_specs(
    "construction-alpha",
    first_id = 7L
  )

  expect_length(specs, 2L)
  expect_identical(vapply(specs, `[[`, integer(1), "id"), 7:8)
  expect_identical(
    lapply(specs, `[[`, "features"),
    list(
      c("extremum_value_rank", "support_rank"),
      c("extremum_value_rank", "mass_rank")
    )
  )
  expect_true(all(vapply(
    specs,
    function(spec) {
      identical(spec$kind, "scatter") &&
        identical(spec$scope, "component_maxima") &&
        identical(spec$type, "max") &&
        identical(spec$point_color, "proposal") &&
        identical(spec$x_scale, "log10") &&
        identical(spec$y_scale, "log10") &&
        identical(
          spec$construction_fingerprint,
          "construction-alpha"
        ) &&
        isTRUE(spec$seeded.default)
    },
    logical(1)
  )))

  seeded <- gflowui:::gflowui_basin_seed_default_plots(
    existing = list(),
    seeded.fingerprints = character(),
    construction.fingerprint = "construction-alpha",
    next.id = 0L
  )
  seeded$specs <- c(
    seeded$specs,
    list(list(id = 3L, kind = "manual"))
  )
  reopened <- gflowui:::gflowui_basin_seed_default_plots(
    existing = seeded$specs,
    seeded.fingerprints = seeded$seeded.fingerprints,
    construction.fingerprint = "construction-alpha",
    next.id = 3L
  )
  expect_identical(reopened$specs, seeded$specs)
  expect_identical(reopened$added, 0L)

  replacement <- gflowui:::gflowui_basin_seed_default_plots(
    existing = list(),
    seeded.fingerprints = character(),
    construction.fingerprint = "construction-beta",
    next.id = 0L
  )
  expect_length(replacement$specs, 2L)
  expect_true(all(vapply(
    replacement$specs,
    function(spec) {
      identical(
        spec$construction_fingerprint,
        "construction-beta"
      )
    },
    logical(1)
  )))
})

test_that("async installation rejects stale attempts, contexts, and sessions", {
  first <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("race-first"),
    session.id = "session-current",
    construction.fingerprint = "construction-first",
    context.generation = 4L
  )
  expect_null(first$diagnostic)
  expect_s3_class(first$job, "gflowui_basin_async_job")
  expect_identical(first$state$active.attempt$outcome, "pending")
  expect_identical(first$state$context.generation, 4L)
  old.completion <- phase4_completion(first$job)

  second <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = first$state,
    bundle = phase4_bundle("race-second"),
    session.id = "session-current",
    construction.fingerprint = "construction-second"
  )
  expect_identical(second$state$context.generation, 5L)
  expect_identical(second$state$active.attempt$attempt.id, 2L)
  before.old <- second$state
  stale.context <- gflowui:::gflowui_basin_install_async_completion(
    second$state,
    old.completion,
    session.id = "session-current",
    construction.fingerprint = "construction-second"
  )
  expect_false(stale.context$installed)
  expect_identical(stale.context$disposition, "stale_construction")
  expect_identical(stale.context$state, before.old)

  current.completion <- phase4_completion(second$job)
  adversarial <- list(
    cross_session = list(
      session = "session-other",
      active = TRUE,
      fingerprint = "construction-second",
      disposition = "stale_session"
    ),
    closed_session = list(
      session = "session-current",
      active = FALSE,
      fingerprint = "construction-second",
      disposition = "session_closed"
    ),
    changed_construction = list(
      session = "session-current",
      active = TRUE,
      fingerprint = "construction-third",
      disposition = "stale_construction"
    )
  )
  for (name in names(adversarial)) {
    case <- adversarial[[name]]
    rejected <- gflowui:::gflowui_basin_install_async_completion(
      second$state,
      current.completion,
      session.id = case$session,
      session.active = case$active,
      construction.fingerprint = case$fingerprint
    )
    expect_false(rejected$installed, info = name)
    expect_identical(
      rejected$disposition,
      case$disposition,
      info = name
    )
    expect_identical(rejected$state, second$state, info = name)
  }

  malformed <- gflowui:::gflowui_basin_install_async_completion(
    second$state,
    42,
    session.id = "session-current"
  )
  expect_false(malformed$installed)
  expect_identical(malformed$disposition, "stale_session")
  expect_identical(malformed$state, second$state)

  invalidated <- gflowui:::gflowui_basin_install_async_completion(
    NULL,
    current.completion,
    session.id = "session-current"
  )
  expect_false(invalidated$installed)
  expect_identical(invalidated$disposition, "state_invalidated")
  expect_null(invalidated$state)

  installed <- gflowui:::gflowui_basin_install_async_completion(
    second$state,
    current.completion,
    session.id = "session-current",
    construction.fingerprint = "construction-second"
  )
  expect_true(installed$installed)
  expect_identical(installed$disposition, "proposal_installed")
  expect_identical(installed$state$display.source, "current")
})

test_that("out-of-order same-context work cannot replace the latest attempt", {
  started <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("same-context"),
    session.id = "session-race",
    construction.fingerprint = "construction-race"
  )
  installed <- gflowui:::gflowui_basin_install_async_completion(
    started$state,
    phase4_completion(started$job),
    session.id = "session-race",
    construction.fingerprint = "construction-race"
  )$state

  first.pending <- gflowui:::gflowui_basin_reduce_state(
    installed,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "coverage.target",
      value = 0.9
    )
  )
  first.job <- gflowui:::gflowui_basin_async_job(
    first.pending,
    "session-race",
    "construction-race"
  )
  first.completion <- phase4_completion(first.job)
  second.pending <- gflowui:::gflowui_basin_reduce_state(
    first.pending,
    gflowui:::gflowui_basin_state_event(
      "control_change",
      name = "coverage.target",
      value = 0.8
    )
  )
  second.job <- gflowui:::gflowui_basin_async_job(
    second.pending,
    "session-race",
    "construction-race"
  )

  rejected <- gflowui:::gflowui_basin_install_async_completion(
    second.pending,
    first.completion,
    session.id = "session-race",
    construction.fingerprint = "construction-race"
  )
  expect_false(rejected$installed)
  expect_identical(rejected$disposition, "stale_attempt")
  expect_identical(rejected$state, second.pending)

  current <- gflowui:::gflowui_basin_install_async_completion(
    second.pending,
    phase4_completion(second.job),
    session.id = "session-race",
    construction.fingerprint = "construction-race"
  )
  expect_true(current$installed)
  expect_identical(current$state$display.source, "current")
  expect_identical(
    current$state$active.attempt$attempt.id,
    second.pending$active.attempt$attempt.id
  )
})

test_that("matching failure, recovery, and internal diagnostics stay typed", {
  started <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("failure"),
    session.id = "session-failure",
    construction.fingerprint = "construction-failure"
  )
  current <- gflowui:::gflowui_basin_install_async_completion(
    started$state,
    phase4_completion(started$job),
    session.id = "session-failure"
  )$state
  pending <- gflowui:::gflowui_basin_reduce_state(
    current,
    gflowui:::gflowui_basin_state_event("recompute")
  )
  failed.job <- gflowui:::gflowui_basin_async_job(
    pending,
    "session-failure",
    "construction-failure"
  )
  failed.completion <- gflowui:::gflowui_basin_async_completion(
    failed.job,
    gflowui:::.gflowui_basin_async_failure_result(
      failed.job,
      "layout_invalid",
      "forced matching failure"
    )
  )
  failed <- gflowui:::gflowui_basin_install_async_completion(
    pending,
    failed.completion,
    session.id = "session-failure"
  )
  expect_true(failed$installed)
  expect_identical(failed$disposition, "construction_failed")
  expect_identical(failed$state$display.source, "retained_last_valid")

  recovery.pending <- gflowui:::gflowui_basin_reduce_state(
    failed$state,
    gflowui:::gflowui_basin_state_event("recompute")
  )
  recovery.job <- gflowui:::gflowui_basin_async_job(
    recovery.pending,
    "session-failure",
    "construction-failure"
  )
  recovered <- gflowui:::gflowui_basin_install_async_completion(
    recovery.pending,
    phase4_completion(recovery.job),
    session.id = "session-failure"
  )
  expect_true(recovered$installed)
  expect_identical(recovered$state$display.source, "current")
  expect_null(recovered$state$retained.last.valid.proposal)

  malformed <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = NULL,
    session.id = "session-diagnostic"
  )
  expect_identical(
    malformed$diagnostic$kind,
    "bundle_input_invalid"
  )
  invalidated <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = current,
    bundle = NULL,
    session.id = "session-diagnostic"
  )
  expect_identical(
    invalidated$diagnostic$kind,
    "bundle_input_invalid"
  )
  expect_identical(
    invalidated$state$active.attempt$outcome,
    "blocked"
  )
  expect_identical(
    invalidated$state$active.attempt$reason,
    "bundle_invalid"
  )
  expect_identical(invalidated$state$display.source, "none")
  expect_null(invalidated$state$current.proposal)
  expect_null(invalidated$state$retained.last.valid.proposal)
  internal <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("internal-transition"),
    session.id = "session-diagnostic",
    reducer = function(...) stop("forced transition defect")
  )
  expect_identical(
    internal$diagnostic$kind,
    "internal_transition_failure"
  )

  execution <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("internal-execution"),
    session.id = "session-diagnostic"
  )
  corrupted.job <- execution$job
  corrupted.job$pending.work <- list()
  completion <- phase4_completion(corrupted.job)
  expect_identical(
    completion$diagnostic$kind,
    "internal_execution_failure"
  )
  installed.failure <-
    gflowui:::gflowui_basin_install_async_completion(
      execution$state,
      completion,
      session.id = "session-diagnostic"
    )
  expect_true(installed.failure$installed)
  expect_identical(
    installed.failure$state$active.attempt$outcome,
    "construction_failed"
  )
})

test_that("deferred launcher invokes one completion and validates delay", {
  started <- gflowui:::gflowui_basin_start_bundle_attempt(
    state = NULL,
    bundle = phase4_bundle("launcher"),
    session.id = "session-launcher",
    construction.fingerprint = "construction-launcher"
  )
  completions <- list()
  gflowui:::gflowui_basin_launch_async_job(
    started$job,
    function(completion) {
      completions[[length(completions) + 1L]] <<- completion
    }
  )
  later::run_now(timeoutSecs = 1)
  expect_length(completions, 1L)
  expect_s3_class(
    completions[[1L]],
    "gflowui_basin_async_completion"
  )
  expect_true(is.list(completions[[1L]]$metrics))
  expect_true(is.finite(
    completions[[1L]]$metrics$proposal.elapsed.ms
  ))
  expect_gte(completions[[1L]]$metrics$proposal.elapsed.ms, 0)
  expect_true(is.finite(
    completions[[1L]]$metrics$callback.queue.delay.ms
  ))
  expect_gte(
    completions[[1L]]$metrics$callback.queue.delay.ms,
    0
  )
  expect_error(
    gflowui:::gflowui_basin_launch_async_job(
      started$job,
      identity,
      delay = Inf
    ),
    class = "gflowui_basin_shell_error"
  )
})
