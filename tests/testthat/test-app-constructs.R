test_that("application object builds", {
  app <- gflowui::gflowui_app()
  expect_s3_class(app, "shiny.appobj")
})

local_projects_data_sandbox <- function() {
  real_registry <- gflowui:::gflowui_registry_path()
  real_manifests <- gflowui:::gflowui_manifests_dir()
  sandbox_dir <- file.path(
    tempdir(),
    sprintf(
      "gflowui-projects-%s-%s",
      as.integer(Sys.getpid()),
      paste(sample(c(letters, 0:9), 8L, replace = TRUE), collapse = "")
    )
  )
  dir.create(sandbox_dir, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(real_registry)) {
    file.copy(real_registry, file.path(sandbox_dir, "registry.rds"), overwrite = TRUE)
  }
  if (dir.exists(real_manifests)) {
    dir.create(file.path(sandbox_dir, "manifests"), recursive = TRUE, showWarnings = FALSE)
    mf_files <- list.files(real_manifests, full.names = TRUE)
    if (length(mf_files) > 0L) {
      file.copy(mf_files, file.path(sandbox_dir, "manifests"), overwrite = TRUE)
    }
  }

  withr::local_options(list(gflowui.projects_data_dir = sandbox_dir))
  ns <- asNamespace("gflowui")
  orig_fun <- get("gflowui_projects_data_dir", envir = ns, inherits = FALSE)
  unlockBinding("gflowui_projects_data_dir", ns)
  assign(
    "gflowui_projects_data_dir",
    function() normalizePath(path.expand(sandbox_dir), mustWork = FALSE),
    envir = ns
  )
  lockBinding("gflowui_projects_data_dir", ns)
  withr::defer(
    {
      unlockBinding("gflowui_projects_data_dir", ns)
      assign("gflowui_projects_data_dir", orig_fun, envir = ns)
      lockBinding("gflowui_projects_data_dir", ns)
    },
    envir = parent.frame()
  )
  invisible(sandbox_dir)
}


test_that("graph adapter returns expected shape", {
  skip_if_not_installed("gflow")
  has_builder <- exists(
    "build.iknn.graphs.and.selectk",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  skip_if_not(has_builder)
  has_fit <- exists(
    "fit.rdgraph.regression",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  has_refit <- exists(
    "refit.rdgraph.regression",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  has_endpoints <- exists(
    "geodesic.core.endpoints",
    envir = asNamespace("gflow"),
    inherits = FALSE
  )
  skip_if_not(has_fit && has_refit && has_endpoints)

  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph(x, kmin = 5, kmax = 9, method = "edit")
  expect_true(is.list(g))
  expect_true("selected.k" %in% names(g))
  expect_true("selected.graph" %in% names(g))
  expect_true("adj.list" %in% names(g))

  fit <- gflowui:::gflow_fit_condexp(
    graph_obj = g,
    X = x,
    y = x[, 1],
    feature.matrix = x[, 2:3, drop = FALSE],
    fit.args = list(max.iterations = 3L, n.eigenpairs = 10L, verbose.level = 0L),
    refit.args = list(per.column.gcv = FALSE)
  )
  expect_equal(length(fit$fitted.values), nrow(x))
  expect_true(is.matrix(fit$feature.fitted.values))
  expect_equal(nrow(fit$feature.fitted.values), nrow(x))
  expect_equal(ncol(fit$feature.fitted.values), 2L)

  ep <- gflowui:::gflow_detect_endpoints(
    graph_obj = g,
    core.quantile = 0.10,
    endpoint.quantile = 0.90,
    use.approx.eccentricity = TRUE,
    n.landmarks = 16L,
    max.endpoints = 4L,
    seed = 1L
  )
  expect_true("endpoints" %in% names(ep))
  expect_true("core.vertices" %in% names(ep))
  expect_true(is.integer(ep$endpoints) || is.numeric(ep$endpoints))
})

test_that("renderer selection survives transient NULL during UI rebuild", {
  skip_if_not_installed("plotly")
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    session$setInputs(graph_layout_renderer = "plotly")
    session$flushReact()
    rr1 <- reference_renderer_state()
    expect_equal(rr1$requested, "plotly")
    expect_equal(rr1$effective, "plotly")

    session$setInputs(graph_layout_renderer = NULL)
    session$flushReact()
    rr2 <- reference_renderer_state()
    expect_equal(rr2$requested, "plotly")
    expect_equal(rr2$effective, "plotly")
  })
})

test_that("legacy html renderer state is normalized to plotly", {
  skip_if_not_installed("plotly")
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    session$setInputs(graph_layout_renderer = "html")
    session$flushReact()

    rr <- reference_renderer_state()
    gs <- graph_structure_state()
    expect_equal(rr$requested, "plotly")
    expect_equal(rr$effective, "plotly")
    expect_equal(gs$renderer_selected, "plotly")
  })
})

test_that("working-set-first endpoint defaults activate the saved working overlay", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    gs <- graph_structure_state()
    expect_equal(gs$renderer_selected, "plotly")
    expect_equal(gs$vertex_layout, "point")

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))
    original <- load_working_endpoint_state(ctx)
    on.exit(save_working_endpoint_state(original, ctx = ctx), add = TRUE)

    custom <- empty_working_endpoint_state(ctx = ctx)
    custom <- upsert_working_endpoint_vertex_state(custom, 11L)
    save_working_endpoint_state(custom, ctx = ctx)

    endpoint_overlay_selection(character(0))
    endpoint_autoselect_done(FALSE)
    endpoint_show_working_set(NA)
    session$flushReact()

    expect_true(isTRUE(endpoint_show_working_set()))
    expect_length(endpoint_overlay_selection(), 0L)

    ep <- endpoint_overlay_active()
    expect_true(11L %in% ep$vertices)
  })
})

test_that("working endpoints and workspace candidates persist across k within a graph set", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    gs0 <- graph_structure_state()
    k_choices <- if (is.list(gs0) && !is.null(gs0$k_choices)) as.character(gs0$k_choices) else character(0)
    if (!("7" %in% k_choices)) {
      skip("AGP k=7 is not available in this environment")
    }

    ctx6 <- current_endpoint_graph_context()
    expect_true(is.list(ctx6))

    state <- empty_working_endpoint_state(ctx = ctx6)
    state <- upsert_working_endpoint_vertex_state(state, 33L)
    save_working_endpoint_state(state, ctx = ctx6)
    expect_no_error(save_working_endpoint_snapshot())
    session$flushReact()

    session$setInputs(graph_k = "7")
    session$flushReact()

    ctx7 <- current_endpoint_graph_context()
    expect_true(is.list(ctx7))
    expect_equal(ctx7$graph_set_id, ctx6$graph_set_id)
    expect_equal(ctx7$k, 7L)

    st7 <- endpoint_panel_state()
    expect_true(is.data.frame(st7$working$rows))
    expect_true(33L %in% st7$working$rows$vertex)
    expect_true(any(as.character(st7$rows$origin) == "workspace"))
    expect_true(any(suppressWarnings(as.integer(st7$rows$k)) == 6L, na.rm = TRUE))
  })
})

test_that("checked show-working-set preference survives k changes", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    gs0 <- graph_structure_state()
    k_choices <- if (is.list(gs0) && !is.null(gs0$k_choices)) as.character(gs0$k_choices) else character(0)
    if (!("7" %in% k_choices)) {
      skip("AGP k=7 is not available in this environment")
    }

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 44L)
    save_working_endpoint_state(state, ctx = ctx)
    endpoint_show_working_set(TRUE)
    session$flushReact()

    session$setInputs(graph_k = "7")
    session$flushReact()

    expect_true(isTRUE(endpoint_show_working_set()))
    st <- endpoint_panel_state()
    expect_true(is.data.frame(st$working$rows))
    expect_true(44L %in% st$working$rows$vertex)
  })
})

test_that("legacy working current state wins over larger snapshots when shared state is absent", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    shared_file <- endpoint_working_file(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    unlink(shared_file, recursive = TRUE, force = TRUE)

    legacy_dir <- endpoint_state_legacy_k_dir(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    legacy_working_dir <- file.path(legacy_dir, "working")
    legacy_snapshot_dir <- file.path(legacy_working_dir, "snapshots")
    dir.create(legacy_snapshot_dir, recursive = TRUE, showWarnings = FALSE)

    current_state <- empty_working_endpoint_state(ctx = ctx)
    current_state <- upsert_working_endpoint_vertex_state(current_state, 11L)
    current_state <- upsert_working_endpoint_vertex_state(current_state, 12L)
    current_state$updated_at <- "2026-03-10 10:00:00"
    saveRDS(current_state, file.path(legacy_working_dir, "current.rds"))

    snapshot_obj <- list(
      project_id = ctx$project_id,
      graph_set_id = ctx$graph_set_id,
      source_k = ctx$k,
      source_dataset_id = "test_snapshot",
      label = "test snapshot",
      vertices = as.integer(c(1L, 2L, 3L, 4L, 5L)),
      labels = sprintf("v%d", 1:5),
      created_at = "2026-03-10 09:00:00"
    )
    saveRDS(snapshot_obj, file.path(legacy_snapshot_dir, "test_snapshot.rds"))

    st <- load_working_endpoint_state(ctx)
    expect_true(is.data.frame(st$rows))
    expect_equal(sort(st$rows$vertex), c(11L, 12L))
  })
})

test_that("replayed load button counts do not overwrite the working set on startup", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    custom <- empty_working_endpoint_state(ctx = ctx)
    custom <- upsert_working_endpoint_vertex_state(custom, 77L)
    custom <- upsert_working_endpoint_vertex_state(custom, 88L)
    save_working_endpoint_state(custom, ctx = ctx)
    session$flushReact()

    st <- endpoint_panel_state()
    rows <- if (is.list(st) && is.data.frame(st$rows)) st$rows else data.frame()
    expect_true(nrow(rows) > 0L)
    load_id <- as.character(rows$load_input_id[[1]] %||% "")
    expect_true(nzchar(load_id))

    endpoint_dataset_load_counts(structure(integer(0), names = character(0)))
    do.call(session$setInputs, stats::setNames(list(1L), load_id))
    session$flushReact()

    st_after <- endpoint_panel_state()
    expect_true(is.data.frame(st_after$working$rows))
    expect_equal(sort(st_after$working$rows$vertex), c(77L, 88L))
  })
})

test_that("working endpoint row selection updates the inspector vertex", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 91L)
    save_working_endpoint_state(state, ctx = ctx)
    session$flushReact()

    session$setInputs(endpoint_working_select_vertex = 91L)
    session$flushReact()

    expect_equal(selected_endpoint_vertex(), 91L)
    expect_equal(endpoint_vertex_state$source, "working_table")
  })
})

test_that("AGP opens without plotly click registration warnings", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  expect_no_warning(
    shiny::testServer(gflowui:::app_server, {
      open_project("agp")
      session$flushReact()
    })
  )
})

test_that("vertex inspector supports manual selection and idempotent working-set upsert", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    session$setInputs(endpoint_vertex_id = 10L)
    session$flushReact()

    expect_equal(selected_endpoint_vertex(), 10L)
    expect_equal(endpoint_vertex_state$source, "manual")

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state0 <- empty_working_endpoint_state(ctx = ctx)
    state1 <- upsert_working_endpoint_vertex_state(state0, 10L)
    expect_equal(nrow(state1$rows), 1L)
    expect_equal(state1$rows$vertex[[1]], 10L)
    expect_true(isTRUE(state1$rows$accepted[[1]]))
    expect_true(isTRUE(state1$rows$visible[[1]]))
    expect_equal(state1$rows$label[[1]], "v10")
    expect_equal(state1$rows$source_type[[1]], "manual")
    expect_true(isTRUE(state1$rows$manually_added[[1]]))

    state2 <- upsert_working_endpoint_vertex_state(state1, 10L)
    expect_equal(nrow(state2$rows), 1L)
    expect_true(isTRUE(state2$rows$accepted[[1]]))
    expect_true(isTRUE(state2$rows$visible[[1]]))
  })
})

test_that("working endpoint table label edits persist immediately", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 71L)
    save_working_endpoint_state(state, ctx = ctx)
    session$flushReact()

    label_event_id <- endpoint_working_label_event_id(71L)
    do.call(session$setInputs, stats::setNames(list("Arm Tip A"), label_event_id))
    session$flushReact()

    st <- endpoint_panel_state()
    expect_true(is.data.frame(st$working$rows))
    hit <- which(st$working$rows$vertex == 71L)
    expect_length(hit, 1L)
    expect_equal(st$working$rows$label[[hit[[1]]]], "Arm Tip A")

    reloaded <- load_working_endpoint_state(ctx)
    hit2 <- which(reloaded$rows$vertex == 71L)
    expect_length(hit2, 1L)
    expect_equal(reloaded$rows$label[[hit2[[1]]]], "Arm Tip A")
  })
})

test_that("working endpoint table supports hide, restore, and delete", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 81L)
    state <- upsert_working_endpoint_vertex_state(state, 82L)
    save_working_endpoint_state(state, ctx = ctx)
    session$flushReact()

    hide_id <- endpoint_working_hide_input_id(82L)
    do.call(session$setInputs, stats::setNames(list(0L), hide_id))
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(1L), hide_id))
    session$flushReact()

    st <- endpoint_panel_state()
    expect_true(is.data.frame(st$working$rows))
    expect_true(82L %in% st$working$rows$vertex)
    hit <- which(st$working$rows$vertex == 82L)
    expect_length(hit, 1L)
    expect_false(isTRUE(st$working$rows$visible[[hit[[1]]]]))
    expect_true(81L %in% st$working$rows$vertex)

    reloaded <- load_working_endpoint_state(ctx)
    hit_reload <- which(reloaded$rows$vertex == 82L)
    expect_length(hit_reload, 1L)
    expect_false(isTRUE(reloaded$rows$visible[[hit_reload[[1]]]]))
    expect_true(81L %in% reloaded$rows$vertex)

    restore_id <- endpoint_working_restore_input_id(82L)
    do.call(session$setInputs, stats::setNames(list(0L), restore_id))
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(1L), restore_id))
    session$flushReact()

    restored <- load_working_endpoint_state(ctx)
    hit_restored <- which(restored$rows$vertex == 82L)
    expect_length(hit_restored, 1L)
    expect_true(isTRUE(restored$rows$visible[[hit_restored[[1]]]]))

    do.call(session$setInputs, stats::setNames(list(0L), hide_id))
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(2L), hide_id))
    session$flushReact()

    delete_id <- endpoint_working_delete_input_id(82L)
    do.call(session$setInputs, stats::setNames(list(0L), delete_id))
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(1L), delete_id))
    session$flushReact()

    deleted <- load_working_endpoint_state(ctx)
    expect_false(82L %in% deleted$rows$vertex)
    expect_true(81L %in% deleted$rows$vertex)
  })
})

test_that("saving a working snapshot preserves endpoint panel state loading", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    original_working <- load_working_endpoint_state(ctx)
    on.exit(save_working_endpoint_state(original_working, ctx = ctx), add = TRUE)

    candidate_dir <- endpoint_candidates_dir(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    snapshot_dir <- endpoint_snapshot_dir(
      graph_set_id = ctx$graph_set_id,
      k = ctx$k,
      project_id = ctx$project_id
    )
    before_candidate_files <- if (dir.exists(candidate_dir)) {
      list.files(candidate_dir, full.names = TRUE)
    } else {
      character(0)
    }
    before_snapshot_files <- if (dir.exists(snapshot_dir)) {
      list.files(snapshot_dir, full.names = TRUE)
    } else {
      character(0)
    }

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 21L)
    save_working_endpoint_state(state, ctx = ctx)

    snap <- NULL
    expect_no_error(snap <- save_working_endpoint_snapshot())
    session$flushReact()
    expect_true(is.list(snap))
    expect_true(isTRUE(snap$ok))

    after_candidate_files <- if (dir.exists(candidate_dir)) {
      list.files(candidate_dir, full.names = TRUE)
    } else {
      character(0)
    }
    after_snapshot_files <- if (dir.exists(snapshot_dir)) {
      list.files(snapshot_dir, full.names = TRUE)
    } else {
      character(0)
    }
    new_candidate_files <- setdiff(after_candidate_files, before_candidate_files)
    new_snapshot_files <- setdiff(after_snapshot_files, before_snapshot_files)
    on.exit(unlink(new_candidate_files, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(unlink(new_snapshot_files, recursive = TRUE, force = TRUE), add = TRUE)

    expect_no_error(st <- endpoint_panel_state())
    expect_true(is.data.frame(st$rows))
    expect_true(any(as.character(st$rows$origin) == "workspace"))
    expect_false(isTRUE(st$working$is_modified))
  })
})

test_that("default endpoint dataset loads into working endpoints when no draft exists", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    ctx <- current_endpoint_graph_context()
    expect_true(is.list(ctx))

    state <- empty_working_endpoint_state(ctx = ctx)
    state <- upsert_working_endpoint_vertex_state(state, 31L)
    save_working_endpoint_state(state, ctx = ctx)
    snap <- save_working_endpoint_snapshot()
    expect_true(isTRUE(snap$ok))

    unlink(endpoint_working_file(ctx$graph_set_id, ctx$k, ctx$project_id), force = TRUE)
    save_endpoint_dataset_meta(list(default_dataset_id = snap$dataset_id), ctx = ctx)
    endpoint_workspace_revision(isolate(endpoint_workspace_revision()) + 1L)
    session$flushReact()

    st <- endpoint_panel_state()
    expect_true(is.data.frame(st$working$rows))
    expect_true(31L %in% st$working$rows$vertex)
    expect_equal(as.character(st$working$base_dataset_id %||% ""), as.character(snap$dataset_id))
  })
})

test_that("symptoms endpoint label provider exposes precomputed and live profile suggestions", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("symptoms" %in% reg$id)) {
    skip("Symptoms project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("symptoms")
    session$flushReact()

    gs <- graph_structure_state()
    k_choices <- if (is.list(gs) && !is.null(gs$k_choices)) as.character(gs$k_choices) else character(0)
    if ("5" %in% k_choices) {
      session$setInputs(graph_k = "5")
      session$flushReact()
    }

    precomputed <- endpoint_label_profile_suggestion(17L, endpoint_panel_state())
    expect_true(is.list(precomputed))
    expect_match(as.character(precomputed$label %||% ""), "L iners")
    expect_true(is.data.frame(precomputed$profile))
    expect_gte(nrow(precomputed$profile), 1L)

    live_only <- endpoint_label_profile_suggestion(1745L, endpoint_panel_state())
    expect_true(is.list(live_only))
    expect_true(is.data.frame(live_only$profile))
    expect_gte(nrow(live_only$profile), 5L)
    expect_false(identical(as.character(live_only$label %||% ""), "v1745"))

    ctx <- current_endpoint_graph_context()
    original <- load_working_endpoint_state(ctx)
    on.exit(save_working_endpoint_state(original, ctx = ctx), add = TRUE)

    session$setInputs(endpoint_vertex_id = 17L)
    session$flushReact()
    add_selected_vertex_to_working_set()
    session$flushReact()

    st <- endpoint_panel_state()
    hit <- which(st$working$rows$vertex == 17L)
    expect_length(hit, 1L)
    expect_match(as.character(st$working$rows$label[[hit[[1]]]]), "L iners")
  })
})

test_that("symptoms subjects panel resolves subject rows and overlay vertices", {
  local_projects_data_sandbox()
  reg <- gflowui::list_projects()
  if (!("symptoms" %in% reg$id)) {
    skip("Symptoms project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("symptoms")
    session$flushReact()

    sp0 <- subject_panel_state()
    expect_true(is.list(sp0))
    expect_true(isTRUE(sp0$available))
    expect_true(is.data.frame(sp0$rows))
    expect_true(all(c("vertex", "subject_id", "sample_id", "week", "day") %in% names(sp0$rows)))
    expect_gt(nrow(sp0$rows), 0L)

    subject_ids <- unique(as.character(sp0$rows$subject_id))
    subject_ids <- subject_ids[nzchar(subject_ids)]
    expect_gte(length(subject_ids), 2L)
    selected_subjects <- subject_ids[seq_len(2L)]
    expected_rows <- sp0$rows[as.character(sp0$rows$subject_id) %in% selected_subjects, , drop = FALSE]
    expect_gt(nrow(expected_rows), 0L)

    session$setInputs(subject_ids = selected_subjects)
    session$flushReact()
    ov0 <- subject_overlay_active()
    expect_length(ov0$vertices, 0L)

    session$setInputs(
      subject_show_overlay = TRUE,
      subject_dim_background = TRUE,
      subject_background_opacity = "0.30"
    )
    session$flushReact()

    sp1 <- subject_panel_state()
    ov1 <- subject_overlay_active()
    expect_equal(sort(as.character(sp1$selected_ids %||% character(0))), sort(selected_subjects))
    expect_equal(sort(as.integer(ov1$vertices)), sort(as.integer(sp1$selected_rows$vertex)))
    expect_equal(length(ov1$hover_text), nrow(sp1$selected_rows))
    expect_true(isTRUE(sp1$dim_background))
    expect_equal(as.numeric(sp1$background_opacity %||% NA_real_), 0.30)
    expect_equal(sort(unique(as.character(ov1$vertex_subject_ids %||% character(0)))), sort(selected_subjects))
    expect_gte(length(unique(as.character(ov1$vertex_colors %||% character(0)))), 2L)
    expect_true(all(c("none", "vertex", "sample", "visit") %in% unname(sp1$label_choices %||% character(0))))

    session$setInputs(
      subject_edge_mode = "graph",
      subject_label_mode = "sample",
      subject_label_size = "1.2"
    )
    session$flushReact()

    sp2 <- subject_panel_state()
    ov2 <- subject_overlay_active()
    expect_equal(as.character(sp2$edge_mode %||% ""), "graph")
    expect_equal(as.character(sp2$label_mode %||% ""), "sample")
    expect_true(is.matrix(ov2$edges))
    expect_equal(ncol(ov2$edges), 2L)
    expect_equal(length(ov2$label_text), nrow(sp2$selected_rows))
    expect_equal(as.character(ov2$label_text), as.character(sp2$selected_rows$sample_id))
    expect_equal(as.numeric(ov2$label_size %||% NA_real_), 1.2)
    expect_true(is.data.frame(ov2$rows))
    expect_equal(nrow(ov2$rows), nrow(sp2$selected_rows))
  })
})
