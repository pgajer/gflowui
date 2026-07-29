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

grouped_selector_project_id <- function() {
  listed <- gflowui::list_projects(include_manifests = TRUE)
  reg <- listed$registry
  manifests <- listed$manifests
  if (!is.data.frame(reg) || nrow(reg) < 1L) {
    return("")
  }

  for (project_id in as.character(reg$id)) {
    manifest <- manifests[[project_id]]
    schema <- NULL
    if (is.list(manifest$metadata) && is.list(manifest$metadata$graph_selector_schema)) {
      schema <- manifest$metadata$graph_selector_schema
    } else if (is.list(manifest$graph_selector_schema)) {
      schema <- manifest$graph_selector_schema
    }
    fields <- schema$fields
    if (!is.null(fields) && length(fields) > 0L) {
      return(project_id)
    }
  }

  ""
}


test_that("graph adapter returns expected shape", {
  skip_if_not_installed("dgraphs")
  has_builder <- exists(
    "build.iknn.graphs.and.selectk",
    envir = asNamespace("dgraphs"),
    inherits = FALSE
  )
  skip_if_not(has_builder)
  has_endpoints <- exists(
    "geodesic.core.endpoints",
    envir = asNamespace("dgraphs"),
    inherits = FALSE
  )
  skip_if_not(has_endpoints)

  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph(x, kmin = 5, kmax = 9, method = "edit")
  expect_true(is.list(g))
  expect_true("selected.k" %in% names(g))
  expect_true("selected.graph" %in% names(g))
  expect_true("adj.list" %in% names(g))

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

test_that("conditional-expectation adapter returns expected shape", {
  skip_if_not_installed("gflow")
  skip_if_not_installed("dgraphs")

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
  skip_if_not(has_fit && has_refit)

  x <- matrix(rnorm(60), nrow = 20, ncol = 3)
  g <- gflowui:::gflow_build_graph(x, kmin = 5, kmax = 9, method = "edit")
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

test_that("project open reactive graph settles after endpoint UI sync", {
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")

    settled <- FALSE
    for (ii in seq_len(12)) {
      if (!isTRUE(session$flushReact())) {
        settled <- TRUE
        break
      }
    }

    expect_true(settled)
    expect_false(isTRUE(session$flushReact()))
  })
})

test_that("grouped selector project settles after project open", {
  local_projects_data_sandbox()

  project_id <- grouped_selector_project_id()
  if (!nzchar(project_id)) {
    skip("No grouped selector project is registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project(project_id)

    settled <- FALSE
    for (ii in seq_len(12)) {
      if (!isTRUE(session$flushReact())) {
        settled <- TRUE
        break
      }
    }

    expect_true(settled)
    expect_false(isTRUE(session$flushReact()))
  })
})

test_that("grouped selector graph selection ignores unrelated inputs", {
  local_projects_data_sandbox()

  project_id <- grouped_selector_project_id()
  if (!nzchar(project_id)) {
    skip("No grouped selector project is registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project(project_id)
    for (ii in seq_len(12)) {
      if (!isTRUE(session$flushReact())) {
        break
      }
    }

    invalidations <- 0L
    obs <- shiny::observeEvent(current_graph_selection(), {
      invalidations <<- invalidations + 1L
    }, ignoreInit = TRUE)
    withr::defer(obs$destroy())

    session$setInputs(graph_layout_renderer = "rglwidget")
    session$flushReact()
    expect_equal(invalidations, 0L)

    selector_fields <- current_graph_selection()$selector_fields
    selector_fields <- if (is.list(selector_fields)) selector_fields else list()
    multi_choice_idx <- which(vapply(selector_fields, function(spec) {
      choices <- unname(as.character(spec$choices %||% character(0)))
      length(unique(choices[nzchar(choices)])) > 1L
    }, logical(1)))
    if (length(multi_choice_idx) < 1L) {
      skip("Grouped selector project does not expose a multi-choice selector")
    }

    spec <- selector_fields[[multi_choice_idx[[1L]]]]
    choices <- unique(unname(as.character(spec$choices %||% character(0))))
    choices <- choices[nzchar(choices)]
    alt_choice <- setdiff(choices, as.character(spec$selected %||% ""))
    if (length(alt_choice) < 1L) {
      skip("No alternate grouped selector choice is available")
    }

    args <- list()
    args[[as.character(spec$input_id %||% "")]] <- alt_choice[[1L]]
    do.call(session$setInputs, args)
    session$flushReact()
    expect_gte(invalidations, 1L)
  })
})

test_that("default sidebar control values do not keep re-invalidating the app", {
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    expect_equal(subject_state$vertex_size, 1.0)
    controls <- htmltools::renderTags(output$workflow_controls)$html
    expect_match(controls, "0.75x", fixed = TRUE)
    expect_match(controls, "1.25x", fixed = TRUE)

    session$setInputs(
      graph_layout_renderer = "plotly",
      graph_layout_vertex = "point",
      graph_layout_size = "1.0x",
      graph_layout_component = "all",
      subject_show_overlay = FALSE,
      subject_dim_background = FALSE,
      subject_background_opacity = "0.22",
      subject_vertex_color = "#dc2626",
      subject_vertex_size = "1.0",
      subject_edge_mode = "none",
      subject_edge_color = "#dc2626",
      subject_edge_width = "2",
      subject_label_mode = "none",
      subject_label_size = "1.0",
      endpoint_show_working_set = FALSE,
      endpoint_datasets_open = FALSE,
      arm_show_working_set = FALSE,
      arm_datasets_open = FALSE,
      arm_preview_layout_open = FALSE
    )

    settled <- FALSE
    for (ii in seq_len(12)) {
      if (!isTRUE(session$flushReact())) {
        settled <- TRUE
        break
      }
    }

    expect_true(settled)
    expect_false(isTRUE(session$flushReact()))
  })
})

test_that("occupation-density display starts with low opacity 0.2", {
  shiny::testServer(gflowui:::app_server, {
    expect_equal(density_display_snapshot()$low_alpha, 0.2)
  })
})

test_that("subject overlay changes preserve density display settings", {
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    session$setInputs(
      occupation_density_low_color = "blue",
      occupation_density_mid_color = "white",
      occupation_density_high_color = "orange",
      occupation_density_low_alpha = "0.2",
      occupation_density_mid_alpha = "0.45",
      occupation_density_high_alpha = "0.8",
      subject_show_overlay = TRUE
    )
    session$setInputs(density_display_client_snapshot = list(
      low = "blue",
      midpoint = "white",
      high = "orange",
      low_alpha = 0.2,
      midpoint_alpha = 0.45,
      high_alpha = 0.8,
      nonce = 1
    ))
    session$flushReact()
    before <- density_display_snapshot()

    session$setInputs(
      workflow_accordion = c(
        "workflow_graph_structure",
        "workflow_subject_structure"
      ),
      subject_show_overlay = FALSE
    )
    session$flushReact()
    after <- density_display_snapshot()

    expect_identical(after, before)
    expect_identical(after$low, "blue")
    expect_identical(after$midpoint, "white")
    expect_identical(after$high, "orange")
    expect_equal(after$low_alpha, 0.2)
    expect_equal(after$midpoint_alpha, 0.45)
    expect_equal(after$high_alpha, 0.8)
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

test_that("basin panel discovers conditional-expectation estimates", {
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  if (!("agp" %in% reg$id)) {
    skip("AGP project is not registered in this environment")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project("agp")
    session$flushReact()

    panel <- basin_panel_state()
    expect_true(isTRUE(panel$has_sources))
    expect_true(any(grepl(
      "CondExp",
      names(panel$choices),
      fixed = TRUE
    )))

    controls <- htmltools::renderTags(output$workflow_controls)$html
    expect_false(grepl("Largest maximum basins", controls, fixed = TRUE))
    expect_false(grepl("Largest minimum basins", controls, fixed = TRUE))
    expect_false(grepl("Ranking measure", controls, fixed = TRUE))
    expect_false(grepl("Field extrema", controls, fixed = TRUE))
    expect_match(
      controls,
      "Compute &amp; Open Basin Inspector",
      fixed = TRUE
    )
    expect_false(grepl(
      ">Open Basin Inspector<",
      controls,
      fixed = TRUE
    ))
    expect_false(grepl("Flow direction", controls, fixed = TRUE))
  })
})

test_that("basin server invalidates changed fields and graph identities", {
  local_projects_data_sandbox()

  project_id <- "hmp_subject15_k03_heat_basin_path"
  reg <- gflowui::list_projects()
  if (!(project_id %in% reg$id)) {
    skip("The Subject 15 reference project is not registered")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project(project_id)
    session$flushReact()
    session$setInputs(
      occupation_density_mode = "parameters",
      occupation_density_subject = "15",
      occupation_density_method = "graph_heat_kernel",
      occupation_density_eta_index = "4",
      subject_show_overlay = FALSE
    )
    show_occupation_density_selection(notify_errors = FALSE)
    session$flushReact()
    expect_false(isTRUE(subject_state$show_overlay))
    session$setInputs(
      basin_source = "occupation_density_active",
      basin_top_k_max = 1L,
      basin_top_k_min = 1L,
      basin_rank_by = "auto",
      basin_compute = 1L
    )
    session$flushReact()

    first <- basin_result()
    expect_true(is.list(first))
    expect_true(isTRUE(basin_inspector_open()))
    expect_gte(nrow(first$all_table), nrow(first$table))
    expect_true(all(is.finite(first$all_table$prominence)))
    expect_length(basin_selected_keys(), 0L)
    expect_false(any(first$table$selected))
    first.identity <- first$construction_identity$fingerprint
    expect_true(nzchar(first.identity))

    workspace <- htmltools::renderTags(output$workspace_view)$html
    expect_match(workspace, "gf_reference_split", fixed = TRUE)
    expect_match(workspace, "General Inspector", fixed = TRUE)
    expect_match(workspace, "Resize General Inspector", fixed = TRUE)
    inspector <- htmltools::renderTags(output$basin_inspector_ui)$html
    expect_match(inspector, "Basin Inspector", fixed = TRUE)
    expect_match(inspector, "Largest maximum basins", fixed = TRUE)
    expect_match(inspector, "Largest minimum basins", fixed = TRUE)
    expect_match(inspector, "Ranking measure", fixed = TRUE)
    expect_match(inspector, "Maximum extrema", fixed = TRUE)
    expect_match(inspector, "Minimum extrema", fixed = TRUE)
    expect_match(inspector, "Selected basins", fixed = TRUE)
    expect_match(inspector, "Listed top-K", fixed = TRUE)
    expect_match(inspector, "Basin characteristics", fixed = TRUE)
    expect_match(inspector, "Extremum / basin", fixed = TRUE)
    expect_match(inspector, "gf-basin-show-column", fixed = TRUE)
    expect_match(inspector, "gf-basin-label-column", fixed = TRUE)
    expect_match(inspector, ">M1<", fixed = TRUE)
    expect_match(inspector, ">m1<", fixed = TRUE)
    expect_match(inspector, ">Extremum value</th>", fixed = TRUE)
    expect_match(inspector, ">Support</th>", fixed = TRUE)
    expect_match(inspector, ">Mass</th>", fixed = TRUE)
    expect_match(inspector, ">Prominence</th>", fixed = TRUE)
    expect_match(inspector, "Ranking measure:", fixed = TRUE)
    expect_false(grepl(">Extremum vertex</th>", inspector, fixed = TRUE))
    expect_false(grepl(">Primary support</th>", inspector, fixed = TRUE))
    expect_false(grepl(">Primary mass</th>", inspector, fixed = TRUE))
    expect_false(grepl(">Allocated mass</th>", inspector, fixed = TRUE))
    expect_false(grepl(">Ranking measure</th>", inspector, fixed = TRUE))
    expect_false(grepl("<th>Type</th>", inspector, fixed = TRUE))
    expect_false(grepl("<th>Rank</th>", inspector, fixed = TRUE))
    expect_false(grepl("<th>Basin</th>", inspector, fixed = TRUE))
    expect_match(
      inspector,
      "gflowui-general-inspector-width",
      fixed = TRUE
    )
    expect_false(grepl("basin_inspector_maximize", inspector, fixed = TRUE))

    plot.workspace <- htmltools::renderTags(
      output$basin_plot_workspace_ui
    )$html
    expect_match(plot.workspace, "Basin Plot Workspace", fixed = TRUE)
    expect_match(plot.workspace, "Characteristics", fixed = TRUE)
    expect_match(plot.workspace, "Add histograms", fixed = TRUE)
    expect_match(plot.workspace, "Add pair plots", fixed = TRUE)
    expect_match(plot.workspace, "Add matrix", fixed = TRUE)
    session$setInputs(
      basin_plot_features = c("support", "mass"),
      basin_plot_builder_scope = "all",
      basin_plot_builder_type = "both",
      basin_plot_add_histograms = 1L
    )
    session$flushReact()
    expect_length(basin_plot_specs(), 2L)
    expect_true(all(vapply(
      basin_plot_specs(),
      function(spec) identical(spec$kind, "histogram"),
      logical(1)
    )))
    plot.workspace.with.cards <- htmltools::renderTags(
      output$basin_plot_workspace_ui
    )$html
    expect_match(
      plot.workspace.with.cards,
      "Support distribution",
      fixed = TRUE
    )
    expect_match(
      plot.workspace.with.cards,
      "Mass distribution",
      fixed = TRUE
    )
    expect_match(plot.workspace.with.cards, "Value scale", fixed = TRUE)
    session$setInputs(basin_plot_clear_all = 1L)
    session$flushReact()
    expect_length(basin_plot_specs(), 0L)
    session$setInputs(
      basin_plot_features = c("support", "mass", "prominence"),
      basin_plot_add_pairs = 1L
    )
    session$flushReact()
    expect_length(basin_plot_specs(), 3L)
    expect_true(all(vapply(
      basin_plot_specs(),
      function(spec) identical(spec$kind, "scatter"),
      logical(1)
    )))
    plot.workspace.with.pairs <- htmltools::renderTags(
      output$basin_plot_workspace_ui
    )$html
    expect_match(plot.workspace.with.pairs, "X-axis scale", fixed = TRUE)
    expect_match(plot.workspace.with.pairs, "Y-axis scale", fixed = TRUE)
    session$setInputs(basin_plot_clear_all = 2L)
    session$flushReact()
    session$setInputs(
      basin_plot_features = c(
        "support", "mass", "extremum_value", "prominence"
      ),
      basin_plot_add_matrix = 1L
    )
    session$flushReact()
    expect_length(basin_plot_specs(), 1L)
    expect_identical(basin_plot_specs()[[1L]]$kind, "matrix")
    plot.workspace.with.matrix <- htmltools::renderTags(
      output$basin_plot_workspace_ui
    )$html
    expect_match(
      plot.workspace.with.matrix,
      "All coordinate scales",
      fixed = TRUE
    )

    session$setInputs(basin_inspector_show_extremum_vertex = TRUE)
    session$flushReact()
    inspector.with.vertex <- htmltools::renderTags(
      output$basin_inspector_ui
    )$html
    expect_match(
      inspector.with.vertex,
      ">Extremum vertex</th>",
      fixed = TRUE
    )
    expect_match(
      inspector.with.vertex,
      sprintf(">%d</td>", as.integer(first$table$extremum.vertex[[1L]])),
      fixed = TRUE
    )
    external.vertex <- as.character(first$table$extremum.vertex.id[[1L]])
    if (!identical(
        external.vertex,
        as.character(first$table$extremum.vertex[[1L]])
    )) {
      expect_false(grepl(external.vertex, inspector.with.vertex, fixed = TRUE))
    }

    selected.key <- as.character(first$table$key[[1L]])
    session$setInputs(basin_inspector_row_event = list(
      key = selected.key,
      role = "selection",
      checked = TRUE,
      value = "",
      nonce = 1
    ))
    session$flushReact()
    expect_true(selected.key %in% basin_selected_keys())
    expect_true(basin_result()$table$selected[[1L]])
    session$setInputs(basin_inspector_row_event = list(
      key = selected.key,
      role = "selection",
      checked = FALSE,
      value = "",
      nonce = 2
    ))
    session$flushReact()
    expect_false(selected.key %in% basin_selected_keys())
    expect_false(basin_result()$table$selected[[1L]])

    if (requireNamespace("plotly", quietly = TRUE)) {
      trace.names <- function() {
        payload <- jsonlite::fromJSON(
          as.character(output$reference_plot),
          simplifyVector = FALSE
        )
        vapply(
          payload$x$data %||% list(),
          function(trace) as.character(trace$name %||% ""),
          character(1)
        )
      }
      session$setInputs(
        basin_extrema_max_scope = "none",
        basin_extrema_min_scope = "none"
      )
      session$flushReact()
      expect_false(any(grepl("^Local max|^Local min", trace.names())))
      session$setInputs(basin_extrema_max_scope = "all")
      session$flushReact()
      expect_true("Local maxima" %in% trace.names())
      session$setInputs(basin_extrema_max_scope = "none")
      session$flushReact()
      expect_false("Local maxima" %in% trace.names())
      session$setInputs(basin_extrema_min_scope = "all")
      session$flushReact()
      expect_true("Local minima" %in% trace.names())
      session$setInputs(basin_extrema_min_scope = "none")
      session$flushReact()
      expect_false("Local minima" %in% trace.names())
    }

    session$setInputs(basin_inspector_width = 760)
    session$flushReact()
    expect_equal(basin_display_settings$inspector_width, 760L)
    resized.workspace <- htmltools::renderTags(output$workspace_view)$html
    expect_match(
      resized.workspace,
      "--gf-general-inspector-width:760px",
      fixed = TRUE
    )

    session$setInputs(basin_inspector_close = 1L)
    session$flushReact()
    expect_false(isTRUE(basin_inspector_open()))
    session$setInputs(basin_compute = 2L)
    session$flushReact()
    expect_true(isTRUE(basin_inspector_open()))
    expect_identical(
      basin_result()$construction_identity$fingerprint,
      first.identity
    )
    expect_match(basin_status(), "without reconstruction", fixed = TRUE)

    session$setInputs(occupation_density_eta_index = "5")
    session$flushReact()
    expect_null(basin_result())
    expect_false(isTRUE(basin_inspector_open()))
    expect_match(basin_status(), "changed|stale", ignore.case = TRUE)

    session$setInputs(basin_compute = 3L)
    session$flushReact()
    second <- basin_result()
    expect_true(is.list(second))
    expect_false(identical(
      first.identity,
      second$construction_identity$fingerprint
    ))

    request <- basin_construction_request(basin_source_state())
    changed.graph <- request$graph_identity
    changed.graph$graph.fingerprint <- paste0(
      changed.graph$graph.fingerprint,
      "-changed"
    )
    request$construction_identity <- gflowui:::gflowui_basin_construction_identity(
      project_id = request$source$graph$project_id,
      graph_set_id = request$source$graph$set_id,
      graph_identity = changed.graph,
      source_key = request$source$key,
      source_fingerprint = request$source_fingerprint,
      field = request$source$values,
      vertex_mass = request$source$values,
      vertex_mass_provenance = request$mass_provenance,
      alignment_validation = request$alignment,
      build_identity = request$build_identity
    )
    expect_true(invalidate_basin_result_if_needed(
      request,
      "Displayed graph changed."
    ))
    expect_null(basin_result())
    expect_match(basin_status(), "graph changed", ignore.case = TRUE)

    session$setInputs(basin_compute = 4L)
    session$flushReact()
    expect_true(is.list(basin_result()))
    expect_true(isTRUE(basin_result()$cache_hit))
  })
})

test_that("arm builder endpoint choices begin with explicit NONE", {
  local_projects_data_sandbox()

  reg <- gflowui::list_projects()
  project_id <- "hmp_subject15_k03_heat_basin_path"
  if (!(project_id %in% reg$id)) {
    skip("The Subject 15 reference project is not registered")
  }

  shiny::testServer(gflowui:::app_server, {
    open_project(project_id)
    session$flushReact()

    choices <- arm_builder_endpoint_choices()
    expect_identical(names(choices)[[1L]], "NONE")
    expect_identical(unname(choices[[1L]]), "none")
    expect_null(resolve_arm_endpoint_choice("none"))
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

test_that("manifest subject provider supports active graph-set filtering and temporal edges", {
  local_projects_data_sandbox()

  root <- tempfile("generic-subject-project-")
  dir.create(file.path(root, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "results"), recursive = TRUE, showWarnings = FALSE)

  make_graph <- function(edges, n = 4L) {
    adj <- vector("list", n)
    wt <- vector("list", n)
    for (ii in seq_len(n)) {
      adj[[ii]] <- integer(0)
      wt[[ii]] <- numeric(0)
    }
    for (ii in seq_len(nrow(edges))) {
      aa <- as.integer(edges[ii, 1])
      bb <- as.integer(edges[ii, 2])
      adj[[aa]] <- c(adj[[aa]], bb)
      adj[[bb]] <- c(adj[[bb]], aa)
      wt[[aa]] <- c(wt[[aa]], 1)
      wt[[bb]] <- c(wt[[bb]], 1)
    }
    list(adj_list = adj, weight_list = wt)
  }

  graph_a <- file.path(root, "results", "set_a_graph.rds")
  graph_b <- file.path(root, "results", "set_b_graph.rds")
  saveRDS(
    list(X.graphs = list(make_graph(matrix(c(1, 2, 2, 3, 3, 4), ncol = 2, byrow = TRUE))), k.values = 3L, selected.k = 3L),
    graph_a
  )
  saveRDS(
    list(X.graphs = list(make_graph(matrix(c(4, 3, 3, 2, 2, 1), ncol = 2, byrow = TRUE))), k.values = 3L, selected.k = 3L),
    graph_b
  )

  feature_cols <- c("Lactobacillus_crispatus", "Gardnerella_vaginalis", "BVAB1")
  feature_matrices <- list(
    graph_representations = list(
      rep_a = matrix(
        c(
          0.90, 0.10, 0.00,
          1.00, 0.00, 0.00,
          0.00, 0.80, 0.20,
          0.00, 0.00, 1.00
        ),
        nrow = 4L,
        byrow = TRUE,
        dimnames = list(paste0("a", seq_len(4L)), feature_cols)
      ),
      rep_b = matrix(
        c(
          0.00, 0.00, 1.00,
          0.00, 1.00, 0.00,
          1.00, 0.00, 0.00,
          0.50, 0.50, 0.00
        ),
        nrow = 4L,
        byrow = TRUE,
        dimnames = list(paste0("b", seq_len(4L)), feature_cols)
      )
    )
  )
  feature_matrix_file <- file.path(root, "data", "feature_matrices.rds")
  saveRDS(feature_matrices, feature_matrix_file)

  vertex_metadata <- rbind(
    data.frame(representation_id = "rep_a", graph_vertex_id = seq_len(4L), first_UID = paste0("A", seq_len(4L))),
    data.frame(representation_id = "rep_b", graph_vertex_id = seq_len(4L), first_UID = paste0("B", seq_len(4L)))
  )
  utils::write.table(
    vertex_metadata,
    file.path(root, "data", "vertex_metadata.tsv"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  subject_rows <- data.frame(
    graph_set_id = c(rep("set_a", 3L), rep("set_b", 3L)),
    vertex = c(1L, 2L, 3L, 4L, 3L, 2L),
    subject_id = "S1",
    sample_id = paste0("sample_", seq_len(6L)),
    week = c(1L, 1L, 2L, 1L, 1L, 2L),
    day = c(1L, 2L, 1L, 1L, 2L, 1L),
    time_idx = c(1, 2, 3, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    subject_rows,
    file.path(root, "data", "subject_rows.tsv"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(
      list(id = "set_a", label = "Set A", graph_file = graph_a, k_values = 3L, selected_k = 3L, representation_id = "rep_a"),
      list(id = "set_b", label = "Set B", graph_file = graph_b, k_values = 3L, selected_k = 3L, representation_id = "rep_b")
    ),
    defaults = list(graph_set_id = "set_a", reference_graph_set_id = "set_a", reference_k = 3L),
    metadata = list(
      subject_provider = list(
        mode = "sample_vertex_map",
        rows_file = "data/subject_rows.tsv",
        graph_set_col = "graph_set_id",
        vertex_col = "vertex",
        subject_col = "subject_id",
        sample_col = "sample_id",
        week_col = "week",
        day_col = "day",
        order_col = "time_idx"
      ),
      endpoint_label_provider = list(
        mode = "feature_matrices",
        matrix_file = "data/feature_matrices.rds",
        representations_object = "graph_representations",
        vertex_metadata_file = "data/vertex_metadata.tsv",
        graph_set_matrix_map = list(set_a = "rep_a", set_b = "rep_b"),
        representation_col = "representation_id",
        vertex_col = "graph_vertex_id",
        sample_col = "first_UID",
        label_style = "taxonomy_profile"
      ),
      overview = list(
        generated_at = "2026-06-08 00:00:00 EDT"
      )
    )
  )
  gflowui::register_project(
    project_root = root,
    project_id = "generic_subject_project",
    project_name = "Generic Subject Project",
    profile = "iknn_3x3",
    project_spec = spec,
    scan_results = FALSE,
    overwrite = TRUE
  )

  shiny::testServer(gflowui:::app_server, {
    open_project("generic_subject_project")
    session$flushReact()

    sp0 <- subject_panel_state()
    expect_true(isTRUE(sp0$available))
    expect_equal(unique(as.character(sp0$rows$graph_set_id)), "set_a")
    overview <- project_overview_state()
    expect_true(is.list(overview))
    expect_equal(overview$artifact_choices, character(0))
    label_a <- endpoint_label_profile_suggestion(2L, endpoint_panel_state())
    expect_equal(label_a$label, "L crispatus")
    expect_equal(label_a$sample_id, "A2")
    expect_equal(label_a$profile$feature[[1L]], "Lactobacillus_crispatus")

    session$setInputs(
      subject_ids = "S1",
      subject_show_overlay = TRUE,
      subject_edge_mode = "temporal"
    )
    session$flushReact()

    ov_a <- subject_overlay_active()
    expect_equal(as.integer(ov_a$vertices), c(1L, 2L, 3L))
    expect_equal(ov_a$edges, matrix(c(1L, 2L, 2L, 3L), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))))

    session$setInputs(graph_data_type = "set_b")
    session$flushReact()

    sp1 <- subject_panel_state()
    expect_equal(unique(as.character(sp1$rows$graph_set_id)), "set_b")
    label_b <- endpoint_label_profile_suggestion(2L, endpoint_panel_state())
    expect_equal(label_b$label, "Gardnerella vaginalis")
    expect_equal(label_b$sample_id, "B2")
    ov_b <- subject_overlay_active()
    expect_equal(as.integer(ov_b$vertices), c(4L, 3L, 2L))
    expect_equal(ov_b$edges, matrix(c(4L, 3L, 3L, 2L), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))))
  })
})
