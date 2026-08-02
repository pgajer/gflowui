.gflowui_basin_panel_stop <- function(
    message,
    class = "gflowui_basin_panel_error") {
  .gflowui_basin_stop(message, class)
}

.gflowui_basin_panel_elapsed_ms <- function(started) {
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  max(0, as.numeric(elapsed) * 1000)
}

.gflowui_basin_panel_attempt_key <- function(state) {
  active <- if (is.list(state)) state$active.attempt else NULL
  if (!is.list(active)) {
    return(NULL)
  }
  list(
    bundle.id = active$bundle.id,
    context.generation = active$context.generation,
    attempt.id = active$attempt.id,
    outcome = active$outcome
  )
}

gflowui_basin_start_panel_event <- function(
    state,
    event,
    session.id,
    construction.fingerprint = "",
    reducer = gflowui_basin_reduce_state) {
  .gflowui_basin_assert_runtime_state(state)
  if (!is.function(reducer)) {
    .gflowui_basin_panel_stop("A basin-analysis reducer is required.")
  }
  before <- .gflowui_basin_panel_attempt_key(state)
  reduced <- tryCatch(
    list(state = reducer(state, event), error = NULL),
    error = function(error) list(state = state, error = error)
  )
  if (!is.null(reduced$error)) {
    return(list(
      state = reduced$state,
      job = NULL,
      disposition = "internal_transition_failure",
      diagnostic = list(
        kind = "internal_transition_failure",
        message = conditionMessage(reduced$error),
        condition.class = class(reduced$error)
      )
    ))
  }
  next.state <- reduced$state
  after <- .gflowui_basin_panel_attempt_key(next.state)
  attempt.changed <- !identical(before, after)
  job <- NULL
  if (attempt.changed &&
      is.list(after) &&
      identical(after$outcome, "pending")) {
    job <- tryCatch(
      gflowui_basin_async_job(
        next.state,
        session.id = session.id,
        construction.fingerprint = construction.fingerprint
      ),
      error = identity
    )
    if (inherits(job, "error")) {
      return(list(
        state = next.state,
        job = NULL,
        disposition = "internal_transition_failure",
        diagnostic = list(
          kind = "internal_transition_failure",
          message = conditionMessage(job),
          condition.class = class(job)
        )
      ))
    }
  }
  disposition <- if (!attempt.changed) {
    "presentation_updated"
  } else if (!is.null(job)) {
    "proposal_pending"
  } else if (is.list(after) &&
      identical(after$outcome, "blocked")) {
    "scientific_blocked"
  } else {
    "state_updated"
  }
  list(
    state = next.state,
    job = job,
    disposition = disposition,
    diagnostic = NULL
  )
}

.gflowui_basin_panel_component_counts <- function(data) {
  ids <- as.integer(data$component.ids)
  values <- vapply(ids, function(component) {
    sum(data$canonical$component == component)
  }, integer(1))
  stats::setNames(values, as.character(ids))
}

.gflowui_basin_panel_overflow <- function(proposal, counts) {
  if (identical(proposal$render.outcome, "renderable")) {
    return(NULL)
  }
  budget <- proposal$accepted.parameters$final.render.budget
  preclosure.count <- length(unique(c(
    proposal$core$ids,
    proposal$sentinels$ids
  )))
  messages <- c(
    core_overflow = sprintf(
      paste(
        "The complete %d-branch core exceeds the final render budget of %d.",
        "No mandatory core branch was trimmed."
      ),
      counts$core,
      budget
    ),
    sentinel_overflow = sprintf(
      paste(
        "The %d-branch pre-closure mandatory union exceeds the final",
        "render budget of %d after sentinels and pins.",
        "No mandatory branch was trimmed."
      ),
      preclosure.count,
      budget
    ),
    closure_overflow = sprintf(
      paste(
        "Canonical ancestor closure expands %d mandatory branches to %d,",
        "exceeding the final render budget of %d.",
        "No canonical ancestor was trimmed."
      ),
      preclosure.count,
      counts$final,
      budget
    )
  )
  list(
    outcome = proposal$render.outcome,
    message = unname(messages[[proposal$render.outcome]]),
    core.count = counts$core,
    preclosure.count = preclosure.count,
    final.count = counts$final,
    budget = as.integer(budget)
  )
}

.gflowui_basin_panel_empty_model <- function(state = NULL) {
  active <- if (is.list(state)) state$active.attempt else NULL
  data <- if (is.list(state)) {
    gflowui_basin_bundle_snapshot(state$bundle)
  } else {
    NULL
  }
  component <- if (is.list(state)) {
    state$context$component
  } else {
    NULL
  }
  component.counts <- if (is.list(data)) {
    .gflowui_basin_panel_component_counts(data)
  } else {
    integer()
  }
  structure(
    list(
      ready = FALSE,
      outcome = as.character(active$outcome %||% "not_started"),
      reason = as.character(active$reason %||% ""),
      messages = as.character(active$messages %||% character()),
      display.source = if (is.list(state)) {
        as.character(state$display.source %||% "none")
      } else {
        "none"
      },
      state = state,
      controls = if (is.list(state)) {
        .gflowui_basin_copy(state$controls)
      } else {
        NULL
      },
      presentation = if (is.list(state)) {
        .gflowui_basin_copy(state$presentation)
      } else {
        NULL
      },
      component = if (is.list(state)) {
        list(
          id = component,
          count = length(data$component.ids),
          ids = as.integer(data$component.ids),
          counts = component.counts,
          maximum.count = as.integer(
            component.counts[[as.character(component)]]
          )
        )
      } else {
        NULL
      },
      diagnostics.visible = if (is.list(state)) {
        isTRUE(state$presentation$diagnostics.visible)
      } else {
        FALSE
      },
      proposal = NULL,
      bundle = if (is.list(state)) state$bundle else NULL,
      layout = NULL,
      layout.elapsed.ms = 0,
      overflow = NULL,
      selected = list(
        ids = character(),
        visible = character(),
        hidden = character(),
        pinned = character()
      )
    ),
    class = c("gflowui_basin_merge_tree_panel_model", "list")
  )
}

gflowui_basin_merge_tree_panel_model <- function(
    state,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  if (is.null(state)) {
    return(.gflowui_basin_panel_empty_model())
  }
  .gflowui_basin_assert_runtime_state(state)
  proposal <- gflowui_basin_displayed_proposal(state)
  if (is.null(proposal)) {
    return(.gflowui_basin_panel_empty_model(state))
  }
  data <- .gflowui_basin_assert_pair(proposal, state$bundle)
  component.data <- data$canonical[
    data$canonical$component == proposal$component$id,
    ,
    drop = FALSE
  ]
  counts <- gflowui_basin_derive_counts(proposal, state$bundle)
  mass <- gflowui_basin_derive_mass(proposal, state$bundle)
  diagnostics <- gflowui_basin_derive_diagnostics(
    proposal,
    state$bundle
  )
  labels <- gflowui_basin_derive_labels(
    proposal,
    state$bundle,
    important.label.n = state$presentation$important.label.n,
    label.mode = state$presentation$label.mode,
    selected.ids = state$selected.ids
  )
  status <- gflowui_basin_derive_status(proposal, state$bundle)
  layout <- NULL
  layout.elapsed.ms <- 0
  if (identical(proposal$render.outcome, "renderable")) {
    if (!is.function(layout.accessor)) {
      .gflowui_basin_panel_stop(
        "The reviewed Phase 1 layout accessor is unavailable."
      )
    }
    started <- unname(proc.time()[["elapsed"]])
    layout <- gflowui_basin_derive_layout(
      proposal,
      state$bundle,
      layout.accessor = layout.accessor
    )
    layout.elapsed.ms <- .gflowui_basin_panel_elapsed_ms(started)
    layout.ids <- sort(
      as.character(layout$basin.ids %||%
        layout$branches$basin.id),
      method = "radix"
    )
    if (!identical(
      layout.ids,
      sort(proposal$final.ids, method = "radix")
    )) {
      .gflowui_basin_panel_stop(
        "The rendered Phase 1 layout differs from the accepted proposal.",
        "gflowui_basin_panel_layout_error"
      )
    }
  }
  sentinel.only <- setdiff(
    proposal$sentinels$ids,
    proposal$core$ids
  )
  classes <- stats::setNames(
    rep("hidden", nrow(component.data)),
    component.data$basin.id
  )
  classes[proposal$final.ids] <- "displayed"
  classes[proposal$ancestor.only.ids] <- "ancestor_only"
  classes[sentinel.only] <- "sentinel_only"
  classes[proposal$core$ids] <- "core"
  classes[proposal$pinned.ids] <- "pinned"
  selected.visible <- intersect(state$selected.ids, proposal$final.ids)
  selected.hidden <- setdiff(
    intersect(state$selected.ids, proposal$component$ids),
    proposal$final.ids
  )
  classes[selected.visible] <- "selected"
  label.text <- stats::setNames(
    component.data$basin.id,
    component.data$basin.id
  )
  coverage <- if (isTRUE(mass$available)) {
    as.numeric(mass$final.coverage)
  } else {
    NULL
  }
  structure(
    list(
      ready = TRUE,
      outcome = as.character(
        state$active.attempt$outcome %||% "proposal_created"
      ),
      reason = as.character(state$active.attempt$reason %||% ""),
      messages = as.character(
        state$active.attempt$messages %||% character()
      ),
      display.source = state$display.source,
      retained = identical(
        state$display.source,
        "retained_last_valid"
      ),
      active.attempt = .gflowui_basin_copy(state$active.attempt),
      context.generation = state$context.generation,
      controls = .gflowui_basin_copy(state$controls),
      presentation = .gflowui_basin_copy(state$presentation),
      component = list(
        id = proposal$component$id,
        count = length(data$component.ids),
        ids = as.integer(data$component.ids),
        counts = .gflowui_basin_panel_component_counts(data),
        maximum.count = nrow(component.data),
        selection.rule = proposal$component$selection.rule,
        fallback.reason = proposal$component$fallback.reason
      ),
      total.maximum.count = nrow(data$canonical),
      proposal = proposal,
      bundle = state$bundle,
      counts = counts,
      preclosure.count = length(unique(c(
        proposal$core$ids,
        proposal$sentinels$ids
      ))),
      mass = mass,
      coverage = coverage,
      status = status,
      diagnostics = diagnostics,
      diagnostics.visible = isTRUE(
        state$presentation$diagnostics.visible
      ),
      labels = c(labels, list(
        text = label.text,
        mode = state$presentation$label.mode,
        important.n = state$presentation$important.label.n
      )),
      membership.class = classes,
      selected = list(
        ids = state$selected.ids,
        visible = selected.visible,
        hidden = selected.hidden,
        pinned = intersect(state$selected.ids, proposal$pinned.ids)
      ),
      overflow = .gflowui_basin_panel_overflow(proposal, counts),
      layout = layout,
      layout.elapsed.ms = layout.elapsed.ms
    ),
    class = c("gflowui_basin_merge_tree_panel_model", "list")
  )
}

gflowui_basin_complete_merge_tree_layout <- function(
    model,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  if (!inherits(model, "gflowui_basin_merge_tree_panel_model") ||
      !isTRUE(model$ready) ||
      !is.function(layout.accessor)) {
    .gflowui_basin_panel_stop(
      "A ready panel model and reviewed layout accessor are required."
    )
  }
  data <- .gflowui_basin_assert_pair(
    model$proposal,
    model$bundle
  )
  layout.accessor(
    data$canonical.tree,
    direction = "max",
    component = model$component$id
  )
}

.gflowui_basin_panel_tree_copy <- function(model) {
  data <- .gflowui_basin_assert_pair(model$proposal, model$bundle)
  tree <- .gflowui_basin_copy(data$canonical.tree)
  index <- match(
    tree$basin.table$basin.id,
    data$canonical$basin.id
  )
  tree$basin.table$gflowui.trajectory.flow.mass <-
    data$canonical$trajectory.flow.mass[index]
  tree$basin.table$gflowui.trajectory.flow.support <-
    data$canonical$trajectory.flow.support[index]
  tree
}

.gflowui_basin_panel_branch_colors <- function(model, ids) {
  palette <- c(
    hidden = "#A6ADB4",
    displayed = "#2F6690",
    ancestor_only = "#66727A",
    sentinel_only = "#D08B18",
    core = "#167D6A",
    pinned = "#7851A9",
    selected = "#C9362B"
  )
  classes <- unname(model$membership.class[ids])
  classes[is.na(classes)] <- "hidden"
  stats::setNames(unname(palette[classes]), ids)
}

.gflowui_basin_panel_labels <- function(
    model,
    ids,
    label.text = NULL) {
  if (is.null(label.text)) {
    label.text <- model$labels$text
  }
  if (!is.character(label.text) ||
      is.null(names(label.text)) ||
      anyNA(label.text) ||
      anyNA(names(label.text)) ||
      any(!nzchar(names(label.text)))) {
    .gflowui_basin_panel_stop(
      "Panel labels must be a named, nonmissing character vector."
    )
  }
  label.ids <- intersect(model$labels$ids, ids)
  ## gflow requires one unique nonempty label per displayed branch even when
  ## leaf text is suppressed. Distinct zero-width labels satisfy that public
  ## contract without turning unlabelled branches into visible annotations.
  output <- stats::setNames(
    vapply(
      seq_along(ids),
      function(index) strrep("\u200B", index),
      character(1)
    ),
    ids
  )
  matched <- match(label.ids, names(label.text))
  valid <- !is.na(matched)
  output[label.ids[valid]] <- label.text[matched[valid]]
  output
}

gflowui_basin_draw_merge_tree <- function(
    model,
    complete = FALSE,
    label.text = NULL,
    plotter = gflow::plot.basin.merge.tree) {
  if (!inherits(model, "gflowui_basin_merge_tree_panel_model") ||
      !isTRUE(model$ready) ||
      !is.logical(complete) ||
      length(complete) != 1L ||
      is.na(complete) ||
      !is.function(plotter)) {
    .gflowui_basin_panel_stop(
      "A ready panel model, display mode, and plotter are required."
    )
  }
  if (!complete && !is.null(model$overflow)) {
    .gflowui_basin_panel_stop(
      "An overflow proposal cannot be drawn as an accepted filtered tree.",
      "gflowui_basin_panel_overflow_error"
    )
  }
  data <- .gflowui_basin_assert_pair(model$proposal, model$bundle)
  ids <- if (complete) {
    data$canonical$basin.id[
      data$canonical$component == model$component$id
    ]
  } else {
    model$proposal$final.ids
  }
  labels <- .gflowui_basin_panel_labels(model, ids, label.text)
  visible.label.ids <- intersect(model$labels$ids, ids)
  has.visible.labels <- length(visible.label.ids) > 0L
  colors <- .gflowui_basin_panel_branch_colors(model, ids)
  longest <- if (has.visible.labels) {
    max(nchar(labels[visible.label.ids], type = "width"))
  } else {
    0L
  }
  started <- unname(proc.time()[["elapsed"]])
  plotted <- plotter(
    .gflowui_basin_panel_tree_copy(model),
    direction = "max",
    component = model$component$id,
    type = "tree_and_barcode",
    label = "basin.id",
    labels = labels,
    mass.measure = "gflowui.trajectory.flow.mass",
    support.measure = "gflowui.trajectory.flow.support",
    show.mass = TRUE,
    show.support = TRUE,
    show.leaf.labels = has.visible.labels,
    show.barcode.guides = TRUE,
    show.barcode.birth.labels = has.visible.labels,
    show.barcode.parent.labels = has.visible.labels,
    branch.col = colors,
    main.tree = if (complete) {
      "Complete crossing-free density-value elder-rule merge tree"
    } else {
      "Filtered crossing-free density-value elder-rule merge tree"
    },
    main.barcode = if (complete) {
      "Complete extremum-to-saddle persistence barcode"
    } else {
      "Filtered extremum-to-saddle persistence barcode"
    },
    field.label = "Selected scalar-field value",
    annotation.cex = if (longest > 24L) 0.48 else 0.58,
    basin.ids = if (complete) NULL else ids,
    close.ancestors = FALSE
  )
  elapsed.ms <- .gflowui_basin_panel_elapsed_ms(started)
  expected <- if (complete) {
    sort(ids, method = "radix")
  } else {
    sort(model$layout$basin.ids %||%
      model$layout$branches$basin.id, method = "radix")
  }
  actual <- sort(plotted$layout$basin.ids, method = "radix")
  if (!identical(actual, expected)) {
    .gflowui_basin_panel_stop(
      "The plotted layout differs from the reviewed Phase 1 layout.",
      "gflowui_basin_panel_layout_error"
    )
  }
  invisible(list(
    elapsed.ms = elapsed.ms,
    branch.count = length(ids),
    complete = complete,
    layout = plotted$layout
  ))
}

gflowui_basin_draw_diagnostics <- function(model) {
  if (!inherits(model, "gflowui_basin_merge_tree_panel_model") ||
      !isTRUE(model$ready)) {
    .gflowui_basin_panel_stop(
      "A ready panel model is required for diagnostics."
    )
  }
  diagnostics <- model$diagnostics
  if (!isTRUE(diagnostics$available)) {
    graphics::plot.new()
    graphics::text(
      0.5,
      0.5,
      sprintf(
        "Mass diagnostics unavailable: %s",
        diagnostics$unavailable.reason
      )
    )
    return(invisible(list(
      available = FALSE,
      elapsed.ms = 0
    )))
  }
  started <- unname(proc.time()[["elapsed"]])
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add = TRUE)
  graphics::par(
    mfrow = c(1L, 3L),
    mar = c(4.2, 4.2, 2.6, 0.8),
    family = "sans"
  )
  graphics::hist(
    diagnostics$log10.mass,
    col = "#78A89B",
    border = "#315F58",
    main = "Positive log10 mass",
    xlab = "log10 trajectory-flow mass"
  )
  graphics::plot(
    diagnostics$ranked$rank,
    diagnostics$ranked$log10.mass,
    type = "l",
    lwd = 2,
    col = "#2F6690",
    main = "Ranked positive mass",
    xlab = "Complete-tie rank",
    ylab = "log10 mass"
  )
  graphics::grid(col = "#D9DEE2", lty = 3)
  graphics::plot(
    seq_along(diagnostics$cumulative),
    diagnostics$cumulative,
    type = "s",
    ylim = c(0, 1),
    lwd = 2,
    col = "#A24A3B",
    main = "Cumulative positive mass",
    xlab = "Complete-tie group",
    ylab = "Coverage"
  )
  graphics::abline(h = 0.99, col = "#7B6A58", lty = 3)
  invisible(list(
    available = TRUE,
    elapsed.ms = .gflowui_basin_panel_elapsed_ms(started)
  ))
}

gflowui_basin_panel_canonical_selection <- function(
    state,
    selected.keys) {
  .gflowui_basin_assert_runtime_state(state)
  if (!is.character(selected.keys) || anyNA(selected.keys)) {
    return(character())
  }
  data <- gflowui_basin_bundle_snapshot(state$bundle)
  component <- data$canonical[
    data$canonical$component == state$context$component,
    ,
    drop = FALSE
  ]
  keys <- paste(
    "max",
    component$trajectory.basin.id,
    sep = "|"
  )
  sort(
    component$basin.id[keys %in% selected.keys],
    method = "radix"
  )
}

gflowui_basin_panel_plot_width <- function(
    branch.count,
    label.mode = "important",
    complete = FALSE) {
  count <- .gflowui_basin_validate_integer(
    branch.count,
    "branch.count",
    minimum = 1L
  )
  if (!count$valid) {
    .gflowui_basin_panel_stop(count$message)
  }
  dense.labels <- label.mode %in% c("displayed", "all")
  pixels.per.branch <- if (dense.labels) 48L else {
    if (isTRUE(complete)) 7L else 26L
  }
  as.integer(max(
    if (isTRUE(complete)) 1320L else 920L,
    min(5200L, count$value * pixels.per.branch)
  ))
}

.gflowui_basin_panel_overflow_text <- function(
    outcome,
    counts,
    budget) {
  if (identical(outcome, "renderable")) {
    return(NULL)
  }
  core <- as.integer(counts$core %||% 0L)
  sentinel.only <- as.integer(counts$sentinel.only %||% 0L)
  final <- as.integer(counts$final %||% 0L)
  budget <- as.integer(budget)
  switch(
    outcome,
    core_overflow = sprintf(
      paste(
        "The complete core contains %d branches and exceeds the final",
        "render budget of %d. No mandatory core branch was trimmed."
      ),
      core,
      budget
    ),
    sentinel_overflow = sprintf(
      paste(
        "The core plus mandatory sentinels contains %d branches and",
        "exceeds the final render budget of %d.",
        "No mandatory sentinel or pin was trimmed."
      ),
      core + sentinel.only,
      budget
    ),
    closure_overflow = sprintf(
      paste(
        "Canonical ancestor closure expands the mandatory display to %d",
        "branches and exceeds the final render budget of %d.",
        "No canonical ancestor was trimmed."
      ),
      final,
      budget
    ),
    .gflowui_basin_panel_stop(
      "The proposal has an unsupported render outcome."
    )
  )
}

gflowui_basin_merge_tree_model <- function(
    state,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  panel <- gflowui_basin_merge_tree_panel_model(
    state,
    layout.accessor = layout.accessor
  )
  if (!isTRUE(panel$ready)) {
    return(list(
      available = FALSE,
      renderable = FALSE,
      attempt.outcome = panel$outcome,
      attempt.reason = panel$reason,
      attempt.messages = panel$messages,
      display.source = panel$display.source,
      controls = panel$controls,
      presentation = panel$presentation,
      component = panel$component$id %||% NULL,
      component.ids = panel$component$ids %||% integer(),
      component.maximum.count =
        panel$component$maximum.count %||% 0L,
      layout = NULL,
      panel = panel
    ))
  }
  proposal <- panel$proposal
  list(
    available = TRUE,
    renderable = is.null(panel$overflow),
    attempt.outcome = panel$outcome,
    attempt.reason = panel$reason,
    attempt.messages = panel$messages,
    display.source = panel$display.source,
    retained = panel$retained,
    component = panel$component$id,
    component.ids = panel$component$ids,
    component.maximum.count = panel$component$maximum.count,
    direction.maximum.count = panel$total.maximum.count,
    controls = panel$controls,
    presentation = panel$presentation,
    proposal = proposal,
    counts = panel$counts,
    mass = panel$mass,
    status = panel$status,
    diagnostics = panel$diagnostics,
    labels = panel$labels,
    selected.hidden = panel$selected$hidden,
    selected.visible = panel$selected$visible,
    pinned.ids = proposal$pinned.ids,
    membership.class = panel$membership.class,
    layout = panel$layout,
    layout.elapsed.ms = panel$layout.elapsed.ms,
    overflow.text = .gflowui_basin_panel_overflow_text(
      proposal$render.outcome,
      panel$counts,
      proposal$accepted.parameters$final.render.budget
    ),
    bundle = panel$bundle,
    panel = panel
  )
}

gflowui_basin_tree_plot_inputs <- function(model) {
  if (!is.list(model) ||
      !isTRUE(model$available) ||
      !isTRUE(model$renderable)) {
    .gflowui_basin_panel_stop(
      "A renderable merge-tree model is required."
    )
  }
  panel <- model$panel
  data <- .gflowui_basin_assert_pair(
    panel$proposal,
    panel$bundle
  )
  tree <- .gflowui_basin_copy(data$canonical.tree)
  index <- match(
    tree$basin.table$basin.id,
    data$canonical$basin.id
  )
  tree$basin.table$trajectory.flow.mass <-
    data$canonical$trajectory.flow.mass[index]
  tree$basin.table$trajectory.flow.support <-
    data$canonical$trajectory.flow.support[index]
  ids <- panel$proposal$final.ids
  list(
    tree = tree,
    layout = panel$layout,
    labels = .gflowui_basin_panel_labels(panel, ids),
    colors = .gflowui_basin_panel_branch_colors(panel, ids)
  )
}

gflowui_basin_plot_merge_tree <- function(model) {
  if (!is.list(model) || !isTRUE(model$available)) {
    .gflowui_basin_panel_stop(
      "An available merge-tree model is required."
    )
  }
  gflowui_basin_draw_merge_tree(model$panel)
}

gflowui_basin_tree_nearest_id <- function(
    model,
    click.x,
    click.y,
    threshold = 0.045) {
  if (!is.list(model) ||
      !isTRUE(model$available) ||
      !isTRUE(model$renderable) ||
      !is.numeric(click.x) ||
      !is.numeric(click.y) ||
      length(click.x) != 1L ||
      length(click.y) != 1L ||
      !is.finite(click.x) ||
      !is.finite(click.y) ||
      !is.numeric(threshold) ||
      length(threshold) != 1L ||
      !is.finite(threshold) ||
      threshold <= 0) {
    return(character())
  }
  branches <- model$layout$coordinates$branches
  if (!is.data.frame(branches) ||
      !all(c(
        "basin.id", "x", "birth.level", "death.level"
      ) %in% names(branches)) ||
      !nrow(branches)) {
    return(character())
  }
  x <- suppressWarnings(as.numeric(branches$x))
  birth <- suppressWarnings(as.numeric(branches$birth.level))
  death <- suppressWarnings(as.numeric(branches$death.level))
  finite <- is.finite(x) & is.finite(birth) & is.finite(death)
  if (!any(finite)) {
    return(character())
  }
  branches <- branches[finite, , drop = FALSE]
  x <- x[finite]
  birth <- birth[finite]
  death <- death[finite]
  x.range <- range(x, finite = TRUE)
  y.range <- range(c(birth, death), finite = TRUE)
  x.span <- diff(x.range)
  y.span <- diff(y.range)
  if (!is.finite(x.span) || x.span <= 0) x.span <- 1
  if (!is.finite(y.span) || y.span <= 0) y.span <- 1
  lower <- pmin(birth, death)
  upper <- pmax(birth, death)
  closest.y <- pmax(lower, pmin(upper, as.numeric(click.y)))
  distance <- sqrt(
    ((x - as.numeric(click.x)) / x.span)^2 +
      ((closest.y - as.numeric(click.y)) / y.span)^2
  )
  finite.distance <- which(is.finite(distance))
  if (!length(finite.distance)) {
    return(character())
  }
  minimum <- min(distance[finite.distance])
  candidates <- finite.distance[distance[finite.distance] == minimum]
  nearest <- candidates[order(
    as.character(branches$basin.id[candidates]),
    method = "radix"
  )][[1L]]
  if (!length(nearest) ||
      !is.finite(distance[[nearest]]) ||
      distance[[nearest]] > threshold) {
    return(character())
  }
  as.character(branches$basin.id[[nearest]])
}

gflowui_basin_plot_diagnostics <- function(model) {
  if (!is.list(model) || !isTRUE(model$available)) {
    .gflowui_basin_panel_stop(
      "An available merge-tree model is required."
    )
  }
  gflowui_basin_draw_diagnostics(model$panel)
  invisible(model$diagnostics)
}

.gflowui_basin_panel_segment_rows <- function(
    x0,
    y0,
    x1,
    y1,
    id) {
  count <- length(x0)
  if (!count) {
    return(data.frame(
      x = numeric(),
      y = numeric(),
      id = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    x = as.vector(rbind(x0, x1, rep(NA_real_, count))),
    y = as.vector(rbind(y0, y1, rep(NA_real_, count))),
    id = rep(as.character(id), each = 3L),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_complete_interactive_data <- function(
    state,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  .gflowui_basin_assert_runtime_state(state)
  proposal <- gflowui_basin_displayed_proposal(state)
  data <- if (is.null(proposal)) {
    gflowui_basin_bundle_snapshot(state$bundle)
  } else {
    .gflowui_basin_assert_pair(proposal, state$bundle)
  }
  component <- if (is.null(proposal)) {
    state$context$component
  } else {
    proposal$component$id
  }
  layout <- layout.accessor(
    data$canonical.tree,
    direction = "max",
    component = component
  )
  coordinates <- layout$coordinates
  index <- match(
    layout$branches$basin.id,
    data$canonical$basin.id
  )
  points <- coordinates$branches
  points$peak.value <- data$canonical$peak.value[index]
  points$prominence <- data$canonical$persistence[index]
  points$trajectory.flow.mass <-
    data$canonical$trajectory.flow.mass[index]
  points$trajectory.flow.support <-
    data$canonical$trajectory.flow.support[index]
  points$selected <- points$basin.id %in% state$selected.ids
  points$pinned <- points$basin.id %in% state$pinned.ids
  vertical <- .gflowui_basin_panel_segment_rows(
    points$x,
    points$birth.level,
    points$x,
    points$death.level,
    points$basin.id
  )
  events <- coordinates$events
  horizontal <- .gflowui_basin_panel_segment_rows(
    events$losing.x,
    events$merge.level,
    events$surviving.x,
    events$merge.level,
    events$event.id
  )
  list(
    layout = layout,
    points = points,
    vertical = vertical,
    horizontal = horizontal
  )
}

.gflowui_basin_panel_percent <- function(value) {
  if (is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)) {
    return("Unavailable")
  }
  sprintf(
    "%.6f%% (%s)",
    100 * value,
    formatC(value, digits = 15L, format = "fg")
  )
}

.gflowui_basin_panel_summary_item <- function(label, value) {
  shiny::div(
    class = "gf-basin-tree-summary-item",
    shiny::span(class = "gf-basin-tree-summary-label", label),
    shiny::span(class = "gf-basin-tree-summary-value", value)
  )
}

.gflowui_basin_panel_rule_disclosure <- function() {
  shiny::p(
    class = "gf-basin-tree-disclosure gf-basin-tree-rule-disclosure",
    paste(
      "Canonical continuation follows the density-value elder rule:",
      "the branch with the greater birth density survives each merge.",
      "Trajectory-flow mass and support are annotations and filtering",
      "quantities; they do not change tree parentage."
    )
  )
}

.gflowui_basin_complete_viewer_title <- function() {
  "Complete Interactive Density-Value Elder-Rule Basin Merge Tree"
}

.gflowui_basin_panel_selection_ui <- function(model) {
  ids <- model$selected$ids
  if (!length(ids)) {
    return(shiny::div(
      class = "gf-basin-tree-selection gf-basin-tree-selection-empty",
      `data-selection-state` = "none",
      shiny::span("No maximum basin selected.")
    ))
  }
  selected <- ids[[1L]]
  choices <- stats::setNames(ids, ids)
  state <- if (selected %in% model$selected$hidden) {
    "hidden"
  } else if (selected %in% model$selected$pinned) {
    "pinned"
  } else {
    "visible"
  }
  disclosure <- switch(
    state,
    hidden = sprintf(
      "%s is selected and hidden from the filtered tree.",
      selected
    ),
    pinned = sprintf(
      "%s is selected, visible, and pinned.",
      selected
    ),
    sprintf("%s is selected and visible.", selected)
  )
  shiny::div(
    class = "gf-basin-tree-selection",
    `data-selection-state` = state,
    shiny::selectInput(
      "basin_tree_selected_id",
      "Selected maximum basin",
      choices = choices,
      selected = selected,
      width = "100%"
    ),
    shiny::p(class = "gf-basin-tree-selection-status", disclosure),
    shiny::actionButton(
      if (state == "pinned") {
        "basin_tree_unpin_selected"
      } else {
        "basin_tree_pin_selected"
      },
      if (state == "pinned") "Unpin selected" else "Pin selected",
      class = "btn btn-sm btn-outline-secondary",
      title = if (state == "pinned") {
        "Remove proposal protection from the selected basin"
      } else {
        "Protect the selected basin and recompute the proposal"
      }
    )
  )
}

.gflowui_basin_panel_controls_ui <- function(model) {
  controls <- model$controls
  mode <- as.character(controls$filter.mode)
  component.labels <- sprintf(
    "Component %s (%s maximum basin%s)",
    names(model$component$counts),
    as.integer(model$component$counts),
    ifelse(model$component$counts == 1L, "", "s")
  )
  component.choices <- stats::setNames(
    names(model$component$counts),
    component.labels
  )
  mode.controls <- switch(
    mode,
    auto = list(
      shiny::numericInput(
        "basin_tree_coverage",
        "Mass coverage",
        value = controls$coverage.target,
        min = 0.000001,
        max = 1,
        step = 0.001
      ),
      shiny::numericInput(
        "basin_tree_strong_gap",
        "Strong-gap threshold (decades)",
        value = controls$strong.gap.decades,
        min = 0,
        step = 0.25
      ),
      shiny::numericInput(
        "basin_tree_core_budget",
        "Core branch budget",
        value = controls$core.branch.budget,
        min = 3,
        step = 1
      )
    ),
    cumulative_mass = list(
      shiny::numericInput(
        "basin_tree_coverage",
        "Mass coverage",
        value = controls$coverage.target,
        min = 0.000001,
        max = 1,
        step = 0.001
      ),
      shiny::numericInput(
        "basin_tree_core_budget",
        "Core branch budget",
        value = controls$core.branch.budget,
        min = 3,
        step = 1
      )
    ),
    minimum_mass = list(
      shiny::numericInput(
        "basin_tree_minimum_mass",
        "Minimum raw trajectory-flow mass",
        value = controls$minimum.mass,
        min = 0,
        step = 0.0001
      )
    ),
    top_k = list(
      shiny::numericInput(
        "basin_tree_top_k",
        "Top K",
        value = controls$top.k,
        min = 1,
        max = model$component$maximum.count,
        step = 1
      )
    ),
    none = list(),
    list()
  )
  shiny::div(
    class = "gf-basin-tree-controls",
    shiny::div(
      class = "gf-basin-tree-control-grid",
      shiny::selectInput(
        "basin_tree_component",
        "Component",
        choices = component.choices,
        selected = as.character(model$component$id)
      ),
      shiny::selectInput(
        "basin_tree_filter_mode",
        "Filter",
        choices = c(
          "Auto" = "auto",
          "Cumulative Mass" = "cumulative_mass",
          "Minimum Mass" = "minimum_mass",
          "Top K" = "top_k",
          "None" = "none"
        ),
        selected = mode
      ),
      mode.controls,
      shiny::numericInput(
        "basin_tree_final_budget",
        "Final render budget",
        value = controls$final.render.budget,
        min = 1,
        step = 1
      ),
      shiny::numericInput(
        "basin_tree_sentinel_n",
        "Sentinel count",
        value = controls$sentinel.top.n,
        min = 0,
        step = 1
      )
    ),
    shiny::tags$fieldset(
      class = "gf-basin-tree-toggle-group",
      shiny::tags$legend("Sentinels"),
      shiny::checkboxInput(
        "basin_tree_peak_sentinel",
        "Peak",
        value = isTRUE(controls$peak.sentinel.enabled)
      ),
      shiny::checkboxInput(
        "basin_tree_prominence_sentinel",
        "Prominence",
        value = isTRUE(controls$prominence.sentinel.enabled)
      ),
      shiny::checkboxInput(
        "basin_tree_support_sentinel",
        "Support",
        value = isTRUE(controls$support.sentinel.enabled)
      )
    ),
    shiny::div(
      class = "gf-basin-tree-control-grid gf-basin-tree-presentation-grid",
      shiny::numericInput(
        "basin_tree_important_labels",
        "Important-label count",
        value = model$presentation$important.label.n,
        min = 0,
        step = 1
      ),
      shiny::selectInput(
        "basin_tree_label_mode",
        "Labels",
        choices = c(
          "Important" = "important",
          "Selected" = "selected",
          "Displayed" = "displayed",
          "None" = "none",
          "All" = "all"
        ),
        selected = model$presentation$label.mode
      ),
      shiny::checkboxInput(
        "basin_tree_show_diagnostic",
        "Show diagnostic",
        value = isTRUE(model$diagnostics.visible)
      )
    ),
    shiny::div(
      class = "gf-basin-tree-actions gf-basin-recipe-actions",
      shiny::actionButton(
        "basin_tree_recipe_save",
        "Save settings recipe",
        class = "btn btn-sm btn-outline-secondary"
      ),
      shiny::actionButton(
        "basin_tree_recipe_apply",
        "Restore saved recipe",
        class = "btn btn-sm btn-outline-secondary"
      )
    ),
    shiny::p(
      class = "gf-basin-recipe-status",
      shiny::textOutput("basin_tree_recipe_status", inline = TRUE)
    )
  )
}

gflowui_basin_merge_tree_panel_ui <- function(model) {
  if (!inherits(model, "gflowui_basin_merge_tree_panel_model")) {
    .gflowui_basin_panel_stop("A typed merge-tree panel model is required.")
  }
  heading <- shiny::h4(
    id = "gf_basin_merge_tree_heading",
    "Basin Superlevel-Set Merge Tree"
  )
  if (!isTRUE(model$ready)) {
    detail <- paste(model$messages, collapse = " ")
    has.state <- is.list(model$state) &&
      is.list(model$controls) &&
      is.list(model$component)
    return(shiny::tags$section(
      id = "gf_basin_merge_tree",
      class = "gf-basin-merge-tree",
      role = "region",
      `aria-labelledby` = "gf_basin_merge_tree_heading",
      `data-analysis-state` = model$outcome,
      shiny::div(
        class = "gf-basin-merge-tree-header",
        heading,
        if (has.state) shiny::div(
          class = "gf-basin-tree-actions",
          shiny::actionButton(
            "basin_tree_show_all",
            "Show all",
            class = "btn btn-sm btn-outline-secondary"
          ),
          shiny::actionButton(
            "basin_tree_open_complete",
            "Open complete interactive tree",
            class = "btn btn-sm btn-outline-secondary"
          )
        ) else NULL
      ),
      shiny::p(
        class = "gf-basin-analysis-shell-status",
        role = "status",
        `aria-live` = "polite",
        sprintf(
          "The current maximum-basin proposal is %s%s.",
          model$outcome,
          if (nzchar(detail)) paste0(": ", detail) else ""
        )
      ),
      .gflowui_basin_panel_rule_disclosure(),
      if (has.state) {
        .gflowui_basin_panel_controls_ui(model)
      } else {
        NULL
      },
      if (has.state) {
        .gflowui_basin_panel_selection_ui(model)
      } else {
        NULL
      }
    ))
  }
  proposal <- model$proposal
  primary.counts <- model$counts$primary.reason.counts
  primary.text <- paste(
    sprintf(
      "%s %d",
      names(primary.counts),
      as.integer(primary.counts)
    ),
    collapse = "; "
  )
  warnings <- unique(c(
    model$status$warnings,
    model$labels$warning
  ))
  warnings <- warnings[
    !is.na(warnings) & nzchar(as.character(warnings))
  ]
  plot.ui <- if (is.null(model$overflow)) {
    plot.width <- gflowui_basin_panel_plot_width(
      model$counts$final,
      model$labels$mode
    )
    shiny::div(
      class = "gf-basin-tree-plot-scroll",
      `data-plot-branch-count` = model$counts$final,
      shiny::plotOutput(
        "basin_merge_tree_plot",
        width = sprintf("%dpx", plot.width),
        height = "760px",
        click = shiny::clickOpts(
          id = "basin_merge_tree_click",
          clip = TRUE
        )
      )
    )
  } else {
    shiny::div(
      class = paste(
        "gf-basin-tree-overflow",
        paste0("gf-basin-tree-", model$overflow$outcome)
      ),
      role = "alert",
      `data-overflow-outcome` = model$overflow$outcome,
      shiny::h5(gsub("_", " ", model$overflow$outcome, fixed = TRUE)),
      shiny::p(model$overflow$message),
      shiny::p(sprintf(
        "Core %d; pre-closure %d; closure %d; budget %d.",
        model$overflow$core.count,
        model$overflow$preclosure.count,
        model$overflow$final.count,
        model$overflow$budget
      ))
    )
  }
  diagnostics.ui <- if (isTRUE(model$diagnostics.visible)) {
    shiny::div(
      class = "gf-basin-tree-diagnostics",
      shiny::plotOutput(
        "basin_merge_tree_diagnostic_plot",
        width = "100%",
        height = "300px"
      ),
      shiny::p(
        class = "gf-basin-tree-diagnostic-note",
        sprintf(
          "%d exact-zero masses are excluded from logarithms.",
          as.integer(model$diagnostics$zero.count %||% 0L)
        )
      )
    )
  } else {
    NULL
  }
  shiny::tags$section(
    id = "gf_basin_merge_tree",
    class = "gf-basin-merge-tree",
    role = "region",
    `aria-labelledby` = "gf_basin_merge_tree_heading",
    `data-analysis-state` = model$outcome,
    `data-display-source` = model$display.source,
    `data-context-generation` = model$context.generation,
    `data-attempt-id` = model$active.attempt$attempt.id,
    `data-render-outcome` = proposal$render.outcome,
    `data-core-outcome` = proposal$core$outcome,
    `data-label-mode` = model$labels$mode,
    shiny::div(
      class = "gf-basin-merge-tree-header",
      heading,
      shiny::div(
        class = "gf-basin-tree-actions",
        shiny::actionButton(
          "basin_tree_show_all",
          "Show all",
          class = "btn btn-sm btn-outline-secondary",
          title = "Set Filter to None and recompute"
        ),
        shiny::actionButton(
          "basin_tree_open_complete",
          "Open complete interactive tree",
          class = "btn btn-sm btn-outline-secondary",
          title = "Open the complete component without changing the proposal"
        )
      )
    ),
    if (isTRUE(model$retained)) {
      shiny::div(
        class = "gf-basin-tree-retained",
        role = "status",
        "Showing the retained last valid proposal while current controls are unresolved."
      )
    } else {
      NULL
    },
    .gflowui_basin_panel_rule_disclosure(),
    shiny::div(
      class = "gf-basin-tree-summary",
      .gflowui_basin_panel_summary_item(
        "All maximum basins",
        model$total.maximum.count
      ),
      .gflowui_basin_panel_summary_item(
        "Components",
        model$component$count
      ),
      .gflowui_basin_panel_summary_item(
        "Selected component",
        sprintf(
          "%d (%d maxima)",
          model$component$id,
          model$component$maximum.count
        )
      ),
      .gflowui_basin_panel_summary_item(
        "Mass core",
        sprintf(
          "%d (%s)",
          model$counts$core,
          proposal$core$outcome
        )
      ),
      .gflowui_basin_panel_summary_item(
        "Final display",
        sprintf(
          "%d (%s)",
          model$counts$final,
          proposal$render.outcome
        )
      ),
      .gflowui_basin_panel_summary_item(
        "Positive-mass coverage",
        .gflowui_basin_panel_percent(model$coverage)
      ),
      .gflowui_basin_panel_summary_item(
        "Sentinel-only",
        sprintf("%d; %s", model$counts$sentinel.only, primary.text)
      ),
      .gflowui_basin_panel_summary_item(
        "Ancestor-only",
        model$counts$ancestor.only
      ),
      .gflowui_basin_panel_summary_item(
        "Mass ownership",
        model$status$mass.owner
      ),
      .gflowui_basin_panel_summary_item(
        "Display source",
        model$display.source
      )
    ),
    if (length(warnings)) {
      shiny::div(
        class = "gf-basin-tree-warning",
        role = "status",
        paste(warnings, collapse = " ")
      )
    } else {
      NULL
    },
    if (length(model$labels$omissions)) {
      shiny::div(
        class = "gf-basin-tree-label-omission",
        paste(model$labels$omissions, collapse = " ")
      )
    } else {
      NULL
    },
    .gflowui_basin_panel_controls_ui(model),
    .gflowui_basin_panel_selection_ui(model),
    plot.ui,
    diagnostics.ui
  )
}
