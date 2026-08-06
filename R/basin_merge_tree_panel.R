.gflowui_basin_panel_stop <- function(
    message,
    class = "gflowui_basin_panel_error") {
  .gflowui_basin_stop(message, class)
}

.gflowui_basin_panel_elapsed_ms <- function(started) {
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  max(0, as.numeric(elapsed) * 1000)
}

gflowui_basin_continuation_rule_choices <- function() {
  c(
    "Field-value elder rule (canonical)" = "field_value",
    "Trajectory-flow basin mass priority" = "mass",
    "Trajectory-flow basin support priority" = "support"
  )
}

gflowui_basin_continuation_rule <- function(rule = "field_value") {
  rule <- as.character(rule %||% "field_value")
  if (length(rule) != 1L ||
      is.na(rule) ||
      !rule %in% unname(gflowui_basin_continuation_rule_choices())) {
    "field_value"
  } else {
    rule
  }
}

gflowui_basin_continuation_description <- function(rule = "field_value") {
  switch(
    gflowui_basin_continuation_rule(rule),
    mass = paste(
      "At every merge, the branch with greater fixed trajectory-flow basin",
      "mass continues. Exact mass ties use the canonical field-value elder",
      "rule, then the extremum-vertex index."
    ),
    support = paste(
      "At every merge, the branch with greater fixed trajectory-flow basin",
      "support continues. Exact support ties use the canonical field-value",
      "elder rule, then the extremum-vertex index."
    ),
    paste(
      "At every merge, the branch born at the more extreme field value",
      "continues. Exact birth-value ties use the extremum-vertex index.",
      "This is the canonical merge-tree elder rule."
    )
  )
}

.gflowui_basin_continuation_spec <- function(
    rule,
    basin.ids,
    mass,
    support) {
  rule <- gflowui_basin_continuation_rule(rule)
  basin.ids <- as.character(basin.ids)
  value <- switch(
    rule,
    mass = suppressWarnings(as.numeric(mass)),
    support = suppressWarnings(as.numeric(support)),
    NULL
  )
  if (!is.null(value) &&
      (length(value) != length(basin.ids) ||
        anyNA(basin.ids) ||
        any(!nzchar(basin.ids)) ||
        anyDuplicated(basin.ids) ||
        any(!is.finite(value)) ||
        any(value < 0))) {
    .gflowui_basin_panel_stop(
      "The selected continuation measure is incomplete or invalid."
    )
  }
  measure <- switch(
    rule,
    mass = "Trajectory-flow basin mass",
    support = "Trajectory-flow basin support",
    "Field value"
  )
  list(
    rule = rule,
    measure = measure,
    priority = if (is.null(value)) {
      NULL
    } else {
      stats::setNames(value, basin.ids)
    },
    label = unname(
      names(gflowui_basin_continuation_rule_choices())[
        match(rule, gflowui_basin_continuation_rule_choices())
      ]
    )
  )
}

gflowui_basin_continuation_policy <- function(
    bundle,
    rule = "field_value") {
  data <- gflowui_basin_bundle_snapshot(bundle)
  .gflowui_basin_continuation_spec(
    rule,
    data$canonical$basin.id,
    data$canonical$trajectory.flow.mass,
    data$canonical$trajectory.flow.support
  )
}

gflowui_basin_continuation_tree_title <- function(
    policy,
    complete = FALSE) {
  prefix <- if (isTRUE(complete)) "Complete" else "Filtered"
  rule <- gflowui_basin_continuation_rule(policy$rule %||% "field_value")
  suffix <- switch(
    rule,
    mass = "trajectory-flow mass-priority continuation tree",
    support = "trajectory-flow support-priority continuation tree",
    "field-value elder-rule merge tree"
  )
  sprintf("%s crossing-free %s", prefix, suffix)
}

gflowui_basin_continuation_barcode_title <- function(
    policy,
    complete = FALSE) {
  prefix <- if (isTRUE(complete)) "Complete" else "Filtered"
  rule <- gflowui_basin_continuation_rule(policy$rule %||% "field_value")
  suffix <- switch(
    rule,
    mass = "trajectory-flow mass continuation-lifetime barcode",
    support = "trajectory-flow support continuation-lifetime barcode",
    "extremum-to-saddle persistence barcode"
  )
  sprintf("%s %s", prefix, suffix)
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
    layout.accessor = gflow::get.basin.merge.tree.layout,
    display.labels = NULL,
    continuation.rule = "field_value") {
  if (is.null(state)) {
    return(.gflowui_basin_panel_empty_model())
  }
  .gflowui_basin_assert_runtime_state(state)
  proposal <- gflowui_basin_displayed_proposal(state)
  if (is.null(proposal)) {
    return(.gflowui_basin_panel_empty_model(state))
  }
  data <- .gflowui_basin_assert_pair(proposal, state$bundle)
  continuation <- gflowui_basin_continuation_policy(
    state$bundle,
    continuation.rule
  )
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
      layout.accessor = layout.accessor,
      continuation.priority = continuation$priority,
      continuation.measure = if (is.null(continuation$priority)) {
        NULL
      } else {
        continuation$measure
      }
    )
    layout.elapsed.ms <- .gflowui_basin_panel_elapsed_ms(started)
    requested.ids <- sort(
      as.character(layout$requested.basin.ids %||%
        layout$basin.ids %||% layout$branches$basin.id),
      method = "radix"
    )
    if (!identical(
      requested.ids,
      sort(proposal$final.ids, method = "radix")
    )) {
      .gflowui_basin_panel_stop(
        "The rendered Phase 1 layout differs from the accepted proposal.",
        "gflowui_basin_panel_layout_error"
      )
    }
  }
  overflow <- .gflowui_basin_panel_overflow(proposal, counts)
  if (is.null(overflow) &&
      !is.null(layout) &&
      nrow(layout$branches) >
        proposal$accepted.parameters$final.render.budget) {
    overflow <- list(
      outcome = "continuation_closure_overflow",
      message = sprintf(
        paste(
          "The selected continuation rule requires %d branches after",
          "adding its connector ancestors, exceeding the final render",
          "budget of %d. No required connector branch was trimmed."
        ),
        nrow(layout$branches),
        proposal$accepted.parameters$final.render.budget
      ),
      core.count = counts$core,
      preclosure.count = length(proposal$final.ids),
      final.count = nrow(layout$branches),
      budget = proposal$accepted.parameters$final.render.budget
    )
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
  if (!is.null(layout)) {
    continuation.ancestors <- setdiff(
      layout$basin.ids,
      proposal$final.ids
    )
    classes[continuation.ancestors] <- "ancestor_only"
    if (identical(state$presentation$label.mode, "important")) {
      labels$ids <- sort(unique(c(
        labels$ids,
        layout$component.root.basin.id
      )), method = "radix")
    }
  }
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
  if (!is.null(display.labels)) {
    if (!is.character(display.labels) ||
        is.null(names(display.labels)) ||
        anyNA(display.labels) ||
        anyNA(names(display.labels)) ||
        any(!nzchar(display.labels)) ||
        any(!nzchar(names(display.labels))) ||
        anyDuplicated(names(display.labels))) {
      .gflowui_basin_panel_stop(
        "Readable basin labels must be a uniquely named character vector."
      )
    }
    matched <- match(names(label.text), names(display.labels))
    valid <- !is.na(matched)
    label.text[valid] <- unname(display.labels[matched[valid]])
  }
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
      overflow = overflow,
      continuation = continuation,
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
    component = model$component$id,
    continuation.priority = model$continuation$priority,
    continuation.measure = if (is.null(model$continuation$priority)) {
      NULL
    } else {
      model$continuation$measure
    }
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
    model$layout$basin.ids
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
    main.tree = gflowui_basin_continuation_tree_title(
      model$continuation,
      complete
    ),
    main.barcode = gflowui_basin_continuation_barcode_title(
      model$continuation,
      complete
    ),
    field.label = "Selected scalar-field value",
    annotation.cex = if (longest > 24L) 0.48 else 0.58,
    basin.ids = if (complete) NULL else ids,
    close.ancestors = FALSE,
    continuation.priority = model$continuation$priority,
    continuation.measure = if (is.null(model$continuation$priority)) {
      NULL
    } else {
      model$continuation$measure
    }
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
    layout.accessor = gflow::get.basin.merge.tree.layout,
    display.labels = NULL,
    continuation.rule = "field_value") {
  panel <- gflowui_basin_merge_tree_panel_model(
    state,
    layout.accessor = layout.accessor,
    display.labels = display.labels,
    continuation.rule = continuation.rule
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
    continuation = panel$continuation,
    layout = panel$layout,
    layout.elapsed.ms = panel$layout.elapsed.ms,
    overflow.text = if (
      identical(
        as.character(panel$overflow$outcome %||% ""),
        "continuation_closure_overflow"
      )
    ) {
      panel$overflow$message
    } else {
      .gflowui_basin_panel_overflow_text(
        proposal$render.outcome,
        panel$counts,
        proposal$accepted.parameters$final.render.budget
      )
    },
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
    layout.accessor = gflow::get.basin.merge.tree.layout,
    label.text = NULL) {
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
  points$display.label <- points$basin.id
  if (!is.null(label.text)) {
    if (!is.character(label.text) || is.null(names(label.text))) {
      .gflowui_basin_panel_stop(
        "Complete-tree labels must be a named character vector."
      )
    }
    matched.labels <- unname(label.text[points$basin.id])
    valid.labels <- !is.na(matched.labels) & nzchar(matched.labels)
    points$display.label[valid.labels] <- matched.labels[valid.labels]
  }
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

.gflowui_basin_tree_component_vector <- function(tree) {
  components <- suppressWarnings(as.integer(
    tree$graph.input$validation$component %||% integer()
  ))
  if (length(components) != tree$n.vertices ||
      anyNA(components) ||
      any(components < 1L)) {
    .gflowui_basin_panel_stop(
      "The canonical merge tree does not provide valid graph components."
    )
  }
  components
}

gflowui_basin_interactive_levels <- function(state, scope = "proposal",
                                             continuation.rule = "field_value") {
  structure <- gflowui_basin_interactive_structure(
    state,
    scope = scope,
    continuation.rule = continuation.rule
  )
  gflowui_basin_interactive_events(structure)$height
}

.gflowui_basin_interactive_label_ids <- function(
    state,
    data,
    proposal,
    scope.ids) {
  mode <- as.character(state$presentation$label.mode %||% "important")
  scope.ids <- sort(unique(as.character(scope.ids)), method = "radix")
  selected <- intersect(state$selected.ids, scope.ids)
  if (identical(mode, "none")) {
    return(character())
  }
  if (mode %in% c("displayed", "all")) {
    return(scope.ids)
  }
  if (identical(mode, "selected")) {
    return(sort(selected, method = "radix"))
  }
  component.data <- data$canonical[
    match(scope.ids, data$canonical$basin.id),
    ,
    drop = FALSE
  ]
  count <- suppressWarnings(as.integer(
    state$presentation$important.label.n %||% 0L
  ))
  if (!is.finite(count) || count < 0L) {
    count <- 0L
  }
  contributions <- list(
    mass = if (identical(
      as.character(data$validation$trajectory_flow_mass %||% ""),
      "valid"
    )) {
      .gflowui_basin_top_with_ties(
        component.data$basin.id,
        component.data$trajectory.flow.mass,
        count
      )
    } else {
      character()
    },
    peak = .gflowui_basin_top_with_ties(
      component.data$basin.id,
      component.data$peak.value,
      count
    ),
    prominence = .gflowui_basin_top_with_ties(
      component.data$basin.id,
      component.data$persistence,
      count
    ),
    support = .gflowui_basin_top_with_ties(
      component.data$basin.id,
      component.data$trajectory.flow.support,
      count
    ),
    survivor = component.data$basin.id[
      is.na(component.data$parent.basin.id)
    ],
    pinned = intersect(state$pinned.ids, scope.ids),
    selected = selected
  )
  sort(
    unique(intersect(unlist(contributions), scope.ids)),
    method = "radix"
  )
}

.gflowui_basin_interactive_palette <- function(ids, colors = NULL) {
  ids <- sort(unique(as.character(ids)), method = "radix")
  palette <- stats::setNames(
    grDevices::hcl.colors(max(3L, length(ids)), "Dynamic")[
      seq_along(ids)
    ],
    ids
  )
  if (is.character(colors) && !is.null(names(colors))) {
    matched <- unname(colors[ids])
    valid <- !is.na(matched) & nzchar(matched)
    palette[valid] <- matched[valid]
  }
  palette
}

.gflowui_basin_interactive_merge_plateaus <- function(
    tree,
    canonical,
    component,
    scope.ids,
    height,
    merge.scope,
    label.text) {
  empty <- data.frame(
    plateau.id = character(),
    merge.level = numeric(),
    label = character(),
    stringsAsFactors = FALSE
  )
  empty$vertices <- I(list())
  if (identical(merge.scope, "hidden")) {
    return(empty)
  }
  events <- tree$merge.table
  component.ids <- canonical$basin.id[canonical$component == component]
  events <- events[
    events$direction == "max" &
      events$losing.basin.id %in% scope.ids &
      events$surviving.basin.id %in% scope.ids &
      events$losing.basin.id %in% component.ids,
    ,
    drop = FALSE
  ]
  if (identical(merge.scope, "current")) {
    events <- events[events$merge.level == height, , drop = FALSE]
  } else if (identical(merge.scope, "reached")) {
    events <- events[events$merge.level >= height, , drop = FALSE]
  } else {
    .gflowui_basin_panel_stop(
      "Merge-plateau scope must be current, reached, or hidden."
    )
  }
  if (!nrow(events)) {
    return(empty)
  }
  groups <- split(seq_len(nrow(events)), events$merge.plateau.id)
  rows <- lapply(names(groups), function(plateau.id) {
    index <- groups[[plateau.id]]
    losing <- unique(as.character(events$losing.basin.id[index]))
    surviving <- unique(as.character(events$surviving.basin.id[index]))
    losing.labels <- unname(label.text[losing])
    surviving.labels <- unname(label.text[surviving])
    losing.labels[
      is.na(losing.labels) | !nzchar(losing.labels)
    ] <- losing[
      is.na(losing.labels) | !nzchar(losing.labels)
    ]
    surviving.labels[
      is.na(surviving.labels) | !nzchar(surviving.labels)
    ] <- surviving[
      is.na(surviving.labels) | !nzchar(surviving.labels)
    ]
    list(
      plateau.id = plateau.id,
      merge.level = unique(events$merge.level[index])[[1L]],
      label = sprintf(
        "s(%s \u2192 %s)",
        paste(losing.labels, collapse = ", "),
        paste(surviving.labels, collapse = ", ")
      ),
      vertices = sort(unique(as.integer(
        unlist(events$merge.vertices[index])
      )))
    )
  })
  result <- data.frame(
    plateau.id = vapply(rows, `[[`, character(1), "plateau.id"),
    merge.level = vapply(rows, `[[`, numeric(1), "merge.level"),
    label = vapply(rows, `[[`, character(1), "label"),
    stringsAsFactors = FALSE
  )
  result$vertices <- I(lapply(rows, `[[`, "vertices"))
  result
}

gflowui_basin_interactive_structure <- function(
    state,
    scope = c("proposal", "complete"),
    continuation.rule = "field_value",
    label.text = NULL,
    basin.colors = NULL,
    layout.accessor = gflow::get.basin.merge.tree.layout) {
  .gflowui_basin_assert_runtime_state(state)
  scope <- match.arg(scope)
  proposal <- gflowui_basin_displayed_proposal(state)
  if (is.null(proposal)) {
    .gflowui_basin_panel_stop(
      "A displayed basin proposal is required for the interactive tree."
    )
  }
  data <- .gflowui_basin_assert_pair(proposal, state$bundle)
  continuation <- gflowui_basin_continuation_policy(
    state$bundle,
    continuation.rule
  )
  component <- proposal$component$id
  component.ids <- data$canonical$basin.id[
    data$canonical$component == component
  ]
  scope.ids <- if (identical(scope, "proposal")) {
    proposal$final.ids
  } else {
    component.ids
  }
  layout <- layout.accessor(
    data$canonical.tree,
    direction = "max",
    component = component,
    basin.ids = if (identical(scope, "proposal")) scope.ids else NULL,
    close.ancestors = identical(scope, "proposal"),
    continuation.priority = continuation$priority,
    continuation.measure = if (is.null(continuation$priority)) {
      NULL
    } else {
      continuation$measure
    }
  )
  render.ids <- as.character(layout$basin.ids)
  if (is.null(label.text)) {
    label.text <- stats::setNames(
      data$canonical$basin.id,
      data$canonical$basin.id
    )
  }
  if (!is.character(label.text) || is.null(names(label.text))) {
    .gflowui_basin_panel_stop(
      "Interactive-tree labels must be a named character vector."
    )
  }
  palette <- .gflowui_basin_interactive_palette(
    component.ids,
    basin.colors
  )
  coordinates <- layout$coordinates
  index <- match(layout$branches$basin.id, data$canonical$basin.id)
  points <- coordinates$branches
  points$peak.value <- data$canonical$peak.value[index]
  points$prominence <- data$canonical$persistence[index]
  points$continuation.lifetime <-
    layout$branches$continuation.lifetime
  points$trajectory.flow.mass <-
    data$canonical$trajectory.flow.mass[index]
  points$trajectory.flow.support <-
    data$canonical$trajectory.flow.support[index]
  points$display.label <- unname(label.text[points$basin.id])
  missing.labels <- is.na(points$display.label) |
    !nzchar(points$display.label)
  points$display.label[missing.labels] <-
    points$basin.id[missing.labels]
  points$selected <- points$basin.id %in% state$selected.ids
  points$pinned <- points$basin.id %in% state$pinned.ids
  points$label.visible <- points$basin.id %in%
    .gflowui_basin_interactive_label_ids(
      state,
      data,
      proposal,
      scope.ids
    )
  if (identical(state$presentation$label.mode, "important")) {
    points$label.visible <- points$label.visible |
      points$basin.id == layout$component.root.basin.id
  }
  points$color <- unname(palette[points$basin.id])
  points$color[points$pinned] <- "#7C3AED"
  points$color[points$selected] <- "#DC2626"
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
  graph.component <- .gflowui_basin_tree_component_vector(
    data$canonical.tree
  )
  component.values <- suppressWarnings(as.numeric(data$source.values))[
    graph.component == component
  ]
  if (!length(component.values) || anyNA(component.values) ||
      any(!is.finite(component.values))) {
    .gflowui_basin_panel_stop(
      "The selected graph component does not provide finite field values."
    )
  }
  context.token <- gflowui_basin_sha256(list(
    schema = "basin-tree-event-context-v1",
    bundle.id = as.character(state$bundle$bundle.id %||% ""),
    scientific.identity = data$identity,
    context.generation = state$context.generation,
    component = component,
    scope = scope,
    requested.scope.ids = sort(as.character(scope.ids), method = "radix"),
    rendered.scope.ids = sort(render.ids, method = "radix"),
    continuation.rule = continuation$rule,
    continuation.priority = continuation$priority,
    labels = label.text[sort(names(label.text), method = "radix")],
    colors = palette[sort(names(palette), method = "radix")],
    label.mode = state$presentation$label.mode,
    important.label.n = state$presentation$important.label.n,
    selected.ids = sort(state$selected.ids, method = "radix"),
    pinned.ids = sort(state$pinned.ids, method = "radix")
  ))
  list(
    scope = scope,
    requested.scope.ids = sort(scope.ids, method = "radix"),
    scope.ids = sort(render.ids, method = "radix"),
    component = component,
    context.token = context.token,
    continuation = continuation,
    data = data,
    proposal = proposal,
    layout = layout,
    points = points,
    vertical = vertical,
    horizontal = horizontal,
    palette = palette,
    label.text = label.text,
    component.values = component.values
  )
}

.gflowui_basin_event_ids_at_height <- function(values, ids, height) {
  unique(as.character(ids[values == height]))
}

gflowui_basin_interactive_events <- function(structure) {
  if (!is.list(structure) || is.null(structure$layout) ||
      is.null(structure$component.values)) {
    .gflowui_basin_panel_stop(
      "Interactive-tree events require a prepared tree structure."
    )
  }
  values <- structure$component.values
  maximum <- max(values)
  minimum <- min(values)
  span <- diff(range(values))
  offset <- sqrt(.Machine$double.eps) *
    max(1, abs(maximum), if (is.finite(span)) span else 0)
  above <- maximum + offset
  if (!is.finite(above) || above <= maximum) {
    .gflowui_basin_panel_stop(
      "A finite threshold strictly above the component maximum could not be represented."
    )
  }
  branches <- structure$layout$coordinates$branches
  merge.events <- structure$layout$events
  birth.levels <- suppressWarnings(as.numeric(branches$birth.level))
  merge.levels <- suppressWarnings(as.numeric(merge.events$merge.level))
  heights <- sort(
    unique(c(above, birth.levels, merge.levels, minimum)),
    decreasing = TRUE
  )
  birth.ids <- lapply(heights, function(height) {
    .gflowui_basin_event_ids_at_height(
      birth.levels,
      branches$basin.id,
      height
    )
  })
  plateau.ids <- lapply(heights, function(height) {
    .gflowui_basin_event_ids_at_height(
      merge.levels,
      merge.events$merge.plateau.id,
      height
    )
  })
  above.flag <- seq_along(heights) == 1L
  floor.flag <- heights == minimum
  birth.count <- lengths(birth.ids)
  merge.count <- lengths(plateau.ids)
  kinds <- vapply(seq_along(heights), function(index) {
    if (above.flag[[index]]) {
      return("above_maximum")
    }
    has.birth <- birth.count[[index]] > 0L
    has.merge <- merge.count[[index]] > 0L
    has.floor <- floor.flag[[index]]
    if (has.birth && has.merge && has.floor) return("birth_merge_and_floor")
    if (has.birth && has.merge) return("birth_and_merge")
    if (has.birth && has.floor) return("birth_and_floor")
    if (has.merge && has.floor) return("merge_and_floor")
    if (has.birth) return("birth")
    if (has.merge) return("merge")
    if (has.floor) return("floor")
    "unknown"
  }, character(1))
  summaries <- vapply(seq_along(heights), function(index) {
    if (above.flag[[index]]) {
      return("above all maxima; superlevel set empty")
    }
    parts <- character()
    if (birth.count[[index]] > 0L) {
      parts <- c(parts, sprintf(
        "%d maximum birth%s",
        birth.count[[index]],
        if (birth.count[[index]] == 1L) "" else "s"
      ))
    }
    if (merge.count[[index]] > 0L) {
      parts <- c(parts, sprintf(
        "%d merge plateau%s",
        merge.count[[index]],
        if (merge.count[[index]] == 1L) "" else "s"
      ))
    }
    if (floor.flag[[index]]) {
      parts <- c(parts, "component floor; complete component active")
    }
    paste(parts, collapse = "; ")
  }, character(1))
  result <- data.frame(
    event.index = seq_along(heights) - 1L,
    event.number = seq_along(heights),
    event.count = rep.int(length(heights), length(heights)),
    height = heights,
    height.text = trimws(formatC(heights, format = "g", digits = 3)),
    birth.count = birth.count,
    merge.count = merge.count,
    event.kind = kinds,
    event.summary = summaries,
    above.maximum = above.flag,
    component.floor = floor.flag,
    stringsAsFactors = FALSE
  )
  result$birth.basin.ids <- I(birth.ids)
  result$merge.plateau.ids <- I(plateau.ids)
  result$aria.value.text <- sprintf(
    "Event %d of %d: %s; h equals %s",
    result$event.number,
    result$event.count,
    result$event.summary,
    result$height.text
  )
  result$kind <- result$event.kind
  result$summary <- result$event.summary
  result$aria.label <- result$aria.value.text
  result
}

gflowui_basin_validate_interactive_events <- function(events) {
  required <- c(
    "event.index", "event.number", "event.count", "height",
    "above.maximum", "component.floor", "birth.count",
    "birth.basin.ids", "merge.count", "merge.plateau.ids",
    "event.kind", "event.summary", "aria.value.text"
  )
  if (!is.data.frame(events) || !nrow(events) ||
      !all(required %in% names(events))) {
    .gflowui_basin_panel_stop(
      "A complete, non-empty topology-event table is required."
    )
  }
  count <- nrow(events)
  valid <- identical(events$event.index, seq_len(count) - 1L) &&
    identical(events$event.number, seq_len(count)) &&
    all(events$event.count == count) &&
    all(is.finite(events$height)) &&
    (count == 1L || all(diff(events$height) < 0)) &&
    identical(which(events$above.maximum), 1L) &&
    identical(which(events$component.floor), count) &&
    identical(lengths(events$birth.basin.ids), events$birth.count) &&
    identical(lengths(events$merge.plateau.ids), events$merge.count) &&
    all(!is.na(events$event.kind) & nzchar(events$event.kind)) &&
    all(!is.na(events$event.summary) & nzchar(events$event.summary)) &&
    all(!is.na(events$aria.value.text) & nzchar(events$aria.value.text))
  if (!isTRUE(valid)) {
    .gflowui_basin_panel_stop(
      "The topology-event table is malformed or not strictly decreasing."
    )
  }
  invisible(events)
}

gflowui_basin_remap_event_index <- function(events, previous.height = NULL) {
  gflowui_basin_validate_interactive_events(events)
  height <- suppressWarnings(as.numeric(previous.height))
  if (length(height) != 1L || !is.finite(height)) {
    return(0L)
  }
  exact <- which(events$height == height)
  if (length(exact)) return(as.integer(events$event.index[[exact[[1L]]]]))
  if (height >= events$height[[1L]]) return(0L)
  if (height <= events$height[[nrow(events)]]) {
    return(as.integer(events$event.index[[nrow(events)]]))
  }
  downward <- which(events$height <= height)
  as.integer(events$event.index[[downward[[1L]]]])
}

.gflowui_basin_empty_ascent_flow_edges <- function() {
  data.frame(
    from = integer(),
    to = integer(),
    root.vertex = integer(),
    basin.id = character(),
    color = character(),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_ascent_flow_edges <- function(
    trajectory.complex,
    active.vertices,
    canonical,
    basin.colors,
    color.mode = c("basin", "single"),
    common.color = "#4B5563",
    forest.accessor = gflow::get.basin.trajectory.forest) {
  color.mode <- match.arg(color.mode)
  forest <- tryCatch(
    forest.accessor(trajectory.complex, required = TRUE),
    error = identity
  )
  if (inherits(forest, "error") ||
      !is.list(forest) ||
      !is.list(forest$next.vertex) ||
      !is.list(forest$root.vertex)) {
    .gflowui_basin_panel_stop(
      "The canonical CLOSEST trajectory forest is unavailable."
    )
  }
  next.vertex <- suppressWarnings(as.integer(forest$next.vertex$max))
  root.vertex <- suppressWarnings(as.integer(forest$root.vertex$max))
  n.vertices <- length(next.vertex)
  if (!n.vertices ||
      length(root.vertex) != n.vertices ||
      any(!is.na(next.vertex) &
        (next.vertex < 1L | next.vertex > n.vertices)) ||
      anyNA(root.vertex) ||
      any(root.vertex < 1L | root.vertex > n.vertices)) {
    .gflowui_basin_panel_stop(
      "The canonical maximum-direction trajectory forest is malformed."
    )
  }
  active <- sort(unique(suppressWarnings(as.integer(active.vertices))))
  active <- active[
    is.finite(active) & active >= 1L & active <= n.vertices
  ]
  if (!length(active)) {
    return(.gflowui_basin_empty_ascent_flow_edges())
  }
  from <- active[!is.na(next.vertex[active])]
  if (!length(from)) {
    return(.gflowui_basin_empty_ascent_flow_edges())
  }
  to <- next.vertex[from]
  if (any(!to %in% active)) {
    .gflowui_basin_panel_stop(
      paste(
        "The active superlevel set is inconsistent with its canonical",
        "ascending CLOSEST trajectory forest."
      )
    )
  }
  roots <- root.vertex[from]
  required.canonical <- c("basin.id", "extremum.vertex")
  if (!is.data.frame(canonical) ||
      !all(required.canonical %in% names(canonical))) {
    .gflowui_basin_panel_stop(
      "Canonical maximum-basin identities are unavailable."
    )
  }
  canonical <- canonical[
    !is.na(canonical$basin.id) &
      nzchar(as.character(canonical$basin.id)),
    required.canonical,
    drop = FALSE
  ]
  root.index <- match(
    roots,
    suppressWarnings(as.integer(canonical$extremum.vertex))
  )
  if (anyNA(root.index)) {
    .gflowui_basin_panel_stop(
      "Trajectory roots do not map one-to-one to canonical maximum basins."
    )
  }
  basin.ids <- as.character(canonical$basin.id[root.index])
  common.color <- as.character(common.color %||% "#4B5563")
  if (length(common.color) != 1L ||
      is.na(common.color) ||
      !nzchar(common.color)) {
    common.color <- "#4B5563"
  }
  colors <- if (identical(color.mode, "single")) {
    rep.int(common.color, length(from))
  } else {
    if (!is.character(basin.colors) || is.null(names(basin.colors))) {
      .gflowui_basin_panel_stop(
        "Assigned-basin connection colors must be a named character vector."
      )
    }
    values <- unname(basin.colors[basin.ids])
    if (anyNA(values) || any(!nzchar(values))) {
      .gflowui_basin_panel_stop(
        "Every trajectory root must have an assigned-basin connection color."
      )
    }
    values
  }
  edges <- data.frame(
    from = as.integer(from),
    to = as.integer(to),
    root.vertex = as.integer(roots),
    basin.id = basin.ids,
    color = as.character(colors),
    stringsAsFactors = FALSE
  )
  edges[!duplicated(edges[c("from", "to")]), , drop = FALSE]
}

gflowui_basin_ascent_flow_plotly_spec <- function(
    edges,
    coordinates,
    visible.vertices = seq_len(nrow(coordinates)),
    color.mode = c("basin", "single"),
    common.color = "#4B5563",
    opacity = 1,
    width = 2) {
  color.mode <- match.arg(color.mode)
  if (!is.data.frame(edges) ||
      !all(c("from", "to", "color") %in% names(edges)) ||
      !is.matrix(coordinates) ||
      ncol(coordinates) < 3L) {
    .gflowui_basin_panel_stop(
      "Ascent-flow plotting requires valid edges and 3D coordinates."
    )
  }
  visible <- unique(suppressWarnings(as.integer(visible.vertices)))
  visible <- visible[
    is.finite(visible) &
      visible >= 1L &
      visible <= nrow(coordinates)
  ]
  opacity <- suppressWarnings(as.numeric(opacity))
  if (length(opacity) != 1L || !is.finite(opacity)) opacity <- 1
  width <- suppressWarnings(as.numeric(width))
  if (length(width) != 1L || !is.finite(width)) width <- 2
  keep <- edges$from %in% visible & edges$to %in% visible
  edges <- edges[keep, , drop = FALSE]
  if (!nrow(edges)) {
    return(list(
      n.edges = 0L,
      x = numeric(),
      y = numeric(),
      z = numeric(),
      line = list(),
      opacity = max(0, min(1, opacity))
    ))
  }
  rows <- seq_len(nrow(edges))
  xyz <- matrix(NA_real_, nrow = 3L * nrow(edges), ncol = 3L)
  xyz[3L * rows - 2L, ] <- coordinates[edges$from, 1:3, drop = FALSE]
  xyz[3L * rows - 1L, ] <- coordinates[edges$to, 1:3, drop = FALSE]
  line <- list(
    width = max(0.5, min(8, width))
  )
  if (identical(color.mode, "single")) {
    color <- as.character(common.color %||% "#4B5563")
    if (length(color) != 1L || is.na(color) || !nzchar(color)) {
      color <- "#4B5563"
    }
    line$color <- color
  } else {
    palette <- unique(as.character(edges$color))
    if (length(palette) == 1L) {
      line$color <- palette[[1L]]
    } else {
      code <- match(as.character(edges$color), palette) - 1L
      color.values <- rep(NA_real_, nrow(xyz))
      color.values[3L * rows - 2L] <- code
      color.values[3L * rows - 1L] <- code
      maximum <- length(palette) - 1L
      line$color <- color.values
      line$cmin <- 0
      line$cmax <- maximum
      line$colorscale <- lapply(seq_along(palette), function(index) {
        list((index - 1L) / maximum, palette[[index]])
      })
      line$showscale <- FALSE
    }
  }
  list(
    n.edges = nrow(edges),
    x = xyz[, 1],
    y = xyz[, 2],
    z = xyz[, 3],
    line = line,
    opacity = max(0, min(1, opacity))
  )
}

gflowui_basin_interactive_cut <- function(
    structure,
    events,
    event.index = 0L,
    component.colors = c("distinct", "single"),
    merge.scope = c("current", "reached", "hidden"),
    cut.accessor = gflow::cut.basin.merge.tree) {
  gflowui_basin_validate_interactive_events(events)
  component.colors <- match.arg(component.colors)
  merge.scope <- match.arg(merge.scope)
  event.index <- suppressWarnings(as.integer(event.index))
  if (length(event.index) != 1L || !is.finite(event.index)) event.index <- 0L
  event.index <- max(0L, min(nrow(events) - 1L, event.index))
  event <- events[event.index + 1L, , drop = FALSE]
  height <- event$height[[1L]]
  continuation <- structure$continuation
  data <- structure$data
  cut <- cut.accessor(
    data$canonical.tree,
    height = height,
    direction = "max",
    component = structure$component,
    continuation.priority = continuation$priority,
    continuation.measure = if (is.null(continuation$priority)) {
      NULL
    } else {
      continuation$measure
    }
  )
  components <- cut$components
  membership <- cut$membership
  component.palette <- if (identical(component.colors, "single")) {
    stats::setNames(rep("#2563EB", nrow(components)), components$component.id)
  } else {
    stats::setNames(
      unname(structure$palette[components$basin.id]),
      components$component.id
    )
  }
  maxima <- data$canonical[
    data$canonical$basin.id %in% structure$scope.ids &
      data$canonical$peak.value >= height,
    c("basin.id", "extremum.vertex", "peak.value"),
    drop = FALSE
  ]
  maxima$label <- unname(structure$label.text[maxima$basin.id])
  missing.labels <- is.na(maxima$label) | !nzchar(maxima$label)
  maxima$label[missing.labels] <- maxima$basin.id[missing.labels]
  plateaus <- .gflowui_basin_interactive_merge_plateaus(
    tree = list(merge.table = structure$layout$events),
    canonical = data$canonical,
    component = structure$component,
    scope.ids = structure$scope.ids,
    height = height,
    merge.scope = merge.scope,
    label.text = structure$label.text
  )
  c(structure[c(
    "scope", "requested.scope.ids", "scope.ids", "component",
    "context.token", "continuation", "layout", "points", "vertical",
    "horizontal", "palette"
  )], list(
    events = events,
    event = event,
    event.index = event.index,
    level.index = event.index,
    levels = events$height,
    height = height,
    relation = ">=",
    cut = cut,
    components = components,
    membership = membership,
    component.colors = component.palette,
    maxima = maxima,
    merge.plateaus = plateaus,
    n.active.vertices = nrow(membership),
    n.active.components = nrow(components),
    above.maximum = isTRUE(event$above.maximum[[1L]])
  ))
}

gflowui_basin_interactive_tree_data <- function(
    state,
    scope = c("proposal", "complete"),
    level.index = 0L,
    component.colors = c("distinct", "single"),
    merge.scope = c("current", "reached", "hidden"),
    continuation.rule = "field_value",
    label.text = NULL,
    basin.colors = NULL,
    layout.accessor = gflow::get.basin.merge.tree.layout,
    cut.accessor = gflow::cut.basin.merge.tree) {
  structure <- gflowui_basin_interactive_structure(
    state,
    scope = scope,
    continuation.rule = continuation.rule,
    label.text = label.text,
    basin.colors = basin.colors,
    layout.accessor = layout.accessor
  )
  events <- gflowui_basin_interactive_events(structure)
  gflowui_basin_interactive_cut(
    structure,
    events,
    event.index = level.index,
    component.colors = component.colors,
    merge.scope = merge.scope,
    cut.accessor = cut.accessor
  )
}

.gflowui_basin_panel_core_outcome_label <- function(outcome) {
  labels <- c(
    strong_gap = "Automatic: strong mass gap",
    coverage = "Mass-coverage target",
    coverage_capped = "Mass coverage capped by the core budget",
    single_positive = "Single positive-mass branch",
    complete = "No filtering",
    minimum_mass = "Minimum-mass threshold",
    threshold_empty = "Minimum-mass threshold selected no branches",
    top_k = "Top K"
  )
  outcome <- as.character(outcome %||% "")
  label <- unname(labels[[outcome]])
  if (is.null(label)) {
    gsub("_", " ", outcome, fixed = TRUE)
  } else {
    label
  }
}

.gflowui_basin_panel_render_outcome_label <- function(outcome) {
  labels <- c(
    renderable = "Ready",
    core_overflow = "Paused: initial selection exceeds the render budget",
    sentinel_overflow = "Paused: required sentinels exceed the render budget",
    closure_overflow = "Paused: ancestor closure exceeds the render budget",
    continuation_closure_overflow =
      "Paused: continuation ancestors exceed the render budget"
  )
  outcome <- as.character(outcome %||% "")
  label <- unname(labels[[outcome]])
  if (is.null(label)) {
    gsub("_", " ", outcome, fixed = TRUE)
  } else {
    label
  }
}

.gflowui_basin_panel_summary_ui <- function(
    total.maximum.count,
    component.count,
    component.id,
    component.maximum.count,
    core.count,
    final.count,
    core.outcome,
    render.outcome) {
  summary.line <- function(label, value) {
    shiny::tags$li(
      shiny::strong(paste0(label, ": ")),
      as.character(value)
    )
  }
  component.line <- if (as.integer(component.count) > 1L) {
    summary.line(
      "Graph component",
      sprintf(
        "%d of %d (%d maximum basins)",
        as.integer(component.id),
        as.integer(component.count),
        as.integer(component.maximum.count)
      )
    )
  } else {
    NULL
  }
  shiny::div(
    class = "gf-basin-tree-summary-list",
    role = "status",
    `aria-live` = "polite",
    shiny::tags$ul(
      summary.line("Maximum basins", as.integer(total.maximum.count)),
      component.line,
      summary.line(
        "Initially selected for display",
        sprintf(
          "%d (%s)",
          as.integer(core.count),
          .gflowui_basin_panel_core_outcome_label(core.outcome)
        )
      ),
      summary.line("Final branches displayed", as.integer(final.count)),
      summary.line(
        "Static rendering",
        .gflowui_basin_panel_render_outcome_label(render.outcome)
      )
    )
  )
}

.gflowui_basin_panel_controls_help <- function(
    continuation.rule = "field_value") {
  continuation.rule <- gflowui_basin_continuation_rule(continuation.rule)
  shiny::div(
    class = "gf-basin-tree-controls-help",
    shiny::h5("How the tree and controls work"),
    shiny::p(
      paste(
        "The tree is built from graph superlevel sets as the density",
        "threshold is lowered. Each local maximum starts a branch.",
        gflowui_basin_continuation_description(continuation.rule),
        "The merge level and terminating branch are recorded in the tree."
      )
    ),
    shiny::p(
      paste(
        "The continuation selector changes only which basin identity follows",
        "a connected component after a merge. The graph superlevel sets and",
        "their merge heights remain fixed. Prominence always reports the",
        "canonical field-value elder-rule quantity; Continuation lifetime",
        "reports the corresponding quantity for the selected rule."
      )
    ),
    shiny::tags$dl(
      shiny::tags$dt("Component"),
      shiny::tags$dd(
        paste(
          "Appears only when the graph has multiple connected components;",
          "it selects which component's maximum-basin tree is displayed."
        )
      ),
      shiny::tags$dt("Filter"),
      shiny::tags$dd(
        paste(
          "Chooses the initial display subset. Auto looks for a strong",
          "trajectory-flow mass gap after reaching the requested mass",
          "coverage and within the core branch budget. The other modes use",
          "cumulative mass, a minimum mass, Top K, or no filtering."
        )
      ),
      shiny::tags$dt("Core branch budget"),
      shiny::tags$dd(
        shiny::p(
          paste(
            "A soft upper limit on the number of positive-mass branches in",
            "the initial subset selected by Auto or Cumulative Mass. It does",
            "not apply to Minimum Mass, Top K, or None, and it is not the",
            "number of branches permitted in the final tree."
          )
        ),
        shiny::p(
          paste(
            "Branches are ordered by decreasing trajectory-flow mass, with",
            "exact mass ties kept together. Cumulative Mass keeps the",
            "smallest tie-complete prefix that reaches the requested",
            "coverage. Auto first finds a tie-complete prefix that reaches",
            "the coverage target and contains at least three positive-mass",
            "branches when available, then looks for the first strong mass",
            "gap at or before the budget. If reaching the target would cross",
            "the budget, selection stops at the budget boundary; an exact",
            "tie that crosses that boundary is kept in full, so the actual",
            "initial count can be slightly larger than the stated budget."
          )
        )
      ),
      shiny::tags$dt("Final render budget"),
      shiny::tags$dd(
        paste(
          "A separate safety limit for the number of branches drawn in the",
          "static tree. The app starts with the initial subset, adds required",
          "sentinels, pinned branches, and the component's surviving branch,",
          "then adds any intermediate ancestor branches needed to connect",
          "them into a valid tree. If the result exceeds this limit, the app",
          "does not discard scientifically required branches: it pauses the",
          "Current displayed proposal view and reports which stage exceeded",
          "the budget. Increase the budget or narrow the filter to render",
          "that view. Choosing Complete component is an explicit request to",
          "inspect every branch and is not silently substituted for it."
        )
      ),
      shiny::tags$dt("Sentinels"),
      shiny::tags$dd(
        paste(
          "Sentinels protect scientifically notable branches from being",
          "lost only because the main filter ranks by trajectory-flow mass.",
          "For each enabled measure (peak value, prominence, or support), the",
          "app adds the requested number of top-ranked branches to the",
          "initial subset. Exact ties at the cutoff are all kept, so an",
          "enabled measure can add more than the requested count. A sentinel",
          "changes only which branches are displayed; it never changes",
          "parentage, merge levels, or the selected continuation tree."
        )
      ),
      shiny::tags$dt("Labels"),
      shiny::tags$dd(
        shiny::p(
          paste(
            "Controls which labels are drawn on branches that are actually",
            "present in the current tree. Label text follows the Basin",
            "labeling method selected at the top of General Inspector."
          )
        ),
        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong("Important: "),
            paste(
              "labels the union of the top Important-label count branches",
              "by trajectory-flow mass, peak value, prominence, and support.",
              "It also labels the component's surviving branch and any",
              "selected or pinned displayed branches. Exact cutoff ties are",
              "included, so the label count can exceed the entered number."
            )
          ),
          shiny::tags$li(
            shiny::strong("Selected: "),
            paste(
              "labels only branches selected in the tree and currently",
              "present in the displayed layout."
            )
          ),
          shiny::tags$li(
            shiny::strong("Displayed: "),
            "labels every branch in the current filtered tree."
          ),
          shiny::tags$li(
            shiny::strong("None: "),
            "draws no branch labels."
          ),
          shiny::tags$li(
            shiny::strong("All: "),
            paste(
              "labels every branch in the current displayed layout and",
              "shows a crowding warning. It cannot label branches omitted",
              "from that layout; choose Complete component as the Tree scope",
              "to inspect those branches."
            )
          )
        )
      ),
      shiny::tags$dt("Interactive tree and threshold"),
      shiny::tags$dd(
        shiny::p(
          paste(
            "Tree scope chooses between the accepted Current displayed",
            "proposal and every maximum branch in the Complete component.",
            "Changing scope changes only what the interactive viewer exposes;",
            "it does not change the accepted proposal or its filter."
          )
        ),
        shiny::p(
          paste(
            "The horizontal navigator starts at a finite level above every",
            "maximum and stops only where topology changes: a branch birth,",
            "one or more exact merge plateaus, or the component floor. Coincident",
            "changes share one event. Previous event and Next event move one",
            "topology event at a time. While you drag, only the dotted line and",
            "preview text move in the browser; releasing the slider commits the",
            "event and computes its exact cut. The 3D graph then uses that same",
            "cut: vertices below h are gray, while vertices with value at least",
            "h are grouped into graph superlevel-set components."
          )
        ),
        shiny::p(
          paste(
            "Active-component colors can be different and stable, using the",
            "continuing basin's color after a merge, or one common color.",
            "Changing Basin vertex color automatically selects One common",
            "color and applies the chosen color to every active basin vertex.",
            "Basin vertex size controls active vertices other than the",
            "separately styled local maxima.",
            "Uncheck",
            "Link h to the 3D graph to leave the graph's ordinary color",
            "source visible while continuing to inspect the tree."
          )
        ),
        shiny::p(
          paste(
            "Active maxima are the displayed-scope maxima whose birth value",
            "has been reached by h. Merge plateaus are the one or more graph",
            "vertices where branches join; a plateau can represent several",
            "simultaneous merges. The styling controls set marker colors,",
            "marker sizes, label visibility, and label sizes. Merge plateaus",
            "can be shown only at the current h, accumulated after they have",
            "been reached, or hidden."
          )
        ),
        shiny::p(
          paste(
            "Canonical ascent-flow connections optionally draw the fixed",
            "maximum-direction CLOSEST trajectory forest on the linked 3D",
            "graph. Every active nonmaximum vertex contributes exactly one",
            "graph edge to its next ascending vertex; following those edges",
            "terminates at that vertex's canonically assigned local maximum.",
            "These are graph edges, not direct geometric rays to a maximum.",
            "Exact equal-value plateaus use the connected-plateau routing",
            "stored by gflow::create.basin.complex(). Connection color,",
            "opacity, and width style this fixed forest without changing",
            "its trajectories or basin assignments. These style updates are",
            "applied directly to the existing 3D layer rather than rebuilding",
            "the graph. The connection controls are",
            "available only while h is linked to the 3D graph."
          )
        ),
        shiny::p(
          paste(
            "Use all branches (Filter: None) is different from Tree scope:",
            "it changes Filter to None and recomputes the scientific display",
            "proposal. Complete component only expands the interactive view."
          )
        )
      ),
      shiny::tags$dt("Display recipe"),
      shiny::tags$dd(
        shiny::p(
          paste(
            "A display recipe lets you reuse the same filtering, budget,",
            "sentinel, and label settings after a reload or with another",
            "compatible basin analysis. Saving places a versioned copy in",
            "browser storage and makes it available as a JSON download."
          )
        ),
        shiny::p(
          paste(
            "Applying a recipe validates those settings against the active",
            "scientific bundle and recomputes the component and display",
            "proposal. A recipe is not a saved analysis or figure: it does",
            "not contain the basin complex, data values, component identity,",
            "selected or pinned basins, proposal results, or tree layout.",
            "It also does not save the current h or interactive styling."
          )
        )
      )
    )
  )
}

.gflowui_basin_complete_viewer_title <- function() {
  "Complete Interactive Density-Value Elder-Rule Basin Merge Tree"
}

.gflowui_basin_panel_selection_ui <- function(model) {
  ids <- model$selected$ids
  if (!length(ids)) {
    return(NULL)
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
    .gflowui_basin_panel_controls_help(),
    shiny::div(
      class = "gf-basin-tree-control-grid",
      if (length(component.choices) > 1L) shiny::selectInput(
        "basin_tree_component",
        "Component",
        choices = component.choices,
        selected = as.character(model$component$id)
      ) else NULL,
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
    "Basin Structure, Selection, and Merge Tree"
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
            "Use all branches (Filter: None)",
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
  warnings <- unique(c(
    model$status$warnings,
    model$labels$warning
  ))
  warnings <- warnings[
    !is.na(warnings) & nzchar(as.character(warnings))
  ]
  plot.ui <- if (is.null(model$overflow)) {
    shiny::div(
      class = "gf-basin-tree-plot-scroll",
      `data-plot-branch-count` = model$counts$final,
      plotly::plotlyOutput(
        "basin_merge_tree_interactive_plot",
        width = "100%",
        height = "680px"
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
  shiny::tags$section(
    id = "gf_basin_merge_tree",
    class = "gf-basin-merge-tree",
    role = "region",
    `aria-labelledby` = "gf_basin_merge_tree_heading",
    `data-analysis-state` = model$outcome,
    `data-display-source` = model$display.source,
    `data-context-generation` = model$context.generation,
    `data-attempt-id` = model$active.attempt$attempt.id,
    `data-render-outcome` = if (is.null(model$overflow)) {
      "renderable"
    } else {
      model$overflow$outcome
    },
    `data-core-outcome` = proposal$core$outcome,
    `data-label-mode` = model$labels$mode,
    shiny::div(
      class = "gf-basin-merge-tree-header",
      heading,
      shiny::div(
        class = "gf-basin-tree-actions",
        shiny::actionButton(
          "basin_tree_show_all",
          "Use all branches (Filter: None)",
          class = "btn btn-sm btn-outline-secondary",
          title = paste(
            "Set Filter to None and recompute; the displayed-proposal view",
            "pauses if",
            "the resulting tree exceeds the Final render budget"
          )
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
    .gflowui_basin_panel_summary_ui(
      total.maximum.count = model$total.maximum.count,
      component.count = model$component$count,
      component.id = model$component$id,
      component.maximum.count = model$component$maximum.count,
      core.count = model$counts$core,
      final.count = if (!is.null(model$layout)) {
        nrow(model$layout$branches)
      } else {
        model$counts$final
      },
      core.outcome = proposal$core$outcome,
      render.outcome = if (is.null(model$overflow)) {
        "renderable"
      } else {
        model$overflow$outcome
      }
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
    plot.ui
  )
}
