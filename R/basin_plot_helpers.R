gflowui_basin_plot_feature_choices <- function() {
  c(
    "Support" = "support",
    "Mass" = "mass",
    "Extremum value" = "extremum_value",
    "Prominence" = "prominence",
    "Extremum value rank" = "extremum_value_rank",
    "Support rank" = "support_rank",
    "Mass rank" = "mass_rank",
    "Prominence rank" = "prominence_rank"
  )
}

gflowui_basin_plot_feature_label <- function(feature) {
  choices <- gflowui_basin_plot_feature_choices()
  matched <- names(choices)[match(as.character(feature), unname(choices))]
  if (length(matched) == 1L && !is.na(matched) && nzchar(matched)) {
    matched
  } else {
    as.character(feature)
  }
}

gflowui_basin_plot_scale_choices <- function() {
  c(
    "Raw" = "raw",
    "Log10 (positive values only)" = "log10"
  )
}

gflowui_basin_plot_scale_map <- function(
    spec,
    x_scale = "raw",
    y_scale = "raw") {
  features <- as.character(spec$features)
  x_scale <- if (identical(as.character(x_scale), "log10")) {
    "log10"
  } else {
    "raw"
  }
  y_scale <- if (identical(as.character(y_scale), "log10")) {
    "log10"
  } else {
    "raw"
  }
  scales <- stats::setNames(rep("raw", length(features)), features)
  kind <- as.character(spec$kind)
  if (identical(kind, "histogram")) {
    scales[[features[[1L]]]] <- x_scale
  } else if (identical(kind, "scatter")) {
    scales[[features[[1L]]]] <- x_scale
    scales[[features[[2L]]]] <- y_scale
  } else if (identical(kind, "ranked")) {
    scales[[features[[1L]]]] <- y_scale
  } else if (identical(kind, "cumulative")) {
    scales[] <- "raw"
  } else {
    scales[] <- x_scale
  }
  scales
}

gflowui_basin_plot_scaled_data <- function(
    data,
    spec,
    x_scale = "raw",
    y_scale = "raw") {
  features <- as.character(spec$features)
  data <- gflowui_basin_plot_complete_rows(data, features)
  initial.rows <- nrow(data)
  scales <- gflowui_basin_plot_scale_map(
    spec,
    x_scale = x_scale,
    y_scale = y_scale
  )
  log.features <- names(scales)[scales == "log10"]
  if (length(log.features) > 0L && nrow(data) > 0L) {
    keep <- rep(TRUE, nrow(data))
    for (feature in log.features) {
      keep <- keep & data[[feature]] > 0
    }
    data <- data[keep, , drop = FALSE]
    for (feature in log.features) {
      data[[feature]] <- log10(data[[feature]])
    }
  }
  attr(data, "gflowui_scale_map") <- scales
  attr(data, "gflowui_nonpositive_excluded") <- initial.rows - nrow(data)
  data
}

gflowui_basin_plot_axis_label <- function(feature, scale = "raw") {
  label <- gflowui_basin_plot_feature_label(feature)
  if (identical(as.character(scale), "log10")) {
    sprintf("log10(%s)", label)
  } else {
    label
  }
}

gflowui_basin_plot_data <- function(
    result,
    scope = c(
      "all", "component_maxima", "initial_display", "core",
      "sentinels", "pinned", "listed", "selected"
    ),
    type = c("both", "max", "min"),
    selected_keys = character(),
    analysis_state = NULL) {
  scope <- match.arg(scope)
  type <- match.arg(type)
  table <- gflowui_basin_proposal_context_table(
    result,
    state = analysis_state,
    selected_keys = selected_keys
  )
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(data.frame(
      key = character(),
      type = character(),
      label = character(),
      rank = integer(),
      support = integer(),
      mass = numeric(),
      extremum_value = numeric(),
      prominence = numeric(),
      extremum_value_rank = integer(),
      support_rank = integer(),
      mass_rank = integer(),
      prominence_rank = integer(),
      canonical_basin_id = character(),
      membership = character(),
      inclusion_reasons = character(),
      pinned = logical(),
      selected = logical(),
      visibility = character(),
      stringsAsFactors = FALSE
    ))
  }
  tie.breaker <- if ("basin.id" %in% names(table)) {
    as.character(table$basin.id)
  } else {
    as.character(table$key)
  }
  direction.rank <- function(value, minimum.lowest = FALSE) {
    value <- suppressWarnings(as.numeric(value))
    rank <- rep.int(NA_integer_, length(value))
    for (direction in c("max", "min")) {
      rows <- which(
        as.character(table$type) == direction & is.finite(value)
      )
      if (length(rows) < 1L) {
        next
      }
      order.value <- if (isTRUE(minimum.lowest) &&
          identical(direction, "min")) {
        value[rows]
      } else {
        -value[rows]
      }
      ordered <- order(
        order.value,
        tie.breaker[rows],
        method = "radix"
      )
      rank[rows[ordered]] <- seq_along(rows)
    }
    rank
  }
  extremum.value <- suppressWarnings(as.numeric(table$extremum.value))
  support <- suppressWarnings(as.integer(table$primary.support.size))
  mass <- suppressWarnings(as.numeric(table$primary.support.mass))
  prominence <- suppressWarnings(as.numeric(table$prominence))
  extremum.value.rank <- direction.rank(
    extremum.value,
    minimum.lowest = TRUE
  )
  support.rank <- direction.rank(support)
  mass.rank <- direction.rank(mass)
  prominence.rank <- direction.rank(prominence)
  if (identical(scope, "component_maxima")) {
    keep <- as.character(table$type) == "max" & table$proposal.component
    table <- table[keep, , drop = FALSE]
    extremum.value <- extremum.value[keep]
    support <- support[keep]
    mass <- mass[keep]
    prominence <- prominence[keep]
    extremum.value.rank <- extremum.value.rank[keep]
    support.rank <- support.rank[keep]
    mass.rank <- mass.rank[keep]
    prominence.rank <- prominence.rank[keep]
  } else if (scope %in% c(
      "initial_display", "core", "sentinels", "pinned"
  )) {
    scoped <- gflowui_basin_inspector_rows(
      result,
      state = analysis_state,
      scope = scope,
      sort.by = "basin_label",
      selected_keys = selected_keys
    )
    keep <- as.character(table$key) %in% as.character(scoped$key)
    table <- table[keep, , drop = FALSE]
    extremum.value <- extremum.value[keep]
    support <- support[keep]
    mass <- mass[keep]
    prominence <- prominence[keep]
    extremum.value.rank <- extremum.value.rank[keep]
    support.rank <- support.rank[keep]
    mass.rank <- mass.rank[keep]
    prominence.rank <- prominence.rank[keep]
  } else if (identical(scope, "listed")) {
    listed.keys <- if (is.data.frame(result$table)) {
      as.character(result$table$key)
    } else {
      character()
    }
    keep <- as.character(table$key) %in% listed.keys
    table <- table[keep, , drop = FALSE]
    extremum.value <- extremum.value[keep]
    support <- support[keep]
    mass <- mass[keep]
    prominence <- prominence[keep]
    extremum.value.rank <- extremum.value.rank[keep]
    support.rank <- support.rank[keep]
    mass.rank <- mass.rank[keep]
    prominence.rank <- prominence.rank[keep]
  } else if (identical(scope, "selected")) {
    keep <- as.character(table$key) %in% as.character(selected_keys)
    table <- table[keep, , drop = FALSE]
    extremum.value <- extremum.value[keep]
    support <- support[keep]
    mass <- mass[keep]
    prominence <- prominence[keep]
    extremum.value.rank <- extremum.value.rank[keep]
    support.rank <- support.rank[keep]
    mass.rank <- mass.rank[keep]
    prominence.rank <- prominence.rank[keep]
  }
  if (type %in% c("max", "min")) {
    keep <- as.character(table$type) == type
    table <- table[keep, , drop = FALSE]
    extremum.value <- extremum.value[keep]
    support <- support[keep]
    mass <- mass[keep]
    prominence <- prominence[keep]
    extremum.value.rank <- extremum.value.rank[keep]
    support.rank <- support.rank[keep]
    mass.rank <- mass.rank[keep]
    prominence.rank <- prominence.rank[keep]
  }
  data.frame(
    key = as.character(table$key),
    type = as.character(table$type),
    label = as.character(table$display.label),
    rank = suppressWarnings(as.integer(table$label.rank)),
    support = support,
    mass = mass,
    extremum_value = extremum.value,
    prominence = prominence,
    extremum_value_rank = extremum.value.rank,
    support_rank = support.rank,
    mass_rank = mass.rank,
    prominence_rank = prominence.rank,
    canonical_basin_id = as.character(table$canonical.basin.id),
    membership = as.character(table$proposal.membership.class),
    inclusion_reasons = as.character(table$proposal.inclusion.reasons),
    pinned = as.logical(table$proposal.pinned),
    selected = as.logical(table$proposal.selected),
    visibility = as.character(table$proposal.visibility),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_plot_label_rows <- function(data, label_top_k = 0L) {
  label_top_k <- suppressWarnings(as.integer(label_top_k))
  if (!is.data.frame(data) ||
      nrow(data) < 1L ||
      !all(c("rank", "label") %in% names(data)) ||
      length(label_top_k) != 1L ||
      !is.finite(label_top_k) ||
      label_top_k < 1L) {
    return(integer())
  }
  rank <- suppressWarnings(as.integer(data$rank))
  label <- as.character(data$label)
  which(
    is.finite(rank) &
      rank >= 1L &
      rank <= label_top_k &
      !is.na(label) &
      nzchar(label)
  )
}

.gflowui_basin_scaled_distance <- function(x, y, target.x, target.y) {
  x.range <- range(x, finite = TRUE)
  y.range <- range(y, finite = TRUE)
  x.span <- diff(x.range)
  y.span <- diff(y.range)
  if (!is.finite(x.span) || x.span <= 0) x.span <- 1
  if (!is.finite(y.span) || y.span <= 0) y.span <- 1
  sqrt(
    ((x - target.x) / x.span)^2 +
      ((y - target.y) / y.span)^2
  )
}

gflowui_basin_plot_nearest_key <- function(
    data,
    spec,
    click.x,
    click.y,
    x_scale = "raw",
    y_scale = "raw",
    threshold = 0.045) {
  if (!is.data.frame(data) ||
      !identical(as.character(spec$kind), "scatter") ||
      length(spec$features) != 2L ||
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
  scaled <- gflowui_basin_plot_scaled_data(
    data,
    spec,
    x_scale = x_scale,
    y_scale = y_scale
  )
  features <- as.character(spec$features)
  if (!nrow(scaled) ||
      !"key" %in% names(scaled) ||
      any(!features %in% names(scaled))) {
    return(character())
  }
  distance <- .gflowui_basin_scaled_distance(
    suppressWarnings(as.numeric(scaled[[features[[1L]]]])),
    suppressWarnings(as.numeric(scaled[[features[[2L]]]])),
    as.numeric(click.x),
    as.numeric(click.y)
  )
  finite.distance <- which(is.finite(distance))
  if (!length(finite.distance)) {
    return(character())
  }
  minimum <- min(distance[finite.distance])
  candidates <- finite.distance[distance[finite.distance] == minimum]
  nearest <- candidates[order(
    as.character(scaled$key[candidates]),
    method = "radix"
  )][[1L]]
  if (!length(nearest) ||
      !is.finite(distance[[nearest]]) ||
      distance[[nearest]] > threshold) {
    return(character())
  }
  as.character(scaled$key[[nearest]])
}

gflowui_basin_new_plot_specs <- function(
    features,
    mode = c("histograms", "ranked", "cumulative", "pairs", "matrix"),
    first_id = 1L,
    scope = "all",
    type = "both",
    construction_fingerprint = "") {
  mode <- match.arg(mode)
  allowed <- unname(gflowui_basin_plot_feature_choices())
  features <- unique(as.character(features))
  features <- features[features %in% allowed]
  if (length(features) < 1L) {
    return(list())
  }
  feature.sets <- switch(
    mode,
    histograms = as.list(features),
    ranked = as.list(features),
    cumulative = as.list(intersect(features, c("support", "mass"))),
    pairs = if (length(features) >= 2L) {
      unname(utils::combn(features, 2L, simplify = FALSE))
    } else {
      list()
    },
    matrix = if (length(features) >= 2L) list(features) else list()
  )
  if (length(feature.sets) < 1L) {
    return(list())
  }
  ids <- seq.int(
    from = suppressWarnings(as.integer(first_id)),
    length.out = length(feature.sets)
  )
  lapply(seq_along(feature.sets), function(index) {
    selected <- as.character(feature.sets[[index]])
    kind <- if (identical(mode, "histograms")) {
      "histogram"
    } else if (identical(mode, "ranked")) {
      "ranked"
    } else if (identical(mode, "cumulative")) {
      "cumulative"
    } else if (identical(mode, "pairs")) {
      "scatter"
    } else {
      "matrix"
    }
    list(
      id = as.integer(ids[[index]]),
      kind = kind,
      features = selected,
      scope = as.character(scope),
      type = as.character(type),
      construction_fingerprint = as.character(construction_fingerprint)
    )
  })
}

gflowui_basin_plot_spec_signature <- function(spec) {
  kind <- as.character(spec$kind)
  if (length(kind) < 1L || !nzchar(kind[[1L]])) {
    kind <- "unknown"
  } else {
    kind <- kind[[1L]]
  }
  features <- sort(unique(as.character(spec$features)))
  fingerprint <- as.character(spec$construction_fingerprint)
  if (length(fingerprint) < 1L || !nzchar(fingerprint[[1L]])) {
    fingerprint <- ""
  } else {
    fingerprint <- fingerprint[[1L]]
  }
  paste(
    fingerprint,
    kind,
    paste(features, collapse = "+"),
    sep = "|"
  )
}

gflowui_basin_filter_new_plot_specs <- function(existing, candidates) {
  existing <- if (is.list(existing)) existing else list()
  candidates <- if (is.list(candidates)) candidates else list()
  seen <- if (length(existing) > 0L) {
    unique(vapply(
      existing,
      gflowui_basin_plot_spec_signature,
      character(1)
    ))
  } else {
    character()
  }
  keep <- logical(length(candidates))
  for (index in seq_along(candidates)) {
    signature <- gflowui_basin_plot_spec_signature(candidates[[index]])
    if (!signature %in% seen) {
      keep[[index]] <- TRUE
      seen <- c(seen, signature)
    }
  }
  list(
    specs = candidates[keep],
    skipped = as.integer(sum(!keep)),
    requested = as.integer(length(candidates))
  )
}

gflowui_basin_plot_title <- function(spec) {
  diagnostic.role <- as.character(spec$diagnostic_role %||% "")
  if (identical(diagnostic.role, "positive_mass_distribution")) {
    return("Positive log10 mass")
  }
  if (identical(diagnostic.role, "ranked_positive_mass")) {
    return("Ranked positive mass")
  }
  if (identical(diagnostic.role, "cumulative_positive_mass")) {
    return("Cumulative positive mass")
  }
  labels <- vapply(
    spec$features,
    gflowui_basin_plot_feature_label,
    character(1)
  )
  switch(
    as.character(spec$kind),
    histogram = sprintf("%s distribution", labels[[1L]]),
    ranked = sprintf("Ranked %s", labels[[1L]]),
    cumulative = sprintf("Cumulative %s share", labels[[1L]]),
    scatter = paste(labels, collapse = " \u00d7 "),
    matrix = sprintf("Basin metric matrix (%s)", paste(labels, collapse = ", ")),
    "Basin plot"
  )
}

gflowui_basin_ranked_curve <- function(data, feature) {
  feature <- as.character(feature)
  if (!is.data.frame(data) ||
      length(feature) != 1L ||
      !feature %in% names(data) ||
      !all(c("type", "canonical_basin_id") %in% names(data))) {
    return(data.frame())
  }
  value <- suppressWarnings(as.numeric(data[[feature]]))
  keep <- is.finite(value)
  data <- data[keep, , drop = FALSE]
  value <- value[keep]
  if (nrow(data) < 1L) {
    return(data.frame())
  }
  pieces <- lapply(unique(as.character(data$type)), function(direction) {
    rows <- which(as.character(data$type) == direction)
    if (length(rows) < 1L) return(NULL)
    order.value <- if (grepl("_rank$", feature)) {
      value[rows]
    } else if (identical(feature, "extremum_value") &&
        identical(direction, "min")) {
      value[rows]
    } else {
      -value[rows]
    }
    ordered <- rows[order(
      order.value,
      as.character(data$canonical_basin_id[rows]),
      method = "radix"
    )]
    data.frame(
      key = as.character(data$key[ordered]),
      type = direction,
      label = as.character(data$label[ordered]),
      canonical_basin_id =
        as.character(data$canonical_basin_id[ordered]),
      membership = as.character(data$membership[ordered]),
      selected = as.logical(data$selected[ordered]),
      position = seq_along(ordered),
      value = value[ordered],
      stringsAsFactors = FALSE
    )
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) < 1L) data.frame() else do.call(rbind, pieces)
}

gflowui_basin_cumulative_curve <- function(data, feature) {
  feature <- as.character(feature)
  if (!feature %in% c("support", "mass")) {
    return(data.frame())
  }
  ranked <- gflowui_basin_ranked_curve(data, feature)
  if (!is.data.frame(ranked) || nrow(ranked) < 1L) {
    return(data.frame())
  }
  pieces <- lapply(unique(ranked$type), function(direction) {
    rows <- which(ranked$type == direction & ranked$value > 0)
    if (length(rows) < 1L) return(NULL)
    current <- ranked[rows, , drop = FALSE]
    values <- current$value
    denominator <- sum(values)
    if (!is.finite(denominator) || denominator <= 0) return(NULL)
    run <- rle(values)
    endpoints <- cumsum(run$lengths)
    cumulative <- cumsum(run$values * run$lengths) / denominator
    data.frame(
      type = direction,
      position = endpoints,
      value = cumulative,
      stringsAsFactors = FALSE
    )
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) < 1L) data.frame() else do.call(rbind, pieces)
}

gflowui_basin_plot_selection_overlay <- function(
    data,
    analysis_state,
    visible = TRUE) {
  unavailable <- function(reason) {
    list(available = FALSE, reason = as.character(reason))
  }
  if (!isTRUE(visible)) {
    return(unavailable("Selection thresholds are hidden."))
  }
  if (!is.data.frame(data) ||
      nrow(data) < 1L ||
      !all(c("mass", "canonical_basin_id", "type") %in% names(data)) ||
      !is.list(analysis_state)) {
    return(unavailable("No active maximum-basin proposal is available."))
  }
  proposal <- tryCatch(
    gflowui_basin_displayed_proposal(analysis_state),
    error = function(error) NULL
  )
  if (!is.list(proposal)) {
    return(unavailable("No active maximum-basin proposal is available."))
  }
  maxima <- data[
    as.character(data$type) == "max" &
      is.finite(suppressWarnings(as.numeric(data$mass))),
    ,
    drop = FALSE
  ]
  if (nrow(maxima) < 1L) {
    return(unavailable("No finite maximum-basin masses are available."))
  }
  mass <- suppressWarnings(as.numeric(maxima$mass))
  ids <- as.character(maxima$canonical_basin_id)
  positive <- is.finite(mass) & mass > 0
  ranked <- which(positive)[order(
    -mass[positive],
    ids[positive],
    method = "radix"
  )]
  if (length(ranked) < 1L) {
    return(unavailable("No positive maximum-basin masses are available."))
  }
  ranked.mass <- mass[ranked]
  ranked.ids <- ids[ranked]
  runs <- rle(ranked.mass)
  endpoints <- cumsum(runs$lengths)
  cumulative <- cumsum(runs$values * runs$lengths) / sum(ranked.mass)
  parameters <- proposal$accepted.parameters %||% list()
  core.ids <- as.character(proposal$core$ids %||% character())
  core.rows <- which(ranked.ids %in% core.ids)
  boundary <- suppressWarnings(as.integer(
    proposal$core$boundary %||%
      if (length(core.rows)) max(core.rows) else NA_integer_
  ))
  if (length(boundary) != 1L || !is.finite(boundary)) {
    boundary <- NA_integer_
  }
  mass.cutoff <- suppressWarnings(as.numeric(
    proposal$core$informational.cutoff %||% NA_real_
  ))
  if (!is.finite(mass.cutoff) && length(core.rows) > 0L) {
    mass.cutoff <- min(ranked.mass[core.rows])
  }
  scalar <- function(name, integer = FALSE) {
    value <- parameters[[name]]
    value <- if (integer) {
      suppressWarnings(as.integer(value))
    } else {
      suppressWarnings(as.numeric(value))
    }
    if (length(value) == 1L && is.finite(value)) value else NA
  }
  list(
    available = TRUE,
    reason = "",
    filter.mode = as.character(parameters$filter.mode %||% ""),
    core.outcome = as.character(proposal$core$outcome %||% ""),
    core.ids = core.ids,
    ranked.ids = ranked.ids,
    ranked.mass = ranked.mass,
    tie.endpoints = endpoints,
    cumulative = cumulative,
    zero.count = as.integer(sum(mass == 0)),
    boundary = boundary,
    mass.cutoff = mass.cutoff,
    coverage.target = scalar("coverage.target"),
    minimum.mass = scalar("minimum.mass"),
    top.k = scalar("top.k", integer = TRUE),
    core.budget = scalar("core.branch.budget", integer = TRUE),
    final.budget = scalar("final.render.budget", integer = TRUE)
  )
}

.gflowui_basin_threshold_legend <- function(labels, colors, lines) {
  keep <- !is.na(labels) & nzchar(labels)
  labels <- labels[keep]
  colors <- colors[keep]
  lines <- lines[keep]
  if (length(labels) < 1L) return(invisible(NULL))
  graphics::legend(
    "topright",
    inset = c(-0.3, 0),
    legend = labels,
    col = colors,
    lty = lines,
    lwd = 1.5,
    xpd = NA,
    bty = "n",
    cex = 0.72
  )
  invisible(NULL)
}

gflowui_basin_plot_complete_rows <- function(data, features) {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    return(data.frame())
  }
  features <- as.character(features)
  if (!all(features %in% names(data))) {
    return(data[FALSE, , drop = FALSE])
  }
  keep <- stats::complete.cases(data[, features, drop = FALSE])
  for (feature in features) {
    keep <- keep & is.finite(suppressWarnings(as.numeric(data[[feature]])))
  }
  data[keep, , drop = FALSE]
}

gflowui_basin_histogram_geometry <- function(x, bins = 20L) {
  bins <- suppressWarnings(as.integer(bins))
  if (!is.finite(bins) || bins < 1L) bins <- 20L
  histogram <- graphics::hist(x, breaks = bins, plot = FALSE)
  heights <- histogram$counts
  if (length(heights) > 0L && max(heights) > 0) {
    heights <- heights / max(heights)
  }
  list(
    left = histogram$breaks[-length(histogram$breaks)],
    right = histogram$breaks[-1L],
    height = heights,
    y_limits = c(0, 1.05)
  )
}

gflowui_draw_basin_plot <- function(
    data,
    spec,
    bins = 20L,
    histogram_color = "#2563EB",
    point_color = "type",
    point_glyph = 19L,
    point_size = 1.1,
    point_opacity = 0.75,
    label_top_k = 0L,
    x_scale = "raw",
    y_scale = "raw",
    selection_overlay = NULL,
    show_selection_thresholds = TRUE) {
  features <- as.character(spec$features)
  kind <- as.character(spec$kind)
  data <- gflowui_basin_plot_scaled_data(
    data,
    spec,
    x_scale = x_scale,
    y_scale = y_scale
  )
  scales <- attr(data, "gflowui_scale_map")
  if (nrow(data) < 1L) {
    graphics::plot.new()
    graphics::text(
      0.5,
      0.5,
      "No eligible basin rows for this scale.",
      col = "#64748B"
    )
    return(invisible(data))
  }
  bins <- suppressWarnings(as.integer(bins))
  if (!is.finite(bins) || bins < 1L) bins <- 20L
  point_glyph <- suppressWarnings(as.integer(point_glyph))
  if (!is.finite(point_glyph)) point_glyph <- 19L
  point_size <- suppressWarnings(as.numeric(point_size))
  if (!is.finite(point_size) || point_size <= 0) point_size <- 1.1
  point_opacity <- suppressWarnings(as.numeric(point_opacity))
  if (!is.finite(point_opacity)) point_opacity <- 0.75
  point_opacity <- max(0, min(1, point_opacity))
  point_colors <- if (identical(as.character(point_color), "type")) {
    ifelse(data$type == "max", "#111827", "#06B6D4")
  } else if (identical(as.character(point_color), "proposal")) {
    membership.colors <- c(
      pinned = "#7C3AED",
      core = "#2563EB",
      sentinel_only = "#EA580C",
      ancestor_only = "#64748B",
      displayed = "#0F766E",
      hidden = "#CBD5E1",
      other_component = "#E2E8F0",
      not_applicable = "#06B6D4",
      unavailable = "#94A3B8"
    )
    colors <- unname(membership.colors[as.character(data$membership)])
    colors[is.na(colors)] <- "#94A3B8"
    selected.rows <- !is.na(data$selected) & data$selected
    colors[selected.rows] <- "#DC2626"
    colors
  } else {
    rep(as.character(point_color), nrow(data))
  }
  point_colors <- grDevices::adjustcolor(
    point_colors,
    alpha.f = point_opacity
  )

  proposal.diagnostic <- isTRUE(spec$proposal_diagnostic)
  proposal.overlay.available <- proposal.diagnostic &&
    is.list(selection_overlay) &&
    isTRUE(selection_overlay$available)
  overlay.available <- proposal.overlay.available &&
    isTRUE(show_selection_thresholds)
  if (proposal.overlay.available) {
    original.mar <- graphics::par("mar")
    on.exit(graphics::par(mar = original.mar), add = TRUE)
    graphics::par(mar = c(
      original.mar[[1L]],
      original.mar[[2L]],
      original.mar[[3L]],
      max(original.mar[[4L]], 8)
    ))
  }

  if (identical(kind, "histogram")) {
    feature <- features[[1L]]
    histogram <- graphics::hist(
      data[[feature]],
      breaks = bins,
      plot = FALSE
    )
    graphics::plot(
      histogram,
      col = if (proposal.diagnostic) "#CBD5E1" else histogram_color,
      border = "#FFFFFF",
      main = sprintf(
        "%s (n=%d)",
        gflowui_basin_plot_title(spec),
        nrow(data)
      ),
      xlab = gflowui_basin_plot_axis_label(feature, scales[[feature]])
    )
    legend.labels <- character()
    legend.colors <- character()
    legend.lines <- integer()
    if (proposal.overlay.available) {
      selected.rows <- as.character(data$canonical_basin_id) %in%
        as.character(selection_overlay$core.ids)
      if (any(selected.rows)) {
        selected.histogram <- graphics::hist(
          data[[feature]][selected.rows],
          breaks = histogram$breaks,
          plot = FALSE
        )
        graphics::plot(
          selected.histogram,
          add = TRUE,
          col = grDevices::adjustcolor("#2563EB", alpha.f = 0.72),
          border = "#FFFFFF"
        )
        legend.labels <- c(legend.labels, "Initially selected")
        legend.colors <- c(legend.colors, "#2563EB")
        legend.lines <- c(legend.lines, 1L)
      }
    }
    if (overlay.available && identical(feature, "mass")) {
      cutoff <- suppressWarnings(as.numeric(
        if (identical(selection_overlay$filter.mode, "minimum_mass")) {
          selection_overlay$minimum.mass
        } else {
          selection_overlay$mass.cutoff
        }
      ))
      if (is.finite(cutoff) && cutoff > 0) {
        if (identical(scales[[feature]], "log10")) cutoff <- log10(cutoff)
        graphics::abline(v = cutoff, col = "#B45309", lty = 3, lwd = 1.5)
        legend.labels <- c(legend.labels, "Active mass cutoff")
        legend.colors <- c(legend.colors, "#B45309")
        legend.lines <- c(legend.lines, 3L)
      }
    }
    .gflowui_basin_threshold_legend(
      legend.labels,
      legend.colors,
      legend.lines
    )
  } else if (identical(kind, "ranked")) {
    feature <- features[[1L]]
    curve <- gflowui_basin_ranked_curve(data, feature)
    if (nrow(curve) < 1L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No eligible ranked values.")
      return(invisible(data))
    }
    graphics::plot(
      NA_real_,
      NA_real_,
      xlim = range(curve$position, finite = TRUE),
      ylim = range(curve$value, finite = TRUE),
      xlab = "Complete-tie rank position",
      ylab = gflowui_basin_plot_axis_label(feature, scales[[feature]]),
      main = sprintf("%s (n=%d)", gflowui_basin_plot_title(spec), nrow(curve))
    )
    graphics::grid(col = "#D9DEE2", lty = 3)
    directions <- unique(as.character(curve$type))
    direction.colors <- c(max = "#111827", min = "#0891B2")
    explicit.color <- if (grepl("^#", as.character(point_color))) {
      as.character(point_color)
    } else {
      NA_character_
    }
    for (direction in directions) {
      rows <- which(curve$type == direction)
      color <- if (is.na(explicit.color)) {
        unname(direction.colors[[direction]] %||% "#475569")
      } else {
        explicit.color
      }
      if (proposal.diagnostic) color <- "#94A3B8"
      graphics::lines(
        curve$position[rows],
        curve$value[rows],
        col = color,
        lwd = 1.8
      )
      graphics::points(
        curve$position[rows],
        curve$value[rows],
        col = grDevices::adjustcolor(color, alpha.f = point_opacity),
        pch = point_glyph,
        cex = point_size
      )
    }
    legend.labels <- character()
    legend.colors <- character()
    legend.lines <- integer()
    if (proposal.overlay.available) {
      selected.rows <- which(curve$canonical_basin_id %in%
        selection_overlay$core.ids)
      if (length(selected.rows)) {
        graphics::points(
          curve$position[selected.rows],
          curve$value[selected.rows],
          col = grDevices::adjustcolor("#2563EB", alpha.f = point_opacity),
          pch = point_glyph,
          cex = max(point_size, 0.7)
        )
        legend.labels <- c(legend.labels, "Initially selected")
        legend.colors <- c(legend.colors, "#2563EB")
        legend.lines <- c(legend.lines, 1L)
      }
    }
    if (overlay.available && identical(feature, "mass")) {
      if (is.finite(selection_overlay$boundary)) {
        graphics::abline(
          v = selection_overlay$boundary,
          col = "#B45309",
          lty = 3,
          lwd = 1.5
        )
        legend.labels <- c(legend.labels, "Initial-selection boundary")
        legend.colors <- c(legend.colors, "#B45309")
        legend.lines <- c(legend.lines, 3L)
      }
      cutoff <- suppressWarnings(as.numeric(selection_overlay$mass.cutoff))
      if (is.finite(cutoff) && cutoff > 0) {
        if (identical(scales[[feature]], "log10")) cutoff <- log10(cutoff)
        graphics::abline(h = cutoff, col = "#B45309", lty = 3, lwd = 1.5)
      }
      if (is.finite(selection_overlay$core.budget) &&
          !identical(
            as.integer(selection_overlay$core.budget),
            as.integer(selection_overlay$boundary)
          )) {
        graphics::abline(
          v = selection_overlay$core.budget,
          col = "#64748B",
          lty = 4,
          lwd = 1.2
        )
        legend.labels <- c(legend.labels, "Core branch budget")
        legend.colors <- c(legend.colors, "#64748B")
        legend.lines <- c(legend.lines, 4L)
      }
    }
    .gflowui_basin_threshold_legend(
      legend.labels,
      legend.colors,
      legend.lines
    )
  } else if (identical(kind, "cumulative")) {
    feature <- features[[1L]]
    curve <- gflowui_basin_cumulative_curve(data, feature)
    if (nrow(curve) < 1L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No positive additive values.")
      return(invisible(data))
    }
    graphics::plot(
      NA_real_,
      NA_real_,
      xlim = c(1, max(curve$position)),
      ylim = c(0, 1),
      xlab = "Complete-tie group endpoint",
      ylab = "Cumulative share",
      main = sprintf("%s (n=%d)", gflowui_basin_plot_title(spec), nrow(data))
    )
    graphics::grid(col = "#D9DEE2", lty = 3)
    directions <- unique(as.character(curve$type))
    direction.colors <- c(max = "#111827", min = "#0891B2")
    explicit.color <- if (grepl("^#", as.character(point_color))) {
      as.character(point_color)
    } else {
      NA_character_
    }
    for (direction in directions) {
      rows <- which(curve$type == direction)
      color <- if (is.na(explicit.color)) {
        unname(direction.colors[[direction]] %||% "#475569")
      } else {
        explicit.color
      }
      if (proposal.diagnostic) color <- "#94A3B8"
      graphics::lines(
        c(1, curve$position[rows]),
        c(0, curve$value[rows]),
        type = "s",
        col = color,
        lwd = 2
      )
      graphics::points(
        curve$position[rows],
        curve$value[rows],
        col = color,
        pch = point_glyph,
        cex = point_size
      )
    }
    legend.labels <- character()
    legend.colors <- character()
    legend.lines <- integer()
    if (proposal.overlay.available && identical(feature, "mass")) {
      boundary <- suppressWarnings(as.integer(selection_overlay$boundary))
      if (is.finite(boundary)) {
        selected <- curve[
          curve$type == "max" & curve$position <= boundary,
          ,
          drop = FALSE
        ]
        if (nrow(selected)) {
          graphics::lines(
            c(1, selected$position),
            c(0, selected$value),
            type = "s",
            col = "#2563EB",
            lwd = 2.5
          )
        }
        legend.labels <- c(legend.labels, "Initially selected")
        legend.colors <- c(legend.colors, "#2563EB")
        legend.lines <- c(legend.lines, 1L)
        if (overlay.available) {
          graphics::abline(
            v = boundary,
            col = "#B45309",
            lty = 3,
            lwd = 1.5
          )
          legend.labels <- c(
            legend.labels,
            "Initial-selection boundary"
          )
          legend.colors <- c(legend.colors, "#B45309")
          legend.lines <- c(legend.lines, 3L)
        }
      }
    }
    if (overlay.available && identical(feature, "mass")) {
      if (is.finite(selection_overlay$coverage.target)) {
        graphics::abline(
          h = selection_overlay$coverage.target,
          col = "#7C3AED",
          lty = 3,
          lwd = 1.5
        )
        legend.labels <- c(legend.labels, "Mass-coverage target")
        legend.colors <- c(legend.colors, "#7C3AED")
        legend.lines <- c(legend.lines, 3L)
      }
      if (is.finite(selection_overlay$core.budget) &&
          !identical(
            as.integer(selection_overlay$core.budget),
            as.integer(selection_overlay$boundary)
          )) {
        graphics::abline(
          v = selection_overlay$core.budget,
          col = "#64748B",
          lty = 4,
          lwd = 1.2
        )
        legend.labels <- c(legend.labels, "Core branch budget")
        legend.colors <- c(legend.colors, "#64748B")
        legend.lines <- c(legend.lines, 4L)
      }
    }
    .gflowui_basin_threshold_legend(
      legend.labels,
      legend.colors,
      legend.lines
    )
  } else if (identical(kind, "scatter")) {
    x.feature <- features[[1L]]
    y.feature <- features[[2L]]
    show.type.legend <- identical(as.character(point_color), "type") &&
      length(unique(data$type)) > 1L
    show.proposal.legend <- identical(as.character(point_color), "proposal")
    if (show.type.legend || show.proposal.legend) {
      original.mar <- graphics::par("mar")
      on.exit(graphics::par(mar = original.mar), add = TRUE)
      graphics::par(mar = c(
        original.mar[[1L]],
        original.mar[[2L]],
        original.mar[[3L]],
        max(original.mar[[4L]], 8)
      ))
    }
    graphics::plot(
      data[[x.feature]],
      data[[y.feature]],
      pch = point_glyph,
      cex = point_size,
      col = point_colors,
      xlab = gflowui_basin_plot_axis_label(
        x.feature,
        scales[[x.feature]]
      ),
      ylab = gflowui_basin_plot_axis_label(
        y.feature,
        scales[[y.feature]]
      ),
      main = sprintf("%s (n=%d)", gflowui_basin_plot_title(spec), nrow(data))
    )
    label.rows <- gflowui_basin_plot_label_rows(data, label_top_k)
    if (length(label.rows) > 0L) {
      graphics::text(
        data[[x.feature]][label.rows],
        data[[y.feature]][label.rows],
        labels = data$label[label.rows],
        pos = 3,
        offset = 0.35,
        cex = 0.78,
        font = 2,
        col = "#111827",
        xpd = NA
      )
    }
    if (show.type.legend) {
      graphics::legend(
        "topright",
        inset = c(-0.24, 0),
        legend = c("Maximum", "Minimum"),
        col = c("#111827", "#06B6D4"),
        pch = point_glyph,
        xpd = NA,
        bty = "n",
        cex = 0.8
      )
    }
    if (show.proposal.legend) {
      present <- unique(as.character(data$membership))
      legend.colors <- c(
        pinned = "#7C3AED",
        core = "#2563EB",
        sentinel_only = "#EA580C",
        ancestor_only = "#64748B",
        displayed = "#0F766E",
        hidden = "#CBD5E1",
        other_component = "#E2E8F0"
      )
      present <- names(legend.colors)[names(legend.colors) %in% present]
      if (any(data$selected, na.rm = TRUE)) {
        legend.colors <- c(selected = "#DC2626", legend.colors)
        present <- c("selected", present)
      }
      graphics::legend(
        "topright",
        inset = c(-0.27, 0),
        legend = gsub("_", " ", present, fixed = TRUE),
        col = unname(legend.colors[present]),
        pch = point_glyph,
        xpd = NA,
        bty = "n",
        cex = 0.75
      )
    }
  } else {
    matrix.data <- data[, features, drop = FALSE]
    names(matrix.data) <- vapply(features, function(feature) {
      gflowui_basin_plot_axis_label(feature, scales[[feature]])
    }, character(1))
    diagonal <- function(x, ...) {
      usr <- graphics::par("usr")
      on.exit(graphics::par(usr = usr))
      geometry <- gflowui_basin_histogram_geometry(x, bins = bins)
      graphics::par(usr = c(usr[1:2], geometry$y_limits))
      graphics::rect(
        geometry$left,
        0,
        geometry$right,
        geometry$height,
        col = histogram_color,
        border = "#FFFFFF"
      )
    }
    panel <- function(x, y, ...) {
      graphics::points(
        x,
        y,
        pch = point_glyph,
        cex = point_size,
        col = point_colors
      )
    }
    graphics::pairs(
      matrix.data,
      lower.panel = panel,
      upper.panel = panel,
      diag.panel = diagonal,
      main = sprintf("%s; n=%d", gflowui_basin_plot_title(spec), nrow(data))
    )
  }
  invisible(data)
}
