gflowui_basin_plot_feature_choices <- function() {
  c(
    "Support" = "support",
    "Mass" = "mass",
    "Extremum value" = "extremum_value",
    "Prominence" = "prominence"
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
  if (identical(as.character(spec$kind), "histogram")) {
    scales[[features[[1L]]]] <- x_scale
  } else if (identical(as.character(spec$kind), "scatter")) {
    scales[[features[[1L]]]] <- x_scale
    scales[[features[[2L]]]] <- y_scale
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
    scope = c("all", "listed", "selected"),
    type = c("both", "max", "min"),
    selected_keys = character()) {
  scope <- match.arg(scope)
  type <- match.arg(type)
  table <- if (identical(scope, "listed")) {
    result$table
  } else {
    result$all_table
  }
  if (!is.data.frame(table) || nrow(table) < 1L) {
    return(data.frame(
      key = character(),
      type = character(),
      label = character(),
      support = integer(),
      mass = numeric(),
      extremum_value = numeric(),
      prominence = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  if (identical(scope, "selected")) {
    table <- table[
      as.character(table$key) %in% as.character(selected_keys),
      ,
      drop = FALSE
    ]
  }
  if (type %in% c("max", "min")) {
    table <- table[as.character(table$type) == type, , drop = FALSE]
  }
  data.frame(
    key = as.character(table$key),
    type = as.character(table$type),
    label = as.character(table$display.label),
    support = suppressWarnings(as.integer(table$primary.support.size)),
    mass = suppressWarnings(as.numeric(table$primary.support.mass)),
    extremum_value = suppressWarnings(as.numeric(table$extremum.value)),
    prominence = suppressWarnings(as.numeric(table$prominence)),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_new_plot_specs <- function(
    features,
    mode = c("histograms", "pairs", "matrix"),
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

gflowui_basin_plot_title <- function(spec) {
  labels <- vapply(
    spec$features,
    gflowui_basin_plot_feature_label,
    character(1)
  )
  switch(
    as.character(spec$kind),
    histogram = sprintf("%s distribution", labels[[1L]]),
    scatter = paste(labels, collapse = " \u00d7 "),
    matrix = sprintf("Basin metric matrix (%s)", paste(labels, collapse = ", ")),
    "Basin plot"
  )
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

gflowui_draw_basin_plot <- function(
    data,
    spec,
    bins = 20L,
    histogram_color = "#2563EB",
    point_color = "type",
    point_glyph = 19L,
    point_size = 1.1,
    point_opacity = 0.75,
    x_scale = "raw",
    y_scale = "raw") {
  features <- as.character(spec$features)
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
  } else {
    rep(as.character(point_color), nrow(data))
  }
  point_colors <- grDevices::adjustcolor(
    point_colors,
    alpha.f = point_opacity
  )

  if (identical(as.character(spec$kind), "histogram")) {
    feature <- features[[1L]]
    graphics::hist(
      data[[feature]],
      breaks = bins,
      col = histogram_color,
      border = "#FFFFFF",
      main = sprintf(
        "%s (n=%d)",
        gflowui_basin_plot_title(spec),
        nrow(data)
      ),
      xlab = gflowui_basin_plot_axis_label(feature, scales[[feature]])
    )
  } else if (identical(as.character(spec$kind), "scatter")) {
    x.feature <- features[[1L]]
    y.feature <- features[[2L]]
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
    if (identical(as.character(point_color), "type") &&
        length(unique(data$type)) > 1L) {
      graphics::legend(
        "topright",
        legend = c("Maximum", "Minimum"),
        col = c("#111827", "#06B6D4"),
        pch = point_glyph,
        bty = "n",
        cex = 0.8
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
      h <- graphics::hist(x, breaks = bins, plot = FALSE)
      y <- h$counts
      if (max(y) > 0) y <- y / max(y)
      graphics::rect(
        h$breaks[-length(h$breaks)],
        0,
        h$breaks[-1L],
        y,
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
      upper.panel = NULL,
      diag.panel = diagonal,
      main = sprintf("%s; n=%d", gflowui_basin_plot_title(spec), nrow(data))
    )
  }
  invisible(data)
}
