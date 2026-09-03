gflowui_require_ivue <- function() {
  if (!requireNamespace("ivue", quietly = TRUE) ||
      utils::packageVersion("ivue") < "0.0.0.9001") {
    stop("Install ivue >= 0.0.0.9001 for 3D widget rendering.", call. = FALSE)
  }
}

gflowui_ivue_layers <- function(specs) {
  gflowui_require_ivue()
  lapply(specs, function(spec) {
    if (is.function(spec)) return(ivue::layer3D.callback(spec))
    if (!is.list(spec) || !is.function(spec$fun)) {
      stop("Invalid application overlay specification.", call. = FALSE)
    }
    args <- spec$args %||% list()
    if (is.null(spec$with_ctx) || isTRUE(spec$with_ctx)) {
      ivue::layer3D.callback(spec$fun, args)
    } else {
      ivue::layer3D.callback(function(ctx) do.call(spec$fun, args))
    }
  })
}

gflowui_ivue_background_alpha <- function(subject_overlay) {
  if (!isTRUE(subject_overlay$dim_background) ||
      length(subject_overlay$vertices %||% integer()) == 0L) return(1)
  alpha <- suppressWarnings(as.numeric(subject_overlay$background_opacity %||% 0.22))
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0) alpha <- 0.22
  min(1, max(0.05, alpha))
}

gflowui_ivue_numeric <- function(values, src, density_settings = list(), alpha = 1) {
  gflowui_require_ivue()
  values <- suppressWarnings(as.numeric(values))
  values[!is.finite(values)] <- NA_real_
  transform <- src$color_transform %||% "identity"
  encoding <- gflowui_numeric_color_encoding(values, transform = transform,
    title = src$colorbar_title %||% src$label %||% "Value")
  mapped <- encoding$mapped_values
  if (identical(transform, "density_asinh")) {
    palette <- gflowui_density_palette(
      low = density_settings$low %||% "yellow",
      midpoint = density_settings$midpoint %||% "none",
      high = density_settings$high %||% "red",
      low_alpha = density_settings$low_alpha %||% 0.2,
      midpoint_alpha = density_settings$midpoint_alpha %||% 1,
      high_alpha = density_settings$high_alpha %||% 1)
    palette <- grDevices::adjustcolor(palette, alpha.f = alpha)
    scale <- ivue::color.scale.cont(mapped, limits = encoding$color_limits,
      palette = palette, na.color = "#9ca3af")
  } else {
    scale <- ivue::color.scale.cont(mapped, mode = "binned", winsor.p = 0.01,
      digits = 2, palette = function(n) grDevices::adjustcolor(
        grDevices::rainbow(n, start = 1/6, end = 0), alpha.f = alpha))
  }
  mapping <- ivue::map.colors(mapped, scale)
  legend <- mapping$legend
  if (identical(transform, "density_asinh") && length(encoding$colorbar$tickvals)) {
    legend <- data.frame(label = encoding$colorbar$ticktext,
      color = ivue::map.colors(encoding$colorbar$tickvals, scale)$colors,
      count = NA_integer_)
    if (anyNA(mapped)) legend <- rbind(legend, data.frame(
      label = "Missing", color = scale$na.color, count = sum(is.na(mapped))))
  }
  list(values = mapped, scale = scale, mapping = mapping, legend = legend)
}

gflowui_ivue_legend_labels <- function(legend) {
  ifelse(is.na(legend$count), legend$label,
         sprintf("%s (%s)", legend$label, format(legend$count, big.mark = ",", trim = TRUE)))
}

gflowui_ivue_css_color <- function(colors) {
  rgba <- grDevices::col2rgb(colors, alpha = TRUE)
  sprintf("rgba(%d,%d,%d,%.4f)", rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ] / 255)
}
