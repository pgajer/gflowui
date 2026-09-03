test_that("ivue overlays and all color families retain point identities", {
  skip_if_not_installed("ivue")
  skip_if_not_installed("rgl")
  X <- matrix(1:18, ncol = 3)
  seen <- NULL
  layers <- gflowui_ivue_layers(list(list(fun = function(ctx, text) {
    seen <<- ctx
    rgl::text3d(ctx$X[1, , drop = FALSE], texts = text)
  }, args = list(text = "overlay"), with_ctx = TRUE)))
  for (type in c("point", "sphere")) {
    encoding <- gflowui_ivue_numeric(1:6, list(label = "Value"), alpha = 0.4)
    w <- ivue::plot3D.cont(X, encoding$values, scale = encoding$scale,
      point.type = type, layers = layers, legend.show = FALSE)
    expect_equal(attr(w, "ivue")$mapping$legend, encoding$legend)
    expect_equal(attr(w, "ivue")$colors, encoding$mapping$colors)
    expect_gt(length(unique(attr(w, "ivue")$colors)), 1)
    expect_equal(seen$row.ids, seq_len(6))
    expect_equal(seen$X, X)
    objects <- Filter(function(x) x$type %in% c("points", "spheres"), attr(w, "ivue")$scene$objects)
    opacity <- unlist(lapply(objects, function(x) x$colors[, 4]), use.names = FALSE)
    expect_equal(opacity, rep(0.4, nrow(X)), tolerance = 1e-7)
  }
  colors <- c(a = "#FF000080", b = "#0000FFFF")
  groups <- rep(c("a", "b"), 3)
  w <- ivue::plot3D.groups(X, groups, scale = ivue::color.scale.groups(groups, colors))
  expect_equal(attr(w, "ivue")$colors, unname(colors[groups]))
  expect_match(gflowui_ivue_css_color(colors[1]), "0.5020")
})

test_that("density color mapping and legend share the same scale", {
  skip_if_not_installed("ivue")
  src <- list(label = "Density", color_transform = "density_asinh")
  encoded <- gflowui_ivue_numeric(c(0, 1e-4, 0.1, 1, NA), src)
  expect_equal(encoded$mapping$colors, ivue::map.colors(encoded$values, encoded$scale)$colors)
  expect_equal(encoded$scale$mode, "continuous")
  expect_true("Missing" %in% encoded$legend$label)
  expect_true(all(grepl("rgba", gflowui_ivue_css_color(encoded$legend$color))))
  expect_equal(gflowui_ivue_background_alpha(list()), 1)
  expect_equal(gflowui_ivue_background_alpha(list(dim_background = TRUE, vertices = 1L, background_opacity = 0.3)), 0.3)
})

test_that("app plotting uses ivue instead of private gflow lookups", {
  body <- paste(deparse(body(app_server)), collapse = "\n")
  expect_match(body, "ivue::plot3D.plain", fixed = TRUE)
  expect_match(body, "ivue::plot3D.cont", fixed = TRUE)
  expect_match(body, "ivue::plot3D.groups", fixed = TRUE)
  expect_false(grepl("resolve_gflow_plot3d_fn", body, fixed = TRUE))
  expect_false(grepl("quantize.for.legend", body, fixed = TRUE))
})

test_that("nearby values retain distinguishable application legend labels", {
  skip_if_not_installed("ivue")
  expect_silent(encoded <- gflowui_ivue_numeric(seq(1, 1.0001, length.out = 20), list(label = "Value")))
  expect_false(anyDuplicated(encoded$legend$label) > 0L)
})
