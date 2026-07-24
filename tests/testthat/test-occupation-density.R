test_that("project specs preserve occupation-density assets", {
  root <- tempfile("gflowui-occupation-spec-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  spec <- gflowui::build_project_spec_iknn_3x3(
    project_root = root,
    graph_sets = list(),
    occupation_density_sets = list(list(
      id = "subject_od",
      subject_ids = c("S1", "S2")
    ))
  )

  expect_equal(spec$occupation_density_sets[[1L]]$id, "subject_od")
  expect_equal(spec$occupation_density_sets[[1L]]$subject_ids, c("S1", "S2"))
})

test_that("selected occupation-density estimates load and normalize", {
  root <- tempfile("gflowui-occupation-fit-")
  dir.create(file.path(root, "fits"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  rho <- array(
    c(1, 2, 1, 4),
    dim = c(2L, 1L, 2L),
    dimnames = list(
      vertex = c("v1", "v2"),
      subject = "S1",
      selector = c("minimum_brier", "minimum_bernoulli_nll")
    )
  )
  saveRDS(
    list(
      subject.id = "S1",
      rho = rho,
      selected = data.frame(
        subject.id = rep("S1", 2L),
        selector = c("minimum_brier", "minimum_bernoulli_nll"),
        graph.k = c(5L, 7L)
      )
    ),
    file.path(root, "fits", "selected.rds")
  )
  manifest <- list(
    project_root = root,
    occupation_density_sets = list(list(
      id = "subject_od",
      methods = list(list(
        id = "graph_heat_kernel",
        selected_fit_file = "fits/selected.rds"
      ))
    ))
  )

  result <- gflowui::gflowui_evaluate_occupation_density(
    manifest = manifest,
    set_id = "subject_od",
    subject_id = "S1",
    method_id = "graph_heat_kernel",
    mode = "selected",
    selector = "minimum_bernoulli_nll"
  )

  expect_equal(result$values, c(0.2, 0.8))
  expect_equal(result$selected$graph.k, 7L)
})
