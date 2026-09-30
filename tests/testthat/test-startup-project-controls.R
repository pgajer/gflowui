test_that("initial page includes current projects and selectize dependencies", {
  withr::local_options(list(gflowui.projects_data_dir = tempfile("startup-registry-")))
  ui <- app_ui()

  rendered <- htmltools::renderTags(ui)
  expect_match(rendered$html, 'id="project_select"', fixed = TRUE)
  expect_match(rendered$html, "Choose a project...", fixed = TRUE)
  expect_match(rendered$html, 'disabled="disabled"', fixed = TRUE)
  expect_true("selectize" %in% vapply(rendered$dependencies, `[[`, "", "name"))

  gflowui_save_registry(data.frame(id = "new-project", label = "New & current"))
  rendered <- htmltools::renderTags(ui)
  expect_match(rendered$html, 'value="new-project"', fixed = TRUE)
  expect_match(rendered$html, "New &amp; current", fixed = TRUE)
  expect_length(gregexpr('id="project_select"', rendered$html, fixed = TRUE)[[1]], 1L)
})

test_that("server project controls still refresh and hide for an active project", {
  withr::local_options(list(gflowui.projects_data_dir = tempfile("startup-server-")))
  gflowui_save_registry(data.frame(id = "first", label = "First project"))

  shiny::testServer(app_server, {
    session$flushReact()
    expect_match(output$project_controls$html, 'value="first"', fixed = TRUE)
    expect_match(output$project_controls$html, 'id="project_new"', fixed = TRUE)
    expect_false(grepl('disabled="disabled"', output$project_controls$html, fixed = TRUE))

    project_registry(gflowui_sanitize_registry(data.frame(id = "second", label = "Second project")))
    session$flushReact()
    expect_match(output$project_controls$html, 'value="second"', fixed = TRUE)
    expect_false(grepl('value="first"', output$project_controls$html, fixed = TRUE))

    rv$project.active <- TRUE
    session$flushReact()
    expect_null(output$project_controls)
  })
})
