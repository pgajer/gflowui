project_order_fixture <- function() {
  data.frame(id=c("a","b","c"),label=c("Alpha","Beta","Gamma"),stringsAsFactors=FALSE)
}

test_that("manual order survives registry refreshes and project renames", {
  withr::local_options(gflowui.projects_data_dir=tempfile("project-order-test-"))
  reg <- project_order_fixture()
  gflowui:::gflowui_save_registry(reg)
  registry_hash <- digest::digest(file=gflowui:::gflowui_registry_path())
  gflowui:::gflowui_save_project_order(c("c","a","b"), reg$id)
  expect_identical(gflowui:::gflowui_load_registry()$id,c("c","a","b"))
  expect_identical(digest::digest(file=gflowui:::gflowui_registry_path()), registry_hash)
  reg$label[[3]] <- "Renamed Gamma"
  gflowui:::gflowui_save_registry(reg)
  expect_identical(gflowui:::gflowui_load_registry()$id,c("c","a","b"))
  expect_identical(gflowui:::gflowui_load_registry()$label[[1]],"Renamed Gamma")
  # A refresh can write rows in a different order without changing the preference.
  gflowui:::gflowui_save_registry(reg[c(2,3,1),])
  expect_identical(gflowui:::gflowui_load_registry()$id,c("c","a","b"))
})

test_that("saving an older dialog reconciles newly added and deleted projects", {
  withr::local_options(gflowui.projects_data_dir=tempfile("project-order-test-"))
  reg <- project_order_fixture()
  current <- rbind(reg[reg$id!="b",],data.frame(id="d",label="Delta"))
  gflowui:::gflowui_save_registry(current)
  gflowui:::gflowui_save_project_order(c("c","b","a"),reg$id)
  expect_identical(gflowui:::gflowui_load_registry()$id,c("c","a","d"))
  expect_error(gflowui:::gflowui_save_project_order(c("c","c","a"),reg$id),"Reopen Projects")
  expect_error(gflowui:::gflowui_save_project_order(c("c","b","unknown"),reg$id),"Reopen Projects")
  expect_identical(gflowui:::gflowui_read_project_order(),c("c","a","d"))
})

test_that("missing, damaged and empty preferences preserve usable project choices", {
  withr::local_options(gflowui.projects_data_dir=tempfile("project-order-test-"))
  reg <- project_order_fixture()
  gflowui:::gflowui_save_registry(reg)
  expect_identical(gflowui:::gflowui_load_registry()$id,reg$id)
  saveRDS(list(bad=TRUE),gflowui:::gflowui_project_order_path())
  expect_identical(gflowui:::gflowui_load_registry()$id,reg$id)
  gflowui:::gflowui_save_registry(gflowui:::gflowui_default_registry())
  gflowui:::gflowui_save_project_order(character(),character())
  expect_equal(nrow(gflowui:::gflowui_load_registry()),0)
})

test_that("manager drafts persist only on save and stale dialog events are ignored", {
  withr::local_options(gflowui.projects_data_dir=tempfile("project-order-test-"))
  reg <- project_order_fixture()
  gflowui:::gflowui_save_registry(reg)
  server <- function(input,output,session) {
    opened <- shiny::reactiveVal("")
    revision <- gflowui:::gflowui_project_manager_server(input,session,
      on_open=function(id){opened(id);TRUE},active_id=function() "")
  }
  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(project_manager=1)
    expect_identical(gflowui:::gflowui_load_registry()$id,reg$id)
    session$setInputs(project_manager_action=list(action="save",token="old",ids=c("c","a","b")))
    expect_equal(revision(),0L)
    session$setInputs(project_manager_action=list(action="save",token="1",ids=c("c","a","b")))
    expect_equal(revision(),1L)
    expect_identical(gflowui:::gflowui_load_registry()$id,c("c","a","b"))
    session$setInputs(project_manager=2)
    session$setInputs(project_manager_action=list(action="open",token="2",id="a",ids=c("b","a","c")))
    expect_identical(opened(),"a")
    expect_identical(gflowui:::gflowui_read_project_order(),c("c","a","b"))
  })
})
