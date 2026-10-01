trash_fixture <- function() {
  base <- tempfile("trash-test-");dir.create(base);base<-normalizePath(base)
  root<-file.path(base,"research");dir.create(root)
  registry<-file.path(base,"registry");dir.create(registry)
  make <- function(p) {dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines("test asset",p);p}
  list(base=base,root=root,registry=registry,
    unique=make(file.path(root,"output","unique.rds")),
    shared=make(file.path(root,"output","shared.rds")),
    external=make(file.path(base,"external.rds")),
    unrelated=make(file.path(root,"source.R")))
}
trash_register <- function(f) {
  register_project(f$root,project_id="first",project_name="First",scan_results=FALSE,
    artifacts=list(unique_file=f$unique,shared_file=f$shared,external_file=f$external))
  register_project(f$root,project_id="second",project_name="Second",scan_results=FALSE,
    artifacts=list(shared_file=f$shared))
}

test_that("deletion bundles owned assets and retains shared, external and unrelated files", {
  f<-trash_fixture();withr::local_options(list(gflowui.projects_data_dir=f$registry))
  trash_register(f)
  state<-file.path(f$registry,"projects","first","state.rds")
  dir.create(dirname(state),recursive=TRUE);saveRDS(list(working=TRUE),state)
  p<-gflowui_project_delete_plan("first")
  expect_true(all(c(f$unique,state)%in%p$files$path[p$files$action=="Move to Trash"]))
  expect_equal(p$files$action[match(f$shared,p$files$path)],"Used by another registered project")
  expect_equal(p$files$action[match(f$external,p$files$path)],"External referenced asset")
  expect_false(f$unrelated%in%p$files$path)
  destination<-file.path(f$base,"test-trash")
  r<-gflowui_trash_project(p,trash=function(path){stopifnot(file.rename(path,destination));destination})
  expect_equal(gflowui_load_registry()$id,"second")
  expect_false(file.exists(f$unique));expect_false(file.exists(state))
  expect_true(all(file.exists(c(f$shared,f$external,f$unrelated))))
  recovery<-readRDS(file.path(r$trash_path,"recovery.rds"))
  expect_equal(recovery$manifest$project_id,"first")
  expect_true(all(file.exists(file.path(r$trash_path,recovery$mapping$bundle_path))))
})

test_that("failed staging, Trash and registry updates restore original assets", {
  for(failure in c("move","trash","registry")) {
    f<-trash_fixture();withr::local_options(list(gflowui.projects_data_dir=f$registry))
    trash_register(f);p<-gflowui_project_delete_plan("first")
    counter<-0
    mover<-function(a,b){counter<<-counter+1; if(failure=="move" && counter==2L)FALSE else file.rename(a,b)}
    trasher<-function(path){if(failure=="trash")stop("trash failed");dest<-file.path(f$base,"test-trash");stopifnot(file.rename(path,dest));dest}
    saver<-function(x,path){stop("registry failed")}
    expect_error(gflowui_trash_project(p,trash=trasher,move=mover,save_registry=saver))
    expect_true(all(file.exists(p$files$path)))
    expect_equal(gflowui_load_registry()$id,c("first","second"))
  }
})

test_that("changed project plans and unreadable peer manifests stop deletion", {
  f<-trash_fixture();withr::local_options(list(gflowui.projects_data_dir=f$registry))
  trash_register(f);p<-gflowui_project_delete_plan("first")
  writeLines("changed asset",f$unique)
  expect_error(gflowui_trash_project(p),"changed")
  expect_true(file.exists(f$unique))
  writeLines("not RDS",gflowui_manifest_path("second"))
  expect_error(gflowui_project_delete_plan("first"),"cannot be read")
  expect_error(gflowui_project_managed_paths("../other"),"safely")
})

test_that("directory references protect shared descendants and external symlinks", {
  skip_on_os("windows")
  f<-trash_fixture();withr::local_options(list(gflowui.projects_data_dir=f$registry))
  trash_register(f)
  link<-file.path(f$root,"external-link.rds");expect_true(file.symlink(f$external,link))
  register_project(f$root,project_id="first",scan_results=FALSE,overwrite=TRUE,
    artifacts=list(output_dir=dirname(f$unique),link_file=link))
  p<-gflowui_project_delete_plan("first")
  expect_true(f$unique%in%p$files$path[p$files$action=="Move to Trash"])
  expect_false(f$shared%in%p$files$path[p$files$action=="Move to Trash"])
  expect_equal(p$files$action[match(link,p$files$path)],"External referenced asset")
})

test_that("Settings deletion requires review and returns to project selection", {
  f<-trash_fixture();withr::local_options(list(gflowui.projects_data_dir=f$registry,rgl.useNULL=TRUE))
  trash_register(f)
  local_mocked_bindings(gflowui_system_trash=function(path){
    dest<-file.path(f$base,"test-trash");stopifnot(file.rename(path,dest));dest})
  shiny::testServer(app_server,{
    open_project("first");session$flushReact()
    expect_false(grepl('id="delete_project"',output$workspace_actions$html,fixed=TRUE))
    session$setInputs(project_settings=0,delete_project=0,confirm_delete_project=0);session$flushReact()
    session$setInputs(project_settings=1);session$flushReact()
    expect_true(isTRUE(rv$project.active));expect_null(pending_project_delete())
    session$setInputs(delete_project=1);session$flushReact()
    expect_equal(pending_project_delete()$project_id,"first")
    expect_true(file.exists(f$unique));expect_true("first"%in%gflowui_load_registry()$id)
    session$setInputs(confirm_delete_project=1);session$flushReact()
    expect_false(isTRUE(rv$project.active))
    expect_equal(project_registry()$id,"second")
    expect_false(file.exists(f$unique));expect_true(file.exists(f$shared))
  })
})
