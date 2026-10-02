regional_test_state <- function(v=1:2) list(rows=data.frame(vertex=as.integer(v),
  label=paste0("tip",v),accepted=TRUE,visible=TRUE))

test_that("regional inheritance uses saved membership and never changes its parent", {
  ds <- gflowui_endpoint_scope(list(),1L,"samples")
  global <- gflowui_endpoint_set_new("Global",regional_test_state(),letters[1:4],ds,list())
  global$display <- list(label_size=1.4,label_offset="2x",marker_size="1x",marker_color="#ef4444")
  store <- list(version=2L,sets=setNames(list(global),global$id),
    active=setNames(list(global$id),ds$key))
  r <- gflowui_atlas_region("Li / 3 samples",c("b","c","d"),letters[1:4],list())
  rs <- setNames(list(r),r$id)
  scoped <- gflowui_endpoint_region_ensure(store,ds,r,rs,regional_test_state(integer()))
  key <- gflowui_endpoint_region_scope(ds,r)$key
  child <- scoped$sets[[scoped$active[[key]]]]
  expect_identical(scoped$sets[[global$id]],global)
  expect_identical(child$state$rows$vertex_id,"b")
  expect_identical(child$display,global$display)
  expect_match(child$name,"Li / 3 samples.*revision 1")
  expect_identical(child$inherited_from$set_id,global$id)
  # Inheritance runs once, regardless of subsequent parent edits.
  scoped$sets[[global$id]]$state$rows$label <- "later edit"
  expect_identical(gflowui_endpoint_region_ensure(scoped,ds,r,rs,regional_test_state(integer())),scoped)
  edit <- gflowui_endpoint_set_project(child,c("d","b","c"))
  edit$rows <- rbind(edit$rows,data.frame(vertex=1L,label="new",accepted=TRUE,visible=TRUE))
  child <- gflowui_endpoint_set_update(child,edit,c("d","b","c"))
  expect_setequal(child$state$rows$vertex_id,c("b","d"))
  partial <- gflowui_endpoint_set_project(child,c("c","b"))
  partial$rows$label <- "renamed"
  child <- gflowui_endpoint_set_update(child,partial,c("c","b"))
  expect_setequal(child$state$rows$vertex_id,c("b","d"))
  bad <- gflowui_endpoint_set_project(child,letters[1:4])
  bad$rows$vertex[1] <- 1L
  expect_error(gflowui_endpoint_set_update(child,bad,letters[1:4]),"saved membership")
  scoped$sets[[child$id]] <- child
  # A new membership revision inherits the selected previous regional set.
  r2 <- gflowui_atlas_region("Li / 2 samples",c("c","d"),letters[1:4],list())
  r2$parent_id <- r$id; r2$revision <- 2L; rs[[r2$id]] <- r2
  updated <- gflowui_endpoint_region_ensure(scoped,ds,r2,rs,regional_test_state(integer()))
  next_key <- gflowui_endpoint_region_scope(ds,r2)$key
  next_set <- updated$sets[[updated$active[[next_key]]]]
  expect_identical(next_set$state$rows$vertex_id,"d")
  expect_identical(next_set$inherited_from$set_id,child$id)
  expect_false(identical(key,next_key))
  expect_identical(updated$sets[[child$id]],child)
})

test_that("regional sets autosave across graphs, retain alternatives, and isolate edits", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
  make_graph <- function(id,ids) {
    f <- file.path(root,paste0(id,".rds"))
    saveRDS(list(X.graphs=list(list(adj_list=list(2L,c(1L,3L),2L),
      weight_list=list(1,c(1,1),1))),k.values=1L,vertex_ids=ids),f)
    list(id=id,label=id,graph_file=f,k_values=1L,endpoint_vertex_namespace="samples")
  }
  parent <- make_graph("parent",letters[1:3])
  local <- make_graph("local",c("c","b","a"))
  register_project(root,"regional",profile="custom",graph_sets=list(parent),
    defaults=list(graph_set_id="parent",reference_graph_set_id="parent",reference_k=1L),
    metadata=list(local_views=list(enabled=TRUE)),scan_results=FALSE)
  r <- gflowui_atlas_region("Region AB",c("a","b"),letters[1:3],list())
  r$views <- list(local)
  f <- file.path(gflowui_projects_data_dir(),"projects","regional","local_views","atlas.rds")
  gflowui_atlas_save(setNames(list(r),r$id),f)
  shiny::testServer(app_server, {
    open_project("regional"); session$flushReact()
    ctx <- current_endpoint_graph_context()
    w <- empty_working_endpoint_state(ctx)
    w <- upsert_working_endpoint_vertex_state(w,1L,label="Parent A")
    w <- upsert_working_endpoint_vertex_state(w,3L,label="Parent C")
    expect_true(save_working_endpoint_state(w,ctx)); session$flushReact()
    global <- shared_endpoint_sets$state()$set
    session$setInputs(`local_atlas-region`=r$id)
    child <- shared_endpoint_sets$state()$set
    expect_false(identical(child$id,global$id))
    expect_identical(child$state$rows$vertex_id,"a")
    expect_match(child$name,"Region AB")
    expect_identical(shared_endpoint_sets$state()$store$sets[[global$id]]$state,global$state)
    w <- load_working_endpoint_state(ctx)
    w <- upsert_working_endpoint_vertex_state(w,2L,label="Regional B")
    expect_true(save_working_endpoint_state(w,ctx)); session$flushReact()
    expect_false(shared_endpoint_sets$state()$set$state$is_modified)
    session$setInputs(`local_atlas-view`="local")
    ctx <- current_endpoint_graph_context()
    w <- load_working_endpoint_state(ctx)
    expect_equal(w$rows$vertex,c(3L,2L))
    expect_equal(w$rows$label,c("Parent A","Regional B"))
    expect_identical(shared_endpoint_sets$state()$set$id,child$id)
    w$rows$label[2] <- "Regional edit"
    expect_true(save_working_endpoint_state(w,ctx)); session$flushReact()
    # A regional snapshot stays regional and does not create legacy files that
    # could be migrated back into a global alternative.
    before <- shared_endpoint_sets$state()$store
    snapshot <- save_working_endpoint_snapshot(); session$flushReact()
    expect_true(snapshot$ok)
    after <- shared_endpoint_sets$state()$store
    expect_length(after$sets,length(before$sets)+1L)
    expect_identical(after$sets[[snapshot$dataset_id]]$scope,child$scope)
    expect_identical(after$sets[[global$id]],before$sets[[global$id]])
    expect_identical(shared_endpoint_sets$state()$set$id,child$id)
    expect_length(list.files(file.path(root,"registry","projects","regional","endpoint_state"),recursive=TRUE),0)
    # Named alternatives remember the selected set independently for each scope.
    session$setInputs(`shared_endpoint_sets-duplicate`=1)
    session$setInputs(`shared_endpoint_sets-name`="Alternate regional set",`shared_endpoint_sets-confirm_name`=1)
    copy <- shared_endpoint_sets$state()$set
    expect_false(identical(copy$id,child$id))
    session$setInputs(`local_atlas-whole`=1)
    expect_identical(shared_endpoint_sets$state()$set$id,global$id)
    expect_identical(shared_endpoint_sets$state()$set$state$rows$vertex_id,c("a","c"))
    session$setInputs(`local_atlas-region`=r$id)
    expect_identical(shared_endpoint_sets$state()$set$id,copy$id)
    session$setInputs(`shared_endpoint_sets-set`=child$id)
    expect_identical(shared_endpoint_sets$state()$set$state$rows$label,c("Parent A","Regional edit"))
    # Explicit imports only add missing in-region rows and keep existing labels.
    session$setInputs(`shared_endpoint_sets-new`=1)
    session$setInputs(`shared_endpoint_sets-name`="Imported",`shared_endpoint_sets-confirm_name`=2)
    session$setInputs(`shared_endpoint_sets-import`=1)
    session$setInputs(`shared_endpoint_sets-import_source`=global$id,`shared_endpoint_sets-confirm_import`=1)
    expect_identical(shared_endpoint_sets$state()$set$state$rows$vertex_id,"a")
    expect_identical(shared_endpoint_sets$state()$store$sets[[global$id]]$state,global$state)
    # Per-set style follows the set when switching region and graph.
    session$setInputs(endpoint_label_size=1,endpoint_label_offset="1x",endpoint_marker_size="1x",endpoint_marker_color="#ef4444")
    session$setInputs(endpoint_label_size=1.7)
    expect_equal(shared_endpoint_sets$display()$label_size,1.7)
    session$setInputs(`local_atlas-whole`=2)
    expect_equal(shared_endpoint_sets$display()$label_size,1)
    session$setInputs(endpoint_label_size=1)
    session$setInputs(`local_atlas-region`=r$id)
    expect_equal(shared_endpoint_sets$display()$label_size,1.7)
    session$setInputs(endpoint_label_size=1.7)
    session$setInputs(`local_atlas-view`="local")
    expect_equal(shared_endpoint_sets$display()$label_size,1.7)
  })
})

test_that("endpoint annotations use one pixel font size at every depth and escape labels", {
  xyz <- rbind(c(0,0,-100),c(1,1,100))
  a <- gflowui_endpoint_annotations(xyz,c("Li","Li & Gardnerella <example>"),1.4)
  expect_equal(vapply(a,function(x)x$font$size,0),c(16.8,16.8))
  expect_identical(a[[2]]$text,"Li &amp; Gardnerella &lt;example&gt;")
  expect_equal(vapply(a,`[[`,0,"z"),c(-100,100))
  expect_length(gflowui_endpoint_annotations(matrix(numeric(),0,3),character(),1),0)
})

test_that("unsaved regional previews cannot write into dataset endpoint sets", {
  root <- withr::local_tempdir()
  draft <- gflowui_atlas_region("Draft",c("a","b"),letters[1:3],list())
  ctx <- list(project_id="p",graph_set_id="g",k=1L)
  ds <- gflowui_endpoint_scope(list(),1L,"p")
  global <- gflowui_endpoint_set_new("Global",regional_test_state(),letters[1:3],ds,list())
  store <- list(version=2L,sets=setNames(list(global),global$id),active=setNames(list(global$id),ds$key),migrated=character())
  p <- file.path(root,"endpoint_sets","sets.rds")
  gflowui_endpoint_store_write(store,p)
  shiny::testServer(gflowui_endpoint_sets_server,args=list(
    context=function()ctx,manifest=function()list(graph_sets=list(list(id="g",label="Graph"))),
    view=function()list(vertex_ids=letters[1:3]),visible_vertices=function()1:3,
    state_dir=function(...)root,legacy_dir=function(...)file.path(root,"legacy"),
    read_ids=function(...)letters[1:3],empty_state=function(...)regional_test_state(integer()),
    sanitize_state=function(x,...)x,snapshot_state=function(x,...)x,
    legacy_load=function(...)NULL,legacy_save=function(...)stop("unexpected legacy write"),
    changed=function()NULL,region=function()draft,regions=function()list()),{
      session$flushReact()
      expect_true(info()$draft)
      expect_null(state()$set)
      expect_error(save(regional_test_state(),ctx),"Save this region")
      expect_identical(readRDS(p),store)
      session$setInputs(new=1)
      expect_null(dialog())
  })
})
