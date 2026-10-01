endpoint_test_state <- function(v = 1:2) list(rows = data.frame(vertex = v,
  label = paste0("tip", v), accepted = TRUE, visible = TRUE,
  notes = "source detector scores"))

test_that("endpoint sets follow sample IDs across reordering and preserve absent samples", {
  scope <- gflowui_endpoint_scope(list(id="a", endpoint_scope_id="same"), 1L, "p")
  set <- gflowui_endpoint_set_new("tips", endpoint_test_state(), c("a","b","c"), scope,
    list(embedding="original"))
  projected <- gflowui_endpoint_set_project(set, c("c","b"))
  expect_identical(projected$rows$vertex, 2L)
  expect_identical(projected$rows$label, "tip2")
  expect_identical(projected$shared_missing, 1L)
  projected$rows$label <- "edited"
  saved <- gflowui_endpoint_set_update(set, projected, c("c","b"))
  expect_setequal(saved$state$rows$vertex_id, c("a","b"))
  restored <- gflowui_endpoint_set_project(saved, c("b","a","c"))
  expect_identical(restored$rows$label[match(1:2, restored$rows$vertex)], c("edited","tip1"))
  expect_identical(saved$provenance$embedding, "original")
  expect_true(all(saved$state$rows$notes == "source detector scores"))
  expect_error(gflowui_endpoint_set_update(saved, projected, c("c","b")), "another session")
  projected <- gflowui_endpoint_set_project(saved, c("b","c"))
  projected$rows <- projected$rows[0,]
  saved <- gflowui_endpoint_set_update(saved, projected, c("b","c"))
  expect_identical(saved$state$rows$vertex_id, "a")
  expect_error(gflowui_endpoint_set_project(set, c("a","a")), "unique stable")
})

test_that("sharing is explicit and keeps distinct k values separate", {
  a <- list(id="a", endpoint_scope_id="graph", endpoint_vertex_namespace="samples")
  b <- a; b$id <- "b"
  expect_identical(gflowui_endpoint_scope(a, 1L, "p"), gflowui_endpoint_scope(b, 1L, "p"))
  expect_false(identical(gflowui_endpoint_scope(a, 1L, "p")$key, gflowui_endpoint_scope(b, 2L, "p")$key))
  a$endpoint_scope_id <- b$endpoint_scope_id <- NULL
  expect_false(identical(gflowui_endpoint_scope(a, 1L, "p")$key, gflowui_endpoint_scope(b, 1L, "p")$key))
})

test_that("migration retains separate tables and snapshots without duplicate imports", {
  root <- withr::local_tempdir(); path <- file.path(root, "sets.rds")
  scope <- gflowui_endpoint_scope(list(id="a"), 1L, "p")
  entry <- list(key="original/current.rds", ids=c("a","b"), scope=scope,
    name="original", state=endpoint_test_state(), provenance=list(embedding="original"))
  snapshot <- entry; snapshot$key <- "original/snapshot.rds"; snapshot$name <- "snapshot"
  store <- gflowui_endpoint_sets_migrate(gflowui_endpoint_store_read(path), list(entry,snapshot))
  expect_length(store$sets, 2)
  expect_identical(gflowui_endpoint_sets_migrate(store, list(entry,snapshot)), store)
  gflowui_endpoint_store_write(store,path)
  expect_identical(gflowui_endpoint_store_read(path),store)
  bad <- entry; bad$key <- "bad"; bad$ids <- NULL
  expect_identical(gflowui_endpoint_sets_migrate(store,list(bad)),store)
})

test_that("the endpoint editor shares edits across routes using stable IDs", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
  sets <- lapply(1:3, function(j) {
    ids <- if (j==2) c("c","b","a") else c("a","b","c")
    file <- file.path(root,paste0(j,".rds"))
    saveRDS(list(X.graphs=list(list(adj_list=list(2L,c(1L,3L),2L),
      weight_list=list(1,c(1,1),1),vertex_ids=ids)),k.values=1L,vertex_ids=ids),file)
    list(id=paste0("route",j),label=paste("Embedding",j),graph_file=file,k_values=1L,
      endpoint_scope_id=if(j<3)"same_graph" else "other_graph",endpoint_vertex_namespace="samples")
  })
  register_project(root,"shared",profile="custom",graph_sets=sets,
    defaults=list(graph_set_id="route1",reference_graph_set_id="route1",reference_k=1L),scan_results=FALSE)
  shiny::testServer(app_server, {
    open_project("shared"); session$flushReact()
    ctx <- current_endpoint_graph_context()
    working <- empty_working_endpoint_state(ctx)
    working <- upsert_working_endpoint_vertex_state(working,1L,label="A tip")
    save_working_endpoint_state(working,ctx); session$flushReact()
    first <- shared_endpoint_sets$state()$set
    expect_identical(first$state$rows$vertex_id,"a")
    session$setInputs(graph_data_type="route2")
    session$flushReact()
    ctx <- current_endpoint_graph_context()
    expect_identical(ctx$graph_set_id,"route2")
    working <- load_working_endpoint_state(ctx)
    expect_identical(working$rows$vertex,3L)
    expect_identical(working$rows$label,"A tip")
    working$rows$label <- "Shared edit"
    save_working_endpoint_state(working,ctx); session$flushReact()
    expect_identical(shared_endpoint_sets$state()$set$id,first$id)
    expect_identical(shared_endpoint_sets$state()$set$provenance$graph_set_id,"route1")
    # Named sets can be duplicated, renamed and selected without changing the original.
    session$setInputs(`shared_endpoint_sets-duplicate`=1)
    session$setInputs(`shared_endpoint_sets-name`="Alternative", `shared_endpoint_sets-confirm_name`=1)
    copy <- shared_endpoint_sets$state()$set
    expect_false(identical(copy$id,first$id))
    expect_identical(copy$name,"Alternative")
    session$setInputs(`shared_endpoint_sets-rename`=1)
    session$setInputs(`shared_endpoint_sets-name`="Renamed", `shared_endpoint_sets-confirm_name`=2)
    expect_identical(shared_endpoint_sets$state()$set$name,"Renamed")
    session$setInputs(`shared_endpoint_sets-set`=first$id)
    expect_identical(shared_endpoint_sets$state()$set$id,first$id)
    session$setInputs(graph_data_type="route3")
    expect_null(shared_endpoint_sets$state()$set)
    session$setInputs(`shared_endpoint_sets-browse`=1)
    session$setInputs(`shared_endpoint_sets-other_set`=first$id, `shared_endpoint_sets-show_other`=1)
    expect_identical(shared_endpoint_sets$overlay()$vertex,1L)
    expect_identical(shared_endpoint_sets$overlay()$label,"Shared edit")
    expect_null(shared_endpoint_sets$state()$set)
    session$setInputs(`shared_endpoint_sets-browse`=2)
    session$setInputs(`shared_endpoint_sets-copy_other`=1)
    expect_identical(shared_endpoint_sets$state()$set$copied_from,first$id)
    expect_false(identical(shared_endpoint_sets$state()$set$scope,first$scope))
    session$setInputs(`shared_endpoint_sets-new`=1)
    session$setInputs(`shared_endpoint_sets-name`="Empty", `shared_endpoint_sets-confirm_name`=3)
    expect_equal(nrow(shared_endpoint_sets$state()$set$state$rows),0)
    session$setInputs(graph_data_type="route1")
    expect_identical(shared_endpoint_sets$state()$set$id,first$id)

  })
})


test_that("legacy files from multiple routes migrate separately and remain untouched", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
  file <- file.path(root,"graph.rds")
  saveRDS(list(X.graphs=list(list(adj_list=list(2L,c(1L,3L),2L),
    weight_list=list(1,c(1,1),1))), k.values=1L,vertex_ids=c("a","b","c")),file)
  sets <- lapply(c("a","b"),function(id)list(id=id,label=id,endpoint_scope_id="same",
    graph_file=file,k_values=1L))
  register_project(root,"migration",profile="custom",graph_sets=sets,
    defaults=list(graph_set_id="a",reference_graph_set_id="a",reference_k=1L),scan_results=FALSE)
  files <- character()
  for (id in c("a","b")) {
    folder <- file.path(gflowui_projects_data_dir(),"projects","migration","endpoint_state",
      paste0("graph_set=",id),"working")
    dir.create(file.path(folder,"snapshots"),recursive=TRUE)
    current <- file.path(folder,"current.rds")
    snapshot <- file.path(folder,"snapshots","one.rds")
    saveRDS(c(endpoint_test_state(1L),list(k=1L)),current)
    saveRDS(list(vertices=2L,labels=paste(id,"snapshot"),k=1L),snapshot)
    files <- c(files,current,snapshot)
  }
  before <- tools::md5sum(files)
  shiny::testServer(app_server, {
    open_project("migration"); session$flushReact()
    store <- shared_endpoint_sets$state()$store
    expect_length(store$sets,4L)
    expect_setequal(vapply(store$sets,function(x)x$state$rows$vertex_id,""),c("a","b"))
    expect_identical(tools::md5sum(files),before)
    session$setInputs(graph_data_type="b")
    expect_length(shared_endpoint_sets$state()$store$sets,4L)
  })
})
