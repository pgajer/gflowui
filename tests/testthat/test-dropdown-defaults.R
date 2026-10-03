test_that("dropdown defaults are scoped without changing reference or graph inventories", {
  m<-list(project_id="test",defaults=list(reference_graph_set_id="reference",graph_set_id="original"),graph_sets=list(list(id="original")))
  catalog<-list(metric=list(label="Base metric",choices=c("hellinger","euclidean")),power=list(label="Power",choices=c("1","2")))
  e<-list(id="power",value="2",value_label="2",context=list(metric="hellinger"),region="")
  saved<-gflowui_dropdown_default_store(m,e,catalog)
  expect_identical(saved$defaults$reference_graph_set_id,"reference")
  expect_identical(saved$graph_sets,m$graph_sets)
  expect_identical(saved$defaults$graph_set_id,"original")
  e$region<-"local1";local<-gflowui_dropdown_default_store(saved,e,catalog,"local1")
  expect_length(local$defaults$dropdowns,2)
  e$value<-"1";local<-gflowui_dropdown_default_store(local,e,catalog,"local1")
  expect_length(local$defaults$dropdowns,2)
  expect_equal(local$defaults$dropdowns[[gflowui_dropdown_default_key("power","local1",e$context)]]$value,"1")
  expect_error(gflowui_dropdown_default_store(m,e,catalog),"region changed")
  e$region<-"";e$value<-"9"
  expect_error(gflowui_dropdown_default_store(m,e,catalog),"available")
  e$value<-"2";e$context$metric<-"missing"
  expect_error(gflowui_dropdown_default_store(m,e,catalog),"dependency")
})

test_that("context keys and generic dependency declarations are deterministic", {
  expect_identical(gflowui_dropdown_default_key("x",context=list(b="2",a="1")),gflowui_dropdown_default_key("x",context=list(a="1",b="2")))
  fields<-list(list(input_id="base"),list(input_id="inner"),list(input_id="power"))
  config<-gflowui_dropdown_defaults_config(fields)
  expect_identical(config$dependencies$power,c("base","inner"))
  expect_true("local_atlas-compute-job"%in%config$exclude)
  custom<-gflowui_dropdown_defaults_config(fields,list(dependencies=list(power="inner"),exclude="transient"))
  expect_identical(custom$dependencies$power,"inner")
  expect_true("transient"%in%custom$exclude)
})

test_that("default save and reset use the registered project, preserving unrelated defaults", {
  current<-shiny::reactiveVal(list(project_id="test",defaults=list(reference_graph_set_id="ref")))
  server<-function(input,output,session)gflowui_dropdown_defaults_server(input,output,session,current,
    region=function()"",fields=function()list(),save=function(entries){m<-shiny::isolate(current());m$defaults$dropdowns<-entries;current(m)})
  shiny::testServer(server,{
    session$flushReact()
    session$setInputs(dropdown_defaults_catalog=list(project_id="test",controls=list(list(id="size",label="Vertex size",choices=c("0.6x","1x")))))
    session$setInputs(dropdown_default_save=list(project_id="test",entry=list(id="size",value="0.6x",value_label="0.6x",context=list(),region="")))
    expect_length(current()$defaults$dropdowns,1)
    expect_identical(current()$defaults$reference_graph_set_id,"ref")
    session$setInputs(dropdown_defaults_reset_all=1)
    expect_length(current()$defaults$dropdowns,0)
    expect_identical(current()$defaults$reference_graph_set_id,"ref")
  })
})


test_that("empty multi-selection can be a default but invalid scalar values cannot", {
  catalog<-list(groups=list(label="Groups",choices=c("a","b"),multiple=TRUE))
  e<-list(id="groups",value=character(),value_label="No selection",region="",context=list())
  saved<-gflowui_dropdown_default_store(list(),e,catalog)
  expect_length(saved$defaults$dropdowns[[1]]$value,0)
  catalog$groups$multiple<-FALSE
  expect_error(gflowui_dropdown_default_store(list(),e,catalog),"available")
})

test_that("first graph resolution applies the whole saved selector chain", {
  helpers <- gflowui_make_server_graph_structure_helpers(new.env())
  sets <- list(
    list(id="old", metric="euclidean", construction="knn", route="direct", k_values=1L),
    list(id="ambient", metric="euclidean", construction="ambient", route="direct", k_values=1L),
    list(id="refined", metric="euclidean", construction="ambient", route="refined", k_values=1L),
    list(id="other", metric="hellinger", construction="knn", route="direct", k_values=1L))
  fields <- lapply(c("metric","construction","route"),function(x)list(id=x,field=x,label=x))
  m <- list(defaults=list(graph_set_id="old",reference_graph_set_id="old",reference_k=1L),
    metadata=list(graph_selector_schema=list(fields=fields)),graph_sets=sets)
  entries <- list(
    list(id="graph_selector_construction",region="",value="ambient",context=list(graph_selector_metric="euclidean")),
    list(id="graph_selector_route",region="",value="refined",context=list(graph_selector_metric="euclidean",graph_selector_construction="ambient")))
  resolve <- function(...) helpers$resolve_graph_selection(m,sets,...)
  expect_identical(resolve()$set_id,"old")
  initial <- resolve(initial_selector_defaults=entries)
  expect_identical(initial$set_id,"refined")
  # Browser mounting echoes these already-resolved values, with no second graph.
  values <- setNames(lapply(initial$selector_fields,`[[`,"selected"),
    vapply(initial$selector_fields,`[[`,"","input_id"))
  expect_identical(resolve(input_selector_values=values,sticky_set_id=initial$set_id)$set_id,"refined")
  expect_identical(resolve(initial_selector_defaults=entries,selector_region="local")$set_id,"old")
  expect_identical(resolve(initial_selector_defaults=entries,
    input_selector_values=list(graph_selector_metric="hellinger"))$set_id,"other")
  entries[[1]]$value <- "removed"
  expect_identical(resolve(initial_selector_defaults=entries)$set_id,"old")
  # Explicit user intent wins over saved defaults.
  entries[[1]]$value <- "ambient"
  expect_identical(resolve(initial_selector_defaults=entries,
    input_selector_values=list(graph_selector_construction="knn"))$set_id,"old")
  entries[[1]]$region <- "local"
  expect_identical(resolve(initial_selector_defaults=entries,selector_region="local")$set_id,"ambient")
})
