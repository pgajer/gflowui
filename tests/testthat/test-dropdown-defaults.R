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
