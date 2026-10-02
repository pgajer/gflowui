test_that("queue freezes synchronous settings instead of stale numeric inputs", {
  fixture_root<-tempfile();dir.create(fixture_root);on.exit(unlink(fixture_root,recursive=TRUE))
  X<-matrix(seq_len(120),30,4);X<-X/rowSums(X);rownames(X)<-paste0("id",1:30);colnames(X)<-letters[1:4]
  asset<-list(sample_ids=rownames(X),taxon_names=colnames(X),indices=lapply(1:30,function(i)order(-X[i,])),abundances=lapply(1:30,function(i)sort(X[i,],decreasing=TRUE)))
  af<-file.path(fixture_root,"abundances.rds");saveRDS(asset,af)
  m<-list(project_id="fixture",metadata=list(vertex_hover=list(abundances_file=af),local_views=list(vertex_namespace="fixture")))
  r<-gflowui_atlas_region("test",rownames(X),rownames(X),list(type="test"));path<-file.path(fixture_root,"atlas.rds")
  gflowui_atlas_save(setNames(list(r),r$id),path)
  testthat::local_mocked_bindings(gflowui_atlas_dispatch=function(...)NULL,gflowui_atlas_winners=function(...)character())
  p<-gflowui_atlas_parameters(list(mode="landmarks",landmarks=20,iterations=3),30)
  request<-list(parameters=p,context=list(project_id="fixture",region_id=r$id))
  shiny::testServer(gflowui_atlas_calculation_server,args=list(manifest=function()m,region=function()r,path=function()path,publish=function(...)NULL),{
    session$setInputs(landmarks=200,chart_threshold=1,chart_anchor="missing")
    preview<-request;preview$parameters$chart_anchor<-r$vertex_ids[1]
    session$setInputs(coverage_request=preview)
    expect_match(output$coverage_text,"30 of 30 samples covered")
    session$setInputs(request=request)
    f<-list.dirs(file.path(fixture_root,"jobs"),recursive=FALSE)
    expect_length(f,1)
    expect_equal(readRDS(file.path(f,"spec.rds"))$parameters$landmarks,20)
    expect_equal(readRDS(file.path(f,"spec.rds"))$region_id,r$id)
  })
  bad<-request;bad$context$region_id<-"other"
  expect_error(gflowui_atlas_request_parameters(bad,"fixture",r$id),"changed")
  bad<-request;bad$parameters$landmarks<-NULL
  expect_error(gflowui_atlas_request_parameters(bad,"fixture",r$id),"Incomplete")
})

test_that("parent workflow rebuild preserves calculation settings", {
  ui<-gflowui_local_views_ui("atlas",values=list(calculation=list(method="pca",coordinates="anchor_chart",
    chart_anchor="retained-id",chart_threshold=.02,chart_policy="exclude",metric="euclidean",inner="ambient",
    power=1.5,k=7,mode="landmarks",landmarks=20,iterations=75,seed=9,memory_mb=512)))
  html<-htmltools::renderTags(ui)$html
  expect_match(html,'value="pca" selected')
  expect_match(html,'value="anchor_chart" selected')
  expect_match(html,'value="retained-id"')
  expect_match(html,'id="atlas-compute-landmarks"[^>]+value="20"')
  expect_match(html,'id="atlas-compute-chart_threshold"[^>]+value="0.02"')
  expect_match(html,'value="exclude" selected')
  expect_match(html,'id="atlas-compute-iterations"[^>]+value="75"')
})
