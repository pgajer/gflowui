test_that("general chart centers its anchor and satisfies its tangent equation", {
  a<-c(.6,.3,.1);X<-rbind(a,c(.2,.4,.4),c(.1,.2,.7));rownames(X)<-c("anchor","b","c")
  x<-gflowui_atlas_chart(X,a)
  expect_equal(unname(x$coords[1,]),rep(0,3),tolerance=1e-14)
  expect_lt(max(abs(x$coords%*%x$q)),1e-14)
  expect_equal(gflowui_atlas_chart(X,5*a)$coords,x$coords)
  expect_true(any(x$coords<0));expect_true(x$usable)
})

test_that("pure-feature chart is exactly the familiar nonreference ratios", {
  X<-rbind(c(.8,.1,.1),c(.4,.3,.3));rownames(X)<-c("a","b")
  x<-gflowui_atlas_chart(X,c(1,0,0))
  expect_equal(unname(x$coords[,1]),c(0,0))
  expect_equal(unname(x$coords[,2:3]),unname(X[,2:3]/X[,1]))
})

test_that("coverage is explicit and frozen membership is never silently edited", {
  X<-rbind(c(1,0),c(0,1),c(1e-8,1-1e-8),c(.5,.5));rownames(X)<-letters[1:4]
  x<-gflowui_atlas_chart(X,c(1,0),threshold=1e-6)
  expect_false(x$usable);expect_identical(x$excluded_ids,c("b","c"))
  expect_true(gflowui_atlas_chart(X,c(1,0),policy="exclude")$usable)
  data<-list(X=X,anchor=list(vertex_id="a",abundances=c(1,0)))
  p<-gflowui_atlas_parameters(list(coordinates="anchor_chart",chart_anchor="a",metric="euclidean"),4)
  expect_error(gflowui_atlas_prepare_coordinates(data,p),"coverage failed")
  p$chart_policy<-"exclude";expect_error(gflowui_atlas_prepare_coordinates(data,p),"fewer than four")
  expect_identical(rownames(data$X),letters[1:4])
  expect_error(gflowui_atlas_parameters(list(coordinates="anchor_chart",chart_anchor="a",metric="jensen_shannon"),4),"Euclidean")
})

test_that("chart worker fits Euclidean geometry and records excluded stable IDs", {
  set.seed(4);X<-matrix(runif(30),10,3);X<-X/rowSums(X);X[1,]<-c(0,.5,.5);rownames(X)<-paste0("id",1:10);colnames(X)<-letters[1:3]
  data<-list(X=X,anchor=list(vertex_id="external-anchor",abundances=c(1,0,0)),region=list(id="r",label="chart"))
  p<-gflowui_atlas_parameters(list(method="pca",coordinates="anchor_chart",metric="euclidean",chart_anchor="external-anchor",chart_policy="exclude"),10)
  folder<-tempfile();dir.create(folder);on.exit(unlink(folder,recursive=TRUE));saveRDS(data,file.path(folder,"input.rds"))
  spec<-list(input_file=file.path(folder,"input.rds"),input_hash=digest::digest(data,algo="sha256"),parameters=p,key="chart",region_id="r",namespace="dataset")
  r<-gflowui_atlas_compute(spec,folder)
  expect_identical(r$graph_set$atlas$excluded_ids,"id1")
  expect_identical(rownames(readRDS(file.path(folder,"layout.rds"))),rownames(X)[-1])
  expect_true(all(c("chart_coverage.csv","chart.rds")%in%names(r$checksums)))
})

test_that("calculation and coverage controls render as a valid Shiny UI", {
  html<-htmltools::renderTags(gflowui_atlas_calculation_ui("chart-test"))$html
  expect_match(html,"Preview chart coverage")
  expect_match(html,"Stop; keep all members",fixed=TRUE)
})
