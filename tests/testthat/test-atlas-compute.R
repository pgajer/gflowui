atlas_fixture <- function() {
  set.seed(9); X<-matrix(runif(60),10,6);X<-X/rowSums(X)
  rownames(X)<-paste0("id",1:10);colnames(X)<-letters[1:6]
  region<-gflowui_atlas_region("fixture",rownames(X),rownames(X),list(type="test"))
  list(X=X,region=region)
}
atlas_compute_fixture <- function(parameters=list()) {
  data<-atlas_fixture(); folder<-tempfile();dir.create(folder)
  saveRDS(data,file.path(folder,"input.rds"))
  spec<-list(input_file=file.path(folder,"input.rds"),input_hash=digest::digest(data,algo="sha256"),
    parameters=gflowui_atlas_parameters(utils::modifyList(list(iterations=3,mode="full"),parameters),10),
    key="fixture",region_id=data$region$id,namespace="test")
  gflowui_atlas_compute(spec,folder)
  list(data=data,folder=folder,spec=spec,targets=readRDS(file.path(folder,"targets.rds")))
}

test_that("base metrics and complete Fermat targets agree with independent small references", {
  for(metric in c("euclidean","hellinger","jensen_shannon")) {
    x<-atlas_compute_fixture(list(metric=metric,inner="fermat",power=2))
    X<-x$data$X; D<-gflowui_atlas_base_rows(X,1:10,metric);diag(D)<-0
    if(metric=="hellinger")expect_equal(D,unname(as.matrix(dist(sqrt(X)/sqrt(2)))),tolerance=1e-7,ignore_attr=TRUE)
    g<-igraph::make_full_graph(10,directed=FALSE);e<-igraph::as_edgelist(g,names=FALSE);igraph::E(g)$weight<-D[e]^2
    expected<-igraph::distances(g,weights=igraph::E(g)$weight)
    expect_equal(x$targets$distances,expected,tolerance=1e-7,ignore_attr=TRUE)
    expect_identical(x$targets$target_ids,rownames(X))
    unlink(x$folder,recursive=TRUE)
  }
})

test_that("graph repair metadata and landmark targets are real graph paths", {
  x<-atlas_compute_fixture(list(inner="sknn_mst",metric="euclidean",k=1,power=3,mode="landmarks",landmarks=4))
  g<-readRDS(file.path(x$folder,"graph.rds"))$X.graphs$geom_pruned_graphs[[1]]
  dg<-dgraphs::dgraph(g$adj_list,g$weight_list)
  expected<-igraph::distances(gflowui_atlas_igraph(dg,g$vertex_ids),v=x$targets$sources)
  expect_equal(x$targets$distances,expected,ignore_attr=TRUE)
  expect_equal(x$spec$parameters$power,1)
  diag<-readRDS(file.path(x$folder,"diagnostics.rds"))
  expect_equal(nrow(diag$graph$repair_edges),diag$graph$original_components-1)
  expect_equal(dim(readRDS(file.path(x$folder,"layout.rds"))),c(10,3))
  unlink(x$folder,recursive=TRUE)
})

test_that("PCA is centered, budget admission and malformed settings are explicit", {
  x<-atlas_compute_fixture(list(method="pca",metric="euclidean"))
  Z<-readRDS(file.path(x$folder,"layout.rds"))
  expect_equal(colMeans(Z),rep(0,3),tolerance=1e-12,ignore_attr=TRUE)
  expect_null(x$targets$distances)
  expect_error(gflowui_atlas_parameters(list(coordinates="sqrt_abundance",metric="hellinger"),10),"Euclidean")
  expect_error(gflowui_atlas_parameters(list(k=10,inner="sknn_mst"),10),"smaller")
  expect_error(gflowui_atlas_parameters(list(landmarks=2,mode="landmarks"),10),"four")
  unlink(x$folder,recursive=TRUE)
})

test_that("background worker completes and cache verifies frozen inputs and output bytes", {
  skip_if_not_installed("callr")
  data<-atlas_fixture();root<-tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
  asset<-list(sample_ids=rownames(data$X),taxon_names=colnames(data$X),
    indices=lapply(1:10,function(i)order(-data$X[i,])),abundances=lapply(1:10,function(i)sort(data$X[i,],decreasing=TRUE)))
  af<-file.path(root,"abundance.rds");saveRDS(asset,af)
  m<-list(project_id="fixture",metadata=list(vertex_hover=list(abundances_file=af),local_views=list(vertex_namespace="fixture")))
  p<-list(method="pca",metric="euclidean",iterations=2)
  j<-gflowui_atlas_enqueue(m,data$region,p,file.path(root,"jobs"));gflowui_atlas_dispatch(file.path(root,"jobs"))
  proc<-get(j$folder,.gflowui_atlas_processes);on.exit(if(proc$is_alive())proc$kill(),add=TRUE)
  proc$wait(timeout=30000)
  expect_false(proc$is_alive());expect_equal(gflowui_atlas_job_status(j$folder)$state,"complete",info=gflowui_atlas_job_status(j$folder)$stage)
  expect_true(gflowui_atlas_enqueue(m,data$region,p,file.path(root,"jobs"))$cached)
  saveRDS(matrix(0,10,3),file.path(j$folder,"layout.rds"))
  expect_null(gflowui_atlas_job_result(j$folder))
  new<-gflowui_atlas_enqueue(m,data$region,p,file.path(root,"jobs"));expect_false(new$cached)
  gflowui_atlas_cancel(new$folder);expect_equal(gflowui_atlas_job_status(new$folder)$state,"cancelled")
  rm(list=j$folder,envir=.gflowui_atlas_processes)
})

test_that("only the replacement attempt wins successive publication polls", {
  root<-tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
  folders<-file.path(root,c("old","new"));for(f in folders)dir.create(f)
  for(f in folders) {
    saveRDS(list(key="same"),file.path(f,"spec.rds"))
    saveRDS(list(state="complete"),file.path(f,"status.rds"))
    saveRDS(1,file.path(f,"input.rds"))
    saveRDS(list(checksums=c(input.rds=digest::digest(file=file.path(f,"input.rds"),algo="sha256"))),file.path(f,"result.rds"))
  }
  saveRDS(2,file.path(folders[1],"input.rds"))
  expect_identical(gflowui_atlas_winners(folders),folders[2])
  expect_identical(gflowui_atlas_winners(folders),folders[2])
  expect_identical(gflowui_atlas_job_status(folders[1])$state,"superseded")
})

test_that("atlas memory allowance reaches both metric-MDS fitting modes", {
  original <- grip::metric.mds
  budgets <- numeric()
  testthat::local_mocked_bindings(metric.mds=function(...) {
    args <- list(...)
    budgets <<- c(budgets,args$sgd.control$max.workspace.bytes)
    do.call(original,args)
  }, .package="grip")
  for(mode in c("full","landmarks")) {
    x <- atlas_compute_fixture(list(inner="ambient",metric="euclidean",mode=mode,landmarks=4,memory_mb=768))
    unlink(x$folder,recursive=TRUE)
  }
  expect_equal(budgets,rep(768*1024^2,2))
})
