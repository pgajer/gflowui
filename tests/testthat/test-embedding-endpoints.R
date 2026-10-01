test_that("straight arms have tips, while a circle has no narrow angular tips", {
  skip_if_not_installed("FNN")
  x<-cbind(seq(0,1,length.out=100),0,0)
  r<-gflowui_detect_embedding_endpoints(x)
  expect_equal(which(r$rows$candidate),c(1L,100L))
  t<-seq(0,2*pi,length.out=101)[-101]
  r<-gflowui_detect_embedding_endpoints(cbind(cos(t),sin(t),0),list(rule="none"))
  expect_equal(sum(r$rows$candidate),0L)
  arms<-rbind(c(0,0,0),do.call(rbind,lapply(c(0,2*pi/3,4*pi/3),function(a)
    cbind((1:40)*cos(a),(1:40)*sin(a),0))))
  r<-gflowui_detect_embedding_endpoints(arms,list(rule="none"))
  expect_equal(which(r$rows$candidate),c(41L,81L,121L))
})

test_that("one angular outlier is optional and recorded", {
  skip_if_not_installed("FNN")
  x<-cbind(c(0,1,2,3,4,-.5),0,0)
  a<-gflowui_detect_embedding_endpoints(x,list(k=5L,rule="none"))
  b<-gflowui_detect_embedding_endpoints(x,list(k=5L,rule="none",drop_one=TRUE))
  expect_false(a$rows$candidate[1])
  expect_true(b$rows$candidate[1])
  expect_equal(b$rows$excluded_neighbor[1],6L)
  expect_equal(b$rows$max_angle[1],180)
  expect_equal(b$rows$angle[1],0)
})

test_that("spacing support can exclude an isolated close pair", {
  skip_if_not_installed("FNN")
  x<-cbind(c(0:20,100,100.01),0,0)
  a<-gflowui_detect_embedding_endpoints(x,list(k=5L,rule="manual",cutoff=3))
  b<-gflowui_detect_embedding_endpoints(x,list(k=5L,rule="manual",cutoff=3,spacing="dk"))
  expect_true(tail(a$rows$candidate,1))
  expect_false(tail(b$rows$candidate,1))
  p<-gflowui_detect_embedding_endpoints(x,list(k=5L,rule="percentile",percentile=75,multiplier=1.2))
  expect_equal(p$cutoff,unname(quantile(p$spacings,.75))*1.2)
})

test_that("coincident points are mapped explicitly without zero directions", {
  skip_if_not_installed("FNN")
  x<-cbind(0:20,0,0);x<-rbind(x,x[1,])
  r<-gflowui_detect_embedding_endpoints(x)
  expect_equal(r$coincident_vertices,1L)
  expect_equal(r$rows$representative_vertex[22],1L)
  expect_false(r$rows$candidate[22])
  expect_true(all(is.finite(r$rows$angle)))
  expect_error(gflowui_detect_embedding_endpoints(matrix(0,20,3)),"distinct positions")
})

test_that("rotation translation and uniform scale preserve geometric decisions", {
  skip_if_not_installed("FNN")
  set.seed(79);x<-matrix(rnorm(120),40,3)
  q<-qr.Q(qr(matrix(rnorm(9),3,3)))
  a<-gflowui_detect_embedding_endpoints(x,list(k=6L,rule="percentile",merge=TRUE,angle=160))
  b<-gflowui_detect_embedding_endpoints(sweep(x%*%q*7,2,c(14,21,7)),list(k=6L,rule="percentile",merge=TRUE,angle=160))
  expect_equal(a$rows$candidate,b$rows$candidate)
  expect_equal(a$rows$retained,b$rows$retained)
  expect_equal(a$rows$angle,b$rows$angle,tolerance=1e-6)
  expect_equal(b$cutoff,7*a$cutoff,tolerance=1e-8)
  expect_true(sum(a$rows$retained)<sum(a$rows$candidate))
  suppressed<-which(!is.na(a$rows$suppressed_by))
  expect_true(all(a$rows$retained[a$rows$suppressed_by[suppressed]]))
})

test_that("invalid inputs fail clearly", {
  skip_if_not_installed("FNN")
  x<-cbind(0:20,0,0)
  expect_error(gflowui_detect_embedding_endpoints(x,list(k=21)),"distinct positions")
  expect_error(gflowui_detect_embedding_endpoints(x,list(angle=181)),"settings")
  expect_error(gflowui_detect_embedding_endpoints(x,list(k=2.5)),"settings")
  expect_error(gflowui_detect_embedding_endpoints(x,vertex_ids=rep("a",21)),"unique ID")
  x[1,1]<-NA_real_;expect_error(gflowui_detect_embedding_endpoints(x),"finite 3D")
})

test_that("module detects, selects, imports and invalidates stale candidates", {
  skip_if_not_installed("FNN")
  f<-shiny::reactiveVal(list(coords=cbind(0:30,0,0),vertex_ids=paste0("s",1:31),
    key="original",project_id="test",set_id="arms"))
  added<-list()
  shiny::testServer(gflowui_embedding_endpoints_server,args=list(frame=f,
    label_for_vertex=function(v)paste("Taxon",v),
    add_vertices=function(v,r)added[[length(added)+1L]]<<-v),{
    session$setInputs(detect=0,add=0);session$flushReact()
    session$setInputs(detect=1);session$flushReact()
    expect_true(valid());expect_equal(selected(),c(1L,31L))
    expect_equal(preview()$label,c("Taxon 1","Taxon 31"))
    session$setInputs(selection=list(vertex=31L,checked=FALSE));session$flushReact()
    expect_equal(selected(),1L)
    session$setInputs(add=1);session$flushReact();expect_equal(added[[1]],1L)
    session$setInputs(angle=60);session$flushReact()
    expect_false(valid());expect_null(preview())
    session$setInputs(add=2);session$flushReact();expect_length(added,1L)
    session$setInputs(detect=2);session$flushReact();expect_true(valid())
    f(list(coords=cbind(0:30,1,0),vertex_ids=paste0("s",1:31),key="changed",project_id="test",set_id="arms"))
    session$flushReact();expect_false(valid());expect_null(preview())
  })
})

test_that("preview markers retain original indices under display filtering", {
  skip_if_not_installed("plotly")
  coords<-cbind(1:5,6:10,11:15)
  rows<-data.frame(vertex=c(1L,5L),label=c("Taxon <A>","Taxon B"),d1=1,dk=2,angle=0,max_angle=0)
  p<-gflowui_add_embedding_endpoint_preview(plotly::plot_ly(),coords,rows,keep_idx=c(3L,5L))
  trace<-plotly::plotly_build(p)$x$data[[1]]
  expect_equal(as.integer(trace$customdata),5L)
  expect_equal(as.numeric(trace$x),5)
  expect_match(trace$hovertext,"Taxon B",fixed=TRUE)
})

test_that("app detector imports labeled candidates with provenance into working endpoints", {
  skip_if_not_installed("FNN");skip_if_not_installed("plotly")
  root<-tempfile("endpoint-detector-app-");dir.create(root)
  withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry"),rgl.useNULL=TRUE))
  n<-31L;ids<-paste0("sample-",1:n);coords<-cbind(0:(n-1L),0,0)
  adj<-lapply(1:n,function(i)as.integer(intersect(c(i-1,i+1),1:n)))
  graph<-list(adj_list=adj,weight_list=lapply(adj,function(a)rep(1,length(a))),vertex_ids=ids)
  gf<-file.path(root,"graph.rds");lf<-file.path(root,"layout.rds");af<-file.path(root,"abundances.rds")
  saveRDS(list(X.graphs=list(graph),k.values=1L,selected.k=1L,vertex_ids=ids),gf);saveRDS(coords,lf)
  saveRDS(list(sample_ids=ids,taxon_names="Taxon_A",indices=rep(list(1L),n),abundances=rep(list(1),n)),af)
  gflowui::register_project(project_root=root,project_id="endpoint_detector_test",profile="custom",scan_results=FALSE,
    graph_sets=list(list(id="line",label="Line",graph_file=gf,k_values=1L,selected_k=1L,n_samples=n,n_features=3L,
      anchor="A",base_metric="euclidean",layout_assets=list(coordinate_normalization="uniform",
        presets=list(renderer="plotly"),grip_layouts=list(list(id="test",k=1L,path=lf))))),
    defaults=list(graph_set_id="line",reference_graph_set_id="line",reference_k=1L),
    metadata=list(vertex_hover=list(abundances_file=af),endpoint_label_provider=list(mode="vertex_abundances",
      metric_coordinates=list(euclidean="abundance"),reference_taxa=list(A="Taxon_A"))))
  shiny::testServer(app_server,{
    open_project("endpoint_detector_test");session$flushReact()
    session$setInputs(`endpoint_detector-detect`=0);session$flushReact()
    session$setInputs(`endpoint_detector-detect`=1);session$flushReact()
    expect_true(endpoint_detector$valid())
    expect_equal(endpoint_detector$preview()$vertex,c(1L,31L))
    expect_equal(endpoint_detector$preview()$label,rep("Taxon A",2))
    session$setInputs(`endpoint_detector-add`=0);session$flushReact()
    session$setInputs(`endpoint_detector-add`=1);session$flushReact()
    rows<-endpoint_panel_state()$working$rows
    expect_equal(rows$vertex,c(1L,31L))
    expect_equal(rows$label,rep("Taxon A",2))
    expect_equal(rows$source_type,rep("embedding_detector",2))
    expect_true(all(grepl("embedding",rows$notes)))
    session$setInputs(`endpoint_detector-add`=2);session$flushReact()
    expect_equal(nrow(endpoint_panel_state()$working$rows),2L)
  })
})
