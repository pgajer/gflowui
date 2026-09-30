property_fixture <- function() {
  g<-list(graph_id="toy",graph_sha256="topology",ids=c("a","b","c"),
    edges=list(list(0L,1L),list(1L,2L)),edge_matrix=matrix(c(1L,2L,2L,3L),2,2,byrow=TRUE),n_vertices=3L,n_edges=2L)
  p<-list(schema_version=1L,algorithm="unit-graph-properties-v1",graph_id="toy",graph_sha256="topology",
    graph_file_sha256="source",vertex_ids=g$ids,edges=g$edges,
    vertex=list(degree=c(1,2,1),betweenness=c(0,1,0),betweenness_normalized=c(0,1,0),
      core_number=c(1,1,1),clustering=c(0,0,0),articulation=c(0,1,0)),
    edge=list(detour_ratio=list(NULL,NULL),betweenness=c(2,2),betweenness_normalized=c(2/3,2/3),
      triangle_count=c(0,0),effective_resistance=c(1,1),bridge=c(1,1),
      bridge_smaller_side=c(1,1),bridge_larger_side=c(2,2),bridge_pairs=c(2,2)))
  root<-tempfile();dir.create(root)
  save<-function(value) {
    file<-file.path(root,"props.json");jsonlite::write_json(value,file,auto_unbox=TRUE,null="null")
    list(root=root,graphs=list(toy=list(file=list(sha256="source"))),property_index=list(graphs=list(toy=list(
      path="props.json",sha256=digest::digest(file=file,algo="sha256")))))
  }
  list(g=g,p=p,save=save)
}

test_that("graph properties preserve infinity and reject wrong graph/vertex/edge identity", {
  f<-property_fixture();index<-f$save(f$p);p<-gflowui_ec_properties(index,f$g)
  expect_true(all(is.infinite(p$edge$detour_ratio)))
  expect_equal(p$vertex$degree,c(1,2,1))
  bad<-f$p;bad$vertex_ids<-rev(bad$vertex_ids)
  expect_error(gflowui_ec_properties(f$save(bad),f$g),"identity/order")
  bad<-f$p;bad$edges<-rev(bad$edges)
  expect_error(gflowui_ec_properties(f$save(bad),f$g),"identity/order")
  bad<-f$p;bad$graph_sha256<-"different"
  expect_error(gflowui_ec_properties(f$save(bad),f$g),"identity/order")
  bad<-f$p;bad$edge$bridge<-c(0,1)
  expect_error(gflowui_ec_properties(f$save(bad),f$g),"bridge property")
  expect_null(gflowui_ec_properties(list(),f$g))
})

test_that("property plotting colors and labels without modifying coordinates", {
  f<-property_fixture();p<-gflowui_ec_properties(f$save(f$p),f$g)
  z<-cbind(c(0,1,2),c(0,1,0),c(0,0,0))
  plot<-gflowui_ec_property_plot(f$g,z,p,vertex_color="degree",edge_color="detour_ratio",
    vertex_label="core_number",vertex_label_scope="all",edge_label="bridge_pairs",edge_label_scope="all",selected="b")
  traces<-plotly::plotly_build(plot)$x$data
  expect_true(any(vapply(traces,function(x)identical(x$name,"Infinite detour (bridge)"),TRUE)))
  vertex<-Filter(function(x)identical(as.character(x$customdata),f$g$ids) && x$mode=="markers",traces)[[1]]
  expect_equal(as.numeric(vertex$x),z[,1]);expect_equal(as.numeric(vertex$y),z[,2])
  expect_equal(as.numeric(vertex$marker$color),p$vertex$degree)
  text<-unlist(lapply(Filter(function(x)x$mode=="text",traces),`[[`,"text"))
  expect_true(all(c("a: 1","b: 1","c: 1","2") %in% text))
  # Every finite edge property and vertex property must be a valid Plotly trace.
  for(key in names(gflowui_ec_property_names("edge"))) {
    expect_s3_class(plotly::plotly_build(gflowui_ec_property_plot(f$g,z,p,edge_color=key)),"plotly")
  }
  for(key in names(gflowui_ec_property_names())) {
    expect_s3_class(plotly::plotly_build(gflowui_ec_property_plot(f$g,z,p,vertex_color=key)),"plotly")
  }
  expect_equal(gflowui_ec_property_text(c(Inf,NA,2)),c("Inf (bridge)","not applicable","2"))
})
