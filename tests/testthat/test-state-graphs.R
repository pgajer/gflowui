state_fixture <- function() {
  n<-data.frame(ID=c("1,2","1,3","2,3","4,5"),freq=c(4L,3L,2L,1L),node=1:4,label=c("A+B","A+C","B+C","D+E"),component=1L)
  e<-data.frame(from=c(1L,1L,2L),to=c(2L,3L,3L),pair.a=c("1,2","1,2","1,3"),pair.b=c("1,3","2,3","2,3"),triple="1,2,3",
    i.a=c(2L,2L,1L),i.b=c(1L,0L,0L),min.support=c(1L,0L,0L),length=1)
  list(reference=list(id="test"),template=list(nodes=n,edges=e,candidate.edges=e,
    faces=data.frame(a=integer(),b=integer(),c=integer(),min.support=integer()),
    membership=data.frame(sample.id=paste0("sample",1:10),pair=rep(n$ID,n$freq),triple=c(rep("1,2,3",2),rep("1,2,4",2),"1,2,3",rep("1,3,5",2),rep("2,3,5",2),"4,5,6")),
    metadata=list(reference.n=10L)))
}
test_that("state construction retains isolates and fixed-reference evidence",{
 a<-state_fixture();g<-gflowui_state_graph_build(a,100)$graph
 expect_equal(nrow(g$edges),1L);expect_equal(g$metadata$components,3L);expect_equal(g$metadata$isolates,2L)
 expect_true(all(g$edges$length==1));expect_equal(g$metadata$selected.n,10)
 shared<-gflowui_state_graph_build(a,100,adjacency="shared_feature")$graph
 expect_equal(nrow(shared$edges),3L)
 strict<-gflowui_state_graph_build(a,100,support=2)$graph
 expect_equal(nrow(strict$edges),0L);expect_equal(strict$metadata$isolates,4L)
 small<-gflowui_state_graph_build(a,60)$graph
 expect_equal(small$nodes$ID,c("1,2","1,3"));expect_identical(small$edges,g$edges)
 expect_identical(small$membership,g$membership)
 empty<-gflowui_state_graph_build(a,"minimum",minimum=100)$graph
 expect_equal(nrow(empty$nodes),0L);expect_equal(empty$metadata$components,0L)
 expect_error(gflowui_state_graph_build(a,support=0),"positive integer")
 expect_error(gflowui_state_graph_build(a,support=1.5),"positive integer")
})
test_that("coverage includes complete frequency ties",{
 a<-state_fixture();a$template$nodes$freq<-c(3,3,3,1)
 expect_equal(nrow(gflowui_state_graph_build(a,40)$graph$nodes),3L)
})
test_that("sample IDs and witness scopes remain exact",{
 g<-gflowui_state_graph_build(state_fixture(),100)$graph;e<-g$edges[1,,drop=FALSE]
 expect_identical(gflowui_state_graph_members(g,"1,2"),paste0("sample",1:4))
 expect_identical(gflowui_state_graph_members(g,edge=e,scope="witnesses"),c("sample1","sample2","sample5"))
 expect_identical(gflowui_state_graph_members(g,edge=e,scope="side_a"),c("sample1","sample2"))
 expect_identical(gflowui_state_graph_members(g,edge=e,scope="side_b"),"sample5")
 expect_length(gflowui_state_graph_members(g,edge=e,scope="third"),0L)
 expect_length(gflowui_state_graph_members(g,states=character()),0L)
 expect_length(gflowui_state_graph_members(g,edge=e,scope="endpoints"),7L)
 expect_false(any(g$nodes$ID %in% g$membership$sample.id))
})
test_that("layout preserves isolates and unit segments without infinite targets",{
 skip_if_not_installed("igraph")
 g<-gflowui_state_graph_build(state_fixture(),100)$graph
 f<-gflowui_state_graph_fit(g)
 expect_true(all(is.finite(f$coords)));expect_equal(nrow(f$coords),4L)
 expect_equal(as.numeric(dist(f$coords[1:2,,drop=FALSE])),1)
 expect_equal(f$error,0)
 g<-gflowui_state_graph_build(state_fixture(),"minimum",minimum=100)$graph
 expect_equal(dim(gflowui_state_graph_fit(g)$coords),c(0L,3L))
})
test_that("persistence distinguishes inactive and empty sample restrictions",{
 a<-gflowui_state_graph_default();expect_null(a$sample_filter)
 b<-gflowui_state_graph_default(list(sample_filter=character()));expect_identical(b$sample_filter,character())
 expect_identical(gflowui_state_graph_default(list(mode="unknown"))$mode,"samples")
 expect_null(gflowui_state_graph_asset(list()))
})
test_that("state controller isolates sample visibility from graph identity and saves selections",{
 skip_if_not_installed("plotly")
 a<-state_fixture();a$references<-list('60'=list(ids=a$template$nodes$ID,coords=matrix(seq_len(12),4,3),label="fixture"))
 p<-tempfile(fileext=".rds");saveRDS(a,p);on.exit(unlink(p))
 m<-list(project_id="fixture",metadata=list(state_graphs=list(file=p,cache_dir=tempdir())),defaults=list())
 saved<-NULL
 shiny::testServer(gflowui_state_graphs_server,args=list(manifest=function()m,
   view=function()list(vertex_ids=a$template$membership$sample.id),visible=function()1:10,
   sample_selected=function()character(),sample_click=function()NULL,
   save=function(x)saved<<-x,open_region=function(...)NULL),{
   session$flushReact();initial<-graph()$identity
   session$setInputs(selected="1,2",members=1)
   expect_identical(state()$sample_filter,paste0("sample",1:4))
   expect_identical(mode(),"linked");expect_identical(graph()$identity,initial)
   session$setInputs(clear_filter=1);expect_null(state()$sample_filter)
   session$setInputs(selected=character(),members=2);expect_identical(state()$sample_filter,character())
   session$setInputs(support=100);expect_equal(nrow(graph()$graph$edges),0L)
   session$setInputs(mode="samples");expect_equal(state()$support,100)
   session$elapse(1000);session$flushReact();expect_equal(saved$support,100)
 })
})
test_that("empty controls cannot erase saved state and witness selections at startup", {
 skip_if_not_installed("plotly")
 a<-state_fixture();a$references<-list('60'=list(ids=a$template$nodes$ID,coords=matrix(seq_len(12),4,3),label="fixture"))
 p<-tempfile(fileext=".rds");saveRDS(a,p);on.exit(unlink(p))
 m<-list(project_id="fixture",metadata=list(state_graphs=list(file=p,cache_dir=tempdir())),
   defaults=list(state_graphs=list(selected="1,2",edge="1,2|1,3")))
 shiny::testServer(gflowui_state_graphs_server,args=list(manifest=function()m,
   view=function()list(vertex_ids=a$template$membership$sample.id),visible=function()1:10,
   sample_selected=function()character(),sample_click=function()NULL,
   save=function(...)NULL,open_region=function(...)NULL),{
   session$flushReact();session$setInputs(selected=character(),edge="")
   expect_identical(state()$selected,"1,2");expect_identical(state()$edge,"1,2|1,3")
   session$setInputs(selected="1,2",edge="1,2|1,3")
   session$setInputs(selected=character(),edge="")
   expect_length(state()$selected,0);expect_identical(state()$edge,"")
 })
})
test_that("explicit reference subsets recompute counts and scientific identity", {
 skip_if_not_installed("linf")
 a<-state_fixture();a$template$dictionary<-data.frame(index=1:6,id=LETTERS[1:6],label=LETTERS[1:6])
 b<-gflowui_state_graph_reference(a,c("sample1","sample2","sample5"))
 expect_identical(b$template$membership$sample.id,c("sample1","sample2","sample5"))
 expect_equal(b$template$nodes$freq,c(2,1))
 g<-gflowui_state_graph_build(b,100)
 expect_equal(g$graph$edges$n.triple,3)
 expect_equal(g$graph$edges$i.a,2);expect_equal(g$graph$edges$i.b,1)
 expect_equal(g$graph$metadata$reference.n,3)
 expect_false(identical(g$identity,gflowui_state_graph_build(a,100)$identity))
 expect_error(gflowui_state_graph_reference(a,character()),"empty")
 expect_identical(gflowui_state_graph_reference(a,NULL),a)
})
