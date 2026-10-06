classification_fixture <- function() {
 a<-list(samples=data.frame(sample_id=c("a","b","c"),udcst_level1=c("A","B","A"),udcst_level2=c("A + B","A + B","A + C")),
 levels=list(udcst_level1=list(label="udCST level 1"),udcst_level2=list(label="udCST level 2")),
 subsets=list(All=list(ids=c("a","b","c")),core=list(ids=c("a","b"))),
 palettes=list(udcst_level1=c(A="red",B="blue"),udcst_level2=c(`A + B`="green",`A + C`="gold")))
 f<-tempfile(fileext=".rds");saveRDS(a,f)
 list(a=a,m=list(project_id="test",metadata=list(classification_catalogue=list(file=f))))
}
test_that("catalogue annotations follow IDs and never mutate layouts or ordered labels",{
 f<-classification_fixture();st<-list(vertex_ids=c("c","a","unknown"),coords=matrix(1:9,3),sources=list(dcst_level2=list(values=c("C","A","Z"))),graph_set=list())
 out<-gflowui_classification_augment(st,f$m)
 expect_identical(out$coords,st$coords)
 expect_identical(out$sources$dcst_level2,st$sources$dcst_level2)
 expect_identical(out$sources$udcst_level2$values,c("A + C","A + B",NA_character_))
 expect_identical(gflowui_classification_subset(f$a,"All",st$vertex_ids),1:3)
 expect_identical(gflowui_classification_subset(f$a,"core",st$vertex_ids),2L)
 expect_length(gflowui_classification_subset(f$a,"core","absent"),0)
 expect_identical(gflowui_classification_augment(st,list()),st)
 f$m$metadata$classification_catalogue$palettes$udcst_level2<-c(`A + B`="#123456")
 expect_identical(gflowui_classification_augment(st,f$m)$graph_set$color_assets$categorical_palettes$udcst_level2[["A + B"]],"#123456")
 expect_identical(unname(gflowui_dcst_options(out$sources,"udcst_level2")$levels),paste0("udcst_level",1:2))
})
test_that("controller keeps type, level, color and groups coherent and persists independently of layouts",{
 f<-classification_fixture();saved<-list()
 server<-function(input,output,session){
  m<-shiny::reactiveVal(f$m)
  cc<-gflowui_classification_server(input,session,m,function(s){saved[[length(saved)+1L]]<<-s;x<-m();x$defaults$classification_state<-s;m(x)})
 }
 shiny::testServer(server,{
  session$flushReact();expect_true(cc$enabled());expect_identical(cc$level(),"udcst_level2")
  session$setInputs(graph_cst_type="dcst");session$setInputs(graph_dcst_level="dcst_level3")
  expect_identical(cc$level(),"dcst_level3");expect_identical(cc$color(NULL),"dcst")
  session$setInputs(graph_cst_type="udcst")
  expect_identical(cc$level(),"udcst_level2")
  session$setInputs(graph_layout_color_by="source_dataset",graph_sample_subset="core")
  session$setInputs(graph_cst_type="dcst")
  expect_identical(cc$color(NULL),"source_dataset")
  expect_identical(cc$level(),"dcst_level3")
  session$setInputs(graph_dcst_table_selection=list(project="test",level="dcst_level3",groups=c("A","B")))
  expect_identical(cc$selection()$groups,c("A","B"))
  session$setInputs(graph_cst_type="udcst",graph_dcst_level="dcst_level3")
  expect_identical(cc$level(),"udcst_level2")
  session$elapse(600);session$flushReact()
  expect_identical(m()$defaults$classification_state,cc$state())
  expect_identical(cc$filter(list(vertex_ids=c("b","c","a")),2:3),3L)
  expect_identical(gflowui_classification_state(m()$defaults$classification_state,f$a),cc$state())
  x<-m();x$project_id<-"legacy";x$metadata<-list();m(x);session$flushReact()
  expect_false(cc$enabled());expect_identical(cc$filter(list(vertex_ids=c("a","b")),1:2),1:2)
 })
})

test_that("hidden arm vertices leave gaps instead of shortcuts",{
 expect_identical(gflowui_visible_path(c(1,2,3,4),c(1,3,4)),c(1L,NA_integer_,3L,4L))
 expect_identical(gflowui_visible_path(c(1,2),integer()),c(NA_integer_,NA_integer_))
})
test_that("empty cross tables and reversed dominance use fixed pair coordinates",{
 records<-data.frame(vertex_id=c("x","y"),record_id=c("1","2"),dataset=c("D","D"))
 expect_equal(dim(gflowui_source_cross(records,character(),character())),c(0L,0L))
 a<-list(sample_ids=c("x","y"),taxon_names=c("A","B","C"),indices=list(1:3,1:3),abundances=list(c(.8,.1,.1),c(.1,.8,.1)))
 x<-gflowui_pair_coordinates(a,c("x","y"),"A","B")
 expect_equal(x$t,c(1/9,8/9));expect_equal(x$r,c(.1,.1))
 expect_equal(x$u,c(.125,8));expect_equal(x$rho,c(.125,1))
})

test_that("empty masks never expand to all vertices",{
 expect_identical(gflowui_visible_indices(NULL,3L),1:3)
 expect_identical(gflowui_visible_indices(integer(),3L),integer())
 expect_identical(gflowui_visible_indices(c(2L,3L),3L),2:3)
})

test_that("saved core membership pauses global coverage without overwriting it",{
 f<-classification_fixture();f$m$defaults$classification_state<-list(subset="core")
 saved<-list()
 server<-function(input,output,session){
   override<-shiny::reactiveVal(NULL);m<-shiny::reactiveVal(f$m)
   cc<-gflowui_classification_server(input,session,m,function(s){saved[[length(saved)+1L]]<<-s},subset_override=override)
 }
 shiny::testServer(server,{
   session$flushReact();st<-list(vertex_ids=c("a","b","c"))
   expect_identical(cc$filter(st,1:3),1:2)
   override(list(subset="All",label="90% core"));session$flushReact()
   expect_identical(cc$filter(st,1:3),1:3)
   expect_identical(cc$filter(st,3L),3L)
   expect_match(as.character(cc$controls(st)),"global coverage preset is paused")
   session$setInputs(graph_sample_subset="All");session$elapse(600);session$flushReact()
   expect_identical(cc$state()$subset,"core");expect_length(saved,0L)
   override(NULL);session$flushReact();expect_identical(cc$filter(st,1:3),1:2)
 })
})

retention_fixture <- function() {
 f<-classification_fixture()
 f$a$within_cell_retention<-list(presets=list(`100`=list(ids=c("a","b","c"),label="All"),
   `95`=list(ids=c("a","c"),label="Closest 95%")),unmodeled_ids="c",policy="Fixed residuals; unmodeled cells retained.")
 saveRDS(f$a,f$m$metadata$classification_catalogue$file);f
}
test_that("retention intersects coverage by ID and preserves unmodeled cells",{
 f<-retention_fixture();ids<-c("c","b","a")
 expect_identical(gflowui_classification_subset(f$a,"All",ids,"100"),1:3)
 expect_identical(gflowui_classification_subset(f$a,"All",ids,"95"),c(1L,3L))
 expect_identical(gflowui_classification_subset(f$a,"core",ids,"95"),3L)
 expect_identical(gflowui_classification_subset(f$a,"core",ids[1],"95"),integer())
 expect_error(gflowui_classification_subset(f$a,"All",ids,"80"),"Unknown within-cell")
 expect_identical(gflowui_classification_state(list(retention="95"),classification_fixture()$a)$retention,"100")
 expect_equal(gflowui_classification_asset(f$m)$within_cell_retention,f$a$within_cell_retention)
})
test_that("retention persists and saved cores pause both filters",{
 f<-retention_fixture();saved<-NULL
 server<-function(input,output,session){
   override<-shiny::reactiveVal(NULL)
   cc<-gflowui_classification_server(input,session,shiny::reactive(f$m),function(s)saved<<-s,subset_override=override)
 }
 shiny::testServer(server,{
   session$flushReact();st<-list(vertex_ids=c("c","b","a"))
   session$setInputs(graph_within_cell_retention="95");expect_identical(cc$filter(st,1:3),c(1L,3L))
   session$setInputs(graph_sample_subset="core");expect_identical(cc$filter(st,1:3),3L)
   session$elapse(600);session$flushReact();expect_identical(saved$retention,"95")
   override(list(subset="All",label="saved core"));session$flushReact();expect_identical(cc$filter(st,1:3),1:3)
   session$setInputs(graph_within_cell_retention="100");expect_identical(cc$state()$retention,"95")
   override(NULL);session$flushReact();expect_identical(cc$filter(st,1:3),3L)
   session$setInputs(graph_within_cell_retention="100");expect_identical(cc$filter(st,1:3),2:3)
 })
})
