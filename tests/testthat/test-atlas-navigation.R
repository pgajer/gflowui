nav_fixture <- function() {
  region<-function(id,definition,n=2L,label=id)list(id=id,label=label,definition=definition,vertex_ids=letters[seq_len(n)],views=list(list(id=paste0(id,"_fit"),label="Saved fit")))
  rs<-list(region("a2",list(type="anchor",anchor="sample-a",size=2,metric="hellinger")),
    region("a3",list(type="anchor",anchor="sample-a",size=3,metric="hellinger"),3L),
    region("import",list(type="import",anchor="sample-a",size=2,selection="Abundance ranking")),
    region("b2",list(type="anchor",anchor="sample-b",size=2,metric="euclidean")),
    region("A",list(type="dcst",level="dcst_level1",groups="A"),4L),
    region("B",list(type="dcst",level="dcst_level1",groups="B"),2L),
    region("AB",list(type="dcst",level="dcst_level2",groups="A → B"),3L),
    region("AC",list(type="dcst",level="dcst_level2",groups="A → C"),2L),
    region("BA",list(type="dcst",level="dcst_level2",groups="B → A"),4L),
    region("union",list(type="dcst",level="dcst_level1",groups=c("A","B")),6L),
    region("unknown",list(type="future-type")))
  setNames(rs,vapply(rs,`[[`,"","id"))
}

test_that("families use definitions and retain imported selection semantics", {
  rs<-nav_fixture();catalog<-gflowui_atlas_catalog(rs)
  expect_identical(catalog$import$family,"anchor")
  expect_identical(catalog$union$family,"custom")
  expect_identical(catalog$unknown$family,"custom")
  rs$a2$label<-"dCST 3 | fake label";expect_identical(gflowui_atlas_catalog(rs)$a2$family,"anchor")
  x<-gflowui_atlas_navigation_resolve(catalog,list(family="anchor",anchor="sample-a",size="2"))
  expect_null(x$target);expect_length(x$controls$definition$choices,2L)
  x<-gflowui_atlas_navigation_resolve(catalog,c(x$state,list(definition="Abundance ranking")))
  # Set directly rather than duplicate list names.
  state<-list(family="anchor",anchor="sample-a",size="2",definition="Abundance ranking")
  expect_identical(gflowui_atlas_navigation_resolve(catalog,state)$target,"import")
})

test_that("dCST sorting, filters and pending choices resolve without picking arbitrary regions", {
  catalog<-gflowui_atlas_catalog(nav_fixture())
  x<-gflowui_atlas_navigation_resolve(catalog,list(family="dcst"));expect_null(x$target)
  x<-gflowui_atlas_navigation_resolve(catalog,list(family="dcst",level="dcst_level2"))
  expect_identical(unname(x$controls$region$choices),c("BA","AB","AC"))
  expect_identical(x$state$first,"__all__");expect_null(x$target)
  x<-gflowui_atlas_navigation_resolve(catalog,list(family="dcst",level="dcst_level2",first="B"))
  expect_identical(x$target,"BA");expect_false(x$controls$region$visible)
  state<-gflowui_atlas_navigation_change(x$state,"level","dcst_level1")
  expect_null(state$first);expect_null(state$region)
  expect_null(gflowui_atlas_navigation_resolve(catalog,state)$target)
})

test_that("revisions stay in a lineage and retired-only lineages remain hidden", {
  rs<-nav_fixture();rs$a2$family_id<-"a2";rs$a2$revision<-1L;rs$a2$retired<-TRUE
  v<-rs$a2;v$id<-"a2v2";v$revision<-2L;v$retired<-FALSE;rs[[v$id]]<-v
  catalog<-gflowui_atlas_catalog(rs)
  expect_length(catalog$a2$versions,1L)
  state<-gflowui_atlas_navigation_state(catalog,"a2v2")
  x<-gflowui_atlas_navigation_resolve(catalog,state);expect_identical(x$target,"a2v2");expect_false(x$controls$revision$visible)
  catalog<-gflowui_atlas_catalog(rs,TRUE);state<-gflowui_atlas_navigation_state(catalog,"a2")
  x<-gflowui_atlas_navigation_resolve(catalog,state);expect_identical(x$target,"a2");expect_true(x$controls$revision$visible)
  rs$a2v2$retired<-TRUE;expect_false("a2"%in%names(gflowui_atlas_catalog(rs)))
})

test_that("navigation preserves the view during narrowing, recalls families and rejects stale messages", {
  base<-tempfile();dir.create(base);on.exit(unlink(base,recursive=TRUE))
  withr::local_options(gflowui.projects_data_dir=base)
  rs<-nav_fixture();p<-file.path(base,"projects","test","local_views","atlas.rds");gflowui_atlas_save(rs,p)
  m<-list(project_id="test",graph_sets=list(list(id="parent")),metadata=list(local_views=list(enabled=TRUE)))
  st<-list(vertex_ids=letters,set_id="parent")
  shiny::testServer(gflowui_local_views_server,args=list(manifest=shiny::reactive(m),view_state=shiny::reactive(st),selected_vertex=function()NULL,dcst_selection=function()NULL),{
    session$flushReact()
    choose<-function(key,value)session$setInputs(nav_choice=list(project="test",token=nav_sent()$token,key=key,value=value))
    choose("family","anchor");expect_null(region())
    choose("anchor","sample-b");expect_identical(region_id(),"b2")
    session$setInputs(region="b2",view="b2_fit");expect_identical(view_id(),"b2_fit")
    choose("family","dcst");expect_identical(region_id(),"b2");expect_identical(view_id(),"b2_fit")
    stale<-list(project="test",token=nav_sent()$token,key="level",value="dcst_level1")
    choose("level","dcst_level2");expect_identical(region_id(),"b2")
    session$setInputs(nav_choice=stale);expect_identical(nav_model()$state$level,"dcst_level2")
    choose("region","AB");expect_identical(region_id(),"AB")
    choose("family","anchor");expect_identical(region_id(),"b2");expect_identical(view_id(),"b2_fit")
    choose("family","dcst");expect_identical(region_id(),"AB")
    session$setInputs(whole=1);expect_null(region())
    choose("family","anchor");expect_identical(region_id(),"b2");expect_identical(view_id(),"b2_fit")
    expect_identical(readRDS(p)$regions,rs)
  })
})

test_that("precomputed cores expose coverage and retention without project-specific branches", {
  rs<-list()
  for(c in c(90L,80L,70L,60L))for(q in c(100L,95L,90L,80L)){
    id<-paste(c,q,sep="_")
    rs[[id]]<-list(id=id,label=id,definition=list(type="coverage_core",coverage=c,retention=q),vertex_ids=letters[1:5],views=list())
  }
  catalog<-gflowui_atlas_catalog(c(nav_fixture(),rs))
  pending<-gflowui_atlas_navigation_resolve(catalog,list(family="core"))
  expect_null(pending$target)
  expect_identical(unname(pending$controls$coverage$choices),c("90","80","70","60"))
  expect_null(pending$controls$retention)
  for(r in rs){
    state<-gflowui_atlas_navigation_state(catalog,r$id)
    resolved<-gflowui_atlas_navigation_resolve(catalog,state)
    expect_identical(resolved$target,r$id)
    expect_true(resolved$controls$coverage$visible)
    expect_true(resolved$controls$retention$visible)
    expect_identical(unname(resolved$controls$retention$choices),c("100","95","90","80"))
    expect_false(resolved$controls$region$visible)
  }
  changed<-gflowui_atlas_navigation_change(state,"coverage","90")
  expect_null(changed$retention)
  expect_null(gflowui_atlas_navigation_resolve(catalog,changed)$target)
  expect_false("Custom"%in%names(pending$controls$family$choices))
})

test_that("core browsing keeps a selected embedding route and waits for complete membership choices", {
  base<-tempfile();dir.create(base);on.exit(unlink(base,recursive=TRUE))
  withr::local_options(gflowui.projects_data_dir=base)
  rs<-list()
  for(c in c(90L,80L))for(q in c(100L,95L)){
    id<-paste(c,q,sep="_")
    rs[[id]]<-list(id=id,label=id,definition=list(type="coverage_core",coverage=c,retention=q,cells=2L,base_n=5L,actual_coverage=.5,unfiltered_n=0L),vertex_ids=letters[1:5],
      views=list(list(id=paste0(id,"_direct"),label="Direct 3D",embedding_route="Direct 3D"),list(id=paste0(id,"_refined"),label="Refined 3D",embedding_route="Refined 3D")))
  }
  p<-file.path(base,"projects","test","local_views","atlas.rds");gflowui_atlas_save(rs,p)
  m<-list(project_id="test",graph_sets=list(list(id="parent")),metadata=list(local_views=list(enabled=TRUE)))
  st<-list(vertex_ids=letters,set_id="parent")
  shiny::testServer(gflowui_local_views_server,args=list(manifest=shiny::reactive(m),view_state=shiny::reactive(st),selected_vertex=function()NULL,dcst_selection=function()NULL),{
    session$flushReact()
    choose<-function(key,value)session$setInputs(nav_choice=list(project="test",token=nav_sent()$token,key=key,value=value))
    choose("family","core");choose("coverage","90");expect_null(region())
    choose("retention","95");expect_identical(region_id(),"90_95")
    session$setInputs(region="90_95",view="90_95_refined");expect_identical(view_id(),"90_95_refined")
    choose("coverage","80");expect_identical(region_id(),"90_95");expect_identical(view_id(),"90_95_refined")
    choose("retention","100");expect_identical(region_id(),"80_100");expect_identical(view_id(),"80_100_refined")
    expect_match(output$context,"5 retained")
    expect_identical(readRDS(p)$regions,rs)
  })
})
