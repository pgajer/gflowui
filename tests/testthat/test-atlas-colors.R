test_that("atlas views inherit dataset dCST metadata and edited palettes by ID", {
  root <- withr::local_tempdir()
  parent_table <- data.frame(sample_id=c("a","b","c","d"),
    dcst_level1=c("A","B","A","C"),dcst_level2=c("AA","BB","AB","CC"),dcst_level3=c("AAA","BBB","AAB","CCC"))
  local_table <- data.frame(Li=c(.9,.8,.7))
  save(parent_table,file=file.path(root,"parent.rda"))
  save(local_table,file=file.path(root,"local.rda"))
  ca <- list(metadata_file=file.path(root,"parent.rda"),metadata_object="parent_table",
    vector_columns=c("dcst_level1","dcst_level2","dcst_level3"),
    categorical_palettes=list(dcst_level1=c(A="#112233",B="#445566",C="#778899")))
  local <- list(id="local",color_assets=list(metadata_file=file.path(root,"local.rda"),
    metadata_object="local_table",vector_columns="Li"))
  parent <- list(id="parent",color_assets=ca)
  manifest <- list(graph_sets=list(parent),defaults=list(reference_graph_set_id="parent"))
  region <- gflowui_atlas_region("subset",c("d","a","c"),letters[1:4],list(),list(local))
  merged <- gflowui_atlas_manifest(manifest,region,"local")
  expect_identical(merged$graph_sets[[1]]$color_assets$categorical_palettes,ca$categorical_palettes)
  helper <- gflowui_make_server_renderer_helpers(new.env(),function()NULL)
  sources <- helper$collect_reference_metadata_sources(merged,merged$graph_sets[[1]],3L,c("d","a","c"))
  expect_identical(sources$dcst_level1$values,c("C","A","A"))
  expect_identical(sources$dcst_level2$values,c("CC","AA","AB"))
  expect_identical(sources$dcst_level3$values,c("CCC","AAA","AAB"))
  expect_equal(sources$li$values,c(.9,.8,.7))
  expect_identical(region$views[[1]],local)
  expect_null(local$color_assets$inherited_metadata)
  # No positional fallback if IDs are missing, duplicated, or from another dataset.
  expect_null(gflowui_metadata_match_vertices(parent_table,c("a","unknown")))
  parent_table$sample_id[2] <- "a"
  expect_null(gflowui_metadata_match_vertices(parent_table,c("a","c")))
  expect_null(gflowui_metadata_match_vertices(parent_table,NULL))
  expect_null(gflowui_metadata_match_vertices(data.frame(value=1:3),c("1","2")))
  # Newly computed views with no local metadata still inherit both levels.
  region$views[[1]]$color_assets <- NULL
  merged <- gflowui_atlas_manifest(manifest,region,"local")
  sources <- helper$collect_reference_metadata_sources(merged,merged$graph_sets[[1]],3L,c("d","a","c"))
  expect_identical(sources$dcst_level1$values,c("C","A","A"))
})

test_that("dCST is the default while numeric coloring remains an explicit choice", {
  st <- list(sources=list(dcst_level1=list(values="A"),dcst_level2=list(values="AA")),
    choices=c("dCST level 1"="dcst_level1","dCST level 2"="dcst_level2","Li abundance"="Li"),default_key="Li")
  expect_identical(gflowui_vertex_color_options(st,NULL,"Li")$selected,"dcst")
  expect_identical(gflowui_vertex_color_options(st,NA_character_,"Li")$selected,"dcst")
  expect_identical(gflowui_vertex_color_options(st,"dcst_level2")$selected,"dcst")
  expect_identical(gflowui_vertex_color_options(st,"Li")$selected,"Li")
  expect_identical(gflowui_vertex_color_options(st,"solid_color")$selected,"solid_color")
  expect_true("Li" %in% gflowui_vertex_color_options(st)$choices)
  st$sources <- list(Li=list(values=.9));st$choices<-c("Li abundance"="Li")
  expect_identical(gflowui_vertex_color_options(st,NULL,"Li")$selected,"Li")
})

test_that("dCST source and level persist between a global graph and reordered local fit", {
  root <- withr::local_tempdir()
  withr::local_options(gflowui.projects_data_dir=file.path(root,"registry"))
  meta <- data.frame(sample_id=letters[1:3],dcst_level1=c("A","B","A"),dcst_level2=c("AA","BB","AB"),Li=c(.1,.2,.3))
  save(meta,file=file.path(root,"meta.rda"))
  make_graph <- function(id,ids) {
    f <- file.path(root,paste0(id,".rds"))
    saveRDS(list(X.graphs=list(list(adj_list=list(2L,c(1L,3L),2L),weight_list=list(1,c(1,1),1))),k.values=1L,vertex_ids=ids),f)
    list(id=id,label=id,graph_file=f,k_values=1L)
  }
  parent <- make_graph("parent",letters[1:3])
  parent$color_assets <- list(metadata_file=file.path(root,"meta.rda"),metadata_object="meta",
    vector_columns=c("dcst_level1","dcst_level2","Li"),categorical_palettes=list(dcst_level1=c(A="#112233",B="#445566"),dcst_level2=c(AA="#112233",BB="#445566",AB="#778899")))
  local <- make_graph("local",c("c","b","a"))
  register_project(root,"colors",profile="custom",graph_sets=list(parent),scan_results=FALSE,
    defaults=list(graph_set_id="parent",reference_graph_set_id="parent",reference_k=1L),metadata=list(local_views=list(enabled=TRUE)))
  r <- gflowui_atlas_region("region",letters[1:3],letters[1:3],list(),list(local))
  gflowui_atlas_save(setNames(list(r),r$id),file.path(gflowui_projects_data_dir(),"projects","colors","local_views","atlas.rds"))
  shiny::testServer(app_server,{
    open_project("colors"); session$flushReact()
    expect_identical(reference_renderer_state()$src_key,"dcst_level1")
    session$setInputs(graph_layout_color_by="dcst",graph_dcst_level="dcst_level2")
    session$setInputs(`local_atlas-region`=r$id,`local_atlas-view`="local")
    expect_identical(reference_renderer_state()$src_key,"dcst_level2")
    expect_identical(reference_view_state()$sources$dcst_level2$values,c("AB","BB","AA"))
    expect_identical(graph_structure_state()$color_selected,"dcst")
    session$setInputs(graph_dcst_table_color=list(project="colors",level="dcst_level2",group="AA",color="#aabbcc"))
    expect_identical(reference_view_state()$graph_set$color_assets$categorical_palettes$dcst_level2[["AA"]],"#aabbcc")
    session$setInputs(`local_atlas-whole`=1)
    expect_identical(reference_view_state()$graph_set$color_assets$categorical_palettes$dcst_level2[["AA"]],"#aabbcc")
    expect_identical(reference_renderer_state()$src_key,"dcst_level2")
    session$setInputs(graph_layout_color_by="li")
    expect_identical(reference_renderer_state()$src_key,"li")
    session$setInputs(graph_layout_color_by="dcst")
    expect_identical(reference_renderer_state()$src_key,"dcst_level2")
  })
})
