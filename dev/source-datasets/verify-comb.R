options(rgl.useNULL=TRUE)
pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
shiny::testServer(gflowui:::app_server,{
  open_project('comb_fermat_embeddings_01_oct_2026');session$flushReact()
  s<-reference_view_state()
  stopifnot(length(s$vertex_ids)==25042,'source_dataset' %in% names(s$sources))
  session$setInputs(`source_datasets-datasets`='HMP')
  stopifnot(length(reference_renderer_state()$keep_idx)==3613)
  session$setInputs(`source_datasets-clear_datasets`=1,`source_datasets-show`=TRUE)
  xy<-source_datasets$visible_coordinates();stopifnot(nrow(xy)>0,all(xy$r>=0),all(xy$t>=0&xy$t<=1))
  session$setInputs(`source_datasets-plotly_selected-within_dcst`=jsonlite::toJSON(data.frame(key=xy$vertex_id[1:3])))
  stopifnot(length(source_datasets$selected())==3)
  session$setInputs(`source_datasets-only_selected`=TRUE)
  stopifnot(length(reference_renderer_state()$keep_idx)==3)
  session$setInputs(`source_datasets-only_selected`=FALSE)
  m<-atlas_base_manifest()
  atlas<-readRDS(file.path(gflowui:::gflowui_projects_data_dir(),'projects',m$project_id,'local_views','atlas.rds'))$regions
  rs<-Filter(function(x)grepl('Li / 4000',x$label,fixed=TRUE),atlas)
  r<-rs[[1]];v<-r$views[[1]]
  session$setInputs(`local_atlas-region`=r$id,`local_atlas-view`=v$id)
  st<-reference_view_state()
  stopifnot(length(st$vertex_ids)==4000,'source_dataset' %in% names(st$sources))
  session$setInputs(`source_datasets-datasets`='HMP')
  aa<-gflowui:::gflowui_source_asset(m)
  expected<-gflowui:::gflowui_source_filter(aa$records,st$vertex_ids,'HMP')
  stopifnot(identical(reference_renderer_state()$keep_idx,expected))
  session$setInputs(graph_dcst_level='dcst_level2',graph_dcst_table_selection=list(project=m$project_id,level='dcst_level2',groups='Lactobacillus_iners → Gardnerella_vaginalis'))
  expected<-intersect(expected,which(st$sources$dcst_level2$values=='Lactobacillus_iners → Gardnerella_vaginalis'))
  stopifnot(identical(reference_renderer_state()$keep_idx,expected),length(source_datasets$selected())==3)
  session$setInputs(`source_datasets-clear_datasets`=2,
    graph_dcst_table_selection=list(project=m$project_id,level='dcst_level2',
      groups=c('Lactobacillus_iners → Gardnerella_vaginalis','Lactobacillus_iners → Lactobacillus_crispatus')))
  both<-reference_renderer_state()$keep_idx
  plot<-source_datasets$visible_coordinates()
  stopifnot(length(both)==1378,setequal(plot$vertex_id,st$vertex_ids[both]),length(unique(plot$dcst))==2)
  session$setInputs(`source_datasets-coordinate_mode`='homogeneous')
  plot<-source_datasets$visible_coordinates()
  stopifnot(nrow(plot)==1378,all(is.finite(plot$rho)),all(plot$rho>=0),
    isTRUE(all.equal(plot$u,plot$b/plot$a)),setequal(plot$vertex_id,st$vertex_ids[both]))
  session$setInputs(graph_dcst_table_selection=list(project=m$project_id,level='dcst_level2',groups=character()))
  stopifnot(nrow(source_datasets$visible_coordinates())==4000)
  level3 <- gflowui:::gflowui_dcst_options(st$sources,'dcst_level3')
  selected3 <- head(level3$groups,2)
  session$setInputs(graph_dcst_level='dcst_level3',graph_layout_color_by='dcst',
    graph_dcst_table_selection=list(project=m$project_id,level='dcst_level3',groups=selected3))
  expected3 <- which(st$sources$dcst_level3$values %in% selected3)
  stopifnot(identical(reference_renderer_state()$keep_idx,expected3),
    identical(reference_renderer_state()$src_key,'dcst_level3'),
    setequal(source_datasets$visible_coordinates()$vertex_id,st$vertex_ids[expected3]))
  cat('PASS: level 3 local filtering and linked 2D/3D IDs:',length(expected3),'vertices in two groups.\n')
  cat('PASS: two selected Li4000 dCSTs share 1378 vertices across 2D/3D in both coordinate systems; clearing selects all 4000.\n')
  cat('PASS: global HMP 3613; local Li4000 HMP+dCST intersection',length(expected),'; stable selection and original-abundance coordinates.\n')
})
