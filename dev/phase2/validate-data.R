args<-commandArgs(TRUE);stopifnot(length(args)==1)
pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
m<-readRDS(file.path(args[1],'manifests/comb_fermat_embeddings_01_oct_2026.manifest.rds'))
a<-gflowui_classification_asset(m)
metrics<-c('Euclidean (L¹-normalized)','Euclidean (square-root)')
routes<-c('Direct 3D MDS','10D MDS → PCA 3D')
sets<-Filter(function(g)g$base_metric %in% metrics && g$embedding_route %in% routes && g$construction=='Ambient (complete p=1)',m$graph_sets)
stopifnot(length(sets)==4L)
checks<-list()
for(gs in sets){
 g<-readRDS(gs$graph_file);ids<-g$vertex_ids
 for(key in names(a$subsets)){
  ii<-gflowui_classification_subset(a,key,ids)
  stopifnot(setequal(ids[ii],a$subsets[[key]]$ids))
  checks[[length(checks)+1L]]<-list(metric=gs$base_metric,route=gs$embedding_route,subset=key,n=length(ii))
 }
}
# Canonical pair orientation includes both dominance directions.
hover<-gflowui_vertex_hover_asset(m)
pair<-a$pairs[a$pairs$group=='Lactobacillus_crispatus + Lactobacillus_iners',]
ids<-a$samples$sample_id[a$samples$udcst_level2==pair$group]
x<-gflowui_pair_coordinates(hover,ids,pair$a,pair$b)
stopifnot(any(x$t<.5),any(x$t>.5),all(is.finite(x$t)),all(x$r>=0))
atlas<-readRDS(file.path(args[1],'projects',m$project_id,'local_views/atlas.rds'))
str(atlas,max.level=1)
local_checks<-lapply(atlas$regions,function(r){
 st<-gflowui_classification_augment(list(vertex_ids=r$vertex_ids,sources=list()),m)
 stopifnot(identical(st$sources$udcst_level2$values,a$samples$udcst_level2[match(r$vertex_ids,a$samples$sample_id)]))
 list(region=r$id,n=length(r$vertex_ids),matched=sum(!is.na(st$sources$udcst_level2$values)))
})
jsonlite::write_json(list(global=checks,local=local_checks,canonical_pair=list(n=nrow(x),dominance_A=sum(x$t<.5),dominance_B=sum(x$t>.5))),'/tmp/phase2-data-validation.json',pretty=TRUE,auto_unbox=TRUE)
cat(length(checks),'global combinations and',length(local_checks),'local-region identity checks passed\n')
