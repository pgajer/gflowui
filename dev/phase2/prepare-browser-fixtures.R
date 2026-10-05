pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
args<-commandArgs(TRUE);stopifnot(length(args)==1L);p<-normalizePath(args[1])
m<-readRDS(file.path(p,'manifests/comb_fermat_embeddings_01_oct_2026.manifest.rds'));a<-gflowui_classification_asset(m)
g<-readRDS(m$graph_sets[[1]]$graph_file);str(g,max.level=1)
ids<-g$vertex_ids;stopifnot(length(ids)==25042)
expected<-lapply(a$subsets,function(s)which(ids %in% s$ids))
jsonlite::write_json(list(subsets=expected,ids=ids,labels=a$samples$udcst_level2[match(ids,a$samples$sample_id)]),'/tmp/phase2-expected.json',auto_unbox=TRUE)
cat('graph selector fields\n');print(m$metadata$graph_selector_schema)

a2<-readRDS(file.path(p,'projects',m$project_id,'local_views/atlas.rds'))
rr<-Filter(function(r)grepl('Li / 4000',r$label,fixed=TRUE)&&!isTRUE(r$retired),a2$regions)
r<-rr[[1L]]
jsonlite::write_json(list(region=r$id,view=r$views[[1]]$id,ids=r$vertex_ids),'/tmp/phase2-local.json',auto_unbox=TRUE)
sa<-readRDS(m$metadata$source_datasets$file)
d<-names(sort(table(sa$records$dataset),decreasing=TRUE))[2L]
jsonlite::write_json(list(dataset=d,ids=unique(sa$records$vertex_id[sa$records$dataset==d])),'/tmp/phase2-source.json',auto_unbox=TRUE)
