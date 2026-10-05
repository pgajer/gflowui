# Deploy only the new catalogue/configuration. Never replace user state with a test copy.
args<-commandArgs(TRUE);stopifnot(length(args)==2)
copy<-normalizePath(args[1]);backup<-args[2]
pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
live<-gflowui_projects_data_dir();id<-'comb_fermat_embeddings_01_oct_2026'
mf<-gflowui_manifest_path(id);before<-readRDS(mf)
stopifnot(is.null(before$metadata$classification_catalogue),!dir.exists(backup))
dir.create(backup,recursive=TRUE)
stopifnot(file.copy(mf,file.path(backup,'manifest.rds')),file.copy(file.path(live,'registry.rds'),file.path(backup,'registry.rds')))
stopifnot(file.copy(file.path(live,'projects',id),backup,recursive=TRUE))
source<-file.path(copy,'projects',id,'classifications/catalogue.rds')
target<-file.path(live,'projects',id,'classifications/catalogue.rds')
stopifnot(!file.exists(target));dir.create(dirname(target),recursive=TRUE)
stopifnot(file.copy(source,target))
# Check no other session wrote a newer manifest during the backup.
stopifnot(identical(readRDS(mf),before))
after<-before
after$metadata$classification_catalogue<-list(file=target,version=1L,palettes=list(),policy='Pure top-rank, fixed full-reference coverage; original feature-order tie rule.')
after$defaults$classification_state<-list(type='udcst',levels=list(udcst='udcst_level2',dcst='dcst_level1'),subset='All',groups=list(),color='udcst')
after$updated_at<-format(Sys.time(),tz='UTC',usetz=TRUE)
gflowui_write_manifest(after,mf)
readback<-readRDS(mf)
check<-readback;check$metadata$classification_catalogue<-NULL;check$defaults$classification_state<-NULL;check$updated_at<-before$updated_at
stopifnot(identical(check,before))
receipt<-list(project_id=id,manifest=mf,backup=normalizePath(backup),deployed_at=after$updated_at,
 before_sha256=digest::digest(before,algo='sha256'),after_sha256=digest::digest(readback,algo='sha256'),
 catalogue_sha256=digest::digest(file=target,algo='sha256'),samples=nrow(gflowui_classification_asset(readback)$samples),
 graph_sets_preserved=identical(before$graph_sets,readback$graph_sets),existing_defaults_preserved=identical(before$defaults,check$defaults),
 preserved_all_other_fields=identical(check,before),live_url='http://127.0.0.1:3874/')
jsonlite::write_json(receipt,file.path(backup,'deployment.json'),pretty=TRUE,auto_unbox=TRUE)
print(receipt)
