# Rebuild the small ID-keyed annotation asset; no embeddings are recomputed.
# Run with Rscript dev/source-datasets/register-comb.R from this checkout.
pkgload::load_all(".", quiet=TRUE)
id <- "comb_fermat_embeddings_01_oct_2026"
path <- gflowui:::gflowui_manifest_path(id)
m <- gflowui:::gflowui_read_manifest(path)
source <- "/Users/pgajer/current_projects/dgraphs/reports/sequencing_count_geometry/results/unique_taxon_compositions/record_to_composition.tsv"
x <- read.delim(source,check.names=FALSE,stringsAsFactors=FALSE)
records <- data.frame(vertex_id=x$composition_id,record_id=x$record_id,dataset=x$study_id)
records <- gflowui:::gflowui_validate_source_records(records)
a <- gflowui:::gflowui_vertex_hover_asset(m)
stopifnot(setequal(records$vertex_id,a$sample_ids))
gs <- m$graph_sets[[1]]
e <- new.env(parent=emptyenv());load(gs$color_assets$metadata_file,envir=e)
meta <- e[[gs$color_assets$metadata_object]]
groups <- sort(unique(as.character(meta$dcst_level2)))
parts <- strsplit(groups," → ",fixed=TRUE)
valid <- vapply(parts,function(p)length(p)==2L && all(p %in% a$taxon_names) && p[1]!=p[2],logical(1))
pairs <- data.frame(group=groups[valid],a=vapply(parts[valid],`[[`,"",1),b=vapply(parts[valid],`[[`,"",2))
asset <- list(version=1L,records=records,pairs=pairs,
  provenance=list(record_mapping=source,record_mapping_sha256=digest::digest(file=source,algo="sha256"),
    classification_file=gs$color_assets$metadata_file,
    pair_definition="Only labels containing exactly two explicit abundance feature names, in label order. Other merged labels excluded.",
    counts=c(vertices=length(unique(records$vertex_id)),records=nrow(records),datasets=length(unique(records$dataset))),
    excluded_groups=groups[!valid]))
out <- file.path(gflowui:::gflowui_projects_data_dir(),"projects",id,"source_datasets","annotations.rds")
dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
saveRDS(asset,out)
backup <- paste0(path,".before_source_datasets_02_oct_2026")
if(!file.exists(backup))file.copy(path,backup)
m$metadata$source_datasets <- list(file=out,palette=m$metadata$source_datasets$palette,
  description="Source-record membership and explicit two-phylotype definitions for linked within-dCST views.")
m$artifacts$source_datasets <- out
m$artifacts$source_datasets_methods <- normalizePath("dev/source-datasets/README.html",mustWork=FALSE)
gflowui:::gflowui_write_manifest(m,path)
cat(sprintf("Registered %d records, %d vertices, %d datasets, %d explicit pairs; %d other dCST groups excluded.\n",nrow(records),length(unique(records$vertex_id)),length(unique(records$dataset)),nrow(pairs),sum(!valid)))
print(gflowui:::gflowui_source_summary(records,a$sample_ids),row.names=FALSE)
