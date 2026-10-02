gflowui_atlas_write_bundle <- function(spec,folder,diagnostics) {
  data<-readRDS(spec$input_file)
  # The bundle carries frozen inputs; the original source path is provenance,
  # not a dependency needed to replay the job elsewhere.
  utils::write.csv(data.frame(vertex_id=rownames(data$X)),file.path(folder,"membership.csv"),row.names=FALSE)
  utils::write.csv(data.frame(feature=colnames(data$X)),file.path(folder,"features.csv"),row.names=FALSE)
  provenance<-list(schema=1L,project_id=spec$project_id,namespace=spec$namespace,region=data$region,
    distance_scope="within_region",parameters=spec$parameters,source_file=spec$source_file,source_hash=spec$source_hash,
    input_hash=spec$input_hash,software=spec$software,diagnostics=diagnostics,
    reproducibility="Frozen input and exact atlas function source included. Package binaries/versions must match; no convergence or biological-validity claim.")
  jsonlite::write_json(provenance,file.path(folder,"provenance.json"),pretty=TRUE,auto_unbox=TRUE,null="null",na="null",digits=NA)
  source_file<-file.path(folder,"atlas-source.R")
  if(!is.null(spec$replay_snapshot))file.copy(spec$replay_snapshot,source_file,overwrite=TRUE)
  else {
    ns<-asNamespace("gflowui"); names<-sort(grep("^gflowui_atlas",ls(ns,all.names=TRUE),value=TRUE),method="radix")
    lines<-c("# Exact atlas function snapshot used by this calculation.","`%||%` <- function(x,y) if(is.null(x)) y else x")
    for(name in names)if(is.function(get(name,ns)))lines<-c(lines,paste0(name," <- "),deparse(get(name,ns),width.cutoff=120),"")
    writeLines(lines,source_file)
  }
  writeLines(c("# Run: Rscript reproduce.R /absolute/path/to/new-output-directory",
    "args <- commandArgs(trailingOnly=TRUE)",
    "script <- sub('^--file=', '', commandArgs()[grep('^--file=',commandArgs())][1])",
    "bundle <- dirname(normalizePath(script))",
    "if(length(args)!=1L)stop('Supply a new output directory.')",
    "manifest <- readRDS(file.path(bundle, 'bundle-manifest.rds'))",
    "source <- file.path(bundle, 'atlas-source.R')",
    "if(!identical(digest::digest(file=source,algo='sha256'),unname(manifest$files['atlas-source.R'])))stop('Bundled source checksum verification failed.')",
    "env <- new.env(parent=globalenv())", "sys.source(file.path(bundle, 'atlas-source.R'), envir=env)", "env$gflowui_atlas_replay(bundle, args[1])"),file.path(folder,"reproduce.R"))
  text<-c("# Local-view reproducibility bundle","",
    "This bundle contains frozen region data, membership and feature IDs, exact calculation settings and seeds, landmark IDs and targets, graph and repair metadata, embedding diagnostics, software fingerprints and the atlas function source used for this view.","",
    "Recompute with `Rscript reproduce.R /absolute/path/to/new-output-directory`. The output directory must be empty. The bundled source supplies the replay loader; required R and calculation-package binaries must match the recorded environment. Execution uses the included atlas source snapshot. No access to the original dataset file is required.","",
    "Distances were recomputed within the frozen region. Chart coverage files, when present, record every retained and uncovered sample. An explicit chart exclusion changes the fitted view, not the saved region membership. MST repair edges and original components are retained in diagnostics.rds. A display-only edge cap does not change graph paths or target distances.","",
    "Metric-MDS uses a fixed SGD iteration budget and inverse-squared distance weighting. Landmark mode uses only landmark-to-all constraints. Inspect warnings and target-reconstruction diagnostics before interpreting an embedding. These diagnostics do not establish biological truth.","",
    "SHA-256 checksums in bundle-manifest.rds cover all replay inputs and view assets. Generated worker logs/status are outside the immutable bundle.")
  writeLines(text,file.path(folder,"README.md"))
  html<-paste0("<!doctype html><meta charset='utf-8'><title>Local-view reproducibility</title><style>body{font:17px/1.6 system-ui;max-width:900px;margin:3em auto;padding:1em}pre{white-space:pre-wrap}</style><pre>",htmltools::htmlEscape(paste(text,collapse="\n")),"</pre>")
  writeLines(html,file.path(folder,"README.html"))
  files<-c("input.rds","spec.rds","membership.csv","features.csv","graph.rds","layout.rds","targets.rds","diagnostics.rds","edge_overlays.rds",
    "provenance.json","atlas-source.R","reproduce.R","README.md","README.html",if(file.exists(file.path(folder,"chart.rds")))c("chart.rds","chart_coverage.csv"))
  hashes<-stats::setNames(vapply(file.path(folder,files),function(f)digest::digest(file=f,algo="sha256"),""),files)
  gflowui_atlas_atomic(list(version=1L,files=hashes,software=spec$software,key=spec$key),file.path(folder,"bundle-manifest.rds"))
  c(files,"bundle-manifest.rds")
}

gflowui_atlas_verify_bundle <- function(folder) {
  b<-readRDS(file.path(folder,"bundle-manifest.rds"))
  if(any(basename(names(b$files))!=names(b$files)))stop("Bundle contains invalid file names.")
  files<-file.path(folder,names(b$files))
  if(!all(file.exists(files)))stop("Bundle files are missing.")
  hashes<-vapply(files,function(f)digest::digest(file=f,algo="sha256"),"")
  if(!identical(unname(hashes),unname(b$files)))stop("Bundle checksum verification failed.")
  b
}

gflowui_atlas_export_bundle <- function(folder,file) {
  b<-gflowui_atlas_verify_bundle(folder)
  zip::zipr(zipfile=file,files=c(names(b$files),"bundle-manifest.rds"),root=folder)
  invisible(file)
}

gflowui_atlas_replay <- function(bundle,output) {
  bundle<-normalizePath(bundle,mustWork=TRUE);b<-gflowui_atlas_verify_bundle(bundle)
  if(dir.exists(output)&&length(list.files(output,all.files=TRUE,no..=TRUE)))stop("Replay output directory must be empty.")
  current<-gflowui_atlas_software()
  dependency_versions<-function(x)x[names(x)!="gflowui"]
  dependency_binaries<-function(x)x[!startsWith(names(x),"gflowui/")]
  if(!identical(b$software$R,current$R)||!identical(dependency_versions(b$software$packages),dependency_versions(current$packages))||
     !identical(dependency_binaries(b$software$binaries),dependency_binaries(current$binaries)))
    stop("Replay requires the recorded R/package versions and binary fingerprints. See provenance.json.")
  env<-new.env(parent=globalenv());sys.source(file.path(bundle,"atlas-source.R"),envir=env)
  spec<-readRDS(file.path(bundle,"spec.rds"));dir.create(output,recursive=TRUE,showWarnings=FALSE);output<-normalizePath(output)
  file.copy(file.path(bundle,"input.rds"),file.path(output,"input.rds"))
  spec$input_file<-file.path(output,"input.rds");spec$replay_snapshot<-file.path(bundle,"atlas-source.R")
  gflowui_atlas_atomic(spec,file.path(output,"spec.rds"))
  env$gflowui_atlas_compute(spec,output)
}
