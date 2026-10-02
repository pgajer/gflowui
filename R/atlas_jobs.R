.gflowui_atlas_processes <- new.env(parent=emptyenv())

gflowui_atlas_job_status <- function(folder) {
  file<-file.path(folder,"status.rds")
  if(file.exists(file))tryCatch(readRDS(file),error=function(e)list(state="unreadable",message=conditionMessage(e))) else list(state="missing")
}

gflowui_atlas_job_result <- function(folder) {
  file<-file.path(folder,"result.rds")
  if(!file.exists(file))return(NULL)
  r<-tryCatch(readRDS(file),error=function(e)NULL)
  if(is.null(r) || !length(r$checksums))return(NULL)
  files<-file.path(folder,names(r$checksums))
  if(!all(file.exists(files)))return(NULL)
  actual<-vapply(files,function(f)digest::digest(file=f,algo="sha256"),"")
  if(!identical(unname(actual),unname(r$checksums)))return(NULL)
  r
}

gflowui_atlas_enqueue <- function(manifest,region,parameters,root) {
  if(is.null(region) || isTRUE(region$retired))stop("Choose an active saved region.")
  p<-gflowui_atlas_parameters(parameters,length(region$vertex_ids))
  asset<-gflowui_vertex_hover_asset(manifest)
  if(8*length(region$vertex_ids)*length(asset$taxon_names)*4 > p$memory_mb*1024^2)
    stop("Frozen input allocation exceeds the memory budget. Use a smaller region or raise the budget.")
  data<-gflowui_atlas_input(asset,region,if(p$coordinates=="anchor_chart")p$chart_anchor else "")
  gflowui_atlas_prepare_coordinates(data,p) # Fail before queueing if coverage is unacceptable.
  source_file<-manifest$metadata$vertex_hover$abundances_file
  if(!grepl("^(/|[A-Za-z]:)",source_file))source_file<-file.path(manifest$project_root,source_file)
  spec<-list(version=1L,project_id=manifest$project_id,region_id=region$id,
    namespace=manifest$metadata$local_views$vertex_namespace,parameters=p,
    input_hash=digest::digest(data,algo="sha256"),source_file=normalizePath(source_file),
    source_hash=digest::digest(file=source_file,algo="sha256"),software=gflowui_atlas_software())
  spec$key<-digest::digest(spec,algo="sha256")
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  folders<-list.dirs(root,recursive=FALSE,full.names=TRUE)
  for(f in folders) {
    sf<-file.path(f,"spec.rds"); if(!file.exists(sf))next
    old<-readRDS(sf); st<-gflowui_atlas_job_status(f)
    if(identical(old$key,spec$key) && st$state %in% c("queued","running"))return(list(folder=f,cached=FALSE,reused=TRUE))
    if(identical(old$key,spec$key) && identical(st$state,"complete")) {
      if(!is.null(gflowui_atlas_job_result(f)))return(list(folder=f,cached=TRUE,reused=TRUE))
      gflowui_atlas_atomic(list(state="invalidated",stage="Cache checksum failed; retained for inspection.",fraction=NA_real_),file.path(f,"status.rds"))
    }
  }
  folder<-tempfile(paste0(substr(spec$key,1,16),"-"),tmpdir=root)
  dir.create(folder); spec$input_file<-file.path(folder,"input.rds")
  gflowui_atlas_atomic(data,spec$input_file)
  gflowui_atlas_atomic(spec,file.path(folder,"spec.rds"))
  gflowui_atlas_atomic(list(state="queued",stage="Waiting",fraction=0,created=as.character(Sys.time())),file.path(folder,"status.rds"))
  list(folder=folder,cached=FALSE,reused=FALSE)
}

gflowui_atlas_worker <- function(folder) {
  progress<-function(stage,fraction)gflowui_atlas_atomic(list(state=if(fraction==1)"complete" else "running",stage=stage,fraction=fraction,
    pid=Sys.getpid(),updated=as.character(Sys.time())),file.path(folder,"status.rds"))
  tryCatch({
    spec<-readRDS(file.path(folder,"spec.rds"))
    actual_software<-gflowui_atlas_software()
    if(!identical(spec$software,actual_software))stop("Software changed since submission. Submit a new job using the current version. ",paste(all.equal(spec$software,actual_software),collapse="; "))
    gflowui_atlas_compute(spec,folder,progress)
  },error=function(e) {
    gflowui_atlas_atomic(list(state="failed",stage=conditionMessage(e),fraction=NA_real_,updated=as.character(Sys.time())),file.path(folder,"status.rds"))
    NULL
  })
}

gflowui_atlas_dispatch <- function(root) {
  if(!requireNamespace("callr",quietly=TRUE))stop("Install callr to run background calculations.")
  # Handles are shared across sessions in this app process. Reloading a browser
  # does not cancel a job; callr's supervisor stops children when the app exits.
  keys<-ls(.gflowui_atlas_processes)
  for(k in keys) {
    process<-get(k,.gflowui_atlas_processes)
    if(process$is_alive())return(invisible(NULL))
    if(identical(gflowui_atlas_job_status(k)$state,"running"))gflowui_atlas_atomic(
      list(state="failed",stage="Worker exited before completion; inspect worker.log.",fraction=NA_real_),file.path(k,"status.rds"))
    rm(list=k,envir=.gflowui_atlas_processes)
  }
  folders<-unlist(lapply(root,function(r)list.dirs(r,recursive=FALSE,full.names=TRUE)))
  for(folder in folders) {
    st<-gflowui_atlas_job_status(folder)
    if(identical(st$state,"running") && !folder %in% ls(.gflowui_atlas_processes)) {
      # A new app process cannot own the old supervised worker. Do not publish
      # orphan output; keep its evidence and require explicit resubmission.
      claim<-file.path(folder,"claim","owner.rds")
      owner<-if(file.exists(claim))readRDS(claim) else NA_integer_
      alive<-is.finite(owner) && isTRUE(tryCatch(tools::pskill(owner,signal=0L),error=function(e)FALSE))
      if(!alive)gflowui_atlas_atomic(list(state="interrupted",stage="App stopped before job completion; submit again.",fraction=NA_real_),file.path(folder,"status.rds"))
    }
    if(!identical(st$state,"queued"))next
    claim<-file.path(folder,"claim")
    if(!dir.create(claim,showWarnings=FALSE))next
    saveRDS(Sys.getpid(),file.path(claim,"owner.rds"))
    gflowui_atlas_atomic(list(state="running",stage="Starting R worker",fraction=0),file.path(folder,"status.rds"))
    source_path<-getNamespaceInfo(asNamespace("gflowui"),"path")
    dev<-file.exists(file.path(source_path,"R","atlas_compute.R"))
    process<-tryCatch(callr::r_bg(function(path,dev,folder) {
      if(dev)pkgload::load_all(path,quiet=TRUE) else library(gflowui)
      gflowui:::gflowui_atlas_worker(folder)
    },args=list(path=source_path,dev=dev,folder=folder),libpath=.libPaths(),
      stdout=file.path(folder,"worker.log"),stderr="2>&1",supervise=TRUE),error=function(e)e)
    if(inherits(process,"error"))gflowui_atlas_atomic(list(state="failed",stage=conditionMessage(process),fraction=NA_real_),file.path(folder,"status.rds"))
    else assign(folder,process,.gflowui_atlas_processes)
    break
  }
  invisible(NULL)
}

gflowui_atlas_cancel <- function(folder) {
  st<-gflowui_atlas_job_status(folder)
  if(!st$state %in% c("queued","running"))return(invisible(FALSE))
  if(exists(folder,.gflowui_atlas_processes,inherits=FALSE)) {
    process<-get(folder,.gflowui_atlas_processes);process$kill();process$wait(timeout=5000)
    if(process$is_alive())stop("Worker has not stopped yet; cancellation not confirmed.")
    rm(list=folder,envir=.gflowui_atlas_processes)
  } else if(st$state=="running")stop("This job belongs to another app process. Cancel it in that app.")
  gflowui_atlas_atomic(list(state="cancelled",stage="Cancelled; partial assets are not registered.",fraction=NA_real_),file.path(folder,"status.rds"))
  invisible(TRUE)
}

# Exactly one successful attempt per scientific key can be published. Superseded
# attempts retain their assets and logs, but never repeatedly replace the winner.
gflowui_atlas_winners <- function(folders) {
  ready<-Filter(function(f)identical(gflowui_atlas_job_status(f)$state,"complete"),folders)
  if(!length(ready))return(character())
  keys<-vapply(ready,function(f)readRDS(file.path(f,"spec.rds"))$key,"")
  winners<-character()
  for(group in split(ready,keys)) {
    ordered<-group[order(file.info(file.path(group,"result.rds"))$mtime,decreasing=TRUE,na.last=TRUE)]
    winner<-NULL
    for(f in ordered) {
      receipt<-file.path(f,"publication.rds")
      valid<-file.exists(receipt) || !is.null(gflowui_atlas_job_result(f))
      if(valid){winner<-f;break}
      gflowui_atlas_atomic(list(state="invalidated",stage="Output checksums failed; rerun this calculation.",fraction=NA_real_),file.path(f,"status.rds"))
    }
    if(!is.null(winner)) {
      winners<-c(winners,winner)
      for(f in setdiff(group,winner))gflowui_atlas_atomic(list(state="superseded",stage="A newer validated attempt is available.",fraction=1),file.path(f,"status.rds"))
    }
  }
  winners
}
