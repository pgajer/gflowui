# Provenance is descriptive metadata. Commands and attached documents are never executed.
gflowui_normalize_provenance <- function(provenance, project_id, project_root="") {
  if(!is.list(provenance)) stop("provenance must be a named list")
  text_fields <- c("summary","data","methods","reproduction","software","seeds","limitations","creator")
  out <- list(schema_version=1L,recorded_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  for(n in text_fields) {
    value <- provenance[[n]] %||% ""
    if(!is.character(value) || anyNA(value)) stop(paste("provenance",n,"must be text"))
    out[[n]] <- paste(value,collapse="\n")
  }
  docs <- .normalize_document_entries(provenance$documents,project_root)
  folder <- file.path(gflowui_project_managed_paths(project_id)[1],"provenance","documents")
  for(i in seq_along(docs)) {
    d <- docs[[i]]
    if(.is_url_path(d$path)) {d$storage<-"remote reference";docs[[i]]<-d;next}
    if(!file.exists(d$path) || dir.exists(d$path)) stop(paste("Provenance document not found:",d$path))
    hash<-digest::digest(file=d$path,algo="sha256")
    dir.create(folder,recursive=TRUE,showWarnings=FALSE)
    target<-file.path(folder,paste0(hash,"-",basename(d$original_path %||% d$path)))
    if(!file.exists(target) && !file.copy(d$path,target)) stop("Unable to attach provenance document")
    if(!identical(digest::digest(file=target,algo="sha256"),hash)) stop("Provenance document copy failed verification")
    d$original_path<-d$original_path %||% d$path;d$path<-target;d$sha256<-hash;d$storage<-"attached snapshot"
    docs[[i]]<-d
  }
  out$documents<-docs
  assets<-provenance$assets %||% list()
  if(is.data.frame(assets))assets<-.rows_to_list(assets)
  if(!is.list(assets))stop("provenance assets must be a list of records or a data frame")
  out$assets<-lapply(assets,function(a){
    if(!is.list(a) || !nzchar(.as_scalar_chr(a$path)))stop("Each provenance asset needs a path")
    a$path<-.normalize_project_path(a$path,project_root)
    a$role<-.as_scalar_chr(a$role,"asset");a$description<-.as_scalar_chr(a$description)
    a$sha256<-.as_scalar_chr(a$sha256)
    if(nzchar(a$sha256) && !grepl("^[a-fA-F0-9]{64}$",a$sha256))stop("Asset sha256 must contain 64 hexadecimal digits")
    a
  })
  out
}

#' Attach provenance and reproduction information to a project
#'
#' @param project_id Registered project identifier.
#' @param provenance Named list with text fields `summary`, `data`, `methods`,
#'   `reproduction`, `software`, `seeds`, `limitations`, and `creator`.
#'   `documents` contains document descriptors (`path`, `label`, optional `id`).
#'   Local documents are copied as verified snapshots; HTTP(S) URLs remain
#'   references. Use self-contained HTML or attach any required companion files.
#'   `assets` is a list or data frame with `path`, `role`, `description`, and
#'   optional `sha256`. Asset files are referenced, not copied or executed.
#' @return Invisibly returns the recorded provenance. Previous records remain
#'   in the project's managed provenance history directory.
#' @export
#' @examples
#' \dontrun{
#' set_project_provenance("my_project", list(
#'   summary="Distances and embeddings of the retained observations",
#'   reproduction="Rscript analysis.R", documents=list(list(path="methods.md"))))
#' }
set_project_provenance <- function(project_id, provenance) {
  reg<-gflowui_load_registry();idx<-match(project_id,reg$id)
  if(is.na(idx))stop("Project is not registered")
  manifest<-gflowui_read_manifest(reg$manifest_file[idx])
  if(!is.list(manifest))stop("Project manifest cannot be read")
  value<-gflowui_normalize_provenance(provenance,project_id,.as_scalar_chr(manifest$project_root))
  if(is.list(manifest$provenance)) {
    history<-file.path(gflowui_project_managed_paths(project_id)[1],"provenance","history")
    dir.create(history,recursive=TRUE,showWarnings=FALSE)
    saveRDS(manifest$provenance,file.path(history,paste0(digest::digest(manifest$provenance,algo="sha256"),".rds")))
  }
  manifest$provenance<-value;manifest$updated_at<-.gflowui_now()
  gflowui_basin_atomic_save_rds(manifest,reg$manifest_file[idx])
  invisible(value)
}

gflowui_provenance_inventory <- function(manifest) {
  explicit<-manifest$provenance$assets %||% list()
  refs<-gflowui_project_asset_references(manifest,include_provenance=FALSE,existing_only=FALSE)
  all<-c(explicit,lapply(setdiff(refs,vapply(explicit,function(a)a$path,"")),function(p)list(path=p,role="Registered asset")))
  if(!length(all))return(data.frame())
  do.call(rbind,lapply(all,function(a){
    remote<-.is_url_path(a$path);present<-!remote && file.exists(a$path)
    data.frame(Role=a$role %||% "asset",Path=a$path,Description=a$description %||% "",
      Status=if(remote)"Remote reference (not checked)" else if(present)"Present" else "Missing",
      Bytes=if(present && !dir.exists(a$path))file.info(a$path)$size else NA_real_,
      SHA256=a$sha256 %||% "",check.names=FALSE)
  }))
}

gflowui_provenance_ui <- function(provenance) {
  if(!is.list(provenance))return(shiny::p("No provenance has been attached. The project creator can supply it when registering or updating this project."))
  labels<-c(summary="Summary",data="Data and selection",methods="Methods",reproduction="Reproduce this project",
    software="Software and revisions",seeds="Random seeds",limitations="Interpretation and limitations",creator="Created by")
  shiny::tagList(shiny::p(paste("Recorded:",provenance$recorded_at)),
    lapply(names(labels),function(n)if(nzchar(provenance[[n]] %||% ""))shiny::tags$details(
      open=if(n=="summary")"open" else NULL,shiny::tags$summary(labels[[n]]),
      shiny::tags$pre(style="white-space:pre-wrap;overflow-wrap:anywhere;",provenance[[n]]))),
    shiny::p("Documents are attached snapshots unless labeled as remote references. Reproduction commands are documentation; they are not run by this viewer."))
}
