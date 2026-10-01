# Project deletion is a recoverable filesystem transaction. Never fall back to unlink.
gflowui_path_within <- function(path, root) {
  nzchar(root) & (path == root | startsWith(path, paste0(root, "/")))
}

gflowui_project_asset_references <- function(manifest) {
  root <- .as_scalar_chr(manifest$project_root)
  paths <- character()
  walk <- function(x, key="") {
    if (is.list(x)) {
      for (i in seq_along(x)) walk(x[[i]], if(length(names(x))) names(x)[i] else key)
    } else if (is.character(x) && !key %in% c("project_root", "source", "label", "title", "description")) {
      named_path <- grepl("(^|_)(path|paths|file|files|dir|root|csv|rds|rda|bundles)$", key)
      for (p in x[!is.na(x) & nzchar(x)]) {
        if (.is_url_path(p) || (!named_path && !.is_absolute_path(p))) next
        if (!.is_absolute_path(p) && !nzchar(root)) next
        pp <- if(.is_absolute_path(p)) path.expand(p) else file.path(root,p)
        pp <- file.path(normalizePath(dirname(pp),mustWork=FALSE),basename(pp))
        if (file.exists(pp) || nzchar(Sys.readlink(pp))) paths <<- c(paths, pp)
      }
    }
  }
  walk(manifest)
  unique(paths)
}

gflowui_project_managed_paths <- function(id) {
  if (length(id)!=1L || is.na(id) || !grepl("^[[:alnum:]_-]+$",id))
    stop("This project ID cannot safely identify a project storage directory.")
  base <- gflowui_projects_data_dir()
  c(file.path(base,"projects",id),file.path(base,"graphs",id),
    file.path(base,"cache","optimal_k",id),file.path(base,"quadform_layout_cache",quadform_safe_token(id,"project")))
}

gflowui_project_delete_plan <- function(project_id) {
  reg <- gflowui_load_registry(); idx <- match(project_id,reg$id)
  if(is.na(idx)) stop("Project is no longer registered.")
  manifests <- lapply(reg$manifest_file,gflowui_read_manifest)
  if(any(!vapply(manifests,is.list,FALSE)))
    stop("A registered manifest cannot be read. Repair it before deleting assets shared between projects.")
  m <- manifests[[idx]]
  root <- .as_scalar_chr(m$project_root)
  root <- if(nzchar(root)) normalizePath(root,mustWork=FALSE) else ""
  refs <- gflowui_project_asset_references(m)
  managed <- gflowui_project_managed_paths(project_id)
  other_refs <- unique(unlist(lapply(manifests[-idx],gflowui_project_asset_references),use.names=FALSE))
  other_managed <- unlist(lapply(reg$id[-idx],gflowui_project_managed_paths),use.names=FALSE)
  other_refs <- unique(c(other_refs,reg$manifest_file[-idx],other_managed,gflowui_registry_path()))
  # Expand referenced output directories, but never sweep the research project root.
  # Symbolic links are considered separately, never traversed as directories.
  leaves <- function(p) {
    if (nzchar(Sys.readlink(p)) || !dir.exists(p)) return(p)
    kids <- list.files(p,full.names=TRUE,all.files=TRUE,no..=TRUE)
    if(!length(kids)) return(character())
    unlist(lapply(kids,leaves),use.names=FALSE)
  }
  refs <- setdiff(refs,root)
  expand_owned <- function(p) {
    canonical <- normalizePath(p,mustWork=FALSE)
    if ((nzchar(root) && gflowui_path_within(canonical,root)) ||
        any(vapply(managed,function(d)gflowui_path_within(canonical,d),FALSE))) leaves(p) else p
  }
  candidates <- unique(c(reg$manifest_file[idx],
    unlist(lapply(c(refs,managed[file.exists(managed)]),expand_owned),use.names=FALSE)))
  candidates <- candidates[file.exists(candidates) | nzchar(Sys.readlink(candidates))]
  canonical <- vapply(candidates,normalizePath,"",mustWork=FALSE)
  owned <- vapply(canonical,function(p) (nzchar(root) && gflowui_path_within(p,root)) ||
    any(vapply(managed,function(d)gflowui_path_within(p,normalizePath(d,mustWork=FALSE)),FALSE)),FALSE)
  owned[candidates==reg$manifest_file[idx]] <- TRUE
  other_canonical <- unique(normalizePath(other_refs,mustWork=FALSE))
  shared <- vapply(canonical,function(p) any(p==other_canonical |
    startsWith(p,paste0(other_canonical,"/")) |
    startsWith(other_canonical,paste0(p,"/"))),FALSE)
  reason <- ifelse(shared,"Used by another registered project",ifelse(!owned,"External referenced asset","Move to Trash"))
  info <- file.info(candidates)
  files <- data.frame(path=candidates,action=reason,bytes=info$size,modified=as.numeric(info$mtime),stringsAsFactors=FALSE)
  files <- files[order(files$path),,drop=FALSE];rownames(files)<-NULL
  list(project_id=project_id,label=reg$label[idx],files=files,entry=reg[idx,,drop=FALSE],
    manifest=m,registry=reg,key=digest::digest(list(reg,manifests,files),algo="sha256"))
}

gflowui_system_trash <- function(path) {
  # Foundation chooses the correct macOS Trash and handles name collisions.
  if (!identical(Sys.info()[["sysname"]],"Darwin") || !nzchar(Sys.which("swift")))
    stop("System Trash is currently supported on macOS with Swift available. No files were permanently deleted.")
  script <- tempfile(fileext=".swift");on.exit(unlink(script),add=TRUE)
  writeLines(c("import Foundation", "var destination: NSURL?", "do {",
    "try FileManager.default.trashItem(at: URL(fileURLWithPath: CommandLine.arguments[1]), resultingItemURL: &destination)",
    "let data = try JSONSerialization.data(withJSONObject: [\"path\": destination?.path ?? \"\"])",
    "print(String(data:data, encoding:.utf8)!)",
    "} catch { fputs(\"\\(error)\\n\", stderr); exit(1) }"),script)
  errors <- tempfile();on.exit(unlink(errors),add=TRUE)
  output <- system2(Sys.which("swift"),c(shQuote(script),shQuote(path)),stdout=TRUE,stderr=errors)
  if(!is.null(attr(output,"status")) && attr(output,"status")!=0L)
    stop(paste("Unable to move project to system Trash:",paste(readLines(errors,warn=FALSE),collapse=" ")))
  destination <- jsonlite::fromJSON(paste(output,collapse="\n"))$path
  if(!length(destination) || !nzchar(destination)) stop("System Trash did not return its destination.")
  destination
}

gflowui_trash_project <- function(plan, trash=gflowui_system_trash, move=file.rename,
    save_registry=gflowui_basin_atomic_save_rds) {
  fresh <- gflowui_project_delete_plan(plan$project_id)
  if(!identical(plan$key,fresh$key)) stop("The project or its assets changed. Review Delete Project again.")
  files <- plan$files$path[plan$files$action=="Move to Trash"]
  base <- gflowui_projects_data_dir()
  bundle <- tempfile(paste0("Deleted-",plan$project_id,"-"),tmpdir=base)
  if(!dir.create(bundle)) stop("Cannot prepare a recoverable deletion bundle.")
  mapping <- data.frame(original=files,bundle_path=sprintf("assets/%06d-%s",seq_along(files),basename(files)))
  dir.create(file.path(bundle,"assets"))
  saveRDS(list(registry_entry=plan$entry,manifest=plan$manifest,mapping=mapping),file.path(bundle,"recovery.rds"))
  utils::write.csv(mapping,file.path(bundle,"restore-map.csv"),row.names=FALSE)
  writeLines(c("gflowui deleted project",paste("Project:",plan$label),
    "This bundle contains the manifest and project-owned assets. Shared/external assets were left in place.",
    "restore-map.csv maps each bundled file to its original location. Restore files there before registering the saved manifest.",
    "recovery.rds contains the original registry entry, manifest and file mapping."),file.path(bundle,"README.txt"))
  moved <- integer();location <- bundle;completed <- FALSE
  on.exit({
    if(!completed) {
      failed <- character()
      for(i in rev(moved)) {
        src<-file.path(location,mapping$bundle_path[i]);dst<-mapping$original[i]
        if(file.exists(dst) || !file.rename(src,dst)) failed<-c(failed,dst)
      }
      if(length(failed)) warning(paste("Some assets could not be restored; recovery bundle:",location,
        "Original paths:",paste(failed,collapse=", ")),call.=FALSE)
      # Keep the recovery journal even after a successful rollback.
    }
  },add=TRUE)
  for(i in seq_along(files)) {
    if(!isTRUE(move(files[i],file.path(bundle,mapping$bundle_path[i]))))
      stop("Unable to stage an asset for Trash (possibly on another filesystem). Earlier moves are being restored.")
    moved<-c(moved,i)
  }
  location <- trash(bundle)
  # The registry is changed only after the entire bundle reaches Trash.
  if(!identical(gflowui_load_registry(),plan$registry))
    stop("The project registry changed during deletion. Files are being restored; review Delete Project again.")
  reg <- plan$registry[plan$registry$id!=plan$project_id,,drop=FALSE]
  save_registry(reg,gflowui_registry_path())
  completed <- TRUE
  list(trash_path=location,registry=reg,moved=length(files),retained=sum(plan$files$action!="Move to Trash"))
}
