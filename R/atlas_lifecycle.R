gflowui_atlas_merge <- function(current,incoming) {
  for(r in incoming) {
    # Recognize legacy imported regions even when descriptive source metadata changes.
    if(identical(r$definition$type,"import") && !r$id %in% names(current)) {
      matches<-Filter(function(old)identical(old$definition$type,"import") &&
        identical(old$definition$source_project,r$definition$source_project) &&
        identical(old$definition$anchor,r$definition$anchor) &&
        identical(old$membership_fingerprint,r$membership_fingerprint) &&
        (old$revision %||% 1L)==1L,current)
      if(length(matches))r$id<-matches[[1]]$id
    }
    old<-current[[r$id]]
    if(!is.null(old)) {
      if(!identical(old$membership_fingerprint,r$membership_fingerprint))stop("Membership changed under an existing region ID; create a revision.")
      views<-old$views %||% list()
      if(length(views))names(views)<-vapply(views,`[[`,"","id")
      for(gs in r$views %||% list())views[[gs$id]]<-gs
      r$views<-views
      for(key in c("vertex_ids","label","family_id","revision","parent_id","retired","retired_at","history","created_at"))
        if(!is.null(old[[key]]))r[[key]]<-old[[key]]
    }
    r$family_id<-r$family_id %||% r$id;r$revision<-r$revision %||% 1L;r$retired<-isTRUE(r$retired)
    current[[r$id]]<-r
  }
  current
}

gflowui_atlas_revision <- function(regions,parent_id,draft,label=NULL) {
  parent<-regions[[parent_id]]
  if(is.null(parent))stop("Choose the saved region to revise.")
  family<-parent$family_id %||% parent$id
  versions<-vapply(Filter(function(r)identical(r$family_id %||% r$id,family),regions),function(r)r$revision %||% 1L,1L)
  revision<-max(versions,1L)+1L
  r<-draft;r$views<-list();r$family_id<-family;r$parent_id<-parent$id;r$revision<-revision;r$retired<-FALSE
  r$id<-paste0("region_",substr(digest::digest(list(family,revision,r$membership_fingerprint,r$definition),algo="sha256"),1,24))
  r$label<-if(!is.null(label)&&nzchar(trimws(label)))trimws(label) else parent$label
  r$created_at<-format(Sys.time(),tz="UTC",usetz=TRUE)
  r$history<-list(list(action="revision",parent_id=parent$id,created_at=r$created_at))
  regions[[r$id]]<-r
  list(regions=regions,region=r)
}

gflowui_atlas_lifecycle <- function(regions,id,action,label=NULL) {
  r<-regions[[id]];if(is.null(r))stop("Choose a saved region.")
  if(!action %in% c("retire","restore","rename"))stop("Unknown region action.")
  when<-format(Sys.time(),tz="UTC",usetz=TRUE)
  if(action=="rename") {
    if(length(label)!=1L || is.na(label)||!nzchar(trimws(label)))stop("Enter a region name.")
    r$label<-trimws(label)
  } else {r$retired<-action=="retire";r$retired_at<-if(r$retired)when else NULL}
  r$history<-c(r$history %||% list(),list(list(action=action,label=r$label,at=when)))
  regions[[id]]<-r;regions
}

gflowui_atlas_region_label <- function(r) {
  paste0(r$label," [revision ",r$revision %||% 1L,if(isTRUE(r$retired))"; retired" else "","]")
}
