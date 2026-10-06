# Browsing categories are derived from region definitions, never project names.
gflowui_atlas_catalog <- function(regions, show_retired = FALSE) {
  if(!length(regions))return(list())
  families<-vapply(regions,function(r)r$family_id %||% r$id,"")
  out<-lapply(unique(families),function(id){
    versions<-regions[families==id]
    versions<-versions[order(vapply(versions,function(r)as.integer(r$revision %||% 1L),1L))]
    origin<-versions[[1]];d<-origin$definition
    visible<-Filter(function(r)show_retired || !isTRUE(r$retired),versions)
    if(!length(visible))return(NULL)
    latest<-visible[[length(visible)]]
    family<-if((d$type %||% "") %in% c("anchor","import") && length(d$anchor))"anchor" else if(identical(d$type,"dcst") && length(d$groups)==1L)"dcst" else if(identical(d$type,"udcst") && length(d$groups)==1L)"udcst" else if(identical(d$type,"coverage_core"))"core" else "custom"
    definition<-if(identical(d$type,"import"))d$selection %||% "Imported membership" else d$metric %||% "Saved membership"
    group<-as.character(d$groups %||% "")
    list(id=id,family=family,coverage=as.character(d$coverage %||% ""),retention=as.character(d$retention %||% ""),anchor=as.character(d$anchor %||% ""),
      anchor_label=as.character(d$anchor_label %||% d$anchor %||% ""),
      size=as.character(d$size %||% length(origin$vertex_ids)),definition=definition,
      level=as.character(d$level %||% ""),group=group,group_label=as.character(d$group_label %||% group),
      first=if(length(group)==1L)trimws(strsplit(group,"\\s*(?:→|->)\\s*",perl=TRUE)[[1]][1]) else "",
      label=latest$label,n=length(latest$vertex_ids),versions=visible)
  })
  out<-Filter(Negate(is.null),out)
  stats::setNames(out,vapply(out,`[[`,"","id"))
}

gflowui_atlas_navigation_fields <- function() c(family="Region family",coverage="Cell coverage",retention="Within-cell retention",anchor="Anchor",size="Neighborhood size",
 definition="Membership definition",level="dCST level",first="First phylotype",region="Region",revision="Revision")

# Resolve only unambiguous single choices. Missing multi-choice fields stay empty;
# the displayed region is not changed while the user is narrowing the catalogue.
gflowui_atlas_navigation_resolve <- function(catalog, state=list(family="whole"), draft=FALSE) {
  controls<-list();s<-state
  add<-function(key,choices,label=NULL,always=FALSE,default=NULL){
    choices<-choices[!duplicated(unname(choices))]
    value<-s[[key]] %||% ""
    if(!value %in% unname(choices))value<-if(!is.null(default))default else if(length(choices)==1L)unname(choices[1]) else ""
    s[[key]]<<-value
    controls[[key]]<<-list(key=key,label=label %||% gflowui_atlas_navigation_fields()[[key]],choices=choices,selected=value,visible=always || length(choices)>1L)
    value
  }
  available<-unique(vapply(catalog,`[[`,"","family"))
  families<-c("Whole dataset"="whole","Anchor neighborhoods"="anchor","dCST regions"="dcst","udCST regions"="udcst","Precomputed cores"="core","Combined / custom regions"="custom")
  families<-families[unname(families)%in%c("whole",available)]
  if(draft)families<-c(families,"Unsaved membership preview"="draft")
  family<-add("family",families,always=TRUE,default="whole")
  target<-NULL
  if(family=="whole")target<-"" else if(family=="draft")target<-"__draft__" else {
    rows<-Filter(function(r)r$family==family,catalog)
    facet<-function(key,label=NULL,always=FALSE,default=NULL){
      vals<-unique(vapply(rows,function(r)r[[key]],""))
      vals<-if(key=="size")vals[order(as.numeric(vals))] else if(key%in%c("coverage","retention"))vals[order(-as.numeric(vals))] else sort(vals)
      labels<-vals
      if(key=="coverage")labels<-paste0(vals,"%")
      if(key=="retention")labels<-ifelse(vals=="100","All — no residual filter",paste0(vals,"% closest to subspace"))
      if(key=="anchor")labels<-vapply(vals,function(a)rows[[which(vapply(rows,function(r)r$anchor==a,FALSE))[1]]]$anchor_label,"")
      if(key=="level")labels<-paste("Level",sub("^dcst_level","",vals))
      if(key=="definition")labels<-vapply(vals,function(x)switch(x,euclidean="Euclidean neighbors",hellinger="Hellinger neighbors",jensen_shannon="Jensen–Shannon neighbors",x),"")
      add(key,stats::setNames(vals,labels),label,always,default)
    }
    if(family=="core"){
      for(key in c("coverage","retention")){
        v<-facet(key,always=TRUE);rows<-Filter(function(r)identical(r[[key]],v),rows)
        if(!length(rows))break
      }
    } else if(family=="anchor"){
      for(key in c("anchor","size","definition")){
        v<-facet(key);rows<-Filter(function(r)identical(r[[key]],v),rows)
        if(!length(rows))break
      }
    } else if(family=="udcst"){
      v<-facet("level",label="udCST level");rows<-Filter(function(r)r$level==v,rows)
    } else if(family=="dcst"){
      v<-facet("level");rows<-Filter(function(r)r$level==v,rows)
      if(length(rows) && !v%in%c("1","dcst_level1")){
        vals<-sort(unique(vapply(rows,`[[`,"","first")))
        first<-add("first",c("All"="__all__",stats::setNames(vals,gsub("_"," ",vals))),default="__all__")
        if(first!="__all__")rows<-Filter(function(r)r$first==first,rows)
      }
    }
    if(length(rows)){
      rows<-rows[order(-vapply(rows,`[[`,1L,"n"),vapply(rows,`[[`,"","label"),names(rows))]
      label<-if(family=="dcst")"dCST" else if(family=="udcst")"udCST" else "Saved region"
      captions<-vapply(rows,function(r)paste0(if(family%in%c("dcst","udcst"))gsub("_"," ",r$group_label) else r$label," (",format(r$n,big.mark=",",trim=TRUE),")"),"")
      if(anyDuplicated(captions))captions<-paste0(captions," · ",seq_along(captions))
      chosen<-add("region",stats::setNames(names(rows),captions),label)
      if(nzchar(chosen)){
        versions<-rows[[chosen]]$versions
        versions<-versions[order(-vapply(versions,function(r)as.integer(r$revision %||% 1L),1L))]
        captions<-vapply(versions,function(r)paste0("Revision ",r$revision %||% 1L," — ",length(r$vertex_ids)," members",if(isTRUE(r$retired))" (retired)" else ""),"")
        target<-add("revision",stats::setNames(vapply(versions,`[[`,"","id"),captions),default=versions[[1]]$id)
      }
    }
  }
  list(state=s,controls=controls,target=target)
}

gflowui_atlas_navigation_state <- function(catalog,id) {
  if(!nzchar(id))return(list(family="whole"))
  if(id=="__draft__")return(list(family="draft"))
  for(r in catalog)if(id %in% vapply(r$versions,`[[`,"","id"))
    return(list(family=r$family,coverage=r$coverage,retention=r$retention,anchor=r$anchor,size=r$size,definition=r$definition,
      level=r$level,first="__all__",region=r$id,revision=id))
  list(family="whole")
}

# Discard downstream choices before applying a new upstream selection.
gflowui_atlas_navigation_change <- function(state,key,value) {
  fields<-names(gflowui_atlas_navigation_fields());i<-match(key,fields)
  if(is.na(i))return(state)
  if(i<length(fields))state[fields[seq.int(i+1L,length(fields))]]<-NULL
  state[[key]]<-value;state
}
