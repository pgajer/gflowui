# Reproducible fixture migration; numerical layout assets remain read-only.
args<-commandArgs(TRUE)
stopifnot(length(args)==2L)
source_root<-normalizePath(args[1]);out<-args[2]
id<-"comb_fermat_embeddings_01_oct_2026"
live<-file.path(tools::R_user_dir("gflowui","data"),"projects")
dir.create(out,recursive=TRUE,showWarnings=FALSE);out<-normalizePath(out)
dir.create(file.path(out,"manifests"),showWarnings=FALSE)
dir.create(file.path(out,"projects"),showWarnings=FALSE)
stopifnot(!file.exists(file.path(out,"registry.rds")))
file.copy(file.path(live,"projects",id),file.path(out,"projects"),recursive=TRUE)
# Rewrite only registry-owned paths. External graph/layout inputs stay shared.
rebase<-function(x){
 if(is.character(x)){ii<-!is.na(x)&startsWith(x,paste0(live,"/"));x[ii]<-paste0(out,substring(x[ii],nchar(live)+1L));return(x)}
 if(is.data.frame(x)){x[]<-lapply(x,rebase);return(x)}
 if(is.list(x))return(lapply(x,rebase));x
}
for(f in list.files(file.path(out,"projects",id),pattern="\\.rds$",recursive=TRUE,full.names=TRUE))saveRDS(rebase(readRDS(f)),f)
m<-readRDS(file.path(live,"manifests",paste0(id,".manifest.rds")))
m<-rebase(m)
# No writes to original data roots from the test copy.
m$project_root<-file.path(out,"project-root");dir.create(m$project_root)
a<-readRDS(file.path(source_root,"results/state_graph/catalogue.rds"))
d<-a$dictionary;c<-a$catalogue
label<-function(keys) vapply(strsplit(keys,",",fixed=TRUE),function(z)paste(d$id[match(as.integer(z),d$index)],collapse=" + "),"")
ss<-data.frame(sample_id=c$sample_id,udcst_level1=label(c$udcst1),udcst_level2=label(c$udcst2))
keys<-sort(unique(c$udcst2));ij<-lapply(strsplit(keys,",",fixed=TRUE),as.integer)
pairs<-data.frame(group=label(keys),a=vapply(ij,function(z)d$id[match(z[1],d$index)],""),b=vapply(ij,function(z)d$id[match(z[2],d$index)],""))
stopifnot(!anyDuplicated(pairs$group),all(vapply(ij,function(x)length(x)==2&&x[1]<x[2],TRUE)))
subsets<-lapply(names(a$masks),function(key){
 ids<-a$masks[[key]];level<-if(startsWith(key,"1"))1L else 2L
 list(ids=ids,label=if(key=="All")"All" else if(key=="1_90")"1-dCST 90% (pure)" else paste0("2-udCST ",sub("2_","",key),"%"),
 states=length(unique(c[[paste0("udcst",level)]][match(ids,c$sample_id)])),
 policy=if(key=="All")"Full reference; no coverage restriction." else "Pure top-rank classification; coverage counts distinct compositions, includes complete frequency ties, and is fixed to the full reference.")
});names(subsets)<-names(a$masks)
asset<-list(version=1L,samples=ss,levels=list(udcst_level1=list(label="udCST level 1"),udcst_level2=list(label="udCST level 2")),
 subsets=subsets,pairs=pairs,dictionary=d,orientation="Canonical original feature index: A precedes B. Never reorient by current dominance.",
 source=file.path(source_root,"results/state_graph/catalogue.rds"),source_sha256=digest::digest(file=file.path(source_root,"results/state_graph/catalogue.rds"),algo="sha256"),
 palettes=lapply(ss[-1],function(x){g<-sort(unique(x));setNames(grDevices::hcl.colors(length(g),"Dark 3"),g)}))
p<-file.path(out,"projects",id,"classifications");dir.create(p)
saveRDS(asset,file.path(p,"catalogue.rds"))
m$metadata$classification_catalogue<-list(file=file.path(p,"catalogue.rds"),version=1L,palettes=list(),policy="Pure top-rank, fixed full-reference coverage; original feature-order tie rule.")
m$defaults$classification_state<-list(type="udcst",levels=list(udcst="udcst_level2",dcst="dcst_level1"),subset="All",groups=list(),color="udcst")
saveRDS(m,file.path(out,"manifests",paste0(id,".manifest.rds")))
r<-readRDS(file.path(live,"registry.rds"));r<-rebase(r[r$id==id,,drop=FALSE]);r$project_root<-m$project_root
saveRDS(r,file.path(out,"registry.rds"))
cat("Copy:",out,"\nSamples:",nrow(ss),"\nPair states:",nrow(pairs),"\n")
