args<-commandArgs(TRUE);stopifnot(length(args)==2L)
source_root<-normalizePath(args[1]);out<-args[2]
id<-"comb_fermat_embeddings_01_oct_2026"
live<-file.path(tools::R_user_dir("gflowui","data"),"projects")
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE);out<-normalizePath(out)
dir.create(file.path(out,"manifests"));dir.create(file.path(out,"projects"))
stopifnot(file.copy(file.path(live,"projects",id),file.path(out,"projects"),recursive=TRUE))
rebase<-function(x){
 if(is.character(x)){ii<-!is.na(x)&startsWith(x,paste0(live,"/"));x[ii]<-paste0(out,substring(x[ii],nchar(live)+1L));return(x)}
 if(is.data.frame(x)){x[]<-lapply(x,rebase);return(x)}
 if(is.list(x))return(lapply(x,rebase));x
}
for(f in list.files(file.path(out,"projects",id),pattern="\\.rds$",recursive=TRUE,full.names=TRUE))saveRDS(rebase(readRDS(f)),f)
m<-rebase(readRDS(file.path(live,"manifests",paste0(id,".manifest.rds"))));m$project_root<-file.path(out,"project-root");dir.create(m$project_root)
pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
base<-file.path(source_root,"results/state_graph")
x<-readRDS(file.path(base,"fits/100_shared.rds"));a<-list(template=x$graph,
 reference=list(label="comb-V3V4-tx: 25,042 distinct compositions",catalogue_sha256=digest::digest(file=file.path(base,"catalogue.rds"),algo="sha256"),
 sample_ids=x$graph$membership$sample.id,dictionary=x$graph$dictionary,tie_rule=x$graph$metadata$tie.rule,classification="pure unordered leading pairs/triples"),
 fits=list(),references=list())
for(cov in c(60,70,80,90,100)) {
 for(rule in c("shared","m1","m5","m10")) {
  x<-readRDS(file.path(base,"fits",paste0(cov,"_",rule,".rds")))
  g<-gflowui_state_graph_build(a,cov,adjacency=if(rule=="shared")"shared_feature" else "observed_triple",support=if(rule=="shared")1L else as.integer(sub("m","",rule)))
  stopifnot(identical(g$graph$nodes$ID,x$graph$nodes$ID),identical(g$graph$edges,x$graph$edges))
  a$fits[[g$identity]]<-x$fit
  if(rule=="m1")a$references[[as.character(cov)]]<-list(ids=x$graph$nodes$ID,coords=x$fit$coords,label="observed triple m=1; grip::metric.mds SGD",source_graph=g$identity)
 }
}
# All baseline references use the matching support-1 SGD fit.
d<-a$template$dictionary;keys<-a$template$nodes$ID
a$palette_keys<-setNames(vapply(strsplit(keys,",",fixed=TRUE),function(z)paste(d$id[match(as.integer(z),d$index)],collapse=" + "),""),keys)
p<-file.path(out,"projects",id,"state_graphs");dir.create(p)
saveRDS(a,file.path(p,"catalogue.rds"))
m$metadata$state_graphs<-list(file=file.path(p,"catalogue.rds"),cache_dir=file.path(p,"fits"),version=1L)
m$defaults$state_graphs<-gflowui_state_graph_default()
saveRDS(m,file.path(out,"manifests",paste0(id,".manifest.rds")))
r<-readRDS(file.path(live,"registry.rds"));r<-rebase(r[r$id==id,,drop=FALSE]);r$project_root<-m$project_root;saveRDS(r,file.path(out,"registry.rds"))
cat("Copy:",out,"\nCached variants:",length(a$fits),"\n")
