args<-commandArgs(TRUE);stopifnot(length(args)==2)
pkgload::load_all('/Users/pgajer/current_projects/gflowui-fermat-palettes',quiet=TRUE)
a<-readRDS(file.path(args[1],'projects/comb_fermat_embeddings_01_oct_2026/state_graphs/catalogue.rds'))
base<-file.path(args[2],'results/state_graph/fits');rows<-list()
for(cov in c(60,70,80,90,100))for(rule in c('shared','m1','m5','m10')){
 z<-gflowui_state_graph_build(a,cov,adjacency=if(rule=='shared')'shared_feature' else 'observed_triple',support=if(rule=='shared')1L else as.integer(sub('m','',rule)))
 old<-readRDS(file.path(base,paste0(cov,'_',rule,'.rds')))$graph;g<-z$graph
 stopifnot(identical(g$edges,old$edges),identical(g$nodes$ID,old$nodes$ID),identical(g$nodes$component,old$nodes$component),
 g$metadata$components==old$metadata$components,g$metadata$isolates==old$metadata$isolates,
 identical(g$faces[,names(old$faces)],old$faces),identical(a$fits[[z$identity]]$coords,readRDS(file.path(base,paste0(cov,'_',rule,'.rds')))$fit$coords))
 rows[[paste(cov,rule)]]<-data.frame(coverage=cov,rule=rule,states=nrow(g$nodes),edges=nrow(g$edges),components=g$metadata$components,isolates=g$metadata$isolates)
}
g<-gflowui_state_graph_build(a,60)$graph;e<-g$edges[g$edges$pair.a=='429,528' & g$edges$pair.b=='429,524',,drop=FALSE]
stopifnot(nrow(e)==1,e$n.triple==982,e$i.a==315,e$i.b==119)
for(scope in c('side_a','side_b','witnesses','triple','third','endpoints')){
 ids<-gflowui_state_graph_members(g,edge=e,scope=scope)
 expect<-switch(scope,side_a=315,side_b=119,witnesses=434,triple=982,third=548,endpoints=3757)
 stopifnot(length(ids)==expect,!anyDuplicated(ids))
}
# Arbitrary thresholds agree with the authoritative linf constructor.
catalogue<-readRDS(file.path(args[2],'results/state_graph/catalogue.rds'))
for(m in c(2L,17L,1000L)){
 x<-gflowui_state_graph_build(a,60,support=m)$graph
 ref<-linf::linf.udcst.graph(pair.fit=catalogue$fits[[2]],triple.fit=catalogue$fits[[3]],coverage=60,min.support=m)
 stopifnot(identical(x$edges,ref$edges),identical(x$nodes$component,ref$nodes$component))
}
result<-list(passed=TRUE,variants=do.call(rbind,rows),witness=list(triple=982,side_a=315,side_b=119,both=434,third=548),custom_support=c(2,17,1000))
jsonlite::write_json(result,file.path(args[2],'results/phase2g-validation.json'),auto_unbox=TRUE,pretty=TRUE)
print(result)
