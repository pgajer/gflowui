args <- commandArgs(TRUE)
pkgload::load_all('/Users/pgajer/current_projects/grip',quiet=TRUE)
r <- jsonlite::fromJSON(args[1]); e <- as.matrix(read.csv(r$edge_file))+1L
warnings <- character()
t <- system.time(fit <- withCallingHandlers(metric.mds(edges=e,n=r$n,dim=3,backend='sgd',init='random',seed=r$seed,max.iter=1000,pair.weights='uniform',diagnostics=FALSE), warning=function(w){warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')}))
write.csv(fit$coords,args[2],row.names=FALSE)
jsonlite::write_json(list(backend='sgd',initialization='random',pair_weights='uniform',max_iter=1000,starts=fit$metadata$starts,warnings=warnings,fit_elapsed_seconds=unname(t['elapsed']),grip_version=as.character(packageVersion('grip'))),paste0(args[2],'.json'),auto_unbox=TRUE,pretty=TRUE)
