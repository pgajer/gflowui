# Process-isolated adapter to the installed, public GRIP API. No source loading.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
req <- jsonlite::fromJSON(args[[1]])
edges <- as.matrix(read.csv(req$edge_file)) + 1L
dimnames(edges) <- NULL
n <- req$n
method <- req$method
seed <- req$seed
base <- list(edges=edges,n=n,edge_weights=rep(1,nrow(edges)),dim=3L)
if (method == "metric_mds") {
  fit <- do.call(grip::metric.mds,c(base,list(init="random",n_init=1L,
                  max_iter=300L,eps=1e-8,seed=seed,diagnostics=FALSE)))
  coords <- fit$coords
  detail <- fit$metadata
} else if (method == "metric_mds_edge_kk") {
  initial <- as.matrix(read.csv(req$initial_file))
  fit <- do.call(grip::edge.kk,c(base,list(coords=initial,stiffness_method="uniform",
                  density_mix_schedule=1,scale_mode="identity",max_iter=100L,
                  seed=seed,diagnostics=FALSE,return_trace=TRUE)))
  coords <- fit$coords
  detail <- fit$metadata
} else if (method == "weighted_grip") {
  coords <- do.call(grip::weighted.grip.nd,c(base,list(seed=seed,rounds=160L,
                     final_rounds=256L,length_normalization="none",disconnected="error")))
  detail <- list(termination="fixed round budget; no convergence claim")
} else stop("Unsupported GRIP adapter")
stopifnot(is.matrix(coords),identical(dim(coords),c(as.integer(n),3L)),all(is.finite(coords)))
write.csv(coords,args[[2]],row.names=FALSE)
jsonlite::write_json(list(method=method,grip_version=as.character(packageVersion("grip")),
     R_version=R.version.string,grip_path=find.package("grip"),metadata=detail),
     paste0(args[[2]],".json"),auto_unbox=TRUE,pretty=TRUE,null="null",digits=16)
