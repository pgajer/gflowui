# Frozen inputs and scientific settings for local computations.
gflowui_atlas_atomic <- function(x, file) {
  dir.create(dirname(file), recursive=TRUE, showWarnings=FALSE)
  tmp <- tempfile("write-", dirname(file)); on.exit(unlink(tmp))
  saveRDS(x,tmp); if(!file.rename(tmp,file)) stop("Could not publish ",file)
  invisible(x)
}

gflowui_atlas_software <- function() {
  packages <- c("gflowui","dgraphs","grip","igraph")
  versions <- stats::setNames(vapply(packages,function(p) as.character(utils::packageVersion(p)),""),packages)
  files <- unlist(lapply(packages,function(p) {
    root<-system.file(package=p); f<-list.files(root,pattern="\\.(rdb|so|dll)$",recursive=TRUE,full.names=TRUE)
    if(!length(f))return(stats::setNames(character(),character()))
    stats::setNames(f,paste0(p,"/",substring(f,nchar(root)+2L)))
  }),use.names=TRUE)
  files<-files[order(names(files),method="radix")]
  ns <- asNamespace("gflowui")
  funs <- sort(grep("^gflowui_atlas",ls(ns,all.names=TRUE),value=TRUE),method="radix")
  code <- lapply(funs,function(n) {f<-get(n,ns); if(is.function(f)) list(paste(deparse(formals(f)),collapse="\n"),paste(deparse(body(f)),collapse="\n")) else NULL})
  list(R=as.character(getRversion()), packages=versions,
       binaries=stats::setNames(vapply(files,function(f)digest::digest(file=f,algo="sha256"),""),names(files)),
       atlas_code=digest::digest(code,algo="sha256"))
}

gflowui_atlas_parameters <- function(parameters, n) {
  defaults <- list(method="mds",coordinates="abundance",metric="hellinger",inner="fermat",
    power=2,k=5L,chart_anchor="",chart_threshold=1e-6,chart_policy="stop",mode="auto",landmarks=200L,iterations=100L,seed=1L,memory_mb=1024)
  unknown <- setdiff(names(parameters),names(defaults))
  if(length(unknown)) stop("Unknown calculation parameter: ",paste(unknown,collapse=", "))
  p <- utils::modifyList(defaults,parameters)
  choices <- list(method=c("pca","mds"),coordinates=c("abundance","sqrt_abundance","anchor_chart"),chart_policy=c("stop","exclude"),
    metric=c("euclidean","hellinger","jensen_shannon"),inner=c("ambient","fermat","sknn_mst"),mode=c("auto","full","landmarks"))
  if(p$method=="pca") {p$metric<-"euclidean";p$inner<-"ambient";p$mode<-"full";p$power<-1}
  for(key in names(choices)) if(length(p[[key]])!=1L || !p[[key]] %in% choices[[key]]) stop("Invalid ",key)
  for(key in c("k","landmarks","iterations","seed","memory_mb")) {
    x<-p[[key]]; if(length(x)!=1L || !is.finite(x) || x!=floor(x) || x<1 || x>.Machine$integer.max) stop("Invalid integer ",key)
  }
  if(length(p$chart_anchor)!=1L || is.na(p$chart_anchor))stop("Choose a chart anchor ID.")
  if(length(p$chart_threshold)!=1L || !is.finite(p$chart_threshold) || p$chart_threshold<=0 || p$chart_threshold>1)stop("Chart threshold must be in (0,1].")
  if(p$coordinates=="anchor_chart" && !nzchar(p$chart_anchor))stop("Choose a chart anchor ID.")
  if(n<4L) stop("A 3D local fit requires at least four vertices.")
  if(length(p$power)!=1L || !is.finite(p$power) || p$power<1 || p$power>10) stop("Fermat power must be between 1 and 10.")
  if(p$coordinates!="abundance" && p$metric!="euclidean") stop("Transformed coordinates require Euclidean distance; Hellinger and Jensen–Shannon act on original abundances.")
  if(p$method=="mds" && p$inner=="sknn_mst" && p$k>=n) stop("Graph neighbors must be smaller than region size.")
  p$mode <- if(p$mode=="auto") if(n<=1200L) "full" else "landmarks" else p$mode
  p$landmarks <- min(n,p$landmarks)
  if(p$mode=="landmarks" && p$landmarks<4L) stop("Use at least four landmarks for a 3D fit.")
  if(p$method=="pca") {p$metric<-"euclidean"; p$inner<-"ambient"; p$mode<-"full"}
  if(p$inner!="fermat") p$power<-1
  p
}

gflowui_atlas_input <- function(asset,region,anchor_id="") {
  rows <- match(region$vertex_ids,asset$sample_ids)
  if(anyNA(rows) || anyDuplicated(region$vertex_ids)) stop("Region IDs are missing or duplicated in the current dataset.")
  X <- matrix(0,length(rows),length(asset$taxon_names),dimnames=list(region$vertex_ids,asset$taxon_names))
  for(i in seq_along(rows)) X[i,asset$indices[[rows[i]]]]<-asset$abundances[[rows[i]]]
  anchor<-NULL
  if(nzchar(anchor_id)) {
    i<-match(anchor_id,asset$sample_ids);if(is.na(i))stop("Chart anchor is not in the dataset.")
    a<-numeric(ncol(X));a[asset$indices[[i]]]<-asset$abundances[[i]];names(a)<-colnames(X)
    anchor<-list(vertex_id=anchor_id,abundances=a)
  }
  list(X=X, anchor=anchor, region=region[c("id","label","vertex_ids","membership_fingerprint","definition","revision","family_id","parent_id","created_at","history")])
}

gflowui_atlas_base_rows <- function(X, sources, metric) {
  if(metric %in% c("euclidean","hellinger")) {
    Z <- if(metric=="hellinger") sqrt(X)/sqrt(2) else X
    d2 <- outer(rowSums(Z[sources,,drop=FALSE]^2),rowSums(Z^2),"+")-2*tcrossprod(Z[sources,,drop=FALSE],Z)
    return(sqrt(pmax(d2,0)))
  }
  out<-matrix(0,length(sources),nrow(X))
  for(h in seq_along(sources)) for(j in seq_len(nrow(X))) {
    a<-X[sources[h],];b<-X[j,];m<-(a+b)/2
    out[h,j]<-sqrt(max(0,(sum(a[a>0]*log(a[a>0]/m[a>0]))+sum(b[b>0]*log(b[b>0]/m[b>0])))/2))
  }
  out
}

gflowui_atlas_igraph <- function(g,ids) {
  adj<-dgraphs::graph.adjacency(g); lens<-dgraphs::graph.lengths(g)
  i<-rep(seq_along(adj),lengths(adj));j<-as.integer(unlist(adj)); w<-unlist(lens)
  use<-i<j; e<-cbind(i[use],j[use]); z<-igraph::make_empty_graph(length(ids),directed=FALSE)
  if(nrow(e)) {z<-igraph::add_edges(z,as.vector(t(e)));igraph::E(z)$weight<-w[use]}
  z
}

gflowui_atlas_compute <- function(spec,folder,progress=function(stage,fraction)NULL) {
  if(is.null(spec$software))spec$software<-gflowui_atlas_software()
  gflowui_atlas_atomic(spec,file.path(folder,"spec.rds"))
  p<-spec$parameters; data<-readRDS(spec$input_file); X<-data$X; ids<-rownames(X); n<-nrow(X)
  if(!identical(digest::digest(data,algo="sha256"),spec$input_hash)) stop("Frozen input checksum mismatch.")
  progress("Preparing coordinates",0.05)
  coordinate_result<-gflowui_atlas_prepare_coordinates(data,p)
  Z<-coordinate_result$Z;ids<-rownames(Z);n<-nrow(Z)
  p<-gflowui_atlas_parameters(p,n)
  if(p$coordinates=="anchor_chart") {
    utils::write.csv(coordinate_result$coverage$coverage,file.path(folder,"chart_coverage.csv"),row.names=FALSE)
    chart_record<-coordinate_result$coverage;chart_record$coords<-NULL
    saveRDS(chart_record,file.path(folder,"chart.rds"))
  }
  # Budget includes copies, explicit weighted graph and MDS constraint storage.
  estimate <- 4*8*length(Z) + if(p$method=="pca") 8*length(Z)*4 else {
    dense<-if(p$metric=="jensen_shannon" && p$inner!="ambient") 200*n*n else 0
    dense + if(p$mode=="full") 120*n*n else 200*n*p$landmarks
  }
  if(estimate>p$memory_mb*1024^2) stop(sprintf("Estimated workspace %.0f MiB exceeds the %d MiB budget. Use landmarks, a smaller region, or explicitly raise the budget.",estimate/1024^2,p$memory_mb))
  set.seed(p$seed)
  sources<-if(p$mode=="full")seq_len(n) else sort(sample.int(n,p$landmarks))
  distances<-NULL; fit<-NULL; warnings<-character(); graph<-NULL
  graph_info<-list(role="display scaffold only",original_components=NA_integer_,repair_edges=matrix(integer(),0,2))
  if(p$method=="pca") {
    progress("PCA projection",0.4)
    fit<-stats::prcomp(Z,center=TRUE,scale.=FALSE,rank.=3)
    coords<-fit$x[,seq_len(min(3,ncol(fit$x))),drop=FALSE]
    if(ncol(coords)<3)coords<-cbind(coords,matrix(0,n,3-ncol(coords)))
    fit_metadata<-list(sdev=fit$sdev,explained_variance=fit$sdev^2/sum(fit$sdev^2),method="PCA; coordinate projection")
    sources<-integer()
  } else {
    progress("Constructing local dissimilarities",0.15)
    if(p$inner=="ambient" || (p$inner=="fermat" && p$power==1)) {
      distances<-gflowui_atlas_base_rows(Z,sources,p$metric)
    } else if(p$inner=="fermat" && p$metric!="jensen_shannon") {
      points<-if(p$metric=="hellinger")sqrt(Z)/sqrt(2) else Z
      result<-dgraphs::fermat.distances(points=points,p=p$power,rooted=FALSE,backend="implicit",sources=sources,
        return.graph=TRUE,max.workspace.bytes=p$memory_mb*1024^2)
      distances<-result$distances; graph<-gflowui_atlas_igraph(result$graph,ids)
      graph_info<-list(role="union of selected complete-Fermat shortest paths",coverage=result$metadata,
        original_components=igraph::components(graph)$no,repair_edges=matrix(integer(),0,2))
    } else {
      input<-if(p$metric=="hellinger")sqrt(Z)/sqrt(2) else Z
      dtype<-"observations"
      if(p$metric=="jensen_shannon") {input<-gflowui_atlas_base_rows(Z,seq_len(n),p$metric);diag(input)<-0;dtype<-"distances"}
      if(p$inner=="sknn_mst") {
        g0<-dgraphs::create.sknn.graph(input,k=p$k,input.type=dtype,neighbor.method="exact",prune.edges=FALSE,connect.components=FALSE,graph.detail="minimal")
        g1<-dgraphs::create.sknn.graph(input,k=p$k,input.type=dtype,neighbor.method="exact",prune.edges=FALSE,connect.components=TRUE,connect.method="component.mst",graph.detail="full")
        original<-gflowui_atlas_igraph(g0,ids);graph<-gflowui_atlas_igraph(g1,ids)
        e0<-igraph::as_edgelist(original,names=FALSE); e1<-igraph::as_edgelist(graph,names=FALSE)
        key<-function(e)paste(pmin(e[,1],e[,2]),pmax(e[,1],e[,2]),sep=":")
        graph_info<-list(role="symmetric kNN with component MST repair; defines target paths",
          original_components=igraph::components(original)$no,original_membership=igraph::components(original)$membership,
          repair_edges=e1[!key(e1)%in%key(e0),,drop=FALSE])
      } else {
        edges<-which(upper.tri(input),arr.ind=TRUE)
        graph<-igraph::make_empty_graph(n,directed=FALSE)
        graph<-igraph::add_edges(graph,as.vector(t(edges)));igraph::E(graph)$weight<-input[edges]^p$power
        graph_info<-list(role="complete JS Fermat graph; defines target paths",original_components=1L,repair_edges=matrix(integer(),0,2))
      }
      distances<-igraph::distances(graph,v=sources,to=seq_len(n),weights=igraph::E(graph)$weight,algorithm="dijkstra")
    }
    for(h in seq_along(sources)) distances[h,sources[h]]<-0
    if(any(!is.finite(distances)) || any(distances<0)) stop("Nonfinite or negative local dissimilarities.")
    nonself<-col(distances)!=sources[row(distances)]
    if(any(distances[nonself]<=0)) stop("Coincident samples produce zero target distances. This inverse-squared MDS fit requires distinct coordinates; membership was not changed.")
    scale<-sqrt(mean(distances[nonself]^2))
    if(!is.finite(scale) || scale<=0)stop("Target scale is not finite and positive.")
    progress("Fitting 3D metric-MDS",0.65)
    fit<-withCallingHandlers({
      if(p$mode=="full") grip::metric.mds(distance.matrix=distances/scale,dim=3,backend="sgd",init="random",
        max.iter=p$iterations,seed=p$seed,pair.weights="inverse_squared",diagnostics=FALSE,
        sgd.control=list(max.workspace.bytes=p$memory_mb*1024^2))
      else {
        constraints<-grip::landmark.mds.constraints(distances/scale,landmarks=sources,weighting="uniform")
        grip::metric.mds(n=n,constraints=constraints,approximation="sparse",dim=3,backend="sgd",init="random",
          max.iter=p$iterations,seed=p$seed,diagnostics=FALSE,
          sgd.control=list(max.workspace.bytes=p$memory_mb*1024^2))
      }
    },warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
    coords<-fit$coords*scale
    fit_metadata<-list(fit=fit$metadata,target_scale=scale,objective="inverse_squared",landmark_weighting="uniform",
      local_pairs="none; landmark-to-all constraints only",warnings=unique(warnings))
  }
  if(any(!is.finite(coords)) || !identical(dim(coords),c(n,3L))) stop("Invalid 3D coordinates.")
  rownames(coords)<-ids;colnames(coords)<-c("x","y","z")
  if(is.null(graph)) {
    # Connectivity scaffold only; never substituted for ambient or PCA targets.
    input<-if(p$metric=="hellinger" && p$coordinates=="abundance")sqrt(Z)/sqrt(2) else Z
    scaffold<-dgraphs::create.sknn.graph(input,k=1,neighbor.method="exact",prune.edges=FALSE,connect.components=TRUE,connect.method="component.mst",graph.detail="full")
    graph<-igraph::mst(gflowui_atlas_igraph(scaffold,ids))
  }
  progress("Writing validated view assets",0.9)
  edges<-igraph::as_edgelist(graph,names=FALSE);weights<-igraph::E(graph)$weight
  split_by<-factor(c(edges[,1],edges[,2]),levels=seq_len(n))
  dg<-list(adj_list=unname(split(as.integer(c(edges[,2],edges[,1])),split_by)),
    weight_list=unname(split(rep(weights,2),split_by)),vertex_ids=ids)
  saveRDS(list(X.graphs=list(geom_pruned_graphs=list(dg),k.values=1L),k.values=1L,selected.k=1L,vertex_ids=ids),file.path(folder,"graph.rds"))
  saveRDS(coords,file.path(folder,"layout.rds"))
  saveRDS(list(distances=distances,sources=sources,source_ids=ids[sources],target_ids=ids),file.path(folder,"targets.rds"))
  diagnostics<-list(fit=fit_metadata,graph=graph_info,estimated_workspace_bytes=estimate,parameters=p)
  if(!is.null(distances)) {
    ss<-0;relative<-0;count<-0L
    for(h in seq_along(sources)) {
      i<-sources[h];j<-which(seq_len(n)!=i & (!seq_len(n)%in%sources | seq_len(n)>i))
      actual<-sqrt(rowSums(sweep(coords[j,,drop=FALSE],2,coords[i,],"-")^2))
      target<-distances[h,j];ss<-ss+sum((actual-target)^2);relative<-relative+sum((actual/target-1)^2);count<-count+length(j)
    }
    diagnostics$reconstruction<-list(evaluated_pairs=count,RMSE=sqrt(ss/count),relative_RMSE=sqrt(relative/count),
      coverage="unique queried source-to-all pairs; descriptive fit error, not independent validation")
  }
  saveRDS(diagnostics,file.path(folder,"diagnostics.rds"))
  overlays<-list(list(label=graph_info$role,edges=edges,color="rgba(90,100,110,0.2)",width=1,visible=FALSE))
  if(nrow(graph_info$repair_edges))overlays[[2]]<-list(label="MST repair edges",edges=graph_info$repair_edges,color="#d1495b",width=4,visible=TRUE)
  # Browser overlays are optional display aids; never change graph/target assets.
  display_cap<-20000L
  overlays[[1]]$edges<-head(edges,display_cap)
  if(nrow(edges)>display_cap)overlays[[1]]$label<-paste0(graph_info$role," (first ",display_cap," of ",nrow(edges)," edges; display only)")
  saveRDS(overlays,file.path(folder,"edge_overlays.rds"))
  label<-paste(data$region$label,p$coordinates,if(p$method=="pca")"PCA 3D" else paste(p$metric,p$inner,if(p$inner=="fermat")paste0("p=",p$power) else if(p$inner=="sknn_mst")paste0("k=",p$k) else "",p$mode,"MDS 3D"),sep=" | ")
  gs<-list(id=paste0("atlas_",spec$key),label=label,data_type_id=paste0("atlas_",spec$key),data_type_label=label,
    endpoint_vertex_namespace=spec$namespace,neighbor_parameter=FALSE,graph_file=file.path(folder,"graph.rds"),k_values=1L,selected_k=1L,
    n_samples=n,n_features=ncol(X),base_metric=p$metric,construction=p$inner,power=p$power,
    layout_assets=list(coordinate_normalization="uniform",edge_overlays_file=file.path(folder,"edge_overlays.rds"),
      presets=list(renderer="plotly",vertex_layout="point",vertex_size="0.6x",component="all"),
      grip_layouts=list(list(id="local_fit",k=1L,path=file.path(folder,"layout.rds"),label=label,source="local atlas worker"))),
    description=paste("Recomputed within frozen region.",graph_info$role),source_result=file.path(folder,"diagnostics.rds"),
    atlas=list(region_id=spec$region_id,job_key=spec$key,bundle_path=folder,distance_scope="within_region",parameters=p,retained_ids=ids,excluded_ids=setdiff(rownames(X),ids)))
  bundle_files<-gflowui_atlas_write_bundle(spec,folder,diagnostics)
  files<-file.path(folder,bundle_files)
  checksums<-stats::setNames(vapply(files,function(f)digest::digest(file=f,algo="sha256"),""),basename(files))
  result<-list(graph_set=gs,region_id=spec$region_id,key=spec$key,checksums=checksums,completed_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  gflowui_atlas_atomic(result,file.path(folder,"result.rds"))
  progress("Complete",1)
  result
}
