# Optional state graph families. State IDs and sample IDs never share a namespace.
gflowui_state_graph_asset <- local({
  cache <- new.env(parent=emptyenv())
  function(manifest) {
    p <- manifest$metadata$state_graphs$file
    if(is.null(p) || !nzchar(p)) return(NULL)
    if(!grepl("^(/|[A-Za-z]:)",p)) p <- file.path(manifest$project_root,p)
    key <- gflowui_file_version(p)
    if(!identical(cache$key,key)) {
      a <- readRDS(p); g <- a$template
      stopifnot(is.data.frame(g$membership), !anyNA(g$membership$sample.id),
        !anyDuplicated(g$membership$sample.id), !anyDuplicated(g$nodes$ID),
        all(g$edges$length==1),all(g$membership$pair %in% g$nodes$ID))
      cache$value <- a;cache$key <- key
    }
    cache$value
  }
})

gflowui_state_graph_build <- function(asset, coverage=60, minimum=1L,
                                       adjacency="observed_triple", support=1L) {
  if(!adjacency %in% c("observed_triple","shared_feature")) stop("Unknown adjacency.")
  if(length(support)!=1L || !is.finite(support) || support<1 || support!=floor(support))
    stop("Minimum per-side support must be a positive integer.")
  g <- asset$template; n <- g$nodes
  if(identical(as.character(coverage),"minimum")) {
    if(length(minimum)!=1L || !is.finite(minimum) || minimum<1 || minimum!=floor(minimum))
      stop("Minimum state size must be a positive integer.")
    n <- n[n$freq>=minimum,,drop=FALSE]
  } else {
    coverage <- as.numeric(coverage)
    if(length(coverage)!=1L || !is.finite(coverage) || coverage<=0 || coverage>100)stop("Invalid coverage.")
    threshold <- if(coverage==100)min(n$freq) else n$freq[which(cumsum(n$freq)>=coverage/100*g$metadata$reference.n)[1]]
    n <- n[n$freq>=threshold,,drop=FALSE]
  }
  e <- g$candidate.edges
  e <- e[e$pair.a %in% n$ID & e$pair.b %in% n$ID,,drop=FALSE]
  if(adjacency=="observed_triple")e<-e[e$i.a>=support & e$i.b>=support,,drop=FALSE]
  e$from<-match(e$pair.a,n$ID);e$to<-match(e$pair.b,n$ID);e$length<-rep(1,nrow(e))
  net<-igraph::make_empty_graph(nrow(n),directed=FALSE)
  if(nrow(e))net<-igraph::add_edges(net,as.vector(t(as.matrix(e[,c("from","to")]))))
  comps<-igraph::components(net);n$node<-seq_len(nrow(n));n$component<-as.integer(comps$membership)
  f<-g$faces
  f$pair.a<-g$nodes$ID[f$a];f$pair.b<-g$nodes$ID[f$b];f$pair.c<-g$nodes$ID[f$c]
  f<-f[f$pair.a %in% n$ID & f$pair.b %in% n$ID & f$pair.c %in% n$ID &
         f$min.support>=if(adjacency=="observed_triple")support else 1,,drop=FALSE]
  f$a<-match(f$pair.a,n$ID);f$b<-match(f$pair.b,n$ID);f$c<-match(f$pair.c,n$ID)
  rownames(n)<-NULL;rownames(e)<-NULL;rownames(f)<-NULL
  g$nodes<-n;g$edges<-e;g$faces<-f
  g$metadata$selected.states<-n$ID;g$metadata$selected.n<-sum(n$freq)
  g$metadata$coverage<-100*sum(n$freq)/g$metadata$reference.n
  g$metadata$requested.coverage<-coverage;g$metadata$n0<-minimum
  g$metadata$adjacency<-adjacency;g$metadata$min.support<-support
  g$metadata$components<-comps$no;g$metadata$component.sizes<-comps$csize
  g$metadata$isolates<-sum(igraph::degree(net)==0)
  identity<-digest::digest(list(reference=asset$reference,states=n$ID,
    adjacency=adjacency,support=if(adjacency=="observed_triple")support else NULL),algo="sha256")
  list(identity=identity,graph=g)
}

gflowui_state_graph_members <- function(g, states=character(), edge=NULL, scope="members") {
  m<-g$membership
  if(scope=="members")return(m$sample.id[m$pair %in% states])
  if(is.null(edge) || nrow(edge)!=1L)return(character())
  hit<-switch(scope,
    endpoints=m$pair %in% c(edge$pair.a,edge$pair.b),
    witnesses=m$triple==edge$triple & m$pair %in% c(edge$pair.a,edge$pair.b),
    side_a=m$triple==edge$triple & m$pair==edge$pair.a,
    side_b=m$triple==edge$triple & m$pair==edge$pair.b,
    third=m$triple==edge$triple & !m$pair %in% c(edge$pair.a,edge$pair.b),
    triple=m$triple==edge$triple,stop("Unknown witness scope."))
  m$sample.id[which(hit)]
}

# Serialized into a separate R process; only finite within-component distances enter MDS.
gflowui_state_graph_fit <- function(g, seed=410040L, passes=500L) {
  Z<-matrix(0,nrow(g$nodes),3);meta<-list();offset<-0;loss<-energy<-0
  for(comp in sort(unique(g$nodes$component))) {
    ix<-which(g$nodes$component==comp);nn<-length(ix)
    e<-g$edges[g$edges$from %in% ix,,drop=FALSE]
    net<-igraph::make_empty_graph(nn,directed=FALSE)
    if(nrow(e))net<-igraph::add_edges(net,as.vector(t(cbind(match(e$from,ix),match(e$to,ix)))))
    D<-igraph::distances(net)
    if(nn==1){z<-matrix(0,1,3);info<-list(engine="isolated vertex")}
    else if(nn==2){z<-rbind(c(-.5,0,0),c(.5,0,0));info<-list(engine="exact unit segment")}
    else {
      fit<-grip::metric.mds(distance.matrix=D,dim=min(3L,nn-1L),backend="sgd",
        pair.weights="uniform",approximation="full",init="classical",n.init=3L,
        max.iter=passes,seed=seed+comp,diagnostics=FALSE)
      z<-cbind(fit$coords,matrix(0,nn,3-ncol(fit$coords)));info<-fit$metadata
    }
    u<-upper.tri(D);loss<-loss+sum((as.matrix(stats::dist(z))[u]-D[u])^2);energy<-energy+sum(D[u]^2)
    z[,1]<-z[,1]-min(z[,1])+offset;offset<-max(z[,1])+3
    Z[ix,]<-z;meta[[length(meta)+1L]]<-list(component=comp,n=nn,metadata=info)
  }
  list(coords=Z,components=meta,error=if(energy>0)sqrt(loss/energy) else NA_real_,
    component_placement="arbitrary offsets; no between-component distance targets",
    settings=list(function_name="grip::metric.mds",backend="sgd",approximation="full",
      pair_weights="uniform",passes=passes,n_init=3L,seed=seed,unit_edges=TRUE))
}

gflowui_state_graph_default <- function(saved=NULL) {
  s<-utils::modifyList(list(mode="samples",coverage="60",minimum=1L,
    adjacency="observed_triple",support=1L,layout="reference",component="all",
    color="state",size=TRUE,width="constant",faces=FALSE,selected=character(),
    edge="",scope="witnesses",sample_filter=NULL,reference_ids=NULL,cameras=list(),names=list()),saved %||% list(),keep.null=TRUE)
  if(!s$mode %in% c("samples","states","linked"))s$mode<-"samples"
  s
}

# Re-tabulate frozen pure assignments on an explicitly chosen reference subset.
# Canonical pair order is sufficient here; no abundance ranks are re-estimated.
gflowui_state_graph_reference <- function(asset, ids) {
  if(is.null(ids))return(asset)
  if(!requireNamespace("linf",quietly=TRUE))stop("Reference reconstruction requires linf.")
  m<-asset$template$membership
  ids<-m$sample.id[m$sample.id %in% ids]
  if(!length(ids))stop("The reference sample set is empty.")
  m<-m[match(ids,m$sample.id),,drop=FALSE];d<-asset$template$dictionary
  pair<-t(vapply(strsplit(m$pair,",",fixed=TRUE),as.integer,integer(2)))
  third<-vapply(seq_len(nrow(m)),function(i)setdiff(as.integer(strsplit(m$triple[i],",",fixed=TRUE)[[1]]),pair[i,]),integer(1))
  triple<-cbind(pair,third)
  make_fit<-function(z){sets<-t(apply(z,1,sort));list(level=as.integer(ncol(z)),
    ordered.features=z,feature.sets=sets,pure.id=apply(sets,1,paste,collapse=","),
    feature.ids=d$id,feature.labels=d$label,input.dimnames=list(ids,d$id))}
  asset$template<-linf::linf.udcst.graph(pair.fit=make_fit(pair),triple.fit=make_fit(triple),
    sample.ids=ids,coverage=100,adjacency="shared_feature")
  asset$reference$parent_catalogue_sha256<-asset$reference$catalogue_sha256
  asset$reference$sample_ids<-ids
  asset$reference$catalogue_sha256<-digest::digest(m,algo="sha256")
  asset$reference$label<-sprintf("Explicit subset: %s distinct compositions; frozen pure assignments",format(length(ids),big.mark=","))
  asset$fits<-list()
  asset
}
