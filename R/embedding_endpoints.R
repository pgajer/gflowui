# Local geometric candidates in the displayed 3D embedding, not graph endpoints.
gflowui_embedding_endpoint_defaults <- function() list(k=10L, angle=90,
  drop_one=FALSE, spacing="d1", rule="mode", percentile=90,
  multiplier=1, bandwidth=1, cutoff=1, merge=FALSE, merge_radius=1)

gflowui_detect_embedding_endpoints <- function(coords, options=gflowui_embedding_endpoint_defaults(),
    vertex_ids=NULL) {
  if (!requireNamespace("FNN", quietly=TRUE)) stop("Install FNN to detect embedding endpoints.")
  o <- utils::modifyList(gflowui_embedding_endpoint_defaults(), options)
  scalar <- function(x, lo, hi, integer=FALSE) is.numeric(x) && length(x)==1L &&
    is.finite(x) && x>=lo && x<=hi && (!integer || x==floor(x))
  if (!is.matrix(coords) || !is.numeric(coords) || ncol(coords)!=3L ||
      nrow(coords)<3L || any(!is.finite(coords))) stop("Supply at least three finite 3D points.")
  if (!scalar(o$k,2,200,TRUE) || !scalar(o$angle,0,180) ||
      !scalar(o$percentile,0,100) || !scalar(o$multiplier,0,1e6) ||
      !scalar(o$bandwidth,.01,100) || !scalar(o$cutoff,0,Inf) ||
      !scalar(o$merge_radius,0,100) ||
      !o$spacing %in% c("d1","dk") || !o$rule %in% c("mode","percentile","manual","none"))
    stop("Invalid endpoint detector settings.")
  if (isTRUE(o$drop_one) && o$k<3L) stop("Excluding one neighbor requires at least three neighbors.")
  n <- nrow(coords)
  if (is.null(vertex_ids)) vertex_ids <- sprintf("v%d",seq_len(n))
  if (length(vertex_ids)!=n || anyNA(vertex_ids) || anyDuplicated(vertex_ids))
    stop("Endpoint detection requires one unique ID per vertex.")
  # Exact coincident positions represent a single geometric location. Keeping
  # first occurrences prevents zero-length directions and false zero modes.
  reps <- which(!duplicated(as.data.frame(coords)))
  key <- do.call(paste, c(as.data.frame(coords), sep="|"))
  groups <- match(key, key[reps])
  # Verify string keys did not conflate distinct double-precision coordinates.
  if (any(coords != coords[reps[groups],,drop=FALSE])) {
    groups <- vapply(seq_len(n),function(i) which(vapply(reps,function(j)
      identical(unname(coords[i,]),unname(coords[j,])),FALSE))[1L],integer(1))
  }
  x <- coords[reps,,drop=FALSE]; m <- nrow(x)
  if (m<=o$k) stop("Neighbor count must be smaller than the number of distinct positions.")
  center <- colMeans(x); x <- sweep(x,2L,center)
  unit <- max(abs(x))
  if (!is.finite(unit) || unit<=0) stop("Embedding has no measurable spatial extent.")
  x <- x/unit
  nn <- FNN::get.knn(x,k=as.integer(o$k),algorithm="kd_tree")
  d1 <- nn$nn.dist[,1L]*unit; dk <- nn$nn.dist[,o$k]*unit
  if (any(nn$nn.dist<=0)) stop("Distinct positions are too close to resolve numerically.")
  spacing <- if(o$spacing=="d1")d1 else dk
  if (diff(range(spacing)) <= 1e-12*max(spacing)) {
    mode <- stats::median(spacing); density <- NULL
  } else {
    # Estimate on scaled positive spacings to preserve uniform-scale equivariance.
    ss <- max(spacing)
    dd <- stats::density(spacing/ss,adjust=o$bandwidth,from=0,
      to=max(spacing/ss)*1.1,n=1024L)
    mode <- dd$x[which.max(dd$y)]*ss
    density <- list(x=dd$x*ss,y=dd$y/ss)
  }
  cutoff <- switch(o$rule,mode=mode*o$multiplier,
    percentile=as.numeric(stats::quantile(spacing,o$percentile/100,names=FALSE))*o$multiplier,
    manual=o$cutoff,none=Inf)
  max_angle <- angle <- rep(NA_real_,m); excluded <- rep(NA_integer_,m)
  for (i in seq_len(m)) {
    nb <- nn$nn.index[i,]; v <- sweep(x[nb,,drop=FALSE],2L,x[i,])
    v <- v/sqrt(rowSums(v*v)); dots <- tcrossprod(v)
    raw <- min(dots[upper.tri(dots)])
    best <- raw
    if (isTRUE(o$drop_one) && o$k>=3L) {
      # An improving deletion must remove an endpoint of a worst-angle pair.
      # Testing those two indices avoids a cubic loop over all deletions.
      worst <- which(upper.tri(dots) & dots==raw,arr.ind=TRUE)[1L,]
      candidates <- vapply(worst,function(j) {
        a<-dots[-j,-j,drop=FALSE];min(a[upper.tri(a)])
      },numeric(1))
      j <- which.max(candidates)
      if (candidates[j]>best+1e-12) {best<-candidates[j];excluded[i]<-reps[nb[worst[j]]]}
    }
    max_angle[i] <- acos(max(-1,min(1,raw)))*180/pi
    angle[i] <- acos(max(-1,min(1,best)))*180/pi
  }
  supported <- spacing<=cutoff+max(spacing)*1e-12
  candidate <- supported & angle<=o$angle+1e-8
  retained <- candidate; suppressed_by <- rep(NA_integer_,m)
  if (isTRUE(o$merge) && any(candidate)) {
    retained[] <- FALSE; chosen <- integer()
    order <- which(candidate)[order(angle[candidate],spacing[candidate],reps[candidate])]
    for (i in order) {
      d <- if(length(chosen))sqrt(rowSums(sweep(x[chosen,,drop=FALSE],2L,x[i,])^2))*unit else numeric()
      radius <- o$merge_radius*pmin(dk[i],dk[chosen])
      hit <- which(d<=radius+1e-12*unit)
      if (length(hit)) suppressed_by[i]<-reps[chosen[hit[1L]]]
      else {retained[i]<-TRUE;chosen<-c(chosen,i)}
    }
  }
  rows <- data.frame(vertex=seq_len(n),sample_id=as.character(vertex_ids),
    representative_vertex=reps[groups],d1=d1[groups],dk=dk[groups],
    max_angle=max_angle[groups],angle=angle[groups],excluded_neighbor=excluded[groups],
    spacing_supported=supported[groups],candidate=candidate[groups] & seq_len(n)%in%reps,
    retained=retained[groups] & seq_len(n)%in%reps,suppressed_by=suppressed_by[groups])
  list(rows=rows,options=o,cutoff=cutoff,mode=mode,density=density,
    spacings=spacing,unique_positions=m,coincident_vertices=n-m,
    method="Embedding nearest-neighbor spacing and angular spread",version=1L)
}

gflowui_embedding_endpoint_frame <- function(st) {
  if (!is.list(st) || !is.null(st$error) || !is.matrix(st$coords)) return(NULL)
  ids <- st$vertex_ids
  if (is.null(ids)) ids <- sprintf("v%d",seq_len(nrow(st$coords)))
  list(coords=st$coords,vertex_ids=ids,project_id=st$project_id,set_id=st$set_id,
    key=digest::digest(list(st$project_id,st$set_id,st$k_actual,ids,st$coords),algo="sha256"))
}

gflowui_embedding_endpoint_result_valid <- function(result,frame,options) {
  is.list(result) && is.list(frame) && identical(result$key,frame$key) &&
    isTRUE(all.equal(result$options,options,check.attributes=FALSE))
}
