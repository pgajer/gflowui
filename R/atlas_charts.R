# Projective chart centered at a composition: q=a/||a||_2 and
# z(x)=x/(q'x)-q. Coordinates are signed and lie in q-perpendicular space.
gflowui_atlas_chart <- function(X,anchor,threshold=1e-6,policy="stop") {
  if(!is.matrix(X) || !is.numeric(X) || any(!is.finite(X)) || any(X<0))stop("Charts require finite nonnegative input compositions.")
  if(length(anchor)!=ncol(X) || any(!is.finite(anchor)) || any(anchor<0) || sum(anchor^2)<=0)stop("Invalid full-feature anchor composition.")
  if(length(threshold)!=1L || !is.finite(threshold) || threshold<=0 || threshold>1)stop("Chart denominator threshold must be in (0,1].")
  if(!policy %in% c("stop","exclude"))stop("Invalid chart coverage policy.")
  q<-as.numeric(anchor)/sqrt(sum(anchor^2));denominator<-as.numeric(X%*%q)
  keep<-is.finite(denominator)&denominator>=threshold
  ids<-rownames(X) %||% as.character(seq_len(nrow(X)))
  coverage<-data.frame(vertex_id=ids,denominator=denominator,retained=keep,
    amplification=ifelse(denominator>0,1/denominator,Inf),stringsAsFactors=FALSE)
  coords<-sweep(X[keep,,drop=FALSE],1,denominator[keep],"/")
  coords<-sweep(coords,2,q,"-")
  if(any(!is.finite(coords)))stop("Chart produced nonfinite coordinates.")
  list(coords=coords,q=q,coverage=coverage,keep=which(keep),excluded_ids=ids[!keep],
    threshold=threshold,policy=policy,usable=all(keep)||policy=="exclude",
    tangent_error=if(nrow(coords))max(abs(coords%*%q)) else NA_real_,
    formula="q = a / ||a||_2; z(x) = x / (q^T x) - q")
}

gflowui_atlas_prepare_coordinates <- function(data,p) {
  X<-data$X
  if(p$coordinates=="anchor_chart") {
    if(is.null(data$anchor) || !identical(data$anchor$vertex_id,p$chart_anchor))stop("Frozen chart anchor does not match requested anchor.")
    result<-gflowui_atlas_chart(X,data$anchor$abundances,p$chart_threshold,p$chart_policy)
    if(!result$usable)stop(sprintf("Chart coverage failed: %d of %d samples have denominator below %.3g. Review coverage or explicitly choose exclusion.",length(result$excluded_ids),nrow(X),p$chart_threshold))
    if(nrow(result$coords)<4L)stop("Chart leaves fewer than four samples; no fit was generated.")
    return(list(Z=result$coords,coverage=result))
  }
  list(Z=if(p$coordinates=="sqrt_abundance")sqrt(X) else X,coverage=NULL)
}
