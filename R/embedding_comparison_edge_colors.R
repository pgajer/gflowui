# One color at both endpoints of each segment, with an explicit numeric legend.
# Translation-only component packing leaves these lengths unchanged.
gflowui_ec_edge_style <- function(z, edges, mode="uniform") {
  if (!identical(mode,"length") || !nrow(edges)) return(list(color="#AAB1B8",width=1))
  lengths <- sqrt(rowSums((z[edges[,1],,drop=FALSE]-z[edges[,2],,drop=FALSE])^2))
  if (any(!is.finite(lengths))) stop("Nonfinite embedded edge length.")
  limits <- range(lengths)
  if (diff(limits)==0) {
    pad <- max(abs(limits[1])*.01,1e-8)
    limits <- limits+c(-pad,pad)
  }
  list(color=rep(lengths,each=3L),width=1,cauto=FALSE,cmin=limits[1],cmax=limits[2],
    colorscale=list(list(0,"#C7782A"),list(1,"#3575B2")),showscale=TRUE,
    colorbar=list(title=list(text="Drawn edge length<br>(layout units)"),thickness=12,len=.65,
                  tickformat=".3g",x=1.02))
}
