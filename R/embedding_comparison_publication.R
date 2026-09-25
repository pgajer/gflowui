# Standalone function body is copied into each bundle for reproducible vector exports.
gflowui_ec_publication_render <- function(table,settings,directory) {
  metrics <- c(chord_error="Euclidean distance error (lower is better)",
    relative_stress="Relative-distance stress (lower is better)",path_error="Fixed-path error (lower is better)",
    edge_error="Edge error (lower is better)",distance_rank_correlation="Distance-rank correlation (higher is better)")
  rows <- table[table$graph_id==settings$graph_id & table$status=="completed",,drop=FALSE]
  out <- character();dir.create(file.path(directory,"figures"),showWarnings=FALSE)
  for(key in names(metrics)) {
    rr <- rows[is.finite(rows[[key]]),,drop=FALSE]
    if(!nrow(rr))next
    rr <- rr[order(rr$method,rr$settings,rr$seed),,drop=FALSE]
    names <- rr$method
    sfdp <- rr$method_id=="sfdp"
    if(any(sfdp)) {
      version <- ifelse(grepl("graphviz version ",rr$settings[sfdp],fixed=TRUE),
        sub("^.*graphviz version ([^ ]+).*$","\\1",rr$settings[sfdp]),"version unavailable")
      names[sfdp] <- paste0("SFDP (Graphviz ",version,")")
    }
    labels <- paste0(names,ifelse(rr$method_id=="lle",paste0(" (",rr$settings,")"),"")," / seed ",rr$seed)
    y <- rev(seq_len(nrow(rr)));lo <- rr[[paste0(key,"_lower")]];hi <- rr[[paste0(key,"_upper")]]
    extent <- range(c(rr[[key]],lo,hi),finite=TRUE)
    if(diff(extent)==0)extent<-extent+c(-1,1)*max(.001,abs(extent[1])*.05)
    if(key!="distance_rank_correlation")extent[1]<-max(0,extent[1]-.04*diff(extent))
    extent[2]<-extent[2]+.04*diff(extent)
    active <- match(settings$run_id,rr$id)
    sampled <- any(rr$evaluation=="uniform_pair_sample") && key!="edge_error"
    note <- if(sampled)"Shared uniform pair sample; bars: approximate 95% sampling intervals" else "Exact evaluation; each point is one retained replicate"
    for(ext in c("pdf","svg")) {
      file <- paste0("figures/",key,".",ext);path<-file.path(directory,file)
      height<-max(6,2.8+.24*nrow(rr))
      if(ext=="pdf")grDevices::pdf(path,width=10,height=height,useDingbats=FALSE) else grDevices::svg(path,width=10,height=height)
      tryCatch({
        graphics::par(mar=c(7,17,5,1),family="sans",cex=.85)
        graphics::plot(rr[[key]],y,type="n",xlim=extent,ylim=c(.5,nrow(rr)+.5),yaxt="n",ylab="",xlab=metrics[[key]],bty="l")
        graphics::axis(2,at=y,labels=labels,las=1,tick=FALSE,cex.axis=.8)
        if(!is.na(active))graphics::abline(v=rr[[key]][active],col="#737A80",lty=2)
        valid<-is.finite(lo)&is.finite(hi)
        graphics::segments(lo[valid],y[valid],hi[valid],y[valid],col="#3575B2",lwd=1.2)
        graphics::points(rr[[key]],y,pch=16,col="#3575B2",cex=.8)
        if(!is.na(active))graphics::points(rr[[key]][active],y[active],pch=1,col="#39434A",cex=1.4,lwd=1.2)
        graphics::title(main=paste(settings$graph_id,"-",metrics[[key]]),line=3,cex.main=1)
        graphics::mtext(note,side=3,line=1.5,cex=.75)
        graphics::mtext("Gray dashed line / open circle: displayed run. Original graph targets; components evaluated separately.",side=1,line=4.5,cex=.7)
        if(sampled)graphics::mtext("Intervals condition on fixed coordinates; they do not describe optimizer-seed variability.",side=1,line=5.7,cex=.7)
      },finally=grDevices::dev.off())
      out<-c(out,file)
    }
  }
  out
}
