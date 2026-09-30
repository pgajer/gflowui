# Properties belong to a graph, never to a particular embedding.
gflowui_ec_property_names <- function(kind="vertex") {
  if (identical(kind,"vertex")) return(c(degree="Degree",betweenness="Vertex betweenness",
    betweenness_normalized="Vertex betweenness (component normalized)",core_number="Core number",
    clustering="Local clustering",articulation="Articulation point"))
  c(detour_ratio="Detour ratio",betweenness="Edge betweenness",
    betweenness_normalized="Edge betweenness (component normalized)",triangle_count="Triangle count",
    effective_resistance="Effective resistance",bridge="Bridge",bridge_smaller_side="Bridge: smaller side",
    bridge_larger_side="Bridge: larger side",bridge_pairs="Bridge: separated pairs")
}

gflowui_ec_load_property_index <- function(index) {
  spec <- index$artifacts[["graph_properties.json"]]
  if(is.null(spec))return(NULL)
  value <- gflowui_ec_json(gflowui_ec_asset(index$root,spec))
  if(!identical(as.integer(value$schema_version),1L) ||
     !identical(value$algorithm,"unit-graph-properties-v1"))stop("Unsupported graph properties.")
  value
}

gflowui_ec_properties <- function(index,g) {
  spec <- index$property_index$graphs[[g$graph_id]]
  if(is.null(spec))return(NULL)
  value <- gflowui_ec_json(gflowui_ec_asset(index$root,spec))
  if(!identical(as.integer(value$schema_version),1L) || !identical(value$algorithm,"unit-graph-properties-v1") ||
     !identical(value$graph_id,g$graph_id) || !identical(value$graph_sha256,g$graph_sha256) ||
     !identical(value$graph_file_sha256,index$graphs[[g$graph_id]]$file$sha256) ||
     !identical(unlist(value$vertex_ids,use.names=FALSE),g$ids) ||
     !identical(value$edges,g$edges))stop("Graph property identity/order mismatch.")
  for(kind in c("vertex","edge")) {
    count <- if(kind=="vertex")g$n_vertices else g$n_edges
    for(key in names(gflowui_ec_property_names(kind))) {
      x <- value[[kind]][[key]]
      if(length(x)!=count)stop("Graph property length mismatch.")
      nullable <- kind=="edge" && key %in% c("detour_ratio","bridge_smaller_side","bridge_larger_side")
      good <- vapply(x,function(a) (nullable && is.null(a)) ||
        (is.numeric(a) && length(a)==1L && is.finite(a) && a>=0),TRUE)
      if(!all(good))stop("Invalid graph property value.")
      value[[kind]][[key]] <- vapply(x,function(a)if(is.null(a))NA_real_ else as.numeric(a),0.)
    }
  }
  bridges <- value$edge$bridge==1
  if(!identical(is.na(value$edge$detour_ratio),bridges) ||
     !identical(is.na(value$edge$bridge_smaller_side),!bridges) ||
     !identical(is.na(value$edge$bridge_larger_side),!bridges)) stop("Invalid bridge property encoding.")
  value$edge$detour_ratio[bridges] <- Inf
  value
}

gflowui_ec_property_text <- function(x) {
  out <- format(signif(x,4),trim=TRUE,scientific=FALSE)
  out[is.infinite(x)] <- "Inf (bridge)";out[is.na(x)] <- "not applicable"
  out
}

gflowui_ec_property_scale <- function(values,title,y=.75) {
  limits <- range(values[is.finite(values)])
  constant <- diff(limits)==0
  if(constant) limits[2] <- limits[1]+max(.01*abs(limits[1]),1)
  title <- sub("^(Vertex|Edge): ","\\1<br>",title)
  title <- sub(" (component normalized)","<br>normalized",title,fixed=TRUE)
  bar <- list(title=list(text=title),thickness=10,len=.35,y=y,x=1.02,
    tickformat=".3g",tickfont=list(size=10))
  if(constant)bar$tickvals <- limits[1]
  list(cauto=FALSE,cmin=limits[1],cmax=limits[2],
    colorscale=list(list(0,"#440154"),list(.5,"#21918C"),list(1,"#FDE725")),showscale=TRUE,
    colorbar=bar)
}

gflowui_ec_property_plot <- function(g,z,properties,vertex_color="#3575B2",edge_color="uniform",
    size=3,show_edges=TRUE,selected=character(),vertex_label="id",vertex_labels=TRUE,
    vertex_label_scope="selected",edge_label="none",edge_label_scope="selected") {
  p <- plotly::plot_ly();e <- g$edge_matrix
  vn <- gflowui_ec_property_names("vertex");en <- gflowui_ec_property_names("edge")
  vp <- properties$vertex;ep <- properties$edge
  add_edges <- function(p,which,line,name=NULL,legend=FALSE) {
    ee<-e[which,,drop=FALSE]
    coord<-function(j)as.vector(rbind(z[ee[,1],j],z[ee[,2],j],NA_real_))
    plotly::add_trace(p,x=coord(1),y=coord(2),z=coord(3),type="scatter3d",mode="lines",
      line=line,hoverinfo="skip",name=name,showlegend=legend,inherit=FALSE)
  }
  if(show_edges && nrow(e)) {
    if(edge_color %in% names(ep)) {
      val<-ep[[edge_color]];finite<-which(is.finite(val))
      if(length(finite)) {
        style<-c(list(color=rep(val[finite],each=3L),width=2),
          gflowui_ec_property_scale(val[finite],paste0("Edge: ",sub("Edge ","",en[[edge_color]])),.25))
        p<-add_edges(p,finite,style)
      }
      if(any(is.infinite(val)))p<-add_edges(p,which(is.infinite(val)),list(color="#D1495B",width=3),"Infinite detour (bridge)",TRUE)
      if(anyNA(val))p<-add_edges(p,which(is.na(val)),list(color="#B8BCC1",width=1),"Not applicable (nonbridge)",TRUE)
    } else p<-add_edges(p,seq_len(nrow(e)),gflowui_ec_edge_style(z,e,edge_color))
    # Midpoint hit targets expose edge values without treating edges as vertices.
    if(length(ep) && (edge_color %in% names(ep) || edge_label %in% names(ep))) {
      mid<-(z[e[,1],,drop=FALSE]+z[e[,2],,drop=FALSE])/2
      hover<-paste0(g$ids[e[,1]]," — ",g$ids[e[,2]])
      for(key in names(en))hover<-paste0(hover,"<br>",en[[key]],": ",gflowui_ec_property_text(ep[[key]]))
      p<-plotly::add_trace(p,x=mid[,1],y=mid[,2],z=mid[,3],type="scatter3d",mode="markers",
        marker=list(size=2,color="#777777",opacity=.12),text=hover,hoverinfo="text",showlegend=FALSE,inherit=FALSE)
      if(edge_label %in% names(ep)) {
        ii<-if(edge_label_scope=="all")seq_len(nrow(e)) else which(g$ids[e[,1]] %in% selected | g$ids[e[,2]] %in% selected)
        if(length(ii))p<-plotly::add_trace(p,x=mid[ii,1],y=mid[ii,2],z=mid[ii,3],type="scatter3d",mode="text",
          text=gflowui_ec_property_text(ep[[edge_label]][ii]),textfont=list(size=11,color="#39434A"),
          hoverinfo="skip",showlegend=FALSE,inherit=FALSE)
      }
    }
  }
  marker<-list(size=size,color=vertex_color)
  if(vertex_color %in% names(vp))marker<-c(list(size=size,color=vp[[vertex_color]]),
    gflowui_ec_property_scale(vp[[vertex_color]],paste0("Vertex: ",sub("Vertex ","",vn[[vertex_color]])),.78))
  else if(!startsWith(vertex_color,"#"))marker$color<-"#3575B2"
  hover<-g$ids
  for(key in names(vp))hover<-paste0(hover,"<br>",vn[[key]],": ",gflowui_ec_property_text(vp[[key]]))
  p<-plotly::add_trace(p,x=z[,1],y=z[,2],z=z[,3],type="scatter3d",mode="markers",marker=marker,
    customdata=g$ids,text=hover,hoverinfo="text",showlegend=FALSE,inherit=FALSE)
  idx<-which(g$ids %in% selected)
  # A larger outline indicates selection while preserving the property's color.
  if(length(idx)) {
    selected_marker<-list(size=size+3,color=if(vertex_color %in% names(vp))vp[[vertex_color]][idx] else marker$color,
      line=list(color="#E66101",width=3),showscale=FALSE)
    if(vertex_color %in% names(vp))selected_marker<-c(selected_marker,marker[c("cmin","cmax","colorscale","cauto")])
    p<-plotly::add_trace(p,x=z[idx,1],y=z[idx,2],z=z[idx,3],type="scatter3d",mode="markers",
      marker=selected_marker,customdata=g$ids[idx],text=hover[idx],hoverinfo="text",showlegend=FALSE,inherit=FALSE)
  }
  if(vertex_labels) {
    ii<-if(vertex_label_scope=="all")seq_along(g$ids) else idx
    label<-g$ids
    if(vertex_label %in% names(vp))label<-paste0(g$ids,": ",gflowui_ec_property_text(vp[[vertex_label]]))
    if(length(ii))p<-plotly::add_trace(p,x=z[ii,1],y=z[ii,2],z=z[ii,3],type="scatter3d",mode="text",
      text=label[ii],customdata=g$ids[ii],textposition="top center",textfont=list(size=12,color="#39434A"),
      showlegend=FALSE,hoverinfo="skip",inherit=FALSE)
  }
  p
}
