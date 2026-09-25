ec_fixture <- function() {
  root <- tempfile("ec-fixture-");dir.create(root)
  put <- function(path,value) {
    dir.create(dirname(file.path(root,path)),recursive=TRUE,showWarnings=FALSE)
    jsonlite::write_json(value,file.path(root,path),auto_unbox=TRUE,pretty=TRUE,null="null")
    list(path=path,sha256=digest::digest(file=file.path(root,path),algo="sha256"))
  }
  coordinates <- function(path,z) {
    dir.create(dirname(file.path(root,path)),recursive=TRUE,showWarnings=FALSE)
    utils::write.csv(z,file.path(root,path),row.names=FALSE)
    list(path=path,sha256=digest::digest(file=file.path(root,path),algo="sha256"))
  }
  graphs <- runs <- list()
  for (name in c("A","B")) {
    g <- list(graph_id=name,graph_sha256=paste0("graph-",name),vertex_ids=c("v1","v2","v3"),
      n_vertices=3L,n_edges=2L,n_components=1L,n_isolates=0L,degree_max=2L,
      mean_clustering=0,recipe="fixture unit path",edge_length=1,
      edges=list(c(0L,1L),c(1L,2L)),component_labels=c(0L,0L,0L))
    graphs[[name]] <- list(id=name,graph_sha256=g$graph_sha256,file=put(paste0("graphs/",name,"/graph.json"),g))
    for(seed in 1:2) {
      key <- paste0(name,seed)
      z <- data.frame(x=c(0,1,2),y=c(0,seed/10,0),z=c(0,0,.1))
      raw <- coordinates(paste0(key,"-raw.csv"),z)
      display <- coordinates(paste0(key,"-display.csv"),z)
      result <- list(status="completed",dimension=3L,graph_sha256=g$graph_sha256,
        method="metric_mds",seed=seed,coords_sha256=raw$sha256,display_sha256=display$sha256,
        input_type="graph distances",cross_component_pairs_excluded=0,
        components=list(list(details=list(termination="converged"))),
        summary=list(chord_error=seed/10,relative_stress=seed/100,path_error=.2,edge_error=.3,
          distance_rank_correlation=.9,neighborhood=list(trustworthiness_5=.9,continuity_5=.8)))
      runs[[key]] <- list(id=key,graph_id=name,method="metric_mds",seed=seed,status="completed",
        result=put(paste0(key,"-result.json"),result),raw_coordinates=raw,display_coordinates=display,
        vertices=put(paste0(key,"-ids.json"),g$vertex_ids),
        diagnostics=put(paste0(key,"-diagnostics.json"),list(coordinates_sha256=raw$sha256,
          shepard=list(list(original_distance=1,fitted_chord=1)),edge_residuals=c(0,.1),
          exact_pair_count=3,sampling="all pairs",scale="fitted")))
    }
    runs[[paste0(name,"-failed")]] <- list(id=paste0(name,"-failed"),graph_id=name,
      method="umap",seed=17L,status="resource_limited",reason="memory allowance exceeded")
  }
  put("viewer_manifest.json",list(kind="gflowui_embedding_comparison",schema_version=1L,
    graphs=unname(graphs),runs=unname(runs),artifacts=list(),indexes=list()))
  root
}

test_that("edge length colors use exact segment lengths without changing geometry",{
  z<-rbind(c(0,0,0),c(1,0,0),c(1,2,0))
  e<-rbind(c(1,2),c(2,3))
  uniform<-gflowui_ec_edge_style(z,e)
  expect_identical(uniform$color,"#AAB1B8")
  style<-gflowui_ec_edge_style(z,e,"length")
  expect_equal(style$color,c(1,1,1,2,2,2))
  expect_equal(c(style$cmin,style$cmax),c(1,2))
  expect_true(style$showscale)
  expect_identical(style$colorscale[[1]][[2]],"#C7782A")
  expect_identical(style$colorscale[[2]][[2]],"#3575B2")
  single<-gflowui_ec_edge_style(z,e[1,,drop=FALSE],"length")
  expect_true(single$cmin<1 && single$cmax>1)
  expect_identical(gflowui_ec_edge_style(z,matrix(integer(),0,2),"length"),uniform)
  expect_identical(unname(gflowui_ec_methods()["sfdp"]),"SFDP — Yifan Hu")
})

test_that("length color changes preserve camera, selection and Inspector data",{
  root<-ec_fixture();on.exit(unlink(root,recursive=TRUE))
  manifest<-shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact();session$setInputs(graph="A",run="A1")
    session$setInputs(vertex_click=list(id="v2",nonce=1),camera=list(eye=list(x=2,y=3,z=4),center=list(x=0,y=0,z=0),up=list(x=0,y=0,z=1)))
    first<-output$graph_plot;table<-output$quality_table;overview<-output$overview
    before<-as.list(counts)
    session$setInputs(edge_coloring="length")
    colored<-output$graph_plot
    plot<-jsonlite::fromJSON(colored,simplifyVector=FALSE)
    line<-Filter(function(t)identical(t$mode,"lines"),plot$x$data)[[1]]
    expect_true(line$line$showscale)
    expect_length(unlist(line$line$color),6L)
    expect_identical(output$quality_table,table);expect_identical(output$overview,overview)
    expect_equal(counts$index,before$index);expect_equal(counts$layout,before$layout)
    expect_identical(selected_vertices(),"v2")
    expect_equal(plot$x$layout$scene$camera$eye,list(x=2,y=3,z=4))
    after<-as.list(counts)
    session$setInputs(camera=list(eye=list(x=3,y=4,z=5),center=list(x=0,y=0,z=0),up=list(x=0,y=0,z=1)))
    expect_identical(as.list(counts),after)
    expect_identical(output$graph_plot,colored)
  })
})

test_that("higher-budget attempts are distinct without relabeling historical runs",{
  root<-ec_fixture();on.exit(unlink(root,recursive=TRUE))
  path<-file.path(root,"viewer_manifest.json");doc<-gflowui_ec_json(path)
  doc$runs[[1]]$attempt_label<-"30 GiB; no time limit; serial"
  jsonlite::write_json(doc,path,auto_unbox=TRUE,null="null")
  idx<-gflowui_ec_load_index(root)
  expect_match(idx$table$settings[1],"30 GiB; no time limit; serial",fixed=TRUE)
  expect_identical(idx$table$settings[2],"fixed pilot settings")
  app_manifest<-shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=app_manifest),{
    session$flushReact()
    expect_match(paste(as.character(output$quality_table),collapse="\n"),"30 GiB; no time limit; serial",fixed=TRUE)
  })
})

test_that("sampled evaluation and vector exports preserve intervals and selected graph",{
  root<-ec_fixture();on.exit(unlink(root,recursive=TRUE))
  path<-file.path(root,"A1-result.json");r<-gflowui_ec_json(path)
  r$summary$n_pairs<-1000;r$summary$evaluation<-list(mode="uniform_pair_sample",pair_count=100)
  r$summary$intervals<-list(chord_error=list(lower=.08,upper=.12))
  jsonlite::write_json(r,path,auto_unbox=TRUE,pretty=TRUE,null="null")
  manifest<-gflowui_ec_json(file.path(root,"viewer_manifest.json"))
  manifest$runs[[1]]$result$sha256<-digest::digest(file=path,algo="sha256")
  jsonlite::write_json(manifest,file.path(root,"viewer_manifest.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  index<-gflowui_ec_load_index(root)
  expect_identical(index$table$evaluation[1],"uniform_pair_sample")
  expect_equal(index$table$chord_error_lower[1],.08)
  expect_equal(index$table$evaluated_pairs[1],100)
  app_manifest<-shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=app_manifest),{
    session$flushReact();session$setInputs(graph="A",run="A1")
    plot<-jsonlite::fromJSON(output$metric_plot,simplifyVector=FALSE)
    line<-Filter(function(trace)identical(trace$mode,"lines"),plot$x$data)[[1]]
    expect_equal(unlist(line$x[1:2]),c(.08,.12))
    expect_match(output$evaluation_note,"approximate 95%",fixed=TRUE)
    expect_match(paste(as.character(output$quality_table),collapse="\n"),"0.08",fixed=TRUE)
  })
  dest<-tempfile("publication-");dir.create(dest);on.exit(unlink(dest,recursive=TRUE),add=TRUE)
  zip<-gflowui_ec_export(index,list(graph_id="A",run_id="A1"),dest)
  stage<-file.path(dest,"unzipped");utils::unzip(zip,exdir=stage)
  expect_true(file.exists(file.path(stage,"figures/chord_error.pdf")))
  expect_true(file.exists(file.path(stage,"figures/chord_error.svg")))
  expect_true(file.exists(file.path(stage,"rebuild_publication_figures.R")))
  hashes<-gflowui_ec_json(file.path(stage,"bundle_checksums.json"))
  expect_true(all(vapply(names(hashes),function(p)identical(hashes[[p]],digest::digest(file=file.path(stage,p),algo="sha256")),TRUE)))
  e<-new.env();old<-setwd(stage);on.exit(setwd(old),add=TRUE)
  source("rebuild_publication_figures.R",local=e)
  expect_true(file.info("figures/chord_error.pdf")$size>1000)
})

test_that("embedding adapter validates identity and finite exact 3D coordinates",{
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  idx <- gflowui_ec_load_index(root);g <- gflowui_ec_graph(idx,"A")
  expect_equal(nrow(idx$table),6L)
  expect_equal(dim(gflowui_ec_load_run(idx,g,"A1")$display),c(3L,3L))
  expect_error(gflowui_ec_load_run(idx,g,"B1"),"No completed")
  expect_error(gflowui_ec_load_run(idx,g,"A-failed"),"No completed")
  expect_error(gflowui_ec_asset(root,list(path="../escape",sha256="bad")),"Unsafe")
  expect_error(gflowui_ec_asset(root,list(path="/absolute",sha256="bad")),"Unsafe")
  bad <- idx;r <- bad$runs$A1
  r$vertices$sha256 <- "wrong";bad$runs$A1 <- r
  expect_error(gflowui_ec_load_run(bad,g,"A1"),"changed")
  path <- file.path(root,"bad.csv")
  for(z in list(data.frame(x=1:3,y=1:3),data.frame(x=1:3,y=1:3,z=c(1,Inf,3)))) {
    utils::write.csv(z,path,row.names=FALSE)
    expect_error(gflowui_ec_coordinates(path,g$ids,g$ids),"finite n-by-3")
  }
  expect_error(gflowui_ec_coordinates(file.path(root,"A1-raw.csv"),rev(g$ids),g$ids),"vertex order")
})

test_that("camera updates do not rerender Inspector or reload geometry",{
  skip_if_not_installed("plotly");skip_if_not_installed("htmlwidgets")
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  manifest <- shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact()
    session$setInputs(graph="A",run="A1")
    first <- output$graph_plot;overview <- output$overview;table <- output$quality_table
    before <- as.list(counts)
    session$setInputs(camera=list(eye=list(x=2,y=3,z=4),center=list(x=0,y=0,z=0),up=list(x=0,y=0,z=1)))
    expect_identical(as.list(counts),before)
    expect_identical(output$overview,overview)
    expect_identical(output$quality_table,table)
    expect_identical(output$graph_plot,first)
    session$setInputs(vertex_click=list(id="v2",nonce=1))
    expect_identical(selected_vertices(),"v2")
    session$setInputs(run="A2")
    expect_identical(run_id(),"A2")
    expect_identical(selected_vertices(),"v2")
    expect_equal(camera()$eye$x,2)
    plot <- jsonlite::fromJSON(output$metric_plot,simplifyVector=FALSE)
    expect_equal(plot$x$layout$shapes[[1L]]$x0,.2)
    expect_equal(plot$x$layout$shapes[[1L]]$yref,"paper")
    expect_equal(plot$x$layout$yaxis$title,"")
    expect_match(output$metric_note,"0.2",fixed=TRUE)
    session$setInputs(graph="B")
    expect_identical(run_id(),"B1")
    expect_length(selected_vertices(),0)
    expect_null(camera())
  })
})

test_that("ZIP exports all runs and rejects a changed manifest",{
  skip_if_not_installed("zip")
  root <- ec_fixture();dest <- tempfile("ec-export-")
  on.exit(unlink(c(root,dest),recursive=TRUE))
  idx <- gflowui_ec_load_index(root)
  bundle <- gflowui_ec_export(idx,list(graph="A",run="A1"),dest)
  expect_true(file.exists(bundle));expect_true(startsWith(bundle,normalizePath(dest)))
  restored <- file.path(dest,"restored");dir.create(restored)
  utils::unzip(bundle,exdir=restored)
  expect_equal(nrow(gflowui_ec_load_index(restored)$table),6L)
  checks <- gflowui_ec_json(file.path(restored,"bundle_checksums.json"))
  for(path in names(checks))expect_identical(digest::digest(file=file.path(restored,path),algo="sha256"),checks[[path]])
  writeLines("changed",file.path(root,"viewer_manifest.json"))
  expect_error(gflowui_ec_export(idx,list(),dest),"changed")
})

test_that("undefined scores and failed-only graphs never receive a guide or layout",{
  skip_if_not_installed("plotly")
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  path <- file.path(root,"viewer_manifest.json");doc <- gflowui_ec_json(path)
  for(i in seq_along(doc$runs)) {
    run <- doc$runs[[i]]
    if(run$graph_id=="B")doc$runs[[i]]$status <- "unsupported"
    if(run$status=="completed" && run$graph_id=="A") {
      file <- file.path(root,run$result$path);value <- gflowui_ec_json(file)
      value$summary$distance_rank_correlation <- NULL
      jsonlite::write_json(value,file,auto_unbox=TRUE,null="null")
      doc$runs[[i]]$result$sha256 <- digest::digest(file=file,algo="sha256")
    }
  }
  jsonlite::write_json(doc,path,auto_unbox=TRUE,null="null")
  manifest <- shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact()
    session$setInputs(graph="A",metric="distance_rank_correlation")
    expect_match(output$metric_note,"no reference line")
    expect_error(output$metric_plot,"unavailable")
    session$setInputs(graph="B")
    expect_identical(run_id(),"")
    expect_error(current(),"No completed")
    expect_match(output$quality_table$html,"unsupported")
    expect_match(output$quality_table$html,"disabled")
  })
})

test_that("bundle figure settings record disabled edges and selected-vertex labels",{
  skip_if_not_installed("plotly")
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  saved <- NULL
  testthat::local_mocked_bindings(gflowui_ec_export=function(index,settings,output_dir){
    saved <<- settings
    file.path(output_dir,"test-bundle.zip")
  },.package="gflowui")
  manifest <- shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact()
    session$setInputs(edges=FALSE,labels=FALSE,export_dir=root)
    session$setInputs(save_bundle=1L)
    expect_false(saved$edges);expect_false(saved$labels)
    expect_identical(saved$edge_coloring,"uniform")
    expect_identical(saved$graph_id,"A");expect_identical(saved$run_id,"A1")
    session$setInputs(edges=TRUE,labels=TRUE,edge_coloring="length")
    session$setInputs(save_bundle=2L)
    expect_true(saved$edges);expect_true(saved$labels)
    expect_identical(saved$edge_coloring,"length")
  })
})
test_that("seed ranges separate settings and retain failures without invented scores", {
  rows <- data.frame(graph_id=rep("g",5),method=c("A","A","A","A","B"),
    settings=c("one","one","one","two","one"),status=c("completed","completed","failed","completed","unsupported"),
    chord_error=c(.2,.4,NA,.8,NA))
  result <- gflowui_ec_seed_ranges(rows,"g")
  expect_equal(result$Available,c(2L,1L,0L))
  expect_equal(result$Listed,c(3L,1L,1L))
  expect_equal(result$Mean,c(.3,.8,NA))
  expect_equal(result$Minimum,c(.2,.8,NA))
  expect_equal(result$Maximum,c(.4,.8,NA))
  expect_equal(nrow(gflowui_ec_seed_ranges(rows,"absent")),0L)
})

test_that("LGS synthetic locality plots stay distinct and do not rerender graph", {
  skip_if_not_installed("plotly")
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  rows <- lapply(c(.2,.3,.4),function(value)list(graph_id="validation_path48",status="completed",
    locality=list(component_k=list(16),component_fraction=list(16/47)),
    scores=list(chord_error=value,edge_error=2*value)))
  data <- list(validation_rows=rows,gallery_rows=list(list(graph_id="A",status="resource_limited",reason="time projection exceeds budget")))
  jsonlite::write_json(data,file.path(root,"phase04_lgs_summary.json"),auto_unbox=TRUE)
  path <- file.path(root,"viewer_manifest.json");doc <- gflowui_ec_json(path)
  doc$artifacts[["phase04_lgs_summary.json"]] <- list(path="phase04_lgs_summary.json",
    sha256=digest::digest(file=file.path(root,"phase04_lgs_summary.json"),algo="sha256"))
  doc$runs[[3]]$method <- "lgs_paper"
  doc$runs[[3]]$locality <- list(component_k=list(16,0),component_fraction=list(1/3,NULL))
  jsonlite::write_json(doc,path,auto_unbox=TRUE,null="null")
  idx <- gflowui_ec_load_index(root)
  expect_match(idx$table$settings[3],"k=16 \\(33.3%\\); 60 epochs")
  manifest <- shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact()
    expect_match(output$lgs_note$html,"not the selected SuiteSparse graph")
    expect_match(output$lgs_note$html,"time projection exceeds budget")
    plot <- jsonlite::fromJSON(output$lgs_plot,simplifyVector=FALSE)
    expect_equal(unlist(plot$x$data[[1]]$y),.3)
    expect_equal(unlist(plot$x$data[[1]]$error_y$array),.1)
    graph <- output$graph_plot;before <- as.list(counts)
    session$setInputs(lgs_metric="edge_error")
    plot <- jsonlite::fromJSON(output$lgs_plot,simplifyVector=FALSE)
    expect_equal(unlist(plot$x$data[[1]]$y),.6)
    expect_identical(output$graph_plot,graph)
    expect_identical(as.list(counts),before)
  })
})

test_that("real ZIP observer records resolved default and selected LGS figures", {
  skip_if_not_installed("plotly");skip_if_not_installed("zip")
  root <- ec_fixture();on.exit(unlink(root,recursive=TRUE))
  rows <- lapply(c(.2,.4,.6),function(value)list(graph_id="validation_cliques48",status="completed",
    locality=list(component_k=list(16),component_fraction=list(16/47)),
    scores=list(chord_error=value/2,edge_error=value)))
  file <- file.path(root,"phase04_lgs_summary.json")
  jsonlite::write_json(list(validation_rows=rows,gallery_rows=list()),file,auto_unbox=TRUE)
  path <- file.path(root,"viewer_manifest.json");doc <- gflowui_ec_json(path)
  doc$artifacts[["phase04_lgs_summary.json"]] <- list(path="phase04_lgs_summary.json",sha256=digest::digest(file=file,algo="sha256"))
  jsonlite::write_json(doc,path,auto_unbox=TRUE,null="null")
  manifest <- shiny::reactiveVal(list(metadata=list(embedding_comparison=list(schema_version=1L,data_root=root))))
  shiny::testServer(gflowui_ec_server,args=list(manifest=manifest),{
    session$flushReact();session$setInputs(export_dir=file.path(root,"exports"))
    session$setInputs(save_bundle=1L)
    files <- list.files(file.path(root,"exports"),full.names=TRUE,pattern="zip$")
    expect_length(files,1L)
    read_spec <- function(path,folder) {
      dir.create(folder);utils::unzip(path,files="figure_specifications.json",exdir=folder)
      gflowui_ec_json(file.path(folder,"figure_specifications.json"))
    }
    spec <- read_spec(files[1],file.path(root,"default-spec"))
    expect_identical(spec$lgs_fixture,"validation_path48")
    expect_identical(spec$lgs_metric,"chord_error")
    session$setInputs(lgs_fixture="validation_cliques48",lgs_metric="edge_error")
    plot <- jsonlite::fromJSON(output$lgs_plot,simplifyVector=FALSE)
    expect_equal(unlist(plot$x$data[[1]]$y),.4)
    session$setInputs(save_bundle=2L)
    new <- setdiff(list.files(file.path(root,"exports"),full.names=TRUE,pattern="zip$"),files)
    expect_length(new,1L)
    spec <- read_spec(new,file.path(root,"selected-spec"))
    expect_identical(spec$graph_id,"A")
    expect_identical(spec$lgs_fixture,"validation_cliques48")
    expect_identical(spec$lgs_metric,"edge_error")
    expect_identical(spec$lgs_variant,"lgs-paper-union-v1")
    expect_match(spec$lgs_population,"distinct from selected gallery")
    expect_match(spec$lgs_aggregation,"not confidence intervals")
    expect_equal(mean(vapply(rows,function(r)r$scores[[spec$lgs_metric]],0.)),unlist(plot$x$data[[1]]$y))
  })
})
