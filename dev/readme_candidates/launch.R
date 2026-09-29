args <- commandArgs(trailingOnly=TRUE)
port <- if(length(args)) as.integer(args[[1]]) else 3888L
options(rgl.useNULL=TRUE)
pkgload::load_all('/Users/pgajer/current_projects/gflowui',quiet=TRUE)
gflowui::run_gflowui(host='127.0.0.1',port=port,launch.browser=FALSE)
