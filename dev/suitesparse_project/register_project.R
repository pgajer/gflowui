args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) normalizePath(args[[1]],mustWork=TRUE) else "/Users/pgajer/current_projects/suitesparse_embedding_comparison"
pkgload::load_all(".",quiet=TRUE)
index <- gflowui:::gflowui_ec_load_index(root)
gflowui::register_project(project_root=root,project_id="suitesparse_3d_embedding_comparison",
  project_name="SuiteSparse 3D Embedding Comparison",profile="custom",scan_results=FALSE,
  metadata=list(embedding_comparison=list(schema_version=1L,data_root=root)),
  overwrite="suitesparse_3d_embedding_comparison" %in% gflowui::list_projects()$id)
cat("Registered",length(index$graphs),"graphs and",nrow(index$table),"run/availability rows.\n")
