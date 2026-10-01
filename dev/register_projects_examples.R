# Dataset preparation belongs in the analysis repository. Supply explicit assets.
gflowui::register_project(
  project_root = "~/my_project",
  project_id = "my_project",
  project_name = "My Project",
  profile = "custom",
  scan_results = FALSE,
  graph_sets = list(list(id = "main", label = "Main graph",
    graph_file = path.expand("~/my_project/results/graphs.rds"), k_values = 5L)),
  metadata = list(endpoint_label_provider = list(
    matrix_file = "data/features.rds", taxonomy_map_file = "data/taxonomy.rds"),
    subject_provider = list(rows_file = "data/subjects.rds")),
  overwrite = TRUE
)
