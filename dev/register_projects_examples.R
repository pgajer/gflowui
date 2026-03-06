pkgload::load_all("/Users/pgajer/current_projects/gflowui", export_all = FALSE)

gflowui::register_project(
  project_root = "/Users/pgajer/current_projects/symptoms",
  project_id = "symptoms",
  project_name = "Symptoms",
  profile = "symptoms_restart",
  overwrite = TRUE
)

gflowui::register_project(
  project_root = "/Users/pgajer/current_projects/AGP",
  project_id = "agp",
  project_name = "AGP",
  profile = "agp_restart",
  overwrite = TRUE
)

# optional quick check
gflowui::list_projects(include_manifests = FALSE)
