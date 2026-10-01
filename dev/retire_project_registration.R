# Retirement hides a viewer without deleting its manifest or research assets.
# Source this file, then call retire_project_registration(project_id).
retire_project_registration <- function(project_id) {
  reg <- gflowui:::gflowui_load_registry()
  idx <- match(project_id, reg$id)
  if (is.na(idx)) stop("Project is not registered.")
  manifest <- gflowui:::gflowui_read_manifest(reg$manifest_file[[idx]])
  stopifnot(is.list(manifest))
  archive <- file.path(gflowui:::gflowui_projects_data_dir(), "retired",
    basename(reg$manifest_file[[idx]]), format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(archive, recursive = TRUE, showWarnings = FALSE)
  snapshot <- list(retired_at = Sys.time(), registry_row = reg[idx, , drop = FALSE],
    manifest = manifest)
  saveRDS(snapshot, file.path(archive, "registration.rds"))
  stopifnot(identical(readRDS(file.path(archive, "registration.rds")), snapshot))
  gflowui::unregister_project(project_id, delete_manifest = FALSE)
  message("Registration archived at ", archive, "; all assets retained.")
  invisible(archive)
}
