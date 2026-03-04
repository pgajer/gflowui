gflowui_make_server_graph_helpers <- function(rv) {
  structure_helpers <- gflowui_make_server_graph_structure_helpers(rv = rv)
  renderer_helpers <- gflowui_make_server_renderer_helpers(
    rv = rv,
    current_reference_info = structure_helpers$current_reference_info
  )

  dup <- intersect(names(structure_helpers), names(renderer_helpers))
  if (length(dup) > 0L) {
    stop(
      sprintf("Duplicate graph helper bindings: %s", paste(dup, collapse = ", ")),
      call. = FALSE
    )
  }

  c(structure_helpers, renderer_helpers)
}
