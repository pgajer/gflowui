test_that("snapshot toggles preserve the sidebar and do not redraw an already visible arm", {
  root <- withr::local_tempdir()
  withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
  adj <- list(2L,c(1L,3L),c(2L,4L),3L)
  weights <- lapply(adj,function(x)rep(1,length(x)))
  graph_file <- file.path(root,"graph.rds")
  saveRDS(list(X.graphs=list(list(adj_list=adj,weight_list=weights)),
    k.values=1L,vertex_ids=letters[1:4]),graph_file)
  register_project(root,"arm-toggle",profile="custom",
    graph_sets=list(list(id="graph",label="Graph",graph_file=graph_file,k_values=1L)),
    defaults=list(graph_set_id="graph",reference_graph_set_id="graph",reference_k=1L),scan_results=FALSE)
  variant <- compute_arm_variant(adj.list=adj,weight.list=weights,
    coords=cbind(1:4,0,0),endpoint_a=1L,endpoint_b=4L,endpoint_a_key="a",endpoint_b_key="d",endpoint_a_label="Li",endpoint_b_label="Lc",thickening_method="path_only")
  shiny::testServer(app_server, {
    open_project("arm-toggle"); session$flushReact()
    ctx <- current_arm_graph_context()
    working <- upsert_working_arm_variant_state(empty_working_arm_state(ctx),variant)
    save_working_arm_state(working,ctx)
    session$flushReact()
    snap <- save_working_arm_snapshot(); session$flushReact()
    expect_true(snap$ok)
    files <- c(arm_dataset_row_by_id(snap$dataset_id)$workspace_file,
      file.path(arm_snapshot_dir(ctx$graph_set_id,ctx$k,ctx$project_id),paste0(snap$dataset_id,".rds")))
    hashes <- tools::md5sum(files)
    counts <- new.env(); counts$overlay <- 0L
    watch_overlay <- shiny::observe({ arm_overlay_active(); counts$overlay <- counts$overlay+1L })
    session$flushReact()
    initial_overlay <- counts$overlay; initial_sidebar <- output$workflow_controls
    for (flag in c(TRUE,FALSE,TRUE,FALSE)) {
      session$setInputs(arm_dataset_toggle=list(dataset_id=snap$dataset_id,checked=flag))
      expect_identical(snap$dataset_id %in% arm_overlay_selection(),flag)
      expect_length(arm_overlay_active()$arms,1L)
      expect_identical(output$workflow_controls,initial_sidebar)
      expect_identical(grepl('checked="checked"', output$arm_dataset_table$html, fixed=TRUE),flag)
    }
    expect_equal(counts$overlay,initial_overlay)
    # With working arms hidden, the saved snapshot must still show/hide normally.
    session$setInputs(arm_show_working_set=FALSE)
    expect_length(arm_overlay_active()$arms,0L)
    session$setInputs(arm_dataset_toggle=list(dataset_id=snap$dataset_id,checked=TRUE))
    expect_length(arm_overlay_active()$arms,1L)
    session$setInputs(arm_dataset_toggle=list(dataset_id=snap$dataset_id,checked=FALSE))
    expect_length(arm_overlay_active()$arms,0L)
    expect_identical(tools::md5sum(files),hashes)
    watch_overlay$destroy()
  })
})
