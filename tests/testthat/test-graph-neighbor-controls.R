test_that("graphs without a neighbor parameter hide k controls but retain state", {
  state <- list(neighbor_parameter = FALSE, k_choices = "1", k_selected = 1L,
    optimal_choices = c("Criterion" = "criterion"), optimal_selected = "criterion")
  html <- as.character(gflowui:::gflowui_graph_neighbor_controls(state))
  expect_match(html, 'id="graph_k"', fixed = TRUE)
  expect_match(html, 'display: none;', fixed = TRUE)
  expect_match(html, 'aria-hidden="true"', fixed = TRUE)
  expect_false(grepl('Set Reference',html,fixed=TRUE))
  expect_false(grepl('>k:</span>', html, fixed = TRUE))
  expect_false(grepl('Optimal k:', html, fixed = TRUE))
  expect_false(grepl('graph_optimal_method', html, fixed = TRUE))
  state$neighbor_parameter <- NULL
  default_html <- as.character(gflowui:::gflowui_graph_neighbor_controls(state))
  state$neighbor_parameter <- TRUE
  expect_identical(default_html, as.character(gflowui:::gflowui_graph_neighbor_controls(state)))
  expect_match(default_html, '>k:</span>', fixed = TRUE)
  expect_match(default_html, 'Optimal k:', fixed = TRUE)
  expect_false(grepl('display: none;', default_html, fixed = TRUE))
})

test_that("manifest normalization preserves the explicit neighbor flag", {
  gs <- gflowui:::gflowui_normalize_graph_sets_manifest(list(list(id="full", label="Full",
    neighbor_parameter=FALSE), list(id="knn", label="Neighbors")))
  expect_false(gflowui:::gflowui_has_neighbor_parameter(gs[[1]]))
  expect_true(gflowui:::gflowui_has_neighbor_parameter(gs[[2]]))
})

test_that("conditional neighbor selector follows construction while retaining selection", {
  helpers<-gflowui:::gflowui_make_server_graph_structure_helpers(new.env())
  sets<-c(list(list(id="fermat",label="Fermat",construction="Complete Fermat",neighbors="complete",k_values=1L),
    list(id="pca",label="PCA",construction="PCA projection",neighbors="none",k_values=1L)),
    lapply(3:10,function(k)list(id=paste0("knn",k),label=paste("kNN",k),construction="Symmetric kNN + MST",neighbors=as.character(k),k_values=k)))
  manifest<-list(defaults=list(graph_set_id="fermat"),metadata=list(graph_selector_schema=list(fields=list(
    list(id="construction",field="construction"),list(id="neighbors",field="neighbors",label="Neighbors (k)",order=as.character(3:10),
      show_when=list(construction="Symmetric kNN + MST"))))))
  resolve<-function(construction,neighbors="8")helpers$resolve_graph_selection(manifest,sets,
    input_selector_values=list(construction=construction,neighbors=neighbors))
  knn<-resolve("Symmetric kNN + MST")
  expect_identical(knn$set_id,"knn8")
  expect_true(knn$selector_fields[[2]]$visible)
  expect_equal(unname(knn$selector_fields[[2]]$choices),as.character(3:10))
  for(construction in c("Complete Fermat","PCA projection")) {
    other<-resolve(construction)
    expect_false(other$selector_fields[[2]]$visible)
    expect_equal(other$k_selected,1L)
    expect_length(other$selector_fields[[2]]$choices,1)
  }
  # No condition preserves the established behavior in other projects.
  manifest$metadata$graph_selector_schema$fields[[2]]$show_when<-NULL
  expect_true(resolve("Complete Fermat")$selector_fields[[2]]$visible)
})
