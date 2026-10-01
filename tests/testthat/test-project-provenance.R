test_that("registration snapshots documents and preserves provenance across updates", {
 root<-tempfile();dir.create(root);root<-normalizePath(root)
 withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
 doc<-file.path(root,"methods.md");writeLines("Original methods",doc)
 p<-list(summary="A provenance test",reproduction="Rscript analysis.R",documents=list(list(path=doc,label="Methods")),
   assets=list(list(path="missing.rds",role="Input",description="Expected input")))
 a<-register_project(root,project_id="test",scan_results=FALSE,provenance=p)
 d<-a$manifest$provenance$documents[[1]]
 expect_false(identical(d$path,doc));expect_equal(readLines(d$path),"Original methods")
 expect_equal(d$sha256,digest::digest(file=doc,algo="sha256"))
 writeLines("Changed methods",doc)
 expect_equal(readLines(d$path),"Original methods")
 b<-register_project(root,project_id="test",scan_results=FALSE,overwrite=TRUE)
 expect_identical(a$manifest$provenance,b$manifest$provenance)
 p2<-b$manifest$provenance;p2$summary<-"Updated summary"
 set_project_provenance("test",p2)
 m<-gflowui_read_manifest(gflowui_manifest_path("test"))
 expect_equal(m$provenance$documents[[1]]$path,d$path)
 expect_equal(m$provenance$summary,"Updated summary")
 expect_length(list.files(file.path(root,"registry/projects/test/provenance/history")),1)
 inventory<-gflowui_provenance_inventory(m)
 expect_equal(inventory$Status[inventory$Role=="Input"],"Missing")
 plan<-gflowui_project_delete_plan("test")
 expect_false(doc%in%plan$files$path)
 expect_true(d$path%in%plan$files$path)
})

test_that("invalid provenance leaves the registered record intact",{
 root<-tempfile();dir.create(root)
 withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry")))
 a<-register_project(root,project_id="test",scan_results=FALSE,provenance=list(summary="Original"))
 expect_error(set_project_provenance("test",list(documents=list(list(path="absent.md")))),"not found")
 expect_error(register_project(root,project_id="test",scan_results=FALSE,overwrite=TRUE,
  provenance=list(assets=list(list(path="x",sha256="bad")))),"sha256")
 expect_identical(gflowui_read_manifest(a$manifest_file),a$manifest)
 html<-as.character(gflowui_provenance_ui(list(summary="<script>bad</script>",recorded_at="now")))
 expect_match(html,"&lt;script&gt;",fixed=TRUE)
})

test_that("Settings can edit provenance without changing graph assets",{
 root<-tempfile();dir.create(root)
 withr::local_options(list(gflowui.projects_data_dir=file.path(root,"registry"),rgl.useNULL=TRUE))
 a<-register_project(root,project_id="test",scan_results=FALSE,provenance=list(summary="Original"))
 shiny::testServer(app_server,{
  open_project("test");session$flushReact()
  session$setInputs(edit_project_provenance=1);session$flushReact()
  session$setInputs(provenance_edit_summary="Updated in Settings",save_project_provenance=1);session$flushReact()
  expect_equal(active_manifest()$provenance$summary,"Updated in Settings")
  expect_equal(active_manifest()$graph_sets,a$manifest$graph_sets)
  expect_match(output$project_provenance_content$html,"Updated in Settings",fixed=TRUE)
 })
})
