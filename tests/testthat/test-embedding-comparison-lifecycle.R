test_that("delayed full-app workspace insertion binds the divider",{
  node <- Sys.which("node")
  skip_if(!nzchar(node),"Node.js is required for the lifecycle regression")
  script <- system.file("app/www/embedding-comparison.js",package="gflowui")
  result <- system2(node,c(shQuote(test_path("embedding-comparison-lifecycle.js")),shQuote(script)),
    stdout=TRUE,stderr=TRUE)
  expect_equal(attr(result,"status") %||% 0L,0L,info=paste(result,collapse="\n"))
})
