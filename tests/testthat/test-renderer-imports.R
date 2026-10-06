test_that("installed renderer declares its pipe dependency", {
  imports<-getNamespaceImports("gflowui")
  expect_true("%>%" %in% names(imports$plotly))
  expect_true(is.function(get("%>%",envir=asNamespace("gflowui"))))
})
