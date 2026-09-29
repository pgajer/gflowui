test_that("graph navigation preserves method and configuration with SGD default", {
  rows <- data.frame(id=c("a-grip","a-sgd","b-grip","b-sgd","b-sparse"),
    method=c("Weighted GRIP","Metric MDS — SGD","Weighted GRIP","Metric MDS — SGD","Metric MDS — SGD"),
    settings=c("weighted","full","weighted","full","sparse"))
  b <- rows[3:5,]
  expect_identical(gflowui_ec_choose_run(b,rows,NULL),"b-sgd")
  expect_identical(gflowui_ec_choose_run(b,rows,"a-grip"),"b-grip")
  expect_identical(gflowui_ec_choose_run(b,rows,"a-sgd"),"b-sgd")
  expect_identical(gflowui_ec_choose_run(b,rows,"b-sparse"),"b-sparse")
  expect_identical(gflowui_ec_choose_run(b[0,],rows,"a-sgd"),"")
  ui <- as.character(gflowui_ec_sidebar_ui("test"))
  expect_match(ui,"Previous")
  expect_match(ui,"Next")
})
