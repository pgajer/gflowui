test_that("favorites persist graph IDs, including empty and singleton selections", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive=TRUE))
  expect_identical(gflowui_ec_read_favorites(root), character())
  for(ids in list(character(), "HB/494_bus", c("HB/494_bus", "Bai/rdb450"))) {
    gflowui_ec_save_favorites(root, ids, c("HB/494_bus", "Bai/rdb450", "HB/bcsstk06"))
    expect_setequal(gflowui_ec_read_favorites(root), ids)
    record <- jsonlite::read_json(file.path(root, "favorites.json"))
    expect_setequal(unlist(record$unselected_graph_ids), setdiff(record$available_graph_ids, ids))
  }
  writeLines('{"schema_version": 9}', file.path(root, "favorites.json"))
  expect_error(gflowui_ec_read_favorites(root), "Invalid favorites")
  expect_match(paste(readLines(file.path(root, "favorites.json")), collapse=""), '9')
})

test_that("exports return exact existing paths without overwriting an earlier selection", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive=TRUE))
  gflowui_ec_save_favorites(root, "HB/494_bus", c("HB/494_bus", "Bai/rdb450"))
  first <- gflowui_ec_export_favorites(root, c("HB/494_bus", "Bai/rdb450"), root)
  second <- gflowui_ec_export_favorites(root, "HB/494_bus", root)
  expect_true(file.exists(first))
  expect_identical(first, normalizePath(first))
  expect_false(identical(first, second))
  expect_identical(jsonlite::read_json(first, simplifyVector=TRUE)$favorite_graph_ids, "HB/494_bus")
})
