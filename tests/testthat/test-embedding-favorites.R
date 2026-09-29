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
