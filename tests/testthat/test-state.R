test_that("get_local_state returns empty data frame for empty directory", {
  tmp <- withr::local_tempdir()
  result <- get_local_state(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("file_name" %in% names(result))
})

test_that("get_local_state detects polecat files", {
  tmp <- withr::local_tempdir()
  # Create a fake polecat file
  writeLines("test", file.path(tmp, "ngecEvents.20230103.Release603.DV.txt"))

  result <- get_local_state(tmp)
  expect_equal(nrow(result), 1)
  expect_equal(result$data_year, 2023L)
  expect_false(result$compressed)
})

test_that("get_local_state identifies compressed files", {
  tmp <- withr::local_tempdir()
  writeLines("test", file.path(tmp, "ngecEvents.20230103.DV.txt.gz"))

  result <- get_local_state(tmp)
  expect_equal(nrow(result), 1)
  expect_true(result$compressed)
})

test_that("get_local_state errors on nonexistent directory", {
  expect_error(get_local_state("/nonexistent/dir"), "does not exist")
})

test_that("get_local_state ignores non-polecat files", {
  tmp <- withr::local_tempdir()
  writeLines("test", file.path(tmp, "ngecEvents.20230103.DV.txt"))
  writeLines("test", file.path(tmp, "other_file.txt"))

  result <- get_local_state(tmp)
  expect_equal(nrow(result), 1)
})

test_that("get_dataverse_state calls get_dataverse_file_list", {
  # Just verify it's a wrapper
  expect_identical(body(get_dataverse_state), body(function() get_dataverse_file_list()))
})
