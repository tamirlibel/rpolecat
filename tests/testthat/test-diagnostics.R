test_that("dr_polecat runs without error", {
  withr::local_options(
    rpolecat.data_dir = NULL,
    rpolecat.use_db = FALSE,
    rpolecat.db_path = NULL
  )
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")

  expect_no_error(result <- dr_polecat())
  expect_type(result, "list")
  expect_false(result$has_token)
})

test_that("dr_polecat reports local files when data_dir exists", {
  tmp <- withr::local_tempdir()
  writeLines("test", file.path(tmp, "ngecEvents.20230103.DV.txt"))

  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")
  result <- dr_polecat(data_dir = tmp)
  expect_s3_class(result$local, "data.frame")
  expect_equal(nrow(result$local), 1)
})
