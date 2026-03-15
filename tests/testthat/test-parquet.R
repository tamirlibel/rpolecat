test_that("write and read parquet roundtrips", {
  skip_if_not_installed("arrow")

  dat <- make_test_data(10)
  tmp <- withr::local_tempfile(fileext = ".parquet")

  write_polecat_parquet(dat, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)

  dat2 <- read_polecat_parquet(tmp)
  expect_s3_class(dat2, "data.frame")
  expect_equal(nrow(dat2), 10)
  expect_equal(names(dat2), names(dat))
})

test_that("read_polecat_parquet errors on missing file", {
  skip_if_not_installed("arrow")
  expect_error(read_polecat_parquet("/nonexistent.parquet"), "not found")
})

test_that("check_arrow errors if package missing", {
  skip_if(requireNamespace("arrow", quietly = TRUE),
          "arrow is installed, cannot test missing package error")
  expect_error(check_arrow(), "arrow")
})
