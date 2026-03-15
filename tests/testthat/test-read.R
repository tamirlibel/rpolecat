test_that("read_polecat_tsv reads a valid TSV file", {
  path <- test_path("fixtures", "sample.txt")
  dat <- read_polecat_tsv(path)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 5)
  expect_true("event_id" %in% names(dat))
  expect_true("story_date" %in% names(dat))
  expect_true("event_type" %in% names(dat))
})

test_that("read_polecat_tsv handles single quotes in values", {
  path <- test_path("fixtures", "sample_malformed.txt")
  dat <- read_polecat_tsv(path)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 2)
  # The value with "Merkel's" should be preserved
  expect_true(any(grepl("Merkel's", dat$source_agent)))
})

test_that("read_polecat reads a directory", {
  # Create a temp dir with a fixture file
  tmp <- withr::local_tempdir()
  file.copy(test_path("fixtures", "sample.txt"),
            file.path(tmp, "ngecEvents.20230103111842.Release603.DV.txt"))

  dat <- read_polecat(tmp, verbose = FALSE)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 5)
})

test_that("read_polecat filters by year", {
  tmp <- withr::local_tempdir()
  # Copy as a 2023 file
  file.copy(test_path("fixtures", "sample.txt"),
            file.path(tmp, "ngecEvents.20230103111842.Release603.DV.txt"))
  # Copy as a 2022 file
  file.copy(test_path("fixtures", "sample.txt"),
            file.path(tmp, "ngecEventsDV-2022.txt"))

  dat <- read_polecat(tmp, years = 2023, verbose = FALSE)
  expect_equal(nrow(dat), 5)  # Only the 2023 file
})

test_that("read_polecat errors on missing directory", {
  expect_error(read_polecat("/nonexistent/path"), "not found|does not exist|No POLECAT")
})

test_that("read_polecat errors on empty directory", {
  tmp <- withr::local_tempdir()
  expect_error(read_polecat(tmp, verbose = FALSE), "No POLECAT data files")
})

test_that("read_polecat reads a single file", {
  path <- test_path("fixtures", "sample.txt")
  dat <- read_polecat(path, verbose = FALSE)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 5)
})
