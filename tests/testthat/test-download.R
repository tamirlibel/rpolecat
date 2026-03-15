test_that("download_polecat validates arguments", {
  tmp <- withr::local_tempdir()
  expect_error(download_polecat(tmp, skip_existing = "yes"), "skip_existing")
  expect_error(download_polecat(tmp, verbose = "yes"), "verbose")
  expect_error(download_polecat(tmp, dryrun = "yes"), "dryrun")
})

test_that("download_polecat creates directory if it doesn't exist", {
  # Use a non-existent subdir
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")
  tmp <- file.path(withr::local_tempdir(), "subdir")
  expect_false(dir.exists(tmp))

  # Should fail on API call but directory should be created first
  tryCatch(
    download_polecat(tmp, verbose = FALSE),
    error = function(e) NULL
  )
  expect_true(dir.exists(tmp))
})

test_that("compare_file_lists correctly identifies files to download", {
  remote <- data.frame(
    label = c("file1.txt", "file2.txt", "file3.txt"),
    id = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  local_files <- c("file1.txt", "other.txt")

  result <- compare_file_lists(remote, local_files)
  expect_equal(nrow(result$to_download), 2)
  expect_equal(nrow(result$already_present), 1)
  expect_equal(result$to_download$label, c("file2.txt", "file3.txt"))
})

test_that("compare_file_lists handles compressed local files", {
  remote <- data.frame(
    label = c("ngecEvents.20230103.txt"),
    id = 1L,
    stringsAsFactors = FALSE
  )
  local_files <- c("ngecEvents.20230103.txt.gz")

  result <- compare_file_lists(remote, local_files)
  expect_equal(nrow(result$to_download), 0)
  expect_equal(nrow(result$already_present), 1)
})

test_that("compare_file_lists identifies obsolete files", {
  remote <- data.frame(
    label = c("ngecEvents.20230103.txt"),
    id = 1L,
    stringsAsFactors = FALSE
  )
  local_files <- c("ngecEvents.20230103.txt", "ngecEvents.20220101.txt")

  result <- compare_file_lists(remote, local_files)
  expect_equal(length(result$obsolete), 1)
  expect_equal(result$obsolete, "ngecEvents.20220101.txt")
})

test_that("extract_data_year extracts years correctly", {
  fnames <- c(
    "ngecEvents.20230103111842.Release603.DV.txt",
    "ngecEventsDV-2022.txt.zip",
    "ngecEvents.20240215.Release700.DV.txt"
  )
  expect_equal(extract_data_year(fnames), c(2023L, 2022L, 2024L))
})
