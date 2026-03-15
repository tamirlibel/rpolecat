test_that("compress_polecat compresses and decompresses roundtrip", {
  tmp <- withr::local_tempdir()

  # Create a fake polecat txt file
  src <- test_path("fixtures", "sample.txt")
  dest <- file.path(tmp, "ngecEvents.20230103.Release603.DV.txt")
  file.copy(src, dest)

  # Compress
  result <- compress_polecat(tmp, action = "compress", remove = TRUE, verbose = FALSE)
  expect_true(file.exists(paste0(dest, ".gz")))
  expect_false(file.exists(dest))

  # Decompress
  result2 <- compress_polecat(tmp, action = "decompress", remove = TRUE, verbose = FALSE)
  expect_true(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".gz")))

  # Verify content roundtrips
  original <- readLines(src)
  roundtripped <- readLines(dest)
  expect_equal(original, roundtripped)
})

test_that("compress_polecat returns empty for no matching files", {
  tmp <- withr::local_tempdir()
  writeLines("test", file.path(tmp, "not_a_polecat_file.txt"))

  result <- compress_polecat(tmp, action = "compress", verbose = FALSE)
  expect_equal(length(result), 0)
})

test_that("compress_polecat validates action parameter", {
  tmp <- withr::local_tempdir()
  expect_error(compress_polecat(tmp, action = "invalid"), "'action' must be")
})
