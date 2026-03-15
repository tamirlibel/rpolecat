test_that("polecat_to_gwcode translates a vector", {
  skip_if_not_installed("countrycode")
  result <- polecat_to_gwcode(c("USA", "GBR", "FRA"))
  expect_type(result, "double")
  expect_equal(length(result), 3)
  # USA should be 2 in GW system
  expect_equal(result[1], 2)
})

test_that("polecat_to_gwcode adds columns to data frame", {
  skip_if_not_installed("countrycode")
  dat <- make_test_data(5)
  result <- polecat_to_gwcode(dat)
  expect_true("source_gwcode" %in% names(result))
  expect_true("target_gwcode" %in% names(result))
})

test_that("polecat_to_cowcode translates a vector", {
  skip_if_not_installed("countrycode")
  result <- polecat_to_cowcode(c("USA", "GBR", "FRA"))
  expect_type(result, "double")
  expect_equal(length(result), 3)
})

test_that("polecat_to_cowcode adds columns to data frame", {
  skip_if_not_installed("countrycode")
  dat <- make_test_data(5)
  result <- polecat_to_cowcode(dat)
  expect_true("source_cowcode" %in% names(result))
  expect_true("target_cowcode" %in% names(result))
})

test_that("check_countrycode errors if package is missing", {
  # This test would need to mock requireNamespace; skip if countrycode is installed
  skip_if(requireNamespace("countrycode", quietly = TRUE),
          "countrycode is installed, cannot test missing package error")
  expect_error(check_countrycode(), "countrycode")
})
