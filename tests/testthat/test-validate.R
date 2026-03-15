test_that("validate_polecat passes on valid data", {
  dat <- make_test_data(10)
  result <- validate_polecat(dat, verbose = FALSE)
  expect_true(result$valid)
  expect_equal(result$n_rows, 10)
  expect_equal(length(result$missing_cols), 0)
})

test_that("validate_polecat detects missing columns", {
  dat <- make_test_data(5)
  dat$event_type <- NULL
  dat$story_date <- NULL

  result <- validate_polecat(dat, verbose = FALSE)
  expect_false(result$valid)
  expect_true("event_type" %in% result$missing_cols)
  expect_true("story_date" %in% result$missing_cols)
})

test_that("validate_polecat detects unknown event types", {
  dat <- make_test_data(5)
  dat$event_type[1] <- "INVALID_TYPE"

  result <- validate_polecat(dat, verbose = FALSE)
  expect_false(result$valid)
  expect_true("invalid_type" %in% result$unknown_event_types)
})

test_that("validate_polecat detects bad country codes", {
  dat <- make_test_data(5)
  dat$source_country[1] <- "XX"  # Too short, not ISO3

  result <- validate_polecat(dat, verbose = FALSE)
  expect_false(result$valid)
  expect_gt(result$country_format_issues, 0)
})

test_that("validate_polecat detects duplicates", {
  dat <- make_test_data_with_dupes()
  result <- validate_polecat(dat, verbose = FALSE)
  expect_gt(result$duplicate_rows, 0)
})

test_that("validate_polecat detects date parse issues", {
  dat <- make_test_data(5)
  dat$story_date[1] <- "not-a-date"

  result <- validate_polecat(dat, verbose = FALSE)
  expect_gt(result$date_issues, 0)
})
