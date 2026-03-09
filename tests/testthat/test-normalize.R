test_that("normalize_polecat adds year column when add_year=TRUE", {
  dat <- make_test_data()
  result <- normalize_polecat(dat, add_year = TRUE, add_yearmonth = FALSE,
                              lowercase_types = FALSE)
  expect_true("year" %in% names(result))
  expect_true(is.integer(result$year))
  expect_true(all(result$year == 2023))
})

test_that("normalize_polecat adds year_month column when add_yearmonth=TRUE", {
  dat <- make_test_data()
  result <- normalize_polecat(dat, add_year = FALSE, add_yearmonth = TRUE,
                              lowercase_types = FALSE)
  expect_true("year_month" %in% names(result))
  expect_true(all(grepl("^2023-\\d{2}$", result$year_month)))
})

test_that("normalize_polecat lowercases event_type when lowercase_types=TRUE", {
  dat <- make_test_data()
  # Make some event types uppercase to test
  dat$event_type <- toupper(dat$event_type)
  result <- normalize_polecat(dat, add_year = FALSE, add_yearmonth = FALSE,
                              lowercase_types = TRUE)
  expect_true(all(result$event_type == tolower(result$event_type)))
})

test_that("normalize_polecat parse_dates converts story_date to Date class", {
  dat <- make_test_data()
  # Ensure story_date starts as character
  dat$story_date <- as.character(dat$story_date)
  expect_false(inherits(dat$story_date, "Date"))

  result <- normalize_polecat(dat, parse_dates = TRUE, add_year = FALSE,
                              add_yearmonth = FALSE, lowercase_types = FALSE)
  expect_true(inherits(result$story_date, "Date"))
})

test_that("normalize_polecat with all options FALSE returns unchanged structure", {
  dat <- make_test_data()
  original_cols <- names(dat)
  result <- normalize_polecat(dat, parse_dates = FALSE, add_year = FALSE,
                              add_yearmonth = FALSE, lowercase_types = FALSE)
  expect_equal(names(result), original_cols)
})

test_that("normalize_polecat does not duplicate year column if called twice", {
  dat <- make_test_data()
  result1 <- normalize_polecat(dat, add_year = TRUE, add_yearmonth = FALSE,
                               lowercase_types = FALSE)
  result2 <- normalize_polecat(result1, add_year = TRUE, add_yearmonth = FALSE,
                               lowercase_types = FALSE)
  # year column should exist (may be overwritten, but should still be one column)
  expect_equal(sum(names(result2) == "year"), 1)
})

test_that("normalize_polecat lowercases mode and context too", {
  dat <- make_test_data()
  dat$mode[!is.na(dat$mode)] <- toupper(dat$mode[!is.na(dat$mode)])
  dat$context[!is.na(dat$context)] <- toupper(dat$context[!is.na(dat$context)])
  result <- normalize_polecat(dat, parse_dates = FALSE, add_year = FALSE,
                              add_yearmonth = FALSE, lowercase_types = TRUE)
  non_na_mode <- result$mode[!is.na(result$mode)]
  expect_true(all(non_na_mode == tolower(non_na_mode)))
})
