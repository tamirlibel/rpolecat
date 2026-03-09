test_that("aggregate_polecat returns a data frame", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "day")
  expect_s3_class(result, "data.frame")
})

test_that("aggregate_polecat works with different units", {
  dat <- make_test_data()

  units <- c("country-day", "country-month", "country-year",
             "dyad-day", "dyad-month", "dyad-year",
             "day", "month", "year")

  for (u in units) {
    result <- aggregate_polecat(dat, unit = u)
    expect_s3_class(result, "data.frame", info = paste("unit:", u))
    expect_true(nrow(result) > 0, info = paste("unit:", u))
  }
})

test_that("count measure produces 'n' column", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "day", measure = "count")
  expect_true("n" %in% names(result))
  expect_true(is.numeric(result$n))
  expect_equal(sum(result$n), nrow(dat))
})

test_that("goldstein measure produces 'mean_goldstein' column", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "day", measure = "goldstein")
  expect_true("mean_goldstein" %in% names(result))
  expect_true(is.numeric(result$mean_goldstein))
})

test_that("by_quad produces quad_category column", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "day", by_quad = TRUE)
  expect_true("quad_category" %in% names(result))
})

test_that("by_type produces event_type column", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "day", by_type = TRUE)
  expect_true("event_type" %in% names(result))
})

test_that("invalid unit produces error", {
  dat <- make_test_data()
  expect_error(aggregate_polecat(dat, unit = "invalid_unit"), "unit")
})

test_that("invalid measure produces error", {
  dat <- make_test_data()
  expect_error(aggregate_polecat(dat, unit = "day", measure = "invalid"), "measure")
})

test_that("country-month unit has source_country and year_month columns", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "country-month")
  expect_true("source_country" %in% names(result))
  expect_true("year_month" %in% names(result))
})

test_that("year unit produces year column", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "year")
  expect_true("year" %in% names(result))
})

test_that("dyad units include both source and target country", {
  dat <- make_test_data()
  result <- aggregate_polecat(dat, unit = "dyad-month")
  expect_true("source_country" %in% names(result))
  expect_true("target_country" %in% names(result))
})
