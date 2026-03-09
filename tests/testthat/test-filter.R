test_that("filter_polecat with countries parameter filters correctly", {
  dat <- make_test_data()
  result <- filter_polecat(dat, countries = "USA")
  expect_s3_class(result, "data.frame")
  # Every row should have USA as source or target

  has_usa <- result$source_country == "USA" | result$target_country == "USA"
  expect_true(all(has_usa))
})

test_that("filter_polecat with years parameter works", {
  dat <- make_test_data()
  result <- filter_polecat(dat, years = 2023)
  expect_s3_class(result, "data.frame")
  # All rows should be from 2023
  event_years <- as.integer(substr(result$story_date, 1, 4))
  expect_true(all(event_years == 2023))
})

test_that("filter_polecat with event_types parameter works", {
  dat <- make_test_data()
  result <- filter_polecat(dat, event_types = c("protest", "assault"))
  expect_s3_class(result, "data.frame")
  expect_true(all(tolower(result$event_type) %in% c("protest", "assault")))
})

test_that("filter_polecat with geolocated=TRUE keeps only rows with coordinates", {
  dat <- make_test_data()
  result <- filter_polecat(dat, geolocated = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true(all(!is.na(result$latitude)))
  expect_true(all(!is.na(result$longitude)))
})

test_that("filter_polecat with geolocated=FALSE keeps only rows without coordinates", {
  dat <- make_test_data()
  result <- filter_polecat(dat, geolocated = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(is.na(result$latitude)))
  expect_true(all(is.na(result$longitude)))
})

test_that("filter_dyadic keeps only rows where both source and target actors are present", {
  dat <- make_test_data()
  # Our test data has rows 1:2 with empty target_actor
  result <- filter_dyadic(dat)
  expect_s3_class(result, "data.frame")
  expect_true(all(result$source_actor != "" & !is.na(result$source_actor)))
  expect_true(all(result$target_actor != "" & !is.na(result$target_actor)))
  expect_true(nrow(result) < nrow(dat))
})

test_that("filter_intrastate keeps only rows where source_country == target_country", {
  dat <- make_test_data()
  result <- filter_intrastate(dat)
  expect_s3_class(result, "data.frame")
  expect_true(all(result$source_country == result$target_country))
})

test_that("filter_interstate keeps only rows where source_country != target_country", {
  dat <- make_test_data()
  result <- filter_interstate(dat)
  expect_s3_class(result, "data.frame")
  expect_true(all(result$source_country != result$target_country))
})

test_that("all filters return data frames", {
  dat <- make_test_data()
  expect_s3_class(filter_polecat(dat), "data.frame")
  expect_s3_class(filter_dyadic(dat), "data.frame")
  expect_s3_class(filter_intrastate(dat), "data.frame")
  expect_s3_class(filter_interstate(dat), "data.frame")
})

test_that("filtering with no matching criteria returns 0-row data frame", {
  dat <- make_test_data()
  result <- filter_polecat(dat, countries = "ZZZ")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), ncol(dat))
})

test_that("filter_polecat with year range works", {
  dat <- make_test_data()
  result <- filter_polecat(dat, years = c(2022, 2024))
  expect_s3_class(result, "data.frame")
  # All test data is 2023, which falls within 2022-2024
  expect_equal(nrow(result), nrow(dat))
})

test_that("filter_polecat with no arguments returns all rows", {
  dat <- make_test_data()
  result <- filter_polecat(dat)
  expect_equal(nrow(result), nrow(dat))
})

test_that("filter_polecat errors on non-data-frame input", {
  expect_error(filter_polecat("not a data frame"))
})
