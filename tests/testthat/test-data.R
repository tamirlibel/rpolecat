test_that("polecat_sample is a data frame with 520 rows and 17 columns", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("polecat_sample", package = "rpolecat", envir = environment())
    polecat_sample
  }, error = function(e) NULL)
  skip_if(is.null(dat), "polecat_sample not available")

  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 520)
  expect_equal(ncol(dat), 17)
})

test_that("polecat_sample has expected column names", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("polecat_sample", package = "rpolecat", envir = environment())
    polecat_sample
  }, error = function(e) NULL)
  skip_if(is.null(dat), "polecat_sample not available")

  expected_cols <- c("event_id", "story_date", "source", "source_actor",
                     "source_agent", "source_country", "target", "target_actor",
                     "target_agent", "target_country", "event_type", "mode",
                     "context", "latitude", "longitude", "goldstein", "url")
  expect_equal(names(dat), expected_cols)
})

test_that("types exists and is a character vector", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("types", package = "rpolecat", envir = environment())
    types
  }, error = function(e) NULL)
  skip_if(is.null(dat), "types not available")

  expect_type(dat, "character")
  expect_true(length(dat) > 0)
})

test_that("modes exists and is a character vector", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("modes", package = "rpolecat", envir = environment())
    modes
  }, error = function(e) NULL)
  skip_if(is.null(dat), "modes not available")

  expect_type(dat, "character")
  expect_true(length(dat) > 0)
})

test_that("contexts exists and is a character vector", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("contexts", package = "rpolecat", envir = environment())
    contexts
  }, error = function(e) NULL)
  skip_if(is.null(dat), "contexts not available")

  expect_type(dat, "character")
  expect_true(length(dat) > 0)
})

test_that("quad_categories is a data frame with expected columns", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("quad_categories", package = "rpolecat", envir = environment())
    quad_categories
  }, error = function(e) NULL)
  skip_if(is.null(dat), "quad_categories not available")

  expect_s3_class(dat, "data.frame")
  expect_true("event_type" %in% names(dat))
  expect_true("quad_category" %in% names(dat))
  expect_true("goldstein" %in% names(dat))
  expect_equal(nrow(dat), 16)
})
