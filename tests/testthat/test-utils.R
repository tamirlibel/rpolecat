test_that("deduplicate_polecat removes duplicate rows", {
  dat <- make_test_data_with_dupes()
  result <- deduplicate_polecat(dat)
  expect_true(nrow(result) < nrow(dat))
  expect_true(nrow(result) <= 30)
})

test_that("deduplicate_polecat with polecat_sample reduces row count", {
  skip_if_not_installed("rpolecat")
  dat <- tryCatch({
    data("polecat_sample", package = "rpolecat", envir = environment())
    polecat_sample
  }, error = function(e) NULL)
  skip_if(is.null(dat), "polecat_sample not available")

  result <- deduplicate_polecat(dat)
  expect_equal(nrow(dat), 520)
  expect_equal(nrow(dat) - nrow(result), 20)
  expect_equal(nrow(result), 500)
})

test_that("deduplicate_polecat with no duplicates returns same data", {
  dat <- make_test_data()
  result <- deduplicate_polecat(dat)
  expect_equal(nrow(result), nrow(dat))
})

test_that("deduplicate_polecat warns when no 'by' columns are found", {
  dat <- data.frame(a = 1:5, b = letters[1:5])
  expect_warning(deduplicate_polecat(dat), "None of the 'by' columns")
})

test_that("expand_polecat_col expands semicolon-separated values", {
  dat <- data.frame(
    id = 1:3,
    context = c("military", "military;economic", "political;economic;military"),
    stringsAsFactors = FALSE
  )
  result <- expand_polecat_col(dat, "context")
  expect_equal(nrow(result), 6)  # 1 + 2 + 3
  expect_true(all(result$context %in% c("military", "economic", "political")))
})

test_that("expand_polecat_col errors with missing column", {
  dat <- make_test_data()
  expect_error(expand_polecat_col(dat, "nonexistent_column"), "not found")
})

test_that("expand_polecat_col preserves other columns", {
  dat <- data.frame(
    id = 1:2,
    context = c("a;b", "c"),
    value = c(10, 20),
    stringsAsFactors = FALSE
  )
  result <- expand_polecat_col(dat, "context")
  expect_equal(nrow(result), 3)
  expect_equal(result$value, c(10, 10, 20))
  expect_equal(result$id, c(1, 1, 2))
})

test_that("expand_polecat_col trims whitespace from expanded values", {
  dat <- data.frame(
    id = 1,
    tags = "foo ; bar ; baz",
    stringsAsFactors = FALSE
  )
  result <- expand_polecat_col(dat, "tags")
  expect_equal(result$tags, c("foo", "bar", "baz"))
})

test_that("expand_polecat_col with custom separator", {
  dat <- data.frame(
    id = 1:2,
    vals = c("a|b", "c|d|e"),
    stringsAsFactors = FALSE
  )
  result <- expand_polecat_col(dat, "vals", sep = "|")
  expect_equal(nrow(result), 5)
})
