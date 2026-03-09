test_that("classify_quad adds quad_category column", {
  dat <- make_test_data()
  result <- classify_quad(dat)
  expect_true("quad_category" %in% names(result))
  expect_equal(nrow(result), nrow(dat))
})

test_that("classify_quad errors without event_type column", {
  dat <- make_test_data()
  dat$event_type <- NULL
  expect_error(classify_quad(dat), "event_type")
})

test_that("classify_quad errors on non-data-frame input", {
  expect_error(classify_quad("not a data frame"))
})

test_that("get_quad returns correct categories for known event types", {
  expect_equal(unname(get_quad("agree")), "Verbal Cooperation")
  expect_equal(unname(get_quad("consult")), "Verbal Cooperation")
  expect_equal(unname(get_quad("support")), "Verbal Cooperation")
  expect_equal(unname(get_quad("concede")), "Verbal Cooperation")

  expect_equal(unname(get_quad("cooperate")), "Material Cooperation")
  expect_equal(unname(get_quad("aid")), "Material Cooperation")
  expect_equal(unname(get_quad("retreat")), "Material Cooperation")

  expect_equal(unname(get_quad("request")), "Verbal Conflict")
  expect_equal(unname(get_quad("accuse")), "Verbal Conflict")
  expect_equal(unname(get_quad("reject")), "Verbal Conflict")
  expect_equal(unname(get_quad("threaten")), "Verbal Conflict")

  expect_equal(unname(get_quad("protest")), "Material Conflict")
  expect_equal(unname(get_quad("sanction")), "Material Conflict")
  expect_equal(unname(get_quad("mobilize")), "Material Conflict")
  expect_equal(unname(get_quad("coerce")), "Material Conflict")
  expect_equal(unname(get_quad("assault")), "Material Conflict")
})

test_that("get_quad returns NA for unknown types", {
  result <- get_quad("unknown_event")
  expect_true(is.na(result))
})

test_that("get_quad is case-insensitive", {
  expect_equal(unname(get_quad("AGREE")), "Verbal Cooperation")
  expect_equal(unname(get_quad("Protest")), "Material Conflict")
})

test_that("all 16 event types map to one of 4 quad categories", {
  all_types <- c("agree", "consult", "support", "concede",
                 "cooperate", "aid", "retreat",
                 "request", "accuse", "reject", "threaten",
                 "protest", "sanction", "mobilize", "coerce", "assault")
  result <- unname(get_quad(all_types))
  expect_equal(length(result), 16)
  expect_true(all(!is.na(result)))
  expected_cats <- c("Verbal Cooperation", "Material Cooperation",
                     "Verbal Conflict", "Material Conflict")
  expect_true(all(result %in% expected_cats))
  expect_equal(length(unique(result)), 4)
})

test_that("get_quad works with vector input", {
  result <- get_quad(c("agree", "assault", "consult"))
  expect_length(result, 3)
  expect_equal(unname(result),
               c("Verbal Cooperation", "Material Conflict", "Verbal Cooperation"))
})
