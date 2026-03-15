test_that("check_api_token returns FALSE when env vars are not set", {
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")
  expect_false(check_api_token())
})

test_that("check_api_token returns TRUE when both env vars are set", {
  withr::local_envvar(DATAVERSE_KEY = "test-key", DATAVERSE_SERVER = "test.server.edu")
  expect_true(check_api_token())
})

test_that("polecat_request errors without DATAVERSE_SERVER", {
  withr::local_envvar(DATAVERSE_KEY = "test-key", DATAVERSE_SERVER = "")
  expect_error(polecat_request("/api/test"), "DATAVERSE_SERVER")
})

test_that("polecat_request errors without DATAVERSE_KEY", {
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "dataverse.harvard.edu")
  expect_error(polecat_request("/api/test"), "DATAVERSE_KEY")
})

test_that("polecat_request builds correct URL", {
  withr::local_envvar(DATAVERSE_KEY = "test-key", DATAVERSE_SERVER = "dataverse.harvard.edu")
  req <- polecat_request("/api/datasets/export")
  expect_true(grepl("https://dataverse.harvard.edu/api/datasets/export", req$url))
})

test_that("polecat_request strips protocol from server", {
  withr::local_envvar(DATAVERSE_KEY = "test-key", DATAVERSE_SERVER = "https://dataverse.harvard.edu")
  req <- polecat_request("/api/test")
  expect_true(grepl("^https://dataverse\\.harvard\\.edu/api/test", req$url))
  # Should not have double https://
  expect_false(grepl("https://https://", req$url))
})

test_that("get_polecat_doi returns expected DOI", {
  expect_equal(get_polecat_doi(), "doi:10.7910/DVN/AJGVIT")
})

test_that("dataverse_api_token errors when not configured", {
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")
  expect_error(dataverse_api_token(), "not set")
})

test_that("validate_api_connection returns FALSE when not configured", {
  withr::local_envvar(DATAVERSE_KEY = "", DATAVERSE_SERVER = "")
  expect_false(validate_api_connection())
})

test_that("null coalescing operator works", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
})
