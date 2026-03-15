test_that("check_db_packages errors for invalid backend", {
  expect_error(check_db_packages("postgres"), "'backend' must be")
})

test_that("create_polecat_db errors on existing db without overwrite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  tmp <- withr::local_tempdir()
  db_path <- file.path(tmp, "test.duckdb")
  file.create(db_path)

  withr::local_options(rpolecat.data_dir = tmp)
  expect_error(create_polecat_db(db_path, tmp), "already exists")
})

test_that("connect_polecat errors without db_path", {
  withr::local_options(rpolecat.db_path = NULL)
  expect_error(connect_polecat(), "No database path")
})

test_that("query_polecat errors without db_path", {
  withr::local_options(rpolecat.db_path = NULL)
  expect_error(query_polecat("SELECT 1"), "No database path")
})

test_that("connect_polecat infers backend from extension", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  tmp <- withr::local_tempdir()
  db_path <- file.path(tmp, "test.duckdb")

  # Create a minimal database
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbWriteTable(con, "test", data.frame(x = 1))
  DBI::dbDisconnect(con, shutdown = TRUE)

  con2 <- connect_polecat(db_path)
  expect_true(DBI::dbIsValid(con2))
  DBI::dbDisconnect(con2, shutdown = TRUE)
})
