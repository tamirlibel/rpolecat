test_that("get_polecat_opts returns a list with expected elements", {
  result <- get_polecat_opts()
  expect_type(result, "list")
  expect_true("data_dir" %in% names(result))
  expect_true("use_db" %in% names(result))
  expect_true("db_path" %in% names(result))
})

test_that("set_polecat_opts sets options correctly", {
  old <- options(
    rpolecat.data_dir = getOption("rpolecat.data_dir"),
    rpolecat.use_db = getOption("rpolecat.use_db"),
    rpolecat.db_path = getOption("rpolecat.db_path")
  )
  on.exit(do.call(options, old), add = TRUE)

  set_polecat_opts(data_dir = "/tmp/test_polecat_data")
  opts <- get_polecat_opts()
  expect_equal(opts$data_dir, normalizePath("/tmp/test_polecat_data", mustWork = FALSE))

  set_polecat_opts(use_db = TRUE)
  opts <- get_polecat_opts()
  expect_true(opts$use_db)

  set_polecat_opts(db_path = "/tmp/test.duckdb")
  opts <- get_polecat_opts()
  expect_equal(opts$db_path, normalizePath("/tmp/test.duckdb", mustWork = FALSE))
})

test_that("unset_polecat_opts clears options", {
  old <- options(
    rpolecat.data_dir = getOption("rpolecat.data_dir"),
    rpolecat.use_db = getOption("rpolecat.use_db"),
    rpolecat.db_path = getOption("rpolecat.db_path")
  )
  on.exit(do.call(options, old), add = TRUE)

  set_polecat_opts(data_dir = "/tmp/test_polecat_data", use_db = TRUE)
  unset_polecat_opts()

  opts <- get_polecat_opts()
  expect_null(opts$data_dir)
  expect_false(opts$use_db)
  expect_null(opts$db_path)
})

test_that("get_polecat_opts use_db defaults to FALSE", {
  old <- options(rpolecat.use_db = getOption("rpolecat.use_db"))
  on.exit(do.call(options, old), add = TRUE)

  options(rpolecat.use_db = NULL)
  opts <- get_polecat_opts()
  expect_false(opts$use_db)
})
