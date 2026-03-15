test_that("POLECAT_FILE_PATTERN matches expected files", {
  expect_true(grepl(POLECAT_FILE_PATTERN, "ngecEvents.20230103.txt"))
  expect_true(grepl(POLECAT_FILE_PATTERN, "ngecEventsDV-2022.txt.zip"))
  expect_false(grepl(POLECAT_FILE_PATTERN, "other_file.txt"))
})

test_that("QUAD_MAP contains all 16 event types", {
  expect_equal(length(QUAD_MAP), 16)
  expect_true(all(c("agree", "assault", "protest") %in% names(QUAD_MAP)))
})

test_that("QUAD_MAP has exactly 4 categories", {
  expect_equal(length(unique(QUAD_MAP)), 4)
  expect_true(all(c("Verbal Cooperation", "Material Cooperation",
                     "Verbal Conflict", "Material Conflict") %in% QUAD_MAP))
})

test_that("extract_data_year works on both filename formats", {
  expect_equal(extract_data_year("ngecEvents.20230103111842.Release603.DV.txt"), 2023L)
  expect_equal(extract_data_year("ngecEventsDV-2022.txt.zip"), 2022L)
})

test_that("compare_file_lists handles empty inputs", {
  remote <- data.frame(label = character(0), id = integer(0),
                       stringsAsFactors = FALSE)
  result <- compare_file_lists(remote, character(0))
  expect_equal(nrow(result$to_download), 0)
  expect_equal(nrow(result$already_present), 0)
  expect_equal(length(result$obsolete), 0)
})
