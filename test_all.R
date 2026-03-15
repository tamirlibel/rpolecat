#!/usr/bin/env Rscript
# Run this script from the rpolecat repo root:
#   Rscript test_all.R
#
# Prerequisites:
#   install.packages(c("httr2", "cli", "testthat", "withr"))

cat("=== rpolecat test runner ===\n\n")

# 1. Install package dependencies
cat("1. Checking dependencies...\n")
required <- c("httr2", "cli", "testthat", "withr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("   Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
}
cat("   All dependencies OK.\n\n")

# 2. Load the package from source
cat("2. Loading rpolecat from source...\n")
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  cat("   Neither devtools nor pkgload found. Installing pkgload...\n")
  install.packages("pkgload", repos = "https://cloud.r-project.org", quiet = TRUE)
  pkgload::load_all(".", quiet = TRUE)
}
cat("   Package loaded.\n\n")

# 3. Run all tests
cat("3. Running tests...\n\n")
results <- testthat::test_dir("tests/testthat", reporter = "summary")
cat("\n")

# 4. Quick smoke tests (no API needed)
cat("4. Smoke tests (no API)...\n")

# Test constants
stopifnot(length(QUAD_MAP) == 16)
cat("   QUAD_MAP: OK\n")

# Test classify
dat <- data.frame(event_type = c("protest", "agree", "assault"),
                  stringsAsFactors = FALSE)
result <- classify_quad(dat)
stopifnot("quad_category" %in% names(result))
stopifnot(result$quad_category[1] == "Material Conflict")
cat("   classify_quad: OK\n")

# Test get_quad
stopifnot(get_quad("protest") == "Material Conflict")
cat("   get_quad: OK\n")

# Test extract_data_year
stopifnot(extract_data_year("ngecEvents.20230103.txt") == 2023L)
cat("   extract_data_year: OK\n")

# Test validate
sample_dat <- make_test_data(10)
v <- validate_polecat(sample_dat, verbose = FALSE)
stopifnot(v$valid)
cat("   validate_polecat: OK\n")

cat("\n   All smoke tests passed.\n\n")

# 5. Optional: API smoke test
cat("5. API connection test...\n")
if (Sys.getenv("DATAVERSE_KEY") != "" && Sys.getenv("DATAVERSE_SERVER") != "") {
  cat("   Credentials found. Testing API...\n")
  tryCatch({
    ok <- validate_api_connection()
    if (ok) {
      cat("   API connection: OK\n")
      files <- get_dataverse_state()
      cat(sprintf("   Found %d files on Dataverse\n", nrow(files)))
      cat("   Sample files:\n")
      print(head(files[, c("label", "data_year")], 5))
    }
  }, error = function(e) {
    cat("   API test failed:", conditionMessage(e), "\n")
  })
} else {
  cat("   Skipped (DATAVERSE_KEY / DATAVERSE_SERVER not set).\n")
  cat("   To test API, run:\n")
  cat('     Sys.setenv(DATAVERSE_KEY = "your-token")\n')
  cat('     Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")\n')
}

cat("\n=== Done ===\n")
