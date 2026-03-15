#
#   Data validation
#

#' Validate a POLECAT data frame
#'
#' Check a POLECAT data frame for common issues: missing expected columns,
#' invalid date formats, unrecognized event types, and provide a summary of
#' data quality.
#'
#' @param x A data frame of POLECAT events.
#' @param verbose Print the validation report? Defaults to \code{TRUE}.
#'
#' @returns Invisibly returns a list with elements:
#'   \item{valid}{Logical; is the data frame valid?}
#'   \item{n_rows}{Number of rows}
#'   \item{n_cols}{Number of columns}
#'   \item{missing_cols}{Character vector of expected but missing columns}
#'   \item{extra_cols}{Character vector of unexpected columns}
#'   \item{date_issues}{Number of rows with unparseable dates}
#'   \item{unknown_event_types}{Character vector of event types not in the PLOVER ontology}
#'   \item{country_format_issues}{Number of country codes that are not 3-letter ISO}
#'   \item{duplicate_rows}{Number of duplicate rows (by key fields)}
#'
#' @examples
#' data(polecat_sample)
#' validate_polecat(polecat_sample)
#'
#' @export
#' @md
validate_polecat <- function(x, verbose = TRUE) {
  stopifnot(is.data.frame(x))

  expected_cols <- c("event_id", "story_date", "source", "source_actor",
                     "source_agent", "source_country", "target", "target_actor",
                     "target_agent", "target_country", "event_type", "mode",
                     "context", "latitude", "longitude", "goldstein", "url")

  known_event_types <- names(QUAD_MAP)

  result <- list(
    valid                 = TRUE,
    n_rows                = nrow(x),
    n_cols                = ncol(x),
    missing_cols          = character(0),
    extra_cols            = character(0),
    date_issues           = 0L,
    unknown_event_types   = character(0),
    country_format_issues = 0L,
    duplicate_rows        = 0L
  )

  # Check columns
  result$missing_cols <- setdiff(expected_cols, names(x))
  result$extra_cols <- setdiff(names(x), expected_cols)

  if (length(result$missing_cols) > 0) result$valid <- FALSE

  # Check dates
  if ("story_date" %in% names(x)) {
    dates <- tryCatch(as.Date(x$story_date), error = function(e) rep(NA, nrow(x)))
    result$date_issues <- sum(is.na(dates) & !is.na(x$story_date))
    if (result$date_issues > 0) result$valid <- FALSE
  }

  # Check event types
  if ("event_type" %in% names(x)) {
    types_in_data <- unique(tolower(x$event_type))
    types_in_data <- types_in_data[!is.na(types_in_data)]
    result$unknown_event_types <- setdiff(types_in_data, known_event_types)
    if (length(result$unknown_event_types) > 0) result$valid <- FALSE
  }

  # Check country codes (should be 3-letter ISO)
  for (col in c("source_country", "target_country")) {
    if (col %in% names(x)) {
      vals <- x[[col]][!is.na(x[[col]]) & x[[col]] != ""]
      bad <- vals[!grepl("^[A-Z]{3}$", vals)]
      result$country_format_issues <- result$country_format_issues + length(bad)
    }
  }
  if (result$country_format_issues > 0) result$valid <- FALSE

  # Check duplicates
  key_cols <- intersect(c("story_date", "source_actor", "target_actor",
                          "event_type", "mode"), names(x))
  if (length(key_cols) > 0) {
    result$duplicate_rows <- sum(duplicated(x[, key_cols, drop = FALSE]))
  }

  # Report
  if (verbose) {
    if (result$valid) {
      cli::cli_h2("POLECAT data validation: PASS")
    } else {
      cli::cli_h2("POLECAT data validation: ISSUES FOUND")
    }

    cli::cli_text("Rows: {result$n_rows}, Columns: {result$n_cols}")

    if (length(result$missing_cols) > 0) {
      cli::cli_alert_warning("Missing expected columns: {paste(result$missing_cols, collapse = ', ')}")
    }
    if (length(result$extra_cols) > 0) {
      cli::cli_alert_info("Extra columns: {paste(result$extra_cols, collapse = ', ')}")
    }
    if (result$date_issues > 0) {
      cli::cli_alert_warning("{result$date_issues} row(s) with unparseable dates")
    }
    if (length(result$unknown_event_types) > 0) {
      cli::cli_alert_warning("Unknown event types: {paste(result$unknown_event_types, collapse = ', ')}")
    }
    if (result$country_format_issues > 0) {
      cli::cli_alert_warning("{result$country_format_issues} country code(s) not in ISO3 format")
    }
    if (result$duplicate_rows > 0) {
      cli::cli_alert_info("{result$duplicate_rows} duplicate row(s) detected")
    }
    if (result$valid && result$duplicate_rows == 0) {
      cli::cli_alert_success("No issues found")
    }
  }

  invisible(result)
}
