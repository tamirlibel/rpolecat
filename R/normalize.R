#
#   Data normalization and cleaning
#

#' Normalize POLECAT data
#'
#' Apply standard cleaning and normalization to a POLECAT data frame:
#' parse dates, ensure consistent casing, trim whitespace, and add
#' convenience columns.
#'
#' @param x A data frame of POLECAT events.
#' @param parse_dates Logical; convert \code{story_date} to \code{Date} class?
#'   Defaults to \code{TRUE}.
#' @param add_year Logical; add a \code{year} column? Defaults to \code{TRUE}.
#' @param add_yearmonth Logical; add a \code{year_month} column
#'   (format \code{"YYYY-MM"})? Defaults to \code{TRUE}.
#' @param lowercase_types Logical; lowercase \code{event_type}, \code{mode},
#'   and \code{context}? Defaults to \code{TRUE}.
#'
#' @returns The input data frame with normalization applied.
#'
#' @examples
#' data(polecat_sample)
#' dat <- normalize_polecat(polecat_sample)
#' class(dat$story_date)
#' head(dat$year)
#' head(dat$year_month)
#'
#' @export
#' @md
normalize_polecat <- function(x, parse_dates = TRUE, add_year = TRUE,
                              add_yearmonth = TRUE,
                              lowercase_types = TRUE) {
  stopifnot(is.data.frame(x))

  # Trim whitespace on character columns
  char_cols <- vapply(x, is.character, logical(1))
  x[char_cols] <- lapply(x[char_cols], trimws)

  # Parse dates
  if (parse_dates && "story_date" %in% names(x)) {
    if (!inherits(x$story_date, "Date")) {
      x$story_date <- as.Date(x$story_date)
    }
  }

  # Add year column
  if (add_year && "story_date" %in% names(x)) {
    if (inherits(x$story_date, "Date")) {
      x$year <- as.integer(format(x$story_date, "%Y"))
    } else {
      x$year <- as.integer(substr(as.character(x$story_date), 1, 4))
    }
  }

  # Add year_month column
  if (add_yearmonth && "story_date" %in% names(x)) {
    if (inherits(x$story_date, "Date")) {
      x$year_month <- format(x$story_date, "%Y-%m")
    } else {
      x$year_month <- substr(as.character(x$story_date), 1, 7)
    }
  }

  # Lowercase event type fields
  if (lowercase_types) {
    for (col in c("event_type", "mode", "context")) {
      if (col %in% names(x)) {
        x[[col]] <- tolower(x[[col]])
      }
    }
  }

  x
}
