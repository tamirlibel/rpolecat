#
#   Aggregate events to common units of analysis
#

#' Aggregate POLECAT events
#'
#' Aggregate a POLECAT data frame to common units of analysis used in
#' quantitative political science research.
#'
#' @param x A data frame of POLECAT events.
#' @param unit Character string specifying the unit of analysis. One of:
#'   \code{"country-day"}, \code{"country-month"}, \code{"country-year"},
#'   \code{"dyad-day"}, \code{"dyad-month"}, \code{"dyad-year"},
#'   \code{"day"}, \code{"month"}, or \code{"year"}.
#' @param measure Character string specifying the summary measure. One of
#'   \code{"count"} (default), \code{"goldstein"} (mean Goldstein score).
#' @param by_quad Logical; if \code{TRUE}, counts are broken out by quad
#'   category (Verbal Cooperation, Material Cooperation, Verbal Conflict,
#'   Material Conflict). Defaults to \code{FALSE}.
#' @param by_type Logical; if \code{TRUE}, counts are broken out by event type.
#'   Defaults to \code{FALSE}.
#'
#' @returns A data frame aggregated to the specified unit with columns for the
#'   grouping variables and the summary measure.
#'
#' @examples
#' data(polecat_sample)
#'
#' # Monthly event counts per country
#' monthly <- aggregate_polecat(polecat_sample, unit = "country-month")
#' head(monthly)
#'
#' # Daily counts with quad categories
#' daily_quad <- aggregate_polecat(polecat_sample, unit = "day", by_quad = TRUE)
#' head(daily_quad)
#'
#' @export
#' @md
aggregate_polecat <- function(x, unit = "country-month",
                              measure = "count",
                              by_quad = FALSE, by_type = FALSE) {
  stopifnot(is.data.frame(x))

  valid_units <- c("country-day", "country-month", "country-year",
                   "dyad-day", "dyad-month", "dyad-year",
                   "day", "month", "year")
  if (!unit %in% valid_units) {
    stop("'unit' must be one of: ", paste(valid_units, collapse = ", "),
         call. = FALSE)
  }
  if (!measure %in% c("count", "goldstein")) {
    stop("'measure' must be 'count' or 'goldstein'", call. = FALSE)
  }

  # Parse dates
  dates <- as.Date(x$story_date)
  x$.year  <- as.integer(format(dates, "%Y"))
  x$.month <- as.integer(format(dates, "%m"))
  x$.day   <- as.character(dates)
  x$.ym    <- format(dates, "%Y-%m")

  # Build grouping columns based on unit
  group_cols <- switch(unit,
    "country-day"   = c("source_country", ".day"),
    "country-month" = c("source_country", ".ym"),
    "country-year"  = c("source_country", ".year"),
    "dyad-day"      = c("source_country", "target_country", ".day"),
    "dyad-month"    = c("source_country", "target_country", ".ym"),
    "dyad-year"     = c("source_country", "target_country", ".year"),
    "day"           = ".day",
    "month"         = ".ym",
    "year"          = ".year"
  )

  # Add quad or type breakdown
  if (by_quad) {
    x <- classify_quad(x)
    group_cols <- c(group_cols, "quad_category")
  }
  if (by_type) {
    group_cols <- c(group_cols, "event_type")
  }

  # Aggregate
  if (measure == "count") {
    agg <- stats::aggregate(
      x[, 1, drop = FALSE],
      by = x[, group_cols, drop = FALSE],
      FUN = length
    )
    names(agg)[ncol(agg)] <- "n"
  } else {
    if (!"goldstein" %in% names(x)) {
      stop("Data must contain a 'goldstein' column for goldstein measure",
           call. = FALSE)
    }
    agg <- stats::aggregate(
      x[, "goldstein", drop = FALSE],
      by = x[, group_cols, drop = FALSE],
      FUN = mean, na.rm = TRUE
    )
    names(agg)[ncol(agg)] <- "mean_goldstein"
  }

  # Clean up internal column names
  nms <- names(agg)
  nms[nms == ".day"]  <- "date"
  nms[nms == ".ym"]   <- "year_month"
  nms[nms == ".year"] <- "year"
  names(agg) <- nms

  agg <- agg[do.call(order, agg[, seq_along(group_cols), drop = FALSE]), ]
  rownames(agg) <- NULL
  agg
}
