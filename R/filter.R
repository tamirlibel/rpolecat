#
#   Filtering and subsetting functions
#

#' Filter POLECAT events
#'
#' Convenience function for common filtering operations on a POLECAT data frame.
#'
#' @param x A data frame of POLECAT events.
#' @param countries Character vector of ISO3 country codes to keep. Matches
#'   against both \code{source_country} and \code{target_country}; an event is
#'   kept if either the source or target matches.
#' @param src_countries Character vector of ISO3 country codes to match on
#'   \code{source_country} only.
#' @param tgt_countries Character vector of ISO3 country codes to match on
#'   \code{target_country} only.
#' @param years Numeric vector of years or a length-2 vector interpreted as a
#'   range. Filters on \code{story_date}.
#' @param event_types Character vector of PLOVER event types to keep
#'   (e.g. \code{"protest"}, \code{"assault"}).
#' @param contexts Character vector of context values to keep. Matches events
#'   where the \code{context} column contains any of the specified values
#'   (handles semicolon-separated multi-value cells).
#' @param modes Character vector of event modes to keep.
#' @param geolocated Logical; if \code{TRUE}, keep only events with non-missing
#'   latitude/longitude. If \code{FALSE}, keep only events without coordinates.
#'   If \code{NULL} (default), no filtering on geolocation.
#'
#' @returns A filtered data frame.
#'
#' @examples
#' data(polecat_sample)
#' # Events involving the USA
#' usa <- filter_polecat(polecat_sample, countries = "USA")
#'
#' # Protests and assaults in 2023
#' violent <- filter_polecat(polecat_sample,
#'                           event_types = c("protest", "assault"),
#'                           years = 2023)
#'
#' # Only geolocated events
#' geo <- filter_polecat(polecat_sample, geolocated = TRUE)
#'
#' @export
#' @md
filter_polecat <- function(x, countries = NULL, src_countries = NULL,
                           tgt_countries = NULL, years = NULL,
                           event_types = NULL, contexts = NULL,
                           modes = NULL, geolocated = NULL) {
  stopifnot(is.data.frame(x))

  keep <- rep(TRUE, nrow(x))

  # Country filter (either source or target)
  if (!is.null(countries)) {
    countries <- toupper(countries)
    src_match <- if ("source_country" %in% names(x)) x$source_country %in% countries else FALSE
    tgt_match <- if ("target_country" %in% names(x)) x$target_country %in% countries else FALSE
    keep <- keep & (src_match | tgt_match)
  }

  # Source country filter
  if (!is.null(src_countries) && "source_country" %in% names(x)) {
    keep <- keep & (x$source_country %in% toupper(src_countries))
  }

  # Target country filter
  if (!is.null(tgt_countries) && "target_country" %in% names(x)) {
    keep <- keep & (x$target_country %in% toupper(tgt_countries))
  }

  # Year filter
  if (!is.null(years) && "story_date" %in% names(x)) {
    event_year <- as.integer(substr(as.character(x$story_date), 1, 4))
    if (length(years) == 2 && diff(years) > 1) {
      keep <- keep & (event_year >= years[1] & event_year <= years[2])
    } else {
      keep <- keep & (event_year %in% as.integer(years))
    }
  }

  # Event type filter
  if (!is.null(event_types) && "event_type" %in% names(x)) {
    keep <- keep & (tolower(x$event_type) %in% tolower(event_types))
  }

  # Context filter (handles semicolon-separated values)
  if (!is.null(contexts) && "context" %in% names(x)) {
    ctx_pattern <- paste0("\\b(", paste(contexts, collapse = "|"), ")\\b")
    keep <- keep & grepl(ctx_pattern, x$context, ignore.case = TRUE)
  }

  # Mode filter
  if (!is.null(modes) && "mode" %in% names(x)) {
    keep <- keep & (tolower(x$mode) %in% tolower(modes))
  }

  # Geolocation filter
  if (!is.null(geolocated)) {
    if ("latitude" %in% names(x) && "longitude" %in% names(x)) {
      has_geo <- !is.na(x$latitude) & !is.na(x$longitude)
      if (geolocated) {
        keep <- keep & has_geo
      } else {
        keep <- keep & !has_geo
      }
    }
  }

  x[keep, , drop = FALSE]
}


#' Filter to dyadic events
#'
#' Keep only events where both source and target actors are identified.
#'
#' @param x A data frame of POLECAT events.
#'
#' @returns A filtered data frame.
#'
#' @examples
#' data(polecat_sample)
#' dyadic <- filter_dyadic(polecat_sample)
#'
#' @export
#' @md
filter_dyadic <- function(x) {
  stopifnot(is.data.frame(x))
  has_source <- !is.na(x$source_actor) & x$source_actor != ""
  has_target <- !is.na(x$target_actor) & x$target_actor != ""
  x[has_source & has_target, , drop = FALSE]
}


#' Filter to intrastate events
#'
#' Keep only events where the source and target are in the same country.
#'
#' @param x A data frame of POLECAT events.
#'
#' @returns A filtered data frame.
#'
#' @examples
#' data(polecat_sample)
#' domestic <- filter_intrastate(polecat_sample)
#'
#' @export
#' @md
filter_intrastate <- function(x) {
  stopifnot(is.data.frame(x))
  if (!all(c("source_country", "target_country") %in% names(x))) {
    stop("Data must contain 'source_country' and 'target_country' columns",
         call. = FALSE)
  }
  both_present <- !is.na(x$source_country) & x$source_country != "" &
    !is.na(x$target_country) & x$target_country != ""
  same_country <- both_present & (x$source_country == x$target_country)
  x[same_country, , drop = FALSE]
}


#' Filter to interstate events
#'
#' Keep only events where the source and target are in different countries.
#'
#' @param x A data frame of POLECAT events.
#'
#' @returns A filtered data frame.
#'
#' @examples
#' data(polecat_sample)
#' foreign <- filter_interstate(polecat_sample)
#'
#' @export
#' @md
filter_interstate <- function(x) {
  stopifnot(is.data.frame(x))
  if (!all(c("source_country", "target_country") %in% names(x))) {
    stop("Data must contain 'source_country' and 'target_country' columns",
         call. = FALSE)
  }
  both_present <- !is.na(x$source_country) & x$source_country != "" &
    !is.na(x$target_country) & x$target_country != ""
  diff_country <- both_present & (x$source_country != x$target_country)
  x[diff_country, , drop = FALSE]
}
