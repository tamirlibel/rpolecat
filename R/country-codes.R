#
#   Country code translation
#

#' Convert country codes to Gleditsch-Ward codes
#'
#' Translate POLECAT country identifiers to the Gleditsch & Ward numeric state
#' codes commonly used in political science datasets. Uses the \pkg{countrycode}
#' package.
#'
#' @param x A data frame of POLECAT events, or a character vector of
#'   country codes/names.
#' @param from The coding scheme of the input. Defaults to \code{"iso3c"}.
#'   See \code{\link[countrycode]{countrycode}} for all options.
#' @param col When \code{x} is a data frame, which column(s) to translate.
#'   Defaults to \code{c("source_country", "target_country")}.
#'
#' @returns If \code{x} is a data frame, returns it with added
#'   \code{source_gwcode} and/or \code{target_gwcode} columns. If \code{x} is
#'   a character vector, returns a numeric vector of Gleditsch-Ward codes.
#'
#' @examples
#' data(polecat_sample)
#' dat <- polecat_to_gwcode(polecat_sample)
#' head(dat[, c("source_country", "source_gwcode",
#'              "target_country", "target_gwcode")])
#'
#' # Translate a vector
#' polecat_to_gwcode(c("USA", "GBR", "FRA"))
#'
#' @export
#' @md
polecat_to_gwcode <- function(x, from = "iso3c",
                              col = c("source_country", "target_country")) {
  check_countrycode()

  if (is.character(x)) {
    return(countrycode::countrycode(x, origin = from, destination = "gwn",
                                    warn = FALSE))
  }

  stopifnot(is.data.frame(x))

  for (cc in col) {
    if (cc %in% names(x)) {
      new_col <- sub("_country$", "_gwcode", cc)
      x[[new_col]] <- countrycode::countrycode(
        x[[cc]], origin = from, destination = "gwn", warn = FALSE
      )
    }
  }
  x
}


#' Convert country codes to COW codes
#'
#' Translate POLECAT country identifiers to the Correlates of War numeric state
#' codes commonly used in political science datasets.
#'
#' @param x A data frame of POLECAT events, or a character vector of
#'   country codes/names.
#' @param from The coding scheme of the input. Defaults to \code{"iso3c"}.
#' @param col When \code{x} is a data frame, which column(s) to translate.
#'   Defaults to \code{c("source_country", "target_country")}.
#'
#' @returns If \code{x} is a data frame, returns it with added
#'   \code{source_cowcode} and/or \code{target_cowcode} columns. If \code{x} is
#'   a character vector, returns a numeric vector of COW codes.
#'
#' @examples
#' data(polecat_sample)
#' dat <- polecat_to_cowcode(polecat_sample)
#' head(dat[, c("source_country", "source_cowcode",
#'              "target_country", "target_cowcode")])
#'
#' @export
#' @md
polecat_to_cowcode <- function(x, from = "iso3c",
                               col = c("source_country", "target_country")) {
  check_countrycode()

  if (is.character(x)) {
    return(countrycode::countrycode(x, origin = from, destination = "cown",
                                    warn = FALSE))
  }

  stopifnot(is.data.frame(x))

  for (cc in col) {
    if (cc %in% names(x)) {
      new_col <- sub("_country$", "_cowcode", cc)
      x[[new_col]] <- countrycode::countrycode(
        x[[cc]], origin = from, destination = "cown", warn = FALSE
      )
    }
  }
  x
}


# Internal: check that countrycode package is available
check_countrycode <- function() {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required for country code conversion. ",
         "Install it with: install.packages('countrycode')", call. = FALSE)
  }
}
