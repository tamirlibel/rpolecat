#
#   Quad category classification
#

#' Classify events into quad categories
#'
#' Add a \code{quad_category} column to a POLECAT data frame, mapping each
#' event type to one of the four standard categories: Verbal Cooperation,
#' Material Cooperation, Verbal Conflict, or Material Conflict.
#'
#' @param x A data frame of POLECAT events with an \code{event_type} column.
#'
#' @details The mapping follows the PLOVER ontology:
#' \itemize{
#'   \item \strong{Verbal Cooperation}: agree, consult, support, concede
#'   \item \strong{Material Cooperation}: cooperate, aid, retreat
#'   \item \strong{Verbal Conflict}: request, accuse, reject, threaten
#'   \item \strong{Material Conflict}: protest, sanction, mobilize, coerce,
#'     assault
#' }
#'
#' @returns The input data frame with an added \code{quad_category} column.
#'
#' @examples
#' data(polecat_sample)
#' dat <- classify_quad(polecat_sample)
#' table(dat$quad_category)
#'
#' @export
#' @md
classify_quad <- function(x) {
  stopifnot(is.data.frame(x))
  if (!"event_type" %in% names(x)) {
    stop("Data must contain an 'event_type' column", call. = FALSE)
  }

  x$quad_category <- QUAD_MAP[tolower(x$event_type)]
  x
}


#' Get the quad category for event types
#'
#' Returns the quad category label for one or more PLOVER event type strings.
#' This is the vectorized lookup version of \code{\link{classify_quad}}.
#'
#' @param event_type Character vector of PLOVER event types.
#'
#' @returns A character vector of quad category labels.
#'
#' @examples
#' get_quad("protest")
#' get_quad(c("agree", "assault", "consult"))
#'
#' @export
#' @md
get_quad <- function(event_type) {
  QUAD_MAP[tolower(event_type)]
}
