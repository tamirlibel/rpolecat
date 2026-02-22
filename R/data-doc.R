#
#   Package data documentation
#

#' Event types
#'
#' @docType data
"types"

#' Event type modes
#'
#' @docType data
"modes"


#' Contexts
#'
#' @docType data
"contexts"


#' Quad categories
#'
#' A data frame mapping PLOVER event types to quad categories (Verbal
#' Cooperation, Material Cooperation, Verbal Conflict, Material Conflict)
#' with associated Goldstein scale scores.
#'
#' @format A data frame with 16 rows and 3 columns:
#' \describe{
#'   \item{event_type}{PLOVER event type}
#'   \item{quad_category}{One of: Verbal Cooperation, Material Cooperation,
#'     Verbal Conflict, Material Conflict}
#'   \item{goldstein}{Approximate Goldstein conflict-cooperation score}
#' }
#' @docType data
"quad_categories"


#' Sample POLECAT data
#'
#' A small synthetic dataset of 520 events (including 20 duplicates) that
#' mirrors the structure of real POLECAT data. Intended for use in examples,
#' tests, and documentation.
#'
#' @format A data frame with 520 rows and 17 columns:
#' \describe{
#'   \item{event_id}{Unique event identifier}
#'   \item{story_date}{Date of the event (YYYY-MM-DD)}
#'   \item{source}{Source actor full identifier}
#'   \item{source_actor}{Source actor type}
#'   \item{source_agent}{Source agent}
#'   \item{source_country}{Source country (ISO3)}
#'   \item{target}{Target actor full identifier}
#'   \item{target_actor}{Target actor type}
#'   \item{target_agent}{Target agent}
#'   \item{target_country}{Target country (ISO3)}
#'   \item{event_type}{PLOVER event type}
#'   \item{mode}{Event mode}
#'   \item{context}{Event context(s), semicolon-separated}
#'   \item{latitude}{Latitude (NA for ~20\% of events)}
#'   \item{longitude}{Longitude (NA for ~20\% of events)}
#'   \item{goldstein}{Goldstein score}
#'   \item{url}{Source URL}
#' }
#' @docType data
"polecat_sample"

