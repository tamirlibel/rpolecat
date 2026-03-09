
#' Deduplicate POLECAT events
#'
#' Remove duplicate events from a POLECAT data frame. The same real-world event
#' may appear in multiple news articles and therefore have multiple rows in the
#' data. This function keeps only one row per unique combination of the key
#' event-identifying fields.
#'
#' @param x A data frame of POLECAT events, as returned by \code{read_polecat()}.
#' @param by Character vector of column names to use for identifying duplicates.
#'   Defaults to the standard POLECAT key fields: \code{"story_date"},
#'   \code{"source_actor"}, \code{"target_actor"}, \code{"event_type"}, and
#'   \code{"mode"}. Adjust this if your data uses different column names.
#'
#' @returns A data frame with duplicate rows removed.
#'
#' @examples
#' \dontrun{
#'   dat <- read_polecat("my/data/dir")
#'   dat_deduped <- deduplicate_polecat(dat)
#'   cat(nrow(dat) - nrow(dat_deduped), "duplicate rows removed\n")
#' }
#' @export
#' @md
deduplicate_polecat <- function(x,
                                by = c("story_date", "source_actor",
                                       "target_actor", "event_type", "mode")) {
  stopifnot(is.data.frame(x))

  # Only deduplicate on columns that actually exist in the data
  by <- intersect(by, names(x))
  if (length(by) == 0) {
    warning("None of the 'by' columns were found in the data; ",
            "returning data unchanged")
    return(x)
  }

  duplicated_rows <- duplicated(x[, by, drop = FALSE])
  x[!duplicated_rows, , drop = FALSE]
}


#' Expand a multi-value column into long format
#'
#' POLECAT data sometimes stores multiple values in a single cell, separated
#' by a delimiter (commonly a semicolon). This function splits those cells and
#' returns a data frame in long format, with one row per value.
#'
#' @param x A data frame of POLECAT events.
#' @param col The name of the column to expand (as a string).
#' @param sep The delimiter used to separate values. Defaults to \code{";"}.
#'
#' @returns A data frame in long format with one row per value in \code{col}.
#'   All other columns are repeated for each value.
#'
#' @examples
#' \dontrun{
#'   dat <- read_polecat("my/data/dir")
#'
#'   # Expand the "context" column (may contain e.g. "military;economic")
#'   dat_long <- expand_polecat_col(dat, "context")
#' }
#' @export
#' @md
expand_polecat_col <- function(x, col, sep = ";") {
  stopifnot(is.data.frame(x))
  if (!col %in% names(x)) {
    stop("Column '", col, "' not found in data frame")
  }

  # Split the target column and replicate other columns accordingly
  values <- strsplit(as.character(x[[col]]), sep, fixed = TRUE)
  rep_idx <- rep(seq_len(nrow(x)), lengths(values))

  out <- x[rep_idx, , drop = FALSE]
  out[[col]] <- unlist(values)
  out[[col]] <- trimws(out[[col]])
  rownames(out) <- NULL
  out
}
