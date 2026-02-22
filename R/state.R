#
#   Local and remote state tracking
#

#' Get local POLECAT file state
#'
#' Return a data frame describing the POLECAT data files present in the
#' local data directory.
#'
#' @param data_dir Path to the local data directory. If \code{NULL}, uses
#'   the directory set via \code{\link{setup_polecat}}.
#'
#' @returns A data frame with columns: \code{file_name}, \code{file_size}
#'   (in bytes), \code{modified}, \code{data_year}, and \code{compressed}
#'   (logical).
#'
#' @examples
#' \dontrun{
#'   get_local_state("~/data/polecat")
#' }
#' @export
#' @md
get_local_state <- function(data_dir = NULL) {
  data_dir <- resolve_data_dir(data_dir)

  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir, call. = FALSE)
  }

  all_files <- dir(data_dir, full.names = TRUE)
  polecat_files <- all_files[grepl("ngecEvents", basename(all_files))]

  if (length(polecat_files) == 0) {
    message("No POLECAT files found in '", data_dir, "'")
    return(data.frame(
      file_name  = character(0),
      file_size  = numeric(0),
      modified   = as.POSIXct(character(0)),
      data_year  = integer(0),
      compressed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(polecat_files)
  fnames <- basename(polecat_files)
  years <- as.integer(regmatches(fnames, regexpr("[0-9]{4}", fnames)))
  compressed <- grepl("\\.zip$|\\.gz$", fnames)

  data.frame(
    file_name  = fnames,
    file_size  = info$size,
    modified   = info$mtime,
    data_year  = years,
    compressed = compressed,
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
}


#' Get Dataverse POLECAT file state
#'
#' Query the Harvard Dataverse API and return a data frame describing the
#' POLECAT data files currently available for download.
#'
#' @returns A data frame with columns: \code{label}, \code{id},
#'   \code{version}, \code{creationDate}, and \code{data_year}.
#'
#' @examples
#' \dontrun{
#'   get_dataverse_state()
#' }
#' @export
#' @md
get_dataverse_state <- function() {
  get_dataverse_file_list()
}
