#
#   Update local data from Dataverse
#

#' Update local POLECAT data
#'
#' Compare local files with what is available on Dataverse and download any
#' new or updated files. By default this is a dry run that only prints a plan
#' of what would be done.
#'
#' @param data_dir Path to the local data directory. If \code{NULL}, uses the
#'   directory set via \code{\link{setup_polecat}}.
#' @param years Optional numeric vector to restrict to specific years.
#' @param delete_obsolete Logical; delete local files that no longer exist on
#'   Dataverse? Defaults to \code{FALSE}.
#' @param dryrun Logical; if \code{TRUE} (the default), only print what would
#'   happen without downloading or deleting anything.
#' @param verbose Print progress messages?
#'
#' @details This function:
#' \enumerate{
#'   \item Queries Dataverse for the current file listing
#'   \item Compares it against local files
#'   \item Downloads new files
#'   \item Optionally removes local files that are no longer on Dataverse
#' }
#'
#' Set \code{dryrun = FALSE} to actually execute the plan.
#'
#' @returns Invisibly returns a list with elements \code{to_download} and
#'   \code{to_delete} (data frames of files).
#'
#' @examples
#' \dontrun{
#'   # See what would happen
#'   update_polecat("~/data/polecat")
#'
#'   # Actually do it
#'   update_polecat("~/data/polecat", dryrun = FALSE)
#' }
#' @export
#' @md
update_polecat <- function(data_dir = NULL, years = NULL,
                           delete_obsolete = FALSE, dryrun = TRUE,
                           verbose = TRUE) {
  data_dir <- resolve_data_dir(data_dir)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    if (verbose) message("Created directory: ", data_dir)
  }

  # Get remote state
  if (verbose) cat("Querying Dataverse for current file listing...\n")
  remote <- get_dataverse_file_list()

  if (!is.null(years)) {
    years <- as.integer(years)
    remote <- remote[remote$data_year %in% years, ]
  }

  # Get local state
  local_files <- dir(data_dir)
  # Strip .zip/.gz extensions for comparison

  local_base <- gsub("\\.zip$|\\.gz$", "", local_files)

  # Determine what to download (remote files not present locally)
  already_present <- remote$label %in% local_base | remote$label %in% local_files
  to_download <- remote[!already_present, ]

  # Determine what to delete (local polecat files not on remote)
  polecat_local <- local_files[grepl("ngecEvents", local_files)]
  polecat_local_base <- gsub("\\.zip$|\\.gz$", "", polecat_local)
  obsolete <- polecat_local[!polecat_local_base %in% remote$label & !polecat_local %in% remote$label]

  # Report
  cat(sprintf("\nUpdate plan for '%s':\n", data_dir))
  cat(sprintf("  Remote files:    %d\n", nrow(remote)))
  cat(sprintf("  Local files:     %d\n", length(polecat_local)))
  cat(sprintf("  To download:     %d\n", nrow(to_download)))
  cat(sprintf("  Obsolete:        %d\n", length(obsolete)))

  if (nrow(to_download) > 0) {
    cat("\nFiles to download:\n")
    cat(paste0("  ", to_download$label, collapse = "\n"), "\n")
  }

  if (length(obsolete) > 0 && delete_obsolete) {
    cat("\nFiles to delete:\n")
    cat(paste0("  ", obsolete, collapse = "\n"), "\n")
  } else if (length(obsolete) > 0) {
    cat("\nObsolete files (set delete_obsolete = TRUE to remove):\n")
    cat(paste0("  ", obsolete, collapse = "\n"), "\n")
  }

  if (dryrun) {
    cat("\nDry run -- no changes made. Set dryrun = FALSE to execute.\n")
    return(invisible(list(
      to_download = to_download,
      to_delete   = if (delete_obsolete) obsolete else character(0)
    )))
  }

  # Execute downloads
  if (nrow(to_download) > 0) {
    cat("\nDownloading...\n")
    for (i in seq_len(nrow(to_download))) {
      file_name <- to_download$label[[i]]
      file_path <- file.path(data_dir, file_name)
      if (verbose) cat(sprintf("  Downloading '%s'\n", file_name))
      f <- dataverse::get_file(to_download$id[[i]],
                               dataset = get_polecat_doi(),
                               format = NULL)
      writeBin(f, file_path)
    }
  }

  # Execute deletions
  if (length(obsolete) > 0 && delete_obsolete) {
    cat("\nDeleting obsolete files...\n")
    for (fname in obsolete) {
      fpath <- file.path(data_dir, fname)
      if (verbose) cat(sprintf("  Deleting '%s'\n", fname))
      file.remove(fpath)
    }
  }

  cat("\nUpdate complete.\n")
  invisible(list(
    to_download = to_download,
    to_delete   = if (delete_obsolete) obsolete else character(0)
  ))
}
