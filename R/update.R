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
    if (verbose) cli::cli_alert_info("Created directory: {.path {data_dir}}")
  }

  # Get remote state
  if (verbose) cli::cli_alert_info("Querying Dataverse for current file listing...")
  remote <- tryCatch(
    get_dataverse_file_list(),
    error = function(e) {
      cli::cli_abort("Failed to query Dataverse: {conditionMessage(e)}")
    }
  )

  if (!is.null(years)) {
    years <- as.integer(years)
    remote <- remote[remote$data_year %in% years, ]
  }

  # Compare remote vs local
  local_files <- dir(data_dir)
  comparison <- compare_file_lists(remote, local_files)
  to_download <- comparison$to_download
  obsolete <- comparison$obsolete

  # Get local polecat file count
  polecat_local <- local_files[grepl(POLECAT_FILE_PATTERN, local_files)]

  # Report
  cli::cli_h2("Update plan for {.path {data_dir}}")
  cli::cli_ul(c(
    "Remote files:    {nrow(remote)}",
    "Local files:     {length(polecat_local)}",
    "To download:     {nrow(to_download)}",
    "Obsolete:        {length(obsolete)}"
  ))

  if (nrow(to_download) > 0) {
    cli::cli_h3("Files to download")
    cli::cli_ul(to_download$label)
  }

  if (length(obsolete) > 0 && delete_obsolete) {
    cli::cli_h3("Files to delete")
    cli::cli_ul(obsolete)
  } else if (length(obsolete) > 0) {
    cli::cli_h3("Obsolete files (set delete_obsolete = TRUE to remove)")
    cli::cli_ul(obsolete)
  }

  if (dryrun) {
    cli::cli_alert_warning("Dry run -- no changes made. Set dryrun = FALSE to execute.")
    return(invisible(list(
      to_download = to_download,
      to_delete   = if (delete_obsolete) obsolete else character(0)
    )))
  }

  # Execute downloads
  if (nrow(to_download) > 0) {
    n_files <- nrow(to_download)
    if (verbose) cli::cli_alert_info("Downloading {n_files} file{?s}...")
    for (i in seq_len(n_files)) {
      file_name <- to_download$label[[i]]
      file_path <- file.path(data_dir, file_name)
      if (verbose) {
        cli::cli_progress_step("Downloading {.file {file_name}} ({i}/{n_files})")
      }
      success <- download_dataverse_file(
        file_id   = to_download$id[[i]],
        dest_path = file_path,
        progress  = verbose
      )
      if (!success) {
        cli::cli_warn("Failed to download {.file {file_name}}")
      }
    }
  }

  # Execute deletions
  if (length(obsolete) > 0 && delete_obsolete) {
    if (verbose) cli::cli_alert_info("Deleting obsolete files...")
    for (fname in obsolete) {
      fpath <- file.path(data_dir, fname)
      if (verbose) cli::cli_progress_step("Deleting {.file {fname}}")
      file.remove(fpath)
    }
  }

  if (verbose) cli::cli_alert_success("Update complete.")
  invisible(list(
    to_download = to_download,
    to_delete   = if (delete_obsolete) obsolete else character(0)
  ))
}
