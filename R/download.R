
#' Download POLECAT
#'
#' Download the POLECAT data from dataverse.
#'
#' @param local_dir Which local directory to download the data to?
#' @param years Do you want to download data for only specific years? The
#'   default (NULL) will download all available data. Otherwise, specify a
#'   numeric vector, e.g. \code{2023}, \code{2022:2023}, or \code{c(2021, 2023)}.
#' @param skip_existing Skip files that are already in "local_dir"?
#' @param verbose Print progress messages as files are being downloaded?
#' @param dryrun Don't actually download the files, just print information on
#'   what files would be downloaded.
#'
#' @details This function will download files from dataverse as they are, which
#'   namely means that the zipped files will remain zipped. Two potential issues:
#'
#'   - You zipped the "ReleaseXXX.DV.txt" files to save space: "skip_existing"
#'     should be able to handle that. It will remove ".zip" or ".gz" from the
#'     filenames before comparing.
#'   - You unzipped the yearly data files, which actually each contain 12
#'     monthly data files. To avoid re-downloading those, use "skip_existing"
#'     and set the "years" argument to only the current year. See the examples.
#'
#' @returns Data will be downloaded to \code{local_dir}.
#'
#' @examples
#' \dontrun{
#'   # Basic, fresh download
#'   download_polecat("my/data/dir")
#'
#'   # Next time, to avoid re-downloading files you already have:
#'   download_polecat("my/data/dir", skip_existing = TRUE)
#'
#'   # To download only fresh data for the current year, or avoid re-downloading
#'   # the yearly historical data files if you unzipped them:
#'   download_polecat("my/data/dir", years = 2023, skip_existing = TRUE)
#' }
#' @export
#' @md
download_polecat <- function(local_dir, years = NULL, skip_existing = TRUE,
                             verbose = TRUE, dryrun = FALSE) {
  # Check arguments
  stopifnot(
    "'skip_existing' must be logical" = is.logical(skip_existing),
    "'verbose' must be logical" = is.logical(verbose),
    "'dryrun' must be logical" = is.logical(dryrun)
  )

  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
    if (verbose) cli::cli_alert_info("Created directory: {.path {local_dir}}")
  }

  if (verbose) cli::cli_alert_info("Querying Dataverse for file listing...")
  dataverse_files <- tryCatch(
    get_dataverse_file_list(),
    error = function(e) {
      cli::cli_abort("Failed to query Dataverse: {conditionMessage(e)}")
    }
  )

  if (!is.null(years)) {
    years <- as.integer(years)
    dataverse_files <- dataverse_files[dataverse_files$data_year %in% years, ]
  }

  # Check for existing local files
  skip_n <- NA_integer_
  if (skip_existing) {
    local_files <- dir(local_dir)
    comparison <- compare_file_lists(dataverse_files, local_files)
    skip_n <- nrow(comparison$already_present)
    dataverse_files <- comparison$to_download

    if (skip_n > 0 && verbose && !dryrun) {
      cli::cli_alert_info("Skipping {skip_n} file{?s} already present locally")
    }
  }

  # Early exit if this is a dryrun
  if (dryrun) {
    cli::cli_h2("Dry run")
    cli::cli_alert_info("Local dir: {.path {local_dir}}")
    if (!is.na(skip_n) && skip_n > 0) {
      cli::cli_alert_info("Skipping {skip_n} file{?s} already present")
    }
    cli::cli_alert_info("Would download {nrow(dataverse_files)} file{?s}:")
    if (nrow(dataverse_files) > 0) {
      cli::cli_ul(dataverse_files$label)
    }
    return(invisible(TRUE))
  }

  if (nrow(dataverse_files) == 0) {
    if (verbose) cli::cli_alert_success("All files are up to date.")
    return(invisible(TRUE))
  }

  n_files <- nrow(dataverse_files)
  if (verbose) cli::cli_alert_info("Downloading {n_files} file{?s}...")

  for (i in seq_len(n_files)) {
    file_name <- dataverse_files$label[[i]]
    file_path <- file.path(local_dir, file_name)
    if (verbose) {
      cli::cli_progress_step("Downloading {.file {file_name}} ({i}/{n_files})")
    }

    success <- download_dataverse_file(
      file_id   = dataverse_files$id[[i]],
      dest_path = file_path,
      progress  = verbose
    )

    if (!success) {
      cli::cli_warn("Failed to download {.file {file_name}}")
    }
  }

  if (verbose) cli::cli_alert_success("Download complete.")
  invisible(TRUE)
}
