#
#   Diagnostic summary
#

#' Diagnose rpolecat setup
#'
#' Print a summary of the current rpolecat configuration: whether options are
#' set, how many local data files exist, the date range they cover, database
#' status, and API token status.
#'
#' @param data_dir Optional path to the local data directory. If \code{NULL},
#'   uses the directory set via \code{\link{setup_polecat}} (if any).
#'
#' @returns Invisibly returns a list with diagnostic information.
#'
#' @examples
#' dr_polecat()
#'
#' @export
#' @md
dr_polecat <- function(data_dir = NULL) {
  info <- list()
  cli::cli_h2("rpolecat diagnostic summary")

  # Package version
  ver <- tryCatch(
    as.character(utils::packageVersion("rpolecat")),
    error = function(e) "unknown"
  )
  cli::cli_text("Package version: {.strong {ver}}")

  # Options
  opts <- get_polecat_opts()
  cli::cli_h3("Options")
  cli::cli_ul(c(
    "data_dir: {if (is.null(opts$data_dir)) '<not set>' else opts$data_dir}",
    "use_db:   {opts$use_db}",
    "db_path:  {if (is.null(opts$db_path)) '<not set>' else opts$db_path}"
  ))
  info$opts <- opts

  # API token
  has_token <- check_api_token()
  cli::cli_h3("API token")
  if (has_token) {
    cli::cli_alert_success("Dataverse API token is set")
  } else {
    cli::cli_alert_warning("Dataverse API token is NOT SET")
  }
  info$has_token <- has_token

  # Local files
  dir_to_check <- data_dir
  if (is.null(dir_to_check)) dir_to_check <- opts$data_dir

  cli::cli_h3("Local data")
  if (!is.null(dir_to_check) && dir.exists(dir_to_check)) {
    local <- get_local_state(dir_to_check)
    cli::cli_text("Directory: {.path {dir_to_check}}")
    cli::cli_text("Files:     {nrow(local)}")

    if (nrow(local) > 0) {
      cli::cli_text("Years:     {paste(range(local$data_year), collapse = ' - ')}")
      total_size <- sum(local$file_size)
      size_label <- if (total_size > 1e9) {
        sprintf("%.1f GB", total_size / 1e9)
      } else {
        sprintf("%.1f MB", total_size / 1e6)
      }
      cli::cli_text("Total size: {size_label}")
      cli::cli_text("Compressed: {sum(local$compressed)} of {nrow(local)} files")
    }
    info$local <- local
  } else {
    cli::cli_text("No data directory configured or found.")
  }

  # Database
  if (opts$use_db && !is.null(opts$db_path)) {
    cli::cli_h3("Database")
    if (file.exists(opts$db_path)) {
      db_size <- file.info(opts$db_path)$size
      cli::cli_text("Path: {.path {opts$db_path}}")
      cli::cli_text("Size: {sprintf('%.1f MB', db_size / 1e6)}")
    } else {
      cli::cli_text("Path: {.path {opts$db_path}} (file does not exist)")
    }
  }

  invisible(info)
}
