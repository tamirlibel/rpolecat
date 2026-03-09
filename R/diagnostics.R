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
  cat("rpolecat diagnostic summary\n")
  cat(paste0(rep("-", 40), collapse = ""), "\n\n")

  # Package version
  ver <- tryCatch(
    as.character(utils::packageVersion("rpolecat")),
    error = function(e) "unknown"
  )
  cat("Package version:", ver, "\n\n")

  # Options
  opts <- get_polecat_opts()
  cat("Options:\n")
  cat("  data_dir:", if (is.null(opts$data_dir)) "<not set>" else opts$data_dir, "\n")
  cat("  use_db:  ", opts$use_db, "\n")
  cat("  db_path: ", if (is.null(opts$db_path)) "<not set>" else opts$db_path, "\n")
  info$opts <- opts
  cat("\n")

  # API token
  has_token <- check_api_token()
  cat("Dataverse API token:", if (has_token) "set" else "NOT SET", "\n\n")
  info$has_token <- has_token

  # Local files
  dir_to_check <- data_dir
  if (is.null(dir_to_check)) dir_to_check <- opts$data_dir

  if (!is.null(dir_to_check) && dir.exists(dir_to_check)) {
    local <- get_local_state(dir_to_check)
    cat("Local data files:\n")
    cat("  Directory: ", dir_to_check, "\n")
    cat("  Files:     ", nrow(local), "\n")

    if (nrow(local) > 0) {
      cat("  Years:     ", paste(range(local$data_year), collapse = " - "), "\n")
      total_size <- sum(local$file_size)
      size_label <- if (total_size > 1e9) {
        sprintf("%.1f GB", total_size / 1e9)
      } else {
        sprintf("%.1f MB", total_size / 1e6)
      }
      cat("  Total size:", size_label, "\n")
      cat("  Compressed:", sum(local$compressed), "of", nrow(local), "files\n")
    }
    info$local <- local
  } else {
    cat("Local data files:\n")
    cat("  No data directory configured or found.\n")
  }
  cat("\n")

  # Database
  if (opts$use_db && !is.null(opts$db_path)) {
    cat("Database:\n")
    if (file.exists(opts$db_path)) {
      db_size <- file.info(opts$db_path)$size
      cat("  Path:", opts$db_path, "\n")
      cat("  Size:", sprintf("%.1f MB", db_size / 1e6), "\n")
    } else {
      cat("  Path:", opts$db_path, "(file does not exist)\n")
    }
    cat("\n")
  }

  invisible(info)
}
