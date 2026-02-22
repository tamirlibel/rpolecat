#
#   File listing functions
#

#' List local POLECAT files
#'
#' Print a formatted summary of the POLECAT data files in the local data
#' directory.
#'
#' @param data_dir Path to the local data directory. If \code{NULL}, uses the
#'   directory set via \code{\link{setup_polecat}}.
#'
#' @returns Invisibly returns a data frame of file information (same as
#'   \code{\link{get_local_state}}).
#'
#' @examples
#' \dontrun{
#'   list_polecat_files("~/data/polecat")
#' }
#' @export
#' @md
list_polecat_files <- function(data_dir = NULL) {
  local <- get_local_state(data_dir)

  if (nrow(local) == 0) {
    cat("No POLECAT files found.\n")
    return(invisible(local))
  }

  cat(sprintf("POLECAT files in '%s':\n\n", resolve_data_dir(data_dir)))

  # Format sizes
  local$size_label <- ifelse(
    local$file_size > 1e6,
    sprintf("%.1f MB", local$file_size / 1e6),
    sprintf("%.0f KB", local$file_size / 1e3)
  )

  for (i in seq_len(nrow(local))) {
    compressed_flag <- if (local$compressed[i]) " [compressed]" else ""
    cat(sprintf("  %-60s %8s  %d%s\n",
                local$file_name[i],
                local$size_label[i],
                local$data_year[i],
                compressed_flag))
  }

  total_size <- sum(local$file_size)
  size_label <- if (total_size > 1e9) {
    sprintf("%.1f GB", total_size / 1e9)
  } else {
    sprintf("%.1f MB", total_size / 1e6)
  }
  cat(sprintf("\n  %d files, %s total\n", nrow(local), size_label))

  invisible(local)
}


#' List Dataverse POLECAT files
#'
#' Query the Harvard Dataverse and print a formatted summary of the POLECAT
#' files available for download.
#'
#' @returns Invisibly returns a data frame of file information (same as
#'   \code{\link{get_dataverse_state}}).
#'
#' @examples
#' \dontrun{
#'   list_dataverse_files()
#' }
#' @export
#' @md
list_dataverse_files <- function() {
  remote <- get_dataverse_file_list()

  cat(sprintf("POLECAT files on Dataverse (%d files):\n\n", nrow(remote)))

  for (i in seq_len(nrow(remote))) {
    cat(sprintf("  %-60s  %d  (v%s)\n",
                remote$label[i],
                remote$data_year[i],
                remote$version[i]))
  }

  invisible(remote)
}
