#
#   Purge local data
#

#' Purge local POLECAT data
#'
#' Delete local POLECAT data files and/or the database. This is useful for
#' starting fresh.
#'
#' @param data_dir Path to the local data directory. If \code{NULL}, uses the
#'   directory set via \code{\link{setup_polecat}}.
#' @param db_path Path to the database file. If \code{NULL}, uses the path
#'   set via \code{\link{setup_polecat}}.
#' @param files Logical; delete data files? Defaults to \code{TRUE}.
#' @param db Logical; delete the database? Defaults to \code{FALSE}.
#' @param confirm Logical; require interactive confirmation before deleting?
#'   Defaults to \code{TRUE}.
#'
#' @returns Invisibly returns \code{TRUE} if anything was deleted.
#'
#' @examples
#' \dontrun{
#'   purge_polecat("~/data/polecat")
#' }
#' @export
#' @md
purge_polecat <- function(data_dir = NULL, db_path = NULL,
                          files = TRUE, db = FALSE,
                          confirm = TRUE) {
  deleted <- FALSE

  if (files) {
    dir_to_purge <- tryCatch(resolve_data_dir(data_dir), error = function(e) NULL)

    if (!is.null(dir_to_purge) && dir.exists(dir_to_purge)) {
      all_files <- dir(dir_to_purge, full.names = TRUE)
      polecat_files <- all_files[grepl("ngecEvents", basename(all_files))]

      if (length(polecat_files) > 0) {
        if (confirm && interactive()) {
          cat(sprintf("About to delete %d POLECAT files from '%s'\n",
                      length(polecat_files), dir_to_purge))
          ans <- readline("Continue? (y/n) ")
          if (!tolower(ans) %in% c("y", "yes")) {
            cat("Aborted.\n")
            return(invisible(FALSE))
          }
        }
        file.remove(polecat_files)
        cat(sprintf("Deleted %d files from '%s'\n",
                    length(polecat_files), dir_to_purge))
        deleted <- TRUE
      } else {
        cat("No POLECAT files to delete.\n")
      }
    }
  }

  if (db) {
    if (is.null(db_path)) db_path <- getOption("rpolecat.db_path")

    if (!is.null(db_path) && file.exists(db_path)) {
      if (confirm && interactive()) {
        cat(sprintf("About to delete database '%s'\n", db_path))
        ans <- readline("Continue? (y/n) ")
        if (!tolower(ans) %in% c("y", "yes")) {
          cat("Aborted.\n")
          return(invisible(FALSE))
        }
      }
      file.remove(db_path)
      cat(sprintf("Deleted database '%s'\n", db_path))
      deleted <- TRUE
    } else {
      cat("No database to delete.\n")
    }
  }

  invisible(deleted)
}
