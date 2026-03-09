#
#   Package options management
#

#' Set up rpolecat
#'
#' Persist the local data directory so you don't have to pass it to every
#' function call. Optionally writes the setting to your \code{.Rprofile} so
#' it is available in future R sessions.
#'
#' @param data_dir Path to the local directory where POLECAT data files are
#'   stored.
#' @param use_db Logical; use a database backend? Defaults to \code{FALSE}.
#' @param db_path Path to the database file (only used when \code{use_db = TRUE}).
#' @param r_profile Logical; write these settings to your user-level
#'   \code{.Rprofile} so they persist across sessions? Defaults to \code{FALSE}.
#'
#' @details The following R options are set:
#' \itemize{
#'   \item \code{rpolecat.data_dir}: path to local data directory
#'   \item \code{rpolecat.use_db}: whether to use a database backend
#'   \item \code{rpolecat.db_path}: path to the database file
#' }
#'
#' @returns Invisibly returns a list of the options that were set.
#'
#' @examples
#' \dontrun{
#'   setup_polecat("~/data/polecat")
#'
#'   # Persist across sessions
#'   setup_polecat("~/data/polecat", r_profile = TRUE)
#' }
#' @export
#' @md
setup_polecat <- function(data_dir, use_db = FALSE, db_path = NULL,
                          r_profile = FALSE) {
  stopifnot(
    "'data_dir' must be a single character string" = is.character(data_dir) && length(data_dir) == 1
  )

  data_dir <- normalizePath(data_dir, mustWork = FALSE)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created directory: ", data_dir)
  }

  opts <- list(
    rpolecat.data_dir = data_dir,
    rpolecat.use_db   = use_db
  )
  if (!is.null(db_path)) {
    opts$rpolecat.db_path <- normalizePath(db_path, mustWork = FALSE)
  }

  do.call(options, opts)

  if (r_profile) {
    write_to_rprofile(opts)
    message("Options written to .Rprofile. They will be set on future R startups.")
  }

  message("rpolecat options set:")
  message("  data_dir: ", data_dir)
  if (use_db) message("  use_db: TRUE")
  if (!is.null(db_path)) message("  db_path: ", db_path)

  invisible(opts)
}


#' Get rpolecat options
#'
#' Return the current values of all rpolecat options.
#'
#' @returns A named list of option values.
#'
#' @examples
#' get_polecat_opts()
#'
#' @export
get_polecat_opts <- function() {
  list(
    data_dir = getOption("rpolecat.data_dir"),
    use_db   = getOption("rpolecat.use_db", default = FALSE),
    db_path  = getOption("rpolecat.db_path")
  )
}


#' Set rpolecat options
#'
#' Programmatically set individual rpolecat options.
#'
#' @param data_dir Path to local data directory.
#' @param use_db Logical; use a database backend?
#' @param db_path Path to the database file.
#'
#' @returns Invisibly returns the new option values.
#'
#' @examples
#' \dontrun{
#'   set_polecat_opts(data_dir = "~/data/polecat")
#' }
#' @export
#' @md
set_polecat_opts <- function(data_dir = NULL, use_db = NULL, db_path = NULL) {
  opts <- list()
  if (!is.null(data_dir)) opts$rpolecat.data_dir <- normalizePath(data_dir, mustWork = FALSE)
  if (!is.null(use_db))   opts$rpolecat.use_db <- use_db
  if (!is.null(db_path))  opts$rpolecat.db_path <- normalizePath(db_path, mustWork = FALSE)

  if (length(opts) > 0) do.call(options, opts)
  invisible(get_polecat_opts())
}


#' Unset rpolecat options
#'
#' Remove all rpolecat options from the current session.
#'
#' @returns Invisibly returns \code{TRUE}.
#'
#' @examples
#' unset_polecat_opts()
#'
#' @export
unset_polecat_opts <- function() {
  options(
    rpolecat.data_dir = NULL,
    rpolecat.use_db   = NULL,
    rpolecat.db_path  = NULL
  )
  invisible(TRUE)
}


# Resolve the data directory from explicit argument or package option
resolve_data_dir <- function(data_dir = NULL) {
  if (!is.null(data_dir)) return(data_dir)
  opt <- getOption("rpolecat.data_dir")
  if (!is.null(opt)) return(opt)
  stop("No data directory specified. Either pass 'data_dir' or run setup_polecat() first.",
       call. = FALSE)
}


# Write options to .Rprofile
write_to_rprofile <- function(opts) {
  rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")

  lines <- character(0)
  if (file.exists(rprofile)) {
    lines <- readLines(rprofile)
  }

  # Remove any existing rpolecat option lines
  rpolecat_lines <- grepl("^options\\(rpolecat\\.", lines)
  lines <- lines[!rpolecat_lines]

  # Add new option lines
  new_lines <- vapply(names(opts), function(nm) {
    val <- opts[[nm]]
    if (is.character(val)) {
      sprintf('options(%s = "%s")', nm, val)
    } else if (is.logical(val)) {
      sprintf("options(%s = %s)", nm, toupper(as.character(val)))
    } else {
      sprintf("options(%s = %s)", nm, as.character(val))
    }
  }, character(1))

  lines <- c(lines, "", "# rpolecat options", new_lines)
  writeLines(lines, rprofile)
}
