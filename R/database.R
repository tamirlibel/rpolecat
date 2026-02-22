#
#   Database support (DuckDB/SQLite)
#

#' Create a POLECAT database
#'
#' Create a DuckDB (preferred) or SQLite database and ingest POLECAT data files
#' into it. This is useful for working with data that is too large to fit in
#' memory.
#'
#' @param db_path Path where the database file should be created.
#' @param data_dir Path to the local data directory containing POLECAT files.
#'   If \code{NULL}, uses the directory set via \code{\link{setup_polecat}}.
#' @param backend Database backend to use: \code{"duckdb"} (default) or
#'   \code{"sqlite"}.
#' @param years Optional numeric vector to restrict to specific years.
#' @param verbose Print progress messages?
#'
#' @returns Invisibly returns the path to the created database.
#'
#' @examples
#' \dontrun{
#'   create_polecat_db("~/data/polecat.duckdb", "~/data/polecat")
#' }
#' @export
#' @md
create_polecat_db <- function(db_path, data_dir = NULL,
                              backend = "duckdb", years = NULL,
                              verbose = TRUE) {
  data_dir <- resolve_data_dir(data_dir)
  check_db_packages(backend)

  if (file.exists(db_path)) {
    stop("Database already exists at '", db_path, "'. ",
         "Delete it first or use sync_db_with_files().", call. = FALSE)
  }

  if (verbose) cat("Reading POLECAT data files...\n")
  dat <- read_polecat(data_dir, years = years, verbose = verbose)

  if (verbose) cat(sprintf("Writing %s events to database...\n", nrow(dat)))
  con <- connect_db(db_path, backend)
  on.exit(DBI::dbDisconnect(con, shutdown = (backend == "duckdb")), add = TRUE)

  DBI::dbWriteTable(con, "events", dat, overwrite = TRUE)

  # Create useful indices
  tryCatch({
    DBI::dbExecute(con, "CREATE INDEX idx_story_date ON events (story_date)")
    DBI::dbExecute(con, "CREATE INDEX idx_source_country ON events (source_country)")
    DBI::dbExecute(con, "CREATE INDEX idx_target_country ON events (target_country)")
    DBI::dbExecute(con, "CREATE INDEX idx_event_type ON events (event_type)")
  }, error = function(e) {
    if (verbose) message("Note: could not create indices: ", e$message)
  })

  if (verbose) {
    db_size <- file.info(db_path)$size
    cat(sprintf("Database created: %s (%.1f MB)\n", db_path, db_size / 1e6))
  }

  invisible(db_path)
}


#' Connect to a POLECAT database
#'
#' Open a connection to an existing POLECAT database.
#'
#' @param db_path Path to the database file. If \code{NULL}, uses the path
#'   set via \code{\link{setup_polecat}}.
#' @param backend Database backend: \code{"duckdb"} or \code{"sqlite"}.
#'   Inferred from the file extension if not specified.
#'
#' @returns A DBI connection object.
#'
#' @examples
#' \dontrun{
#'   con <- connect_polecat("~/data/polecat.duckdb")
#'   DBI::dbListTables(con)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' @export
#' @md
connect_polecat <- function(db_path = NULL, backend = NULL) {
  if (is.null(db_path)) {
    db_path <- getOption("rpolecat.db_path")
    if (is.null(db_path)) {
      stop("No database path specified. Either pass 'db_path' or ",
           "run setup_polecat() with a db_path.", call. = FALSE)
    }
  }

  if (is.null(backend)) {
    backend <- if (grepl("\\.duckdb$|\\.duck$", db_path)) "duckdb" else "sqlite"
  }

  check_db_packages(backend)
  connect_db(db_path, backend)
}


#' Query the POLECAT database
#'
#' Execute a SQL query against a POLECAT database and return the results as
#' a data frame.
#'
#' @param query A SQL query string.
#' @param db_path Path to the database file. If \code{NULL}, uses the path
#'   set via \code{\link{setup_polecat}}.
#' @param backend Database backend: \code{"duckdb"} or \code{"sqlite"}.
#'
#' @returns A data frame with query results.
#'
#' @examples
#' \dontrun{
#'   # Count events by year
#'   query_polecat("SELECT substr(story_date, 1, 4) as year, COUNT(*) as n
#'                  FROM events GROUP BY year ORDER BY year")
#'
#'   # Events involving the USA
#'   query_polecat("SELECT * FROM events
#'                  WHERE source_country = 'USA' OR target_country = 'USA'
#'                  LIMIT 100")
#' }
#' @export
#' @md
query_polecat <- function(query, db_path = NULL, backend = NULL) {
  if (is.null(db_path)) {
    db_path <- getOption("rpolecat.db_path")
    if (is.null(db_path)) {
      stop("No database path specified. Either pass 'db_path' or ",
           "run setup_polecat() with a db_path.", call. = FALSE)
    }
  }

  if (is.null(backend)) {
    backend <- if (grepl("\\.duckdb$|\\.duck$", db_path)) "duckdb" else "sqlite"
  }

  check_db_packages(backend)
  con <- connect_db(db_path, backend)
  on.exit(DBI::dbDisconnect(con, shutdown = (backend == "duckdb")), add = TRUE)

  DBI::dbGetQuery(con, query)
}


#' Sync database with local files
#'
#' Synchronize a POLECAT database with whatever local files exist, without
#' downloading anything new. Useful when files are manually added or removed.
#'
#' @param db_path Path to the database file.
#' @param data_dir Path to the local data directory.
#' @param backend Database backend: \code{"duckdb"} or \code{"sqlite"}.
#' @param verbose Print progress messages?
#'
#' @returns Invisibly returns the number of rows in the updated database.
#'
#' @examples
#' \dontrun{
#'   sync_db_with_files("~/data/polecat.duckdb", "~/data/polecat")
#' }
#' @export
#' @md
sync_db_with_files <- function(db_path, data_dir = NULL,
                               backend = NULL, verbose = TRUE) {
  data_dir <- resolve_data_dir(data_dir)

  if (is.null(backend)) {
    backend <- if (grepl("\\.duckdb$|\\.duck$", db_path)) "duckdb" else "sqlite"
  }

  check_db_packages(backend)

  if (verbose) cat("Reading POLECAT data files...\n")
  dat <- read_polecat(data_dir, verbose = verbose)

  if (verbose) cat(sprintf("Rebuilding database with %s events...\n", nrow(dat)))
  con <- connect_db(db_path, backend)
  on.exit(DBI::dbDisconnect(con, shutdown = (backend == "duckdb")), add = TRUE)

  DBI::dbWriteTable(con, "events", dat, overwrite = TRUE)

  if (verbose) cat("Sync complete.\n")
  invisible(nrow(dat))
}


# Internal: check that required packages for the backend are installed
check_db_packages <- function(backend) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for database support. ",
         "Install it with: install.packages('DBI')", call. = FALSE)
  }
  if (backend == "duckdb") {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("Package 'duckdb' is required for DuckDB support. ",
           "Install it with: install.packages('duckdb')", call. = FALSE)
    }
  } else if (backend == "sqlite") {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
      stop("Package 'RSQLite' is required for SQLite support. ",
           "Install it with: install.packages('RSQLite')", call. = FALSE)
    }
  } else {
    stop("'backend' must be 'duckdb' or 'sqlite'", call. = FALSE)
  }
}


# Internal: create a connection to the database
connect_db <- function(db_path, backend) {
  if (backend == "duckdb") {
    DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  } else {
    DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  }
}
