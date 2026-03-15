#
#   Arrow/Parquet support
#

#' Write POLECAT data to Parquet format
#'
#' Write a POLECAT data frame to a Parquet file for efficient storage and fast
#' reading. Parquet files are typically much smaller and faster to read than
#' the original TSV files.
#'
#' @param x A data frame of POLECAT events.
#' @param path File path to write to (should end in \code{.parquet}).
#' @param compression Compression codec to use. One of \code{"snappy"}
#'   (default), \code{"gzip"}, \code{"zstd"}, or \code{"none"}.
#'
#' @details Requires the \pkg{arrow} package. Install it with
#'   \code{install.packages("arrow")}.
#'
#' @returns Invisibly returns the path to the written file.
#'
#' @examples
#' \dontrun{
#'   dat <- read_polecat("~/data/polecat")
#'   write_polecat_parquet(dat, "~/data/polecat.parquet")
#' }
#' @export
#' @md
write_polecat_parquet <- function(x, path, compression = "snappy") {
  check_arrow()
  stopifnot(is.data.frame(x))

  arrow::write_parquet(x, sink = path, compression = compression)
  cli::cli_alert_success(
    "Wrote {nrow(x)} rows to {.path {path}} ({sprintf('%.1f MB', file.info(path)$size / 1e6)})"
  )
  invisible(path)
}


#' Read POLECAT data from Parquet format
#'
#' Read a Parquet file previously written by \code{\link{write_polecat_parquet}}.
#'
#' @param path Path to a Parquet file.
#'
#' @details Requires the \pkg{arrow} package.
#'
#' @returns A data frame of POLECAT events.
#'
#' @examples
#' \dontrun{
#'   dat <- read_polecat_parquet("~/data/polecat.parquet")
#' }
#' @export
#' @md
read_polecat_parquet <- function(path) {
  check_arrow()
  if (!file.exists(path)) {
    stop("File not found: '", path, "'", call. = FALSE)
  }
  as.data.frame(arrow::read_parquet(path))
}


# Internal: check that arrow is installed
check_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for Parquet support. ",
         "Install it with: install.packages('arrow')", call. = FALSE)
  }
}
