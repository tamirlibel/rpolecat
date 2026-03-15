
#' Read POLECAT data files
#'
#' Read one or more downloaded POLECAT data files into a data frame.
#'
#' @param path Path to a POLECAT data file (.txt or .zip), or a directory
#'   containing multiple POLECAT files. If a directory is given, all matching
#'   POLECAT files (".txt" or ".zip") in that directory are read and combined.
#' @param years Optional numeric vector to filter to specific years when
#'   \code{path} is a directory.
#' @param verbose Print progress messages while reading files?
#'
#' @details
#' POLECAT data files come in two forms:
#' \itemize{
#'   \item Weekly \code{.txt} files (tab-separated) for the current year, e.g.
#'     \code{ngecEvents.20230103111842.Release603.DV.txt}
#'   \item Yearly \code{.zip} archives for historical data, each containing
#'     monthly \code{.txt} files, e.g. \code{ngecEventsDV-2022.txt.zip}
#' }
#'
#' Several of the data files are slightly malformed TSV files. This function
#' handles the two known issues:
#' \enumerate{
#'   \item Some files contain extra trailing tab characters that create empty
#'     columns. These are dropped automatically.
#'   \item R's default \code{quote} setting includes single quotes, but some
#'     field values in the data include single quotes (e.g. possessive forms
#'     like "Merkel's"). The function sets \code{quote = "\""} to handle this.
#' }
#'
#' @returns A data frame containing all events from the specified file(s).
#'
#' @examples
#' \dontrun{
#'   # Read a single weekly file
#'   dat <- read_polecat("my/data/dir/ngecEvents.20230103111842.Release603.DV.txt")
#'
#'   # Read a single yearly zip archive
#'   dat <- read_polecat("my/data/dir/ngecEventsDV-2022.txt.zip")
#'
#'   # Read all files in a directory
#'   dat <- read_polecat("my/data/dir")
#'
#'   # Read only specific years from a directory
#'   dat <- read_polecat("my/data/dir", years = 2022:2023)
#' }
#' @export
#' @md
read_polecat <- function(path, years = NULL, verbose = TRUE) {
  if (dir.exists(path)) {
    all_files <- dir(path, full.names = TRUE)
    polecat_files <- all_files[grepl(
      paste0(POLECAT_FILE_PATTERN, ".*\\.txt$|", POLECAT_FILE_PATTERN,
             ".*\\.txt\\.gz$|", POLECAT_FILE_PATTERN, "DV.*\\.zip$"),
      basename(all_files)
    )]

    if (!is.null(years)) {
      years <- as.integer(years)
      years_pattern <- paste0(years, collapse = "|")
      year_match <- extract_data_year(basename(polecat_files))
      polecat_files <- polecat_files[year_match %in% years]
    }

    if (length(polecat_files) == 0) {
      stop("No POLECAT data files found in '", path, "'")
    }

    results <- lapply(polecat_files, function(f) {
      read_polecat_file(f, verbose = verbose)
    })
    out <- do.call(rbind, results)
    rownames(out) <- NULL
    return(out)
  }

  if (!file.exists(path)) {
    stop("File not found: '", path, "'")
  }

  read_polecat_file(path, verbose = verbose)
}


# Internal: read a single POLECAT file (.txt, .txt.gz, or .zip)
read_polecat_file <- function(path, verbose = TRUE) {
  if (grepl("\\.zip$", path)) {
    read_polecat_zip(path, verbose = verbose)
  } else if (grepl("\\.gz$", path)) {
    if (verbose) cat(sprintf("Reading '%s'\n", basename(path)))
    read_polecat_tsv(gzfile(path))
  } else {
    if (verbose) cat(sprintf("Reading '%s'\n", basename(path)))
    read_polecat_tsv(path)
  }
}


# Internal: read a zip archive containing monthly .txt files
read_polecat_zip <- function(path, verbose = TRUE) {
  if (verbose) cat(sprintf("Reading '%s'\n", basename(path)))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(path, exdir = tmp_dir)
  txt_files <- dir(tmp_dir, pattern = "\\.txt$", full.names = TRUE,
                   recursive = TRUE)

  if (length(txt_files) == 0) {
    warning("No .txt files found inside '", basename(path), "'")
    return(data.frame())
  }

  results <- lapply(txt_files, read_polecat_tsv)
  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}


# Internal: read a single tab-separated POLECAT .txt file
read_polecat_tsv <- function(path) {
  # quote = "\"" avoids issues with single quotes in field values (e.g.
  # possessive forms like "Merkel's")
  dat <- utils::read.table(path, sep = "\t", header = TRUE,
                           quote = "\"", fill = TRUE,
                           stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8")

  # Some files have excess trailing tabs, creating empty columns with names
  # like "X", "X.1", etc. Drop any columns that are entirely NA or empty.
  non_empty <- vapply(dat, function(col) {
    !all(is.na(col) | col == "")
  }, logical(1))
  dat <- dat[, non_empty, drop = FALSE]

  dat
}
