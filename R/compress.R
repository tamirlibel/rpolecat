#
#   File compression
#

#' Compress or decompress local POLECAT files
#'
#' Compress uncompressed \code{.txt} files to \code{.gz} format to save disk
#' space, or decompress \code{.gz} files back to \code{.txt}. The
#' \code{\link{read_polecat}} function handles \code{.gz} files transparently.
#'
#' @param data_dir Path to the local data directory. If \code{NULL}, uses the
#'   directory set via \code{\link{setup_polecat}}.
#' @param action One of \code{"compress"} (default) or \code{"decompress"}.
#' @param remove Logical; remove the original files after compression/
#'   decompression? Defaults to \code{TRUE}.
#' @param verbose Print progress messages?
#'
#' @returns Invisibly returns a character vector of the resulting file paths.
#'
#' @examples
#' \dontrun{
#'   # Compress all .txt files
#'   compress_polecat("~/data/polecat")
#'
#'   # Decompress back
#'   compress_polecat("~/data/polecat", action = "decompress")
#' }
#' @export
#' @md
compress_polecat <- function(data_dir = NULL, action = "compress",
                             remove = TRUE, verbose = TRUE) {
  data_dir <- resolve_data_dir(data_dir)
  if (!action %in% c("compress", "decompress")) {
    stop("'action' must be 'compress' or 'decompress'", call. = FALSE)
  }

  if (action == "compress") {
    files <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
    # Only compress POLECAT files
    files <- files[grepl("ngecEvents", basename(files))]

    if (length(files) == 0) {
      message("No uncompressed POLECAT .txt files found.")
      return(invisible(character(0)))
    }

    if (verbose) cat(sprintf("Compressing %d files...\n", length(files)))

    out_files <- character(length(files))
    for (i in seq_along(files)) {
      out_path <- paste0(files[i], ".gz")
      if (verbose) cat(sprintf("  %s -> %s\n", basename(files[i]), basename(out_path)))

      con_in  <- file(files[i], "rb")
      con_out <- gzfile(out_path, "wb")
      while (length(chunk <- readBin(con_in, "raw", n = 65536L)) > 0) {
        writeBin(chunk, con_out)
      }
      close(con_in)
      close(con_out)

      if (remove) file.remove(files[i])
      out_files[i] <- out_path
    }

    if (verbose) {
      orig_size <- sum(file.info(files)$size, na.rm = TRUE)
      new_size  <- sum(file.info(out_files)$size)
      cat(sprintf("Done. Saved %.1f MB (%.0f%% reduction)\n",
                  (orig_size - new_size) / 1e6,
                  (1 - new_size / orig_size) * 100))
    }

    return(invisible(out_files))
  }

  # Decompress
  files <- dir(data_dir, pattern = "\\.txt\\.gz$", full.names = TRUE)
  files <- files[grepl("ngecEvents", basename(files))]

  if (length(files) == 0) {
    message("No compressed POLECAT .txt.gz files found.")
    return(invisible(character(0)))
  }

  if (verbose) cat(sprintf("Decompressing %d files...\n", length(files)))

  out_files <- character(length(files))
  for (i in seq_along(files)) {
    out_path <- sub("\\.gz$", "", files[i])
    if (verbose) cat(sprintf("  %s -> %s\n", basename(files[i]), basename(out_path)))

    con_in  <- gzfile(files[i], "rb")
    con_out <- file(out_path, "wb")
    while (length(chunk <- readBin(con_in, "raw", n = 65536L)) > 0) {
      writeBin(chunk, con_out)
    }
    close(con_in)
    close(con_out)

    if (remove) file.remove(files[i])
    out_files[i] <- out_path
  }

  if (verbose) cat("Done.\n")
  invisible(out_files)
}
