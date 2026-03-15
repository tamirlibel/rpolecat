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
    files <- files[grepl(POLECAT_FILE_PATTERN, basename(files))]

    if (length(files) == 0) {
      cli::cli_alert_info("No uncompressed POLECAT .txt files found.")
      return(invisible(character(0)))
    }

    if (verbose) cli::cli_alert_info("Compressing {length(files)} file{?s}...")

    out_files <- character(length(files))
    for (i in seq_along(files)) {
      out_path <- paste0(files[i], ".gz")
      if (verbose) cli::cli_progress_step("{basename(files[i])} -> {basename(out_path)}")

      tryCatch({
        con_in  <- file(files[i], "rb")
        on.exit(close(con_in), add = TRUE)
        con_out <- gzfile(out_path, "wb")
        on.exit(close(con_out), add = TRUE)
        while (length(chunk <- readBin(con_in, "raw", n = 65536L)) > 0) {
          writeBin(chunk, con_out)
        }
        close(con_out)
        close(con_in)
        on.exit()  # clear on.exit since we closed manually

        # Verify output
        if (!file.exists(out_path) || file.info(out_path)$size == 0) {
          cli::cli_warn("Compression produced empty file: {.file {out_path}}")
          next
        }

        if (remove) file.remove(files[i])
        out_files[i] <- out_path
      }, error = function(e) {
        cli::cli_warn("Failed to compress {.file {basename(files[i])}}: {conditionMessage(e)}")
      })
    }

    if (verbose) {
      new_size <- sum(file.info(out_files[out_files != ""])$size, na.rm = TRUE)
      cli::cli_alert_success("Compression complete.")
    }

    return(invisible(out_files))
  }

  # Decompress
  files <- dir(data_dir, pattern = "\\.txt\\.gz$", full.names = TRUE)
  files <- files[grepl(POLECAT_FILE_PATTERN, basename(files))]

  if (length(files) == 0) {
    cli::cli_alert_info("No compressed POLECAT .txt.gz files found.")
    return(invisible(character(0)))
  }

  if (verbose) cli::cli_alert_info("Decompressing {length(files)} file{?s}...")

  out_files <- character(length(files))
  for (i in seq_along(files)) {
    out_path <- sub("\\.gz$", "", files[i])
    if (verbose) cli::cli_progress_step("{basename(files[i])} -> {basename(out_path)}")

    tryCatch({
      con_in  <- gzfile(files[i], "rb")
      on.exit(close(con_in), add = TRUE)
      con_out <- file(out_path, "wb")
      on.exit(close(con_out), add = TRUE)
      while (length(chunk <- readBin(con_in, "raw", n = 65536L)) > 0) {
        writeBin(chunk, con_out)
      }
      close(con_out)
      close(con_in)
      on.exit()

      if (remove) file.remove(files[i])
      out_files[i] <- out_path
    }, error = function(e) {
      cli::cli_warn("Failed to decompress {.file {basename(files[i])}}: {conditionMessage(e)}")
    })
  }

  if (verbose) cli::cli_alert_success("Decompression complete.")
  invisible(out_files)
}
