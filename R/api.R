#
#   Core HTTP client for Dataverse REST API
#
#   All HTTP communication with Dataverse goes through this file.
#   Uses httr2 for requests with built-in retry and streaming support.
#

# Internal: build a base httr2 request for the Dataverse API
#
# @param endpoint API endpoint path (e.g. "/api/datasets/export")
# @param server Dataverse server hostname. If NULL, reads from DATAVERSE_SERVER env var.
# @param token API token. If NULL, reads from DATAVERSE_KEY env var.
# @param timeout Request timeout in seconds.
# @return An httr2 request object.
polecat_request <- function(endpoint, server = NULL, token = NULL,
                            timeout = 30) {
  if (is.null(server)) {
    server <- Sys.getenv("DATAVERSE_SERVER")
    if (server == "") {
      stop("DATAVERSE_SERVER environment variable is not set. See ?dataverse_api_token",
           call. = FALSE)
    }
  }
  if (is.null(token)) {
    token <- Sys.getenv("DATAVERSE_KEY")
    if (token == "") {
      stop("DATAVERSE_KEY environment variable is not set. See ?dataverse_api_token",
           call. = FALSE)
    }
  }

  # Ensure server doesn't have protocol prefix
 server <- sub("^https?://", "", server)

  url <- paste0("https://", server, endpoint)

  httr2::request(url) |>
    httr2::req_headers("X-Dataverse-key" = token) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L),
      backoff = ~ 2
    )
}


# Internal: perform a request and return the response
#
# @param req An httr2 request object.
# @return An httr2 response object.
polecat_perform <- function(req) {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("HTTP request failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  resp
}


# Internal: get the file listing for the POLECAT dataset from Dataverse
#
# Uses the dataset export endpoint to get metadata in JSON format,
# then extracts the file listing.
#
# @param doi Dataset DOI. Defaults to the POLECAT DOI.
# @param server Dataverse server hostname.
# @return A data frame with columns: label, id, version, creationDate, data_year.
get_dataverse_file_list <- function(doi = get_polecat_doi(), server = NULL) {
  endpoint <- paste0("/api/datasets/export?exporter=dataverse_json&persistentId=",
                     utils::URLencode(doi, reserved = TRUE))

  req <- polecat_request(endpoint, server = server, timeout = 30)
  resp <- polecat_perform(req)

  body <- httr2::resp_body_json(resp)

  # Extract files from the metadata
  if (is.null(body$datasetVersion$files)) {
    stop("No files found in Dataverse response. The dataset may be empty or ",
         "the DOI may be incorrect.", call. = FALSE)
  }

  files_raw <- body$datasetVersion$files

  file_list <- data.frame(
    label        = vapply(files_raw, function(f) f$dataFile$filename %||% f$label %||% NA_character_, character(1)),
    id           = vapply(files_raw, function(f) f$dataFile$id %||% NA_integer_, integer(1)),
    version      = vapply(files_raw, function(f) as.integer(f$version %||% NA_integer_), integer(1)),
    creationDate = vapply(files_raw, function(f) f$dataFile$creationDate %||% NA_character_, character(1)),
    stringsAsFactors = FALSE
  )

  # Extract the data year from filenames
  # Both filename formats start with a 4-digit year:
  # - ngecEvents.20230103111842.Release603.DV.txt
  # - ngecEventsDV-2022.txt.zip
  file_list$data_year <- as.integer(
    regmatches(file_list$label, regexpr("[0-9]{4}", file_list$label))
  )

  file_list
}


# Internal: download a single file from Dataverse by file ID
#
# Streams the file to disk rather than loading into memory.
#
# @param file_id Numeric file ID from the Dataverse file listing.
# @param dest_path Local file path to write to.
# @param server Dataverse server hostname.
# @param progress Show download progress? (requires cli package)
# @return TRUE if successful, FALSE otherwise.
download_dataverse_file <- function(file_id, dest_path, server = NULL,
                                    progress = TRUE) {
  endpoint <- paste0("/api/access/datafile/", file_id)

  req <- polecat_request(endpoint, server = server, timeout = 600)

  if (progress && requireNamespace("cli", quietly = TRUE)) {
    req <- httr2::req_progress(req)
  }

  tryCatch({
    resp <- httr2::req_perform(req, path = dest_path)

    # Verify the file was written
    if (!file.exists(dest_path) || file.info(dest_path)$size == 0) {
      warning("Downloaded file is empty or missing: ", dest_path, call. = FALSE)
      if (file.exists(dest_path)) file.remove(dest_path)
      return(FALSE)
    }

    TRUE
  }, error = function(e) {
    # Clean up partial downloads
    if (file.exists(dest_path)) file.remove(dest_path)
    warning("Failed to download file ", file_id, ": ", conditionMessage(e),
            call. = FALSE)
    FALSE
  })
}


#' Validate the Dataverse API connection
#'
#' Test that the Dataverse API token and server are correctly configured by
#' making a lightweight API call.
#'
#' @return \code{TRUE} if the connection is valid, \code{FALSE} otherwise.
#'   A message is printed with the result.
#'
#' @examples
#' \dontrun{
#'   validate_api_connection()
#' }
#' @export
#' @md
validate_api_connection <- function() {
  if (!check_api_token()) {
    message("API token or server are not set. See ?dataverse_api_token")
    return(invisible(FALSE))
  }

  tryCatch({
    endpoint <- paste0("/api/datasets/export?exporter=dataverse_json&persistentId=",
                       utils::URLencode(get_polecat_doi(), reserved = TRUE))
    req <- polecat_request(endpoint, timeout = 15)
    resp <- polecat_perform(req)
    message("API connection successful.")
    invisible(TRUE)
  }, error = function(e) {
    message("API connection failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}


# Null-coalescing operator (internal utility)
`%||%` <- function(x, y) if (is.null(x)) y else x
