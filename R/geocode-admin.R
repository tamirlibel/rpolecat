#
#   GADM administrative unit geocoding
#

#' Geocode events to administrative units
#'
#' Map the latitude/longitude of POLECAT events to their corresponding GADM
#' administrative units. Uses GADM boundary data from \url{https://gadm.org}
#' to perform point-in-polygon lookups.
#'
#' @param x A data frame of POLECAT events with \code{latitude} and
#'   \code{longitude} columns.
#' @param level Integer indicating the GADM administrative level to use:
#'   0 = country, 1 = state/province, 2 = district/county, etc.
#'   Defaults to \code{1}.
#' @param gadm_dir Directory to cache downloaded GADM data files. If
#'   \code{NULL}, uses a \code{gadm} subdirectory inside the rpolecat data
#'   directory (or \code{tempdir()} if no data directory is configured).
#' @param countries Optional character vector of ISO3 country codes to limit
#'   the GADM data downloads to. If \code{NULL}, all countries present in the
#'   data's \code{source_country} column are used.
#' @param verbose Print progress messages?
#'
#' @details
#' This function requires the \pkg{sf} and \pkg{geodata} packages to be
#' installed. GADM boundary data is downloaded once per country and cached
#' locally for reuse.
#'
#' Events without coordinates (\code{NA} latitude/longitude) will have
#' \code{NA} for all administrative unit columns.
#'
#' The function adds the following columns to the data frame:
#' \itemize{
#'   \item \code{admin0}: country name from GADM
#'   \item \code{admin1}: first-level administrative unit (state/province)
#'   \item \code{admin2}: second-level administrative unit (district/county),
#'     if \code{level >= 2}
#'   \item Higher levels if requested
#' }
#'
#' @returns The input data frame with added administrative unit columns.
#'
#' @examples
#' \dontrun{
#'   data(polecat_sample)
#'   dat <- geocode_polecat_admin(polecat_sample, level = 1)
#'   head(dat[, c("latitude", "longitude", "admin0", "admin1")])
#' }
#' @export
#' @md
geocode_polecat_admin <- function(x, level = 1, gadm_dir = NULL,
                                  countries = NULL, verbose = TRUE) {
  stopifnot(is.data.frame(x))
  check_sf_packages()

  if (!all(c("latitude", "longitude") %in% names(x))) {
    stop("Data must contain 'latitude' and 'longitude' columns", call. = FALSE)
  }

  # Determine cache directory
  if (is.null(gadm_dir)) {
    opt_dir <- getOption("rpolecat.data_dir")
    if (!is.null(opt_dir)) {
      gadm_dir <- file.path(opt_dir, "gadm")
    } else {
      gadm_dir <- file.path(tempdir(), "rpolecat_gadm")
    }
  }
  if (!dir.exists(gadm_dir)) dir.create(gadm_dir, recursive = TRUE)

  # Identify rows with valid coordinates
  has_coords <- !is.na(x$latitude) & !is.na(x$longitude)

  if (sum(has_coords) == 0) {
    message("No events have valid coordinates; nothing to geocode.")
    x$admin0 <- NA_character_
    if (level >= 1) x$admin1 <- NA_character_
    if (level >= 2) x$admin2 <- NA_character_
    return(x)
  }

  # Determine which countries to download GADM data for
  if (is.null(countries)) {
    if ("source_country" %in% names(x)) {
      countries <- unique(x$source_country[has_coords])
      countries <- countries[!is.na(countries) & countries != ""]
    } else {
      stop("Cannot determine countries. Provide the 'countries' argument or ",
           "ensure data has a 'source_country' column.", call. = FALSE)
    }
  }

  # Convert event coordinates to sf points
  coords <- x[has_coords, c("longitude", "latitude")]
  points <- sf::st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)

  # Download and merge GADM data for each country
  if (verbose) cat(sprintf("Downloading GADM data for %d countries...\n",
                           length(countries)))

  gadm_list <- lapply(countries, function(iso) {
    tryCatch({
      if (verbose) cat(sprintf("  %s...", iso))
      gadm <- geodata::gadm(country = iso, level = level, path = gadm_dir)
      gadm_sf <- sf::st_as_sf(gadm)
      if (verbose) cat(" OK\n")
      gadm_sf
    }, error = function(e) {
      if (verbose) cat(sprintf(" FAILED (%s)\n", e$message))
      NULL
    })
  })

  # Remove failed downloads
  gadm_list <- gadm_list[!vapply(gadm_list, is.null, logical(1))]

  if (length(gadm_list) == 0) {
    warning("Could not download GADM data for any country.", call. = FALSE)
    x$admin0 <- NA_character_
    if (level >= 1) x$admin1 <- NA_character_
    if (level >= 2) x$admin2 <- NA_character_
    return(x)
  }

  # Harmonize column names and combine
  all_gadm <- harmonize_and_bind_gadm(gadm_list, level)

  # Spatial join: find which admin unit each point falls in
  if (verbose) cat("Performing spatial join...\n")
  joined <- sf::st_join(points, all_gadm, join = sf::st_within)

  # Extract admin columns
  admin_cols <- paste0("admin", 0:level)
  admin_cols <- admin_cols[admin_cols %in% names(joined)]

  # Add results back to original data frame
  for (ac in admin_cols) {
    x[[ac]] <- NA_character_
    if (ac %in% names(joined)) {
      x[[ac]][has_coords] <- as.character(joined[[ac]])
    }
  }

  if (verbose) {
    matched <- sum(!is.na(x$admin0[has_coords]))
    cat(sprintf("Matched %d of %d geolocated events to admin units.\n",
                matched, sum(has_coords)))
  }

  x
}


# Internal: harmonize GADM column names across countries and combine
harmonize_and_bind_gadm <- function(gadm_list, level) {
  result_list <- lapply(gadm_list, function(g) {
    # GADM fields: COUNTRY (admin0), NAME_1 (admin1), NAME_2 (admin2), etc.
    out <- data.frame(admin0 = character(nrow(g)), stringsAsFactors = FALSE)

    if ("COUNTRY" %in% names(g)) {
      out$admin0 <- g$COUNTRY
    } else if ("NAME_0" %in% names(g)) {
      out$admin0 <- g$NAME_0
    }

    for (i in seq_len(level)) {
      col_name <- paste0("NAME_", i)
      admin_name <- paste0("admin", i)
      if (col_name %in% names(g)) {
        out[[admin_name]] <- g[[col_name]]
      } else {
        out[[admin_name]] <- NA_character_
      }
    }

    out$geometry <- sf::st_geometry(g)
    sf::st_as_sf(out)
  })

  do.call(rbind, result_list)
}


# Internal: check that required spatial packages are installed
check_sf_packages <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for geocoding. ",
         "Install it with: install.packages('sf')", call. = FALSE)
  }
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop("Package 'geodata' is required for downloading GADM data. ",
         "Install it with: install.packages('geodata')", call. = FALSE)
  }
}
