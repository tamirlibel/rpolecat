#
#   Shared constants and helpers
#

# File pattern used to identify POLECAT data files
POLECAT_FILE_PATTERN <- "ngecEvents"

# The quad category mapping (PLOVER event types -> quad categories)
# Used by classify_quad() and get_quad()
QUAD_MAP <- c(
  agree     = "Verbal Cooperation",
  consult   = "Verbal Cooperation",
  support   = "Verbal Cooperation",
  concede   = "Verbal Cooperation",
  cooperate = "Material Cooperation",
  aid       = "Material Cooperation",
  retreat   = "Material Cooperation",
  request   = "Verbal Conflict",
  accuse    = "Verbal Conflict",
  reject    = "Verbal Conflict",
  threaten  = "Verbal Conflict",
  protest   = "Material Conflict",
  sanction  = "Material Conflict",
  mobilize  = "Material Conflict",
  coerce    = "Material Conflict",
  assault   = "Material Conflict"
)


# Internal: extract the data year from a POLECAT filename
#
# Both filename formats have a 4-digit year as the first number:
# - ngecEvents.20230103111842.Release603.DV.txt
# - ngecEventsDV-2022.txt.zip
#
# @param filenames Character vector of filenames.
# @return Integer vector of years.
extract_data_year <- function(filenames) {
  as.integer(regmatches(filenames, regexpr("[0-9]{4}", filenames)))
}


# Internal: compare remote and local file lists
#
# @param remote Data frame with at least a `label` column (from get_dataverse_file_list()).
# @param local_files Character vector of local filenames.
# @return A list with elements:
#   - to_download: subset of `remote` not present locally
#   - already_present: subset of `remote` that is present locally
#   - obsolete: character vector of local polecat files not in `remote`
compare_file_lists <- function(remote, local_files) {
  # Strip .zip/.gz for comparison
  local_base <- gsub("\\.zip$|\\.gz$", "", local_files)

  # Determine what to download
  already <- remote$label %in% local_base | remote$label %in% local_files
  to_download <- remote[!already, ]
  already_present <- remote[already, ]

  # Determine obsolete local files (polecat files not on remote)
  polecat_local <- local_files[grepl(POLECAT_FILE_PATTERN, local_files)]
  polecat_base <- gsub("\\.zip$|\\.gz$", "", polecat_local)
  obsolete_mask <- !polecat_base %in% remote$label & !polecat_local %in% remote$label
  obsolete <- polecat_local[obsolete_mask]

  list(
    to_download     = to_download,
    already_present = already_present,
    obsolete        = obsolete
  )
}
