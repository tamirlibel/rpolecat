# Helper: create a sample dataset that mirrors polecat_sample structure
# This is used in tests so they don't depend on the package data being loadable.

make_test_data <- function(n = 30) {
  set.seed(42)

  event_types <- c("agree", "consult", "support", "concede",
                   "cooperate", "aid", "retreat",
                   "request", "accuse", "reject", "threaten",
                   "protest", "sanction", "mobilize", "coerce", "assault")
  countries <- c("USA", "GBR", "FRA", "RUS", "CHN", "DEU")
  actors <- c("GOV", "MIL", "CIV", "OPP", "IGO", "NGO")
  modes_vec <- c("conventional", "unconventional", NA)
  contexts_vec <- c("military", "economic", "political", "military;economic",
                    "political;economic", NA)

  dates <- seq.Date(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  story_dates <- sort(sample(dates, n, replace = TRUE))

  src_countries <- sample(countries, n, replace = TRUE)
  tgt_countries <- sample(countries, n, replace = TRUE)
  # Ensure some intrastate (same country) rows
  intra_idx <- sample(seq_len(n), ceiling(n / 3))
  tgt_countries[intra_idx] <- src_countries[intra_idx]

  src_actors <- sample(actors, n, replace = TRUE)
  tgt_actors <- sample(actors, n, replace = TRUE)
  # Make a couple target actors empty to test filter_dyadic
  tgt_actors[1:2] <- ""

  lat <- runif(n, -50, 60)
  lon <- runif(n, -120, 150)
  # Set some to NA to test geolocated filter
  na_geo <- sample(seq_len(n), ceiling(n * 0.2))
  lat[na_geo] <- NA
  lon[na_geo] <- NA

  data.frame(
    event_id       = paste0("EVT-", sprintf("%04d", seq_len(n))),
    story_date     = as.character(story_dates),
    source         = paste0(src_countries, "/", src_actors),
    source_actor   = src_actors,
    source_agent   = sample(c("president", "minister", "commander", NA), n, replace = TRUE),
    source_country = src_countries,
    target         = paste0(tgt_countries, "/", tgt_actors),
    target_actor   = tgt_actors,
    target_agent   = sample(c("president", "minister", "commander", NA), n, replace = TRUE),
    target_country = tgt_countries,
    event_type     = sample(event_types, n, replace = TRUE),
    mode           = sample(modes_vec, n, replace = TRUE),
    context        = sample(contexts_vec, n, replace = TRUE),
    latitude       = lat,
    longitude      = lon,
    goldstein      = round(runif(n, -10, 10), 1),
    url            = paste0("https://example.com/article/", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

# Create a version with known duplicates (like polecat_sample has 20)
make_test_data_with_dupes <- function() {
  base <- make_test_data(30)
  dupes <- base[sample(nrow(base), 5, replace = FALSE), ]
  rbind(base, dupes)
}
