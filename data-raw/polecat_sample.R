#
#   Sample POLECAT data for use in examples and tests
#
#   This creates a small synthetic dataset that mirrors the structure and
#   column names of real POLECAT event data. It is intended for use in
#   package examples and unit tests only.
#

set.seed(42)

n <- 500

event_types <- c("agree", "consult", "support", "concede", "cooperate", "aid",
                 "retreat", "request", "accuse", "reject", "threaten",
                 "protest", "sanction", "mobilize", "coerce", "assault")

# Weight toward more common event types
type_weights <- c(0.05, 0.08, 0.06, 0.03, 0.07, 0.05,
                  0.02, 0.08, 0.10, 0.06, 0.08,
                  0.10, 0.04, 0.05, 0.06, 0.07)

countries <- c("USA", "GBR", "FRA", "DEU", "RUS", "CHN", "IND", "BRA",
               "JPN", "KOR", "IRN", "ISR", "SAU", "UKR", "TUR", "EGY",
               "NGA", "ZAF", "MEX", "AUS")

actors <- c("Government", "Military", "Police", "Citizen", "Media",
            "Opposition", "Rebel", "NGO", "IGO", "Business")

contexts_vec <- c("military", "intelligence", "executive", "legislative",
                  "election", "economic", "legal", "human_rights", "diplomatic",
                  "territory", "health", "migration", "environment",
                  "terrorism", "cyber", "crime", "corruption")

mode_map <- list(
  consult = c("visit", "third-party", "multilateral", "phone"),
  retreat = c("withdraw", "release", "return", "ceasefire"),
  request = c("assist", "change", "yield", "meet"),
  accuse = c("disapprove", "investigate", "allege"),
  reject = c("assist", "change", "yield", "meet"),
  threaten = c("restrict", "ban", "arrest", "violence"),
  protest = c("demo", "riot", "strike", "boycott"),
  sanction = c("convict", "expel", "withdraw", "discontinue"),
  mobilize = c("troops", "weapons", "police", "militia"),
  coerce = c("seize", "restrict", "ban", "arrest", "censor"),
  assault = c("beat", "firearms", "explosives", "aerial", "drone")
)

# Country centroids for approximate geolocation
country_coords <- data.frame(
  code = c("USA", "GBR", "FRA", "DEU", "RUS", "CHN", "IND", "BRA",
           "JPN", "KOR", "IRN", "ISR", "SAU", "UKR", "TUR", "EGY",
           "NGA", "ZAF", "MEX", "AUS"),
  lat  = c(38.0, 51.5, 46.2, 51.2, 55.8, 35.9, 20.6, -14.2,
           36.2, 35.9, 32.4, 31.0, 23.9, 48.4, 39.9, 26.8,
           9.1, -30.6, 23.6, -25.3),
  lon  = c(-97.0, -0.1, 2.2, 10.4, 37.6, 104.2, 79.0, -51.9,
           138.3, 127.8, 53.7, 34.8, 45.1, 31.2, 32.9, 30.8,
           8.7, 22.9, -102.6, 133.8),
  stringsAsFactors = FALSE
)

sampled_types <- sample(event_types, n, replace = TRUE, prob = type_weights)

# Generate dates spanning 2023
start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2023-12-31")
dates <- start_date + sort(sample(0:as.integer(end_date - start_date), n, replace = TRUE))

src_countries <- sample(countries, n, replace = TRUE)
tgt_countries <- sample(countries, n, replace = TRUE)
# ~30% intrastate
intra <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7))
tgt_countries[intra] <- src_countries[intra]

src_actors <- sample(actors, n, replace = TRUE)
tgt_actors <- sample(actors, n, replace = TRUE)

# Generate modes (empty for types without modes)
sampled_modes <- vapply(sampled_types, function(et) {
  if (et %in% names(mode_map)) {
    sample(mode_map[[et]], 1)
  } else {
    ""
  }
}, character(1), USE.NAMES = FALSE)

# Generate 1-2 contexts per event, semicolon-separated
sampled_contexts <- vapply(seq_len(n), function(i) {
  nc <- sample(1:2, 1)
  paste(sample(contexts_vec, nc), collapse = ";")
}, character(1))

# Generate coordinates: ~80% have geolocation, 20% are NA
has_geo <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.8, 0.2))
lats <- numeric(n)
lons <- numeric(n)
for (i in seq_len(n)) {
  if (has_geo[i]) {
    cc <- country_coords[country_coords$code == src_countries[i], ]
    lats[i] <- cc$lat + rnorm(1, sd = 2)
    lons[i] <- cc$lon + rnorm(1, sd = 2)
  } else {
    lats[i] <- NA_real_
    lons[i] <- NA_real_
  }
}

# Generate fake URLs
urls <- paste0("https://example.com/article/", sprintf("%06d", seq_len(n)))

polecat_sample <- data.frame(
  event_id       = paste0("EVT-2023-", sprintf("%06d", seq_len(n))),
  story_date     = format(dates, "%Y-%m-%d"),
  source         = paste(src_countries, src_actors, sep = "/"),
  source_actor   = src_actors,
  source_agent   = "",
  source_country = src_countries,
  target         = paste(tgt_countries, tgt_actors, sep = "/"),
  target_actor   = tgt_actors,
  target_agent   = "",
  target_country = tgt_countries,
  event_type     = sampled_types,
  mode           = sampled_modes,
  context        = sampled_contexts,
  latitude       = lats,
  longitude      = lons,
  goldstein      = round(runif(n, -10, 10), 1),
  url            = urls,
  stringsAsFactors = FALSE
)

# Add a handful of duplicates (same key fields, different url)
dup_idx <- sample(seq_len(n), 20)
dups <- polecat_sample[dup_idx, ]
dups$event_id <- paste0("EVT-2023-D", sprintf("%05d", seq_len(20)))
dups$url <- paste0("https://example.com/article/dup-", sprintf("%05d", seq_len(20)))
polecat_sample <- rbind(polecat_sample, dups)
rownames(polecat_sample) <- NULL

usethis::use_data(polecat_sample, overwrite = TRUE)
