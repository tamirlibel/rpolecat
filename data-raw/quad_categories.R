#
#   Quad categories
#
#   Maps PLOVER event types to the four standard categories used in
#   quantitative conflict/cooperation analysis.
#

quad_categories <- data.frame(
  event_type = c("agree", "consult", "support", "concede",
                 "cooperate", "aid", "retreat",
                 "request", "accuse", "reject", "threaten",
                 "protest", "sanction", "mobilize", "coerce", "assault"),
  quad_category = c(
    rep("Verbal Cooperation", 4),
    rep("Material Cooperation", 3),
    rep("Verbal Conflict", 4),
    rep("Material Conflict", 5)
  ),
  goldstein = c(
    # Verbal Cooperation
    3.0, 2.0, 2.5, 1.0,
    # Material Cooperation
    5.0, 7.0, 4.0,
    # Verbal Conflict
    -1.0, -2.0, -3.0, -5.0,
    # Material Conflict
    -6.0, -5.5, -7.0, -8.0, -10.0
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(quad_categories, overwrite = TRUE)
