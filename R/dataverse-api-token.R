#' Dataverse API token
#'
#' Check that the Dataverse API token and server are set and if so show them.
#'
#' @param print Print the token and server? (They are invisibly returned as well.)
#'
#' @details Interaction with Dataverse requires two environment variables:
#' \code{DATAVERSE_KEY} and \code{DATAVERSE_SERVER}.
#'
#' The links below have the latest official information, but generally what
#' needs to be done:
#'
#' The token can be obtained by signing in to your Dataverse account and
#' selecting "API token" from the dropdown menu. The token should then be used
#' to set the DATAVERSE_KEY option, i.e. \code{Sys.setenv(DATAVERSE_KEY = "{api key}")}.
#' The other environment variable should be \code{Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")}.
#'
#' I do this in my R profile file so it's automatically done for every R
#' session (\code{usethis::edit_r_profile()}).
#'
#' \url{https://guides.dataverse.org/en/latest/user/account.html#api-token}
#'
#' NOTE that the token is sensitive and should not be publicly shared. I.e. don't
#' put it on GitHub or GitLab.
#'
#' @returns The token and server. If the token or server are not set, an error
#' message will be thrown.
#'
#' @export
dataverse_api_token <- function(print = TRUE) {
  if (!check_api_token()) {
    stop("API token or server are not set. See ?dataverse_api_token",
         call. = FALSE)
  }
  token <- Sys.getenv("DATAVERSE_KEY")
  server <- Sys.getenv("DATAVERSE_SERVER")
  if (print) {
    cli::cli_text("Server: {.url {server}}")
    cli::cli_text("Token:  {.val {token}}")
  }
  invisible(list(token = token, server = server))
}


check_api_token <- function() {
  token <- Sys.getenv("DATAVERSE_KEY")
  server <- Sys.getenv("DATAVERSE_SERVER")
  # return TRUE if both token and server are set, FALSE otherwise
  token != "" & server != ""
}
