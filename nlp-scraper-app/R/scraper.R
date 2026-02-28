# ── Web Scraping Helpers ───────────────────────────────────────────────────────
# Static scraping (rvest) + dynamic scraping (RSelenium) utilities, plus a
# safe code-execution wrapper used by the Shiny app.

# ── Static (rvest) ────────────────────────────────────────────────────────────

#' Fetch a page and return its HTML as a string (for LLM context)
fetch_page_source <- function(url) {
  resp <- httr::GET(url, httr::user_agent("rpolecat-scraper/1.0"))
  if (httr::status_code(resp) != 200) {
    stop(sprintf("HTTP %s fetching %s", httr::status_code(resp), url))
  }
  as.character(rvest::read_html(resp))
}


# ── Dynamic (RSelenium) ──────────────────────────────────────────────────────

#' Start a headless browser session
start_selenium <- function(browser = "chrome", port = 4445L) {
  extra <- if (browser == "chrome") {
    list(chromeOptions = list(args = list(
      "--headless", "--no-sandbox", "--disable-dev-shm-usage"
    )))
  } else {
    list(`moz:firefoxOptions` = list(args = list("-headless")))
  }

  rD <- RSelenium::rsDriver(
    browser            = browser,
    port               = port,
    chromever          = "latest",
    extraCapabilities  = extra,
    verbose            = FALSE
  )

  list(remDr = rD$client, server = rD$server)
}


#' Stop a Selenium session
stop_selenium <- function(session) {
  tryCatch({
    session$remDr$close()
    if (!is.null(session$server)) session$server$stop()
  }, error = function(e) message("Selenium cleanup warning: ", e$message))
  invisible(NULL)
}


#' Navigate and optionally wait for a CSS element
selenium_navigate <- function(remDr, url, wait_css = NULL, timeout = 10) {
  remDr$navigate(url)
  Sys.sleep(2)

  if (!is.null(wait_css)) {
    t0 <- Sys.time()
    while (difftime(Sys.time(), t0, units = "secs") < timeout) {
      found <- tryCatch(
        length(remDr$findElements("css selector", wait_css)) > 0,
        error = function(e) FALSE
      )
      if (found) break
      Sys.sleep(0.5)
    }
  }

  remDr$getPageSource()[[1]]
}


# ── Page-type detection ──────────────────────────────────────────────────────

#' Simple heuristic: if the static HTML body is nearly empty and has SPA
#' markers, flag it as dynamic.
detect_page_type <- function(url) {
  page <- tryCatch(rvest::read_html(url), error = function(e) NULL)
  if (is.null(page)) return("dynamic")

  body <- tryCatch(rvest::html_text2(rvest::html_element(page, "body")),
                   error = function(e) "")
  src  <- as.character(page)

  spa_ids <- c("__next", "__nuxt", "app", "root")
  has_spa <- any(vapply(spa_ids, function(id) grepl(sprintf('id="%s"', id), src, fixed = TRUE), logical(1)))

  if (has_spa && nchar(body) < 200) "dynamic" else "static"
}


# ── Safe code execution ──────────────────────────────────────────────────────

#' Execute generated R code in an isolated environment with a timeout.
#'
#' @param code  Character string of R code
#' @param remDr Optional RSelenium remote driver (injected into the eval env)
#' @param timeout_sec Maximum execution time in seconds
#' @return The last evaluated expression, or an error-list on failure.
run_code <- function(code, remDr = NULL, timeout_sec = 30) {
  env <- new.env(parent = globalenv())
  if (!is.null(remDr)) env$remDr <- remDr

  tryCatch(
    R.utils::withTimeout(
      eval(parse(text = code), envir = env),
      timeout = timeout_sec, onTimeout = "error"
    ),
    error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    }
  )
}
