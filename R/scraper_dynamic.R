#' Dynamic Web Scraping with RSelenium
#'
#' Functions for scraping dynamic (JavaScript-rendered) web pages using RSelenium.
#'
#' @name scraper_dynamic
NULL


#' Start a Selenium browser session
#'
#' Launches a Selenium browser for scraping dynamic pages. Requires a Selenium
#' server to be running (e.g., via Docker or a local installation).
#'
#' @param browser Browser to use: "chrome" or "firefox". Default "chrome".
#' @param port Port for the Selenium server. Default 4445L.
#' @param headless Run the browser in headless mode? Default TRUE.
#' @param selenium_url URL of a remote Selenium server (e.g. Docker). If NULL,
#'   starts a local server using rsDriver.
#'
#' @returns A list with:
#'   \item{remDr}{The remote driver object}
#'   \item{server}{The server object (NULL if using remote URL), needed for cleanup}
#'
#' @export
start_browser <- function(browser = c("chrome", "firefox"),
                          port = 4445L,
                          headless = TRUE,
                          selenium_url = NULL) {
  browser <- match.arg(browser)

  if (!is.null(selenium_url)) {
    # Connect to an existing remote Selenium server (e.g. Docker)
    remDr <- RSelenium::remoteDriver(
      remoteServerAddr = selenium_url,
      port = port,
      browserName = browser
    )
    remDr$open(silent = TRUE)
    return(list(remDr = remDr, server = NULL))
  }

  extra_capabilities <- list()
  if (headless) {
    if (browser == "chrome") {
      extra_capabilities <- list(
        chromeOptions = list(args = list("--headless", "--no-sandbox", "--disable-dev-shm-usage"))
      )
    } else {
      extra_capabilities <- list(
        `moz:firefoxOptions` = list(args = list("-headless"))
      )
    }
  }

  rD <- RSelenium::rsDriver(
    browser = browser,
    port = port,
    chromever = "latest",
    extraCapabilities = extra_capabilities,
    verbose = FALSE
  )

  list(remDr = rD$client, server = rD$server)
}


#' Stop a Selenium browser session
#'
#' Cleanly shuts down the browser and Selenium server.
#'
#' @param session A list returned by \code{start_browser()}.
#'
#' @returns NULL (invisibly).
#' @export
stop_browser <- function(session) {
  tryCatch({
    session$remDr$close()
    if (!is.null(session$server)) {
      session$server$stop()
    }
  }, error = function(e) {
    message("Warning: error stopping browser session: ", e$message)
  })
  invisible(NULL)
}


#' Navigate to a URL and wait for page load
#'
#' Navigates the Selenium browser to a URL and optionally waits for a specific
#' element to appear.
#'
#' @param remDr A remoteDriver object.
#' @param url The URL to navigate to.
#' @param wait_for_css Optional CSS selector to wait for before returning.
#' @param timeout Maximum seconds to wait for the element. Default 10.
#'
#' @returns The page source as a character string.
#' @export
navigate_and_wait <- function(remDr, url, wait_for_css = NULL, timeout = 10) {
  remDr$navigate(url)
  Sys.sleep(2)  # base wait for initial load

  if (!is.null(wait_for_css)) {
    wait_for_element(remDr, css = wait_for_css, timeout = timeout)
  }

  page_source <- remDr$getPageSource()[[1]]
  page_source
}


#' Wait for a DOM element to appear
#'
#' Polls the page until a matching element exists or timeout is reached.
#'
#' @param remDr A remoteDriver object.
#' @param css CSS selector to look for.
#' @param timeout Maximum seconds to wait. Default 10.
#'
#' @returns TRUE if found, or throws an error on timeout.
#' @export
wait_for_element <- function(remDr, css, timeout = 10) {
  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    elements <- tryCatch(
      remDr$findElements(using = "css selector", value = css),
      error = function(e) list()
    )
    if (length(elements) > 0) return(TRUE)
    Sys.sleep(0.5)
  }

  stop(sprintf("Timeout: element '%s' not found within %s seconds.", css, timeout))
}


#' Get the current page source from Selenium
#'
#' Retrieves the rendered HTML source from a running Selenium session.
#'
#' @param remDr A remoteDriver object.
#'
#' @returns Character string with the page HTML source.
#' @export
get_dynamic_page_source <- function(remDr) {
  remDr$getPageSource()[[1]]
}


#' Execute generated code with a Selenium session
#'
#' Runs LLM-generated scraping code that expects a \code{remDr} object to be
#' available.
#'
#' @param code Character string of R code to execute.
#' @param remDr A remoteDriver object to make available to the code.
#' @param timeout Maximum seconds for execution. Default 60.
#'
#' @returns The result of the executed code.
#' @export
execute_dynamic_code <- function(code, remDr, timeout = 60) {
  stopifnot(is.character(code), length(code) == 1)

  scrape_env <- new.env(parent = globalenv())
  scrape_env$remDr <- remDr

  tryCatch(
    {
      expr <- parse(text = code)
      R.utils::withTimeout(
        eval(expr, envir = scrape_env),
        timeout = timeout,
        onTimeout = "error"
      )
    },
    TimeoutException = function(e) {
      list(
        success = FALSE,
        error = sprintf("Code execution timed out after %s seconds.", timeout)
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error = sprintf("Code execution failed: %s", e$message)
      )
    }
  )
}
