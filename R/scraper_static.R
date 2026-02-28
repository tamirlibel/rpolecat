#' Static Web Scraping with rvest
#'
#' Functions for scraping static web pages using rvest/httr.
#'
#' @name scraper_static
NULL


#' Fetch a static web page
#'
#' Downloads and parses the HTML content of a static web page.
#'
#' @param url The URL to fetch.
#' @param user_agent Custom user agent string. Defaults to a polite bot identifier.
#' @param delay Delay in seconds before fetching (for politeness). Default 1.
#'
#' @returns A parsed HTML document (xml_document object from rvest).
#' @export
fetch_page <- function(url,
                       user_agent = "rpolecat-scraper/1.0 (R web scraper)",
                       delay = 1) {
  stopifnot(is.character(url), length(url) == 1)

  if (delay > 0) Sys.sleep(delay)

  response <- tryCatch(
    httr::GET(url, httr::user_agent(user_agent)),
    error = function(e) {
      stop(sprintf("Failed to fetch '%s': %s", url, e$message))
    }
  )

  if (httr::status_code(response) != 200) {
    stop(sprintf("HTTP %s error when fetching '%s'",
                 httr::status_code(response), url))
  }

  rvest::read_html(response)
}


#' Get page source as text
#'
#' Fetch a page and return its HTML source as a character string.
#' Used to provide context to the LLM for code generation.
#'
#' @param url The URL to fetch.
#'
#' @returns Character string with the HTML source.
#' @export
get_page_source <- function(url) {
  page <- fetch_page(url, delay = 0)
  as.character(page)
}


#' Extract elements from a static page
#'
#' A convenience wrapper around rvest for common extraction tasks.
#'
#' @param page An HTML document from \code{fetch_page()}.
#' @param css CSS selector to target elements.
#' @param xpath XPath expression to target elements (used if css is NULL).
#' @param what What to extract: "text", "attr", or "table".
#' @param attr_name If what = "attr", the attribute name to extract.
#'
#' @returns Character vector of extracted text/attributes, or a data.frame for tables.
#' @export
extract_elements <- function(page,
                             css = NULL,
                             xpath = NULL,
                             what = c("text", "attr", "table"),
                             attr_name = NULL) {
  what <- match.arg(what)

  if (is.null(css) && is.null(xpath)) {
    stop("Either 'css' or 'xpath' must be provided.")
  }

  elements <- if (!is.null(css)) {
    rvest::html_elements(page, css = css)
  } else {
    rvest::html_elements(page, xpath = xpath)
  }

  if (length(elements) == 0) {
    message("No elements found matching the selector.")
    return(character(0))
  }

  switch(what,
    text = rvest::html_text2(elements),
    attr = {
      if (is.null(attr_name)) stop("'attr_name' required when what = 'attr'")
      rvest::html_attr(elements, attr_name)
    },
    table = {
      tables <- rvest::html_table(elements)
      if (length(tables) == 1) tables[[1]] else tables
    }
  )
}


#' Execute generated R scraping code safely
#'
#' Evaluates LLM-generated code in a controlled environment with the necessary
#' packages loaded, and returns the result.
#'
#' @param code Character string of R code to execute.
#' @param timeout Maximum seconds to allow execution. Default 30.
#'
#' @returns The result of the executed code, or an error message.
#' @export
execute_scraping_code <- function(code, timeout = 30) {
  stopifnot(is.character(code), length(code) == 1)

  # Create a clean environment with access to needed packages
  scrape_env <- new.env(parent = globalenv())

  result <- tryCatch(
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

  result
}
