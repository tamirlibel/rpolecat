#' NLP Web Scraper - Main Interface
#'
#' High-level functions that combine LLM code generation with web scraping
#' execution. Give a URL and natural language instructions, and the LLM
#' translates them into R scraping code that is then executed.
#'
#' @name nlp_scraper
NULL


#' Scrape a web page using natural language instructions
#'
#' The main entry point for NLP-powered web scraping. Provide a URL and
#' describe what you want to extract in plain English. The function will:
#' 1. Fetch the page (static or via Selenium)
#' 2. Send the URL, instruction, and page HTML to an LLM
#' 3. Receive generated R scraping code
#' 4. Execute the code and return the results
#'
#' @param url The URL of the page to scrape.
#' @param instruction Natural language description of what data to extract.
#'   Example: "Extract all product names and prices from the table".
#' @param method Scraping method: "auto" (detect automatically), "static"
#'   (use rvest), or "dynamic" (use RSelenium). Default "auto".
#' @param execute If TRUE (default), execute the generated code and return
#'   results. If FALSE, return only the generated code.
#' @param config An llm_config object. If NULL, creates a default one.
#' @param selenium_session An existing Selenium session from
#'   \code{start_browser()}. Only used when method = "dynamic". If NULL and
#'   method is "dynamic", a new session is started and stopped automatically.
#' @param verbose Print status messages? Default TRUE.
#'
#' @returns A list with:
#'   \item{result}{The scraped data (if execute = TRUE)}
#'   \item{code}{The generated R code}
#'   \item{explanation}{The LLM's explanation of the code}
#'   \item{method}{The method used ("static" or "dynamic")}
#'   \item{url}{The original URL}
#'
#' @examples
#' \dontrun{
#'   # Static page scraping
#'   result <- nlp_scrape(
#'     url = "https://en.wikipedia.org/wiki/List_of_countries_by_population",
#'     instruction = "Extract the country names and populations from the main table"
#'   )
#'   head(result$result)
#'
#'   # Dynamic page (JavaScript-rendered)
#'   result <- nlp_scrape(
#'     url = "https://example.com/spa-page",
#'     instruction = "Get all product cards with title and price",
#'     method = "dynamic"
#'   )
#'
#'   # Just generate code without executing
#'   code_only <- nlp_scrape(
#'     url = "https://example.com",
#'     instruction = "Extract all links",
#'     execute = FALSE
#'   )
#'   cat(code_only$code)
#' }
#'
#' @export
nlp_scrape <- function(url,
                       instruction,
                       method = c("auto", "static", "dynamic"),
                       execute = TRUE,
                       config = NULL,
                       selenium_session = NULL,
                       verbose = TRUE) {
  method <- match.arg(method)
  if (is.null(config)) config <- llm_config()

  # Auto-detect method
  if (method == "auto") {
    method <- detect_scraping_method(url, verbose)
  }

  if (verbose) message(sprintf("Using %s scraping for: %s", method, url))

  # Fetch page source for LLM context
  page_source <- NULL
  own_session <- FALSE

  if (method == "static") {
    if (verbose) message("Fetching page with rvest...")
    page_source <- tryCatch(
      get_page_source(url),
      error = function(e) {
        if (verbose) message("Could not fetch page source: ", e$message)
        NULL
      }
    )
    llm_method <- "rvest"
  } else {
    llm_method <- "rselenium"

    if (is.null(selenium_session)) {
      if (verbose) message("Starting Selenium browser...")
      selenium_session <- start_browser()
      own_session <- TRUE
    }

    if (verbose) message("Navigating to page with Selenium...")
    page_source <- tryCatch(
      navigate_and_wait(selenium_session$remDr, url),
      error = function(e) {
        if (verbose) message("Could not fetch dynamic page: ", e$message)
        NULL
      }
    )
  }

  # Generate scraping code via LLM
  if (verbose) message("Generating scraping code via LLM...")
  llm_result <- generate_scraping_code(
    url = url,
    instruction = instruction,
    page_source = page_source,
    method = llm_method,
    config = config
  )

  if (verbose) {
    message("Generated code:")
    message(llm_result$code)
    message("\nExplanation: ", llm_result$explanation)
  }

  # Execute the code if requested
  result <- NULL
  if (execute) {
    if (verbose) message("\nExecuting generated code...")

    if (method == "static") {
      result <- execute_scraping_code(llm_result$code)
    } else {
      result <- execute_dynamic_code(llm_result$code, selenium_session$remDr)
    }

    if (is.list(result) && !is.null(result$success) && !result$success) {
      warning("Code execution failed: ", result$error)
    }
  }

  # Clean up own Selenium session
  if (own_session && !is.null(selenium_session)) {
    if (verbose) message("Stopping Selenium browser...")
    stop_browser(selenium_session)
  }

  structure(
    list(
      result = result,
      code = llm_result$code,
      explanation = llm_result$explanation,
      method = method,
      url = url
    ),
    class = "nlp_scrape_result"
  )
}


#' Print method for nlp_scrape results
#' @param x An nlp_scrape_result object.
#' @param ... Additional arguments (ignored).
#' @export
print.nlp_scrape_result <- function(x, ...) {
  cat("NLP Web Scrape Result\n")
  cat("=====================\n")
  cat("URL:   ", x$url, "\n")
  cat("Method:", x$method, "\n\n")
  cat("--- Generated Code ---\n")
  cat(x$code, "\n\n")
  if (nzchar(x$explanation)) {
    cat("--- Explanation ---\n")
    cat(x$explanation, "\n\n")
  }
  if (!is.null(x$result)) {
    cat("--- Result Preview ---\n")
    if (is.data.frame(x$result)) {
      print(utils::head(x$result))
    } else {
      utils::str(x$result, max.level = 2)
    }
  }
  invisible(x)
}


#' Detect whether a page needs static or dynamic scraping
#'
#' Tries a static fetch first. If the page seems to rely heavily on JavaScript
#' (very little text content, or framework indicators), suggests dynamic scraping.
#'
#' @param url The URL to test.
#' @param verbose Print detection info? Default TRUE.
#'
#' @returns Character: "static" or "dynamic".
#' @noRd
detect_scraping_method <- function(url, verbose = TRUE) {
  page <- tryCatch(fetch_page(url, delay = 0), error = function(e) NULL)

  if (is.null(page)) {
    if (verbose) message("Could not fetch page statically; defaulting to dynamic.")
    return("dynamic")
  }

  body_text <- tryCatch(
    rvest::html_text2(rvest::html_element(page, "body")),
    error = function(e) ""
  )

  html_source <- as.character(page)

  # Heuristics for JS-heavy pages
  js_indicators <- c(
    "id=\"__next\"",       # Next.js
    "id=\"__nuxt\"",       # Nuxt.js
    "id=\"app\"",          # Vue.js / generic SPA
    "id=\"root\"",         # React
    "ng-app",              # Angular
    "data-reactroot"       # React
  )

  is_spa <- any(vapply(js_indicators, function(ind) grepl(ind, html_source, fixed = TRUE), logical(1)))
  text_is_sparse <- nchar(body_text) < 200

  if (is_spa && text_is_sparse) {
    if (verbose) message("Detected SPA/JavaScript-heavy page -> using dynamic scraping.")
    return("dynamic")
  }

  if (verbose) message("Page appears to be static HTML -> using static scraping.")
  "static"
}


#' Iteratively refine scraping code
#'
#' If the initial code doesn't produce the expected result, sends the error
#' back to the LLM for a corrected version.
#'
#' @param url The target URL.
#' @param instruction The original instruction.
#' @param previous_code The code that failed.
#' @param error_message The error message from execution.
#' @param config An llm_config object.
#' @param max_retries Maximum refinement attempts. Default 2.
#'
#' @returns A list with corrected code and explanation.
#' @export
refine_scraping_code <- function(url,
                                 instruction,
                                 previous_code,
                                 error_message,
                                 config = NULL,
                                 max_retries = 2) {
  if (is.null(config)) config <- llm_config()

  refinement_prompt <- sprintf(
    "The following R scraping code for URL '%s' failed with this error:

Error: %s

Original instruction: %s

Failed code:
```r
%s
```

Please fix the code. Return the corrected code in a ```r code block.",
    url, error_message, instruction, previous_code
  )

  system_prompt <- "You are an expert R programmer. Fix the web scraping code based on the error message. Return only the corrected code in a ```r code block followed by a brief explanation of what you changed."

  for (i in seq_len(max_retries)) {
    response <- call_llm_api(system_prompt, refinement_prompt, config)
    result <- parse_llm_response(response)

    # Try executing the new code
    exec_result <- execute_scraping_code(result$code)
    if (!is.list(exec_result) || is.null(exec_result$success) || exec_result$success) {
      return(list(code = result$code, explanation = result$explanation, result = exec_result))
    }

    # Update prompt with new error for next iteration
    refinement_prompt <- sprintf(
      "The corrected code still fails:

Error: %s

Code:
```r
%s
```

Please fix it again.",
      exec_result$error, result$code
    )
  }

  warning("Could not fix the code after ", max_retries, " attempts.")
  list(code = result$code, explanation = result$explanation, result = exec_result)
}
