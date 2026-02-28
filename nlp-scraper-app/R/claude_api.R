# ── Claude API Helper ──────────────────────────────────────────────────────────
# Handles all communication with the Anthropic Claude API to generate R
# scraping code from natural language instructions.

#' Call the Claude API to generate scraping code
#'
#' @param url Target URL to scrape
#' @param instruction Natural language description of what to extract
#' @param page_html Optional HTML source for context (truncated to fit tokens)
#' @param method "rvest" or "rselenium"
#' @param api_key Anthropic API key
#' @param model Claude model to use
#' @return List with `code` and `explanation`
call_claude <- function(url,
                        instruction,
                        page_html = NULL,
                        method = "rvest",
                        api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                        model = "claude-sonnet-4-20250514") {

  if (!nzchar(api_key)) {
    stop("ANTHROPIC_API_KEY is not set. Provide it in the app or set the env var.")
  }

  system_prompt <- build_system_prompt(method)
  user_prompt   <- build_user_prompt(url, instruction, page_html, method)

  body <- list(
    model = model,
    max_tokens = 4096,
    system = system_prompt,
    messages = list(list(role = "user", content = user_prompt))
  )

  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key`         = api_key,
      `anthropic-version`  = "2023-06-01",
      `content-type`       = "application/json"
    ),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(resp) != 200) {
    err <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("Claude API error (HTTP %s): %s", httr::status_code(resp), err))
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  parse_response(parsed$content$text[[1]])
}


# ── Prompt builders ───────────────────────────────────────────────────────────

build_system_prompt <- function(method) {
  base <- "You are an expert R programmer specializing in web scraping.
Generate clean, working R code based on the user's natural language instruction.

RULES:
1. Return ONLY valid R code inside a single ```r code block, followed by a brief explanation.
2. The code must be self-contained and immediately runnable.
3. Wrap the main logic in tryCatch for error handling.
4. Return the final result as a data.frame (preferred) or list.
5. Add short comments on key steps.
6. Do NOT call install.packages(); only use library()."

  if (method == "rvest") {
    paste0(base, "\n\nUse the 'rvest' and 'httr' packages for static HTML scraping.
Key functions: read_html(), html_elements(), html_text2(), html_attr(), html_table().")
  } else {
    paste0(base, "\n\nUse the 'RSelenium' package. A remote driver object called `remDr`
is already open and available in the environment.
Key methods: remDr$navigate(), remDr$findElement(), remDr$findElements(),
remDr$getPageSource(), remDr$executeScript(), element$getElementText(),
element$getElementAttribute(), element$clickElement().
Use Sys.sleep() for simple waits between actions.")
  }
}


build_user_prompt <- function(url, instruction, page_html, method) {
  prompt <- sprintf("URL: %s\nInstruction: %s\nMethod: %s", url, instruction, method)

  if (!is.null(page_html) && nzchar(page_html)) {
    # Truncate to ~8 000 chars so we don't blow the context window
    if (nchar(page_html) > 8000) {
      page_html <- paste0(substr(page_html, 1, 8000), "\n... [truncated]")
    }
    prompt <- paste0(prompt, "\n\nPage HTML (for context):\n", page_html)
  }

  prompt
}


# ── Response parser ───────────────────────────────────────────────────────────

parse_response <- function(text) {
  # Pull out the first ```r ... ``` block
  m <- regmatches(text, regexpr("```[rR]?\n(.*?)```", text, perl = TRUE))

  if (length(m) > 0 && nzchar(m)) {
    code <- sub("^```[rR]?\n", "", m)
    code <- sub("\n?```$", "", code)
  } else {
    code <- text
  }

  explanation <- trimws(gsub("```[rR]?\n.*?```", "", text, perl = TRUE))

  list(code = code, explanation = explanation)
}
