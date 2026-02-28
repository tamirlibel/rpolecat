#' LLM API Integration for NLP Web Scraping
#'
#' Functions to interact with LLM APIs (Anthropic Claude or OpenAI) to translate
#' natural language scraping instructions into executable R code.
#'
#' @name llm_api
NULL


#' Set up LLM API configuration
#'
#' Configure which LLM provider and API key to use for generating scraping code.
#'
#' @param provider Character string: "anthropic" or "openai".
#' @param api_key API key for the chosen provider. If NULL, reads from
#'   environment variables ANTHROPIC_API_KEY or OPENAI_API_KEY.
#' @param model Model name to use. Defaults to "claude-sonnet-4-20250514" for
#'   Anthropic or "gpt-4o" for OpenAI.
#'
#' @returns A list with provider, api_key, and model.
#' @export
llm_config <- function(provider = c("anthropic", "openai"),
                       api_key = NULL,
                       model = NULL) {
  provider <- match.arg(provider)

  if (is.null(api_key)) {
    env_var <- switch(provider,
      anthropic = "ANTHROPIC_API_KEY",
      openai = "OPENAI_API_KEY"
    )
    api_key <- Sys.getenv(env_var, unset = "")
    if (api_key == "") {
      stop(sprintf(
        "No API key provided and %s environment variable is not set.\n
        Set it with Sys.setenv(%s = 'your-key') or pass api_key directly.",
        env_var, env_var
      ))
    }
  }

  if (is.null(model)) {
    model <- switch(provider,
      anthropic = "claude-sonnet-4-20250514",
      openai = "gpt-4o"
    )
  }

  structure(
    list(provider = provider, api_key = api_key, model = model),
    class = "llm_config"
  )
}


#' Generate scraping code from natural language instructions
#'
#' Sends a URL and natural language instructions to an LLM, which returns
#' R code to scrape the specified data.
#'
#' @param url The target URL to scrape.
#' @param instruction Natural language description of what data to extract.
#' @param page_source Optional HTML source of the page (used to give the LLM
#'   context about the page structure). If NULL, the LLM generates code based
#'   on the URL and instruction alone.
#' @param method Character: "rvest" for static scraping or "rselenium" for
#'   dynamic pages.
#' @param config An llm_config object. If NULL, uses default Anthropic config.
#'
#' @returns A list with components:
#'   \item{code}{Character string of generated R code}
#'   \item{explanation}{LLM's explanation of what the code does}
#'
#' @export
generate_scraping_code <- function(url,
                                   instruction,
                                   page_source = NULL,
                                   method = c("rvest", "rselenium"),
                                   config = NULL) {
  method <- match.arg(method)
  if (is.null(config)) config <- llm_config()

  system_prompt <- build_system_prompt(method)
  user_prompt <- build_user_prompt(url, instruction, page_source, method)

  response <- call_llm_api(system_prompt, user_prompt, config)
  parse_llm_response(response)
}


#' Build the system prompt for scraping code generation
#' @noRd
build_system_prompt <- function(method) {
  base_prompt <- "You are an expert R programmer specializing in web scraping.
You generate clean, working R code to scrape web pages based on user instructions.

RULES:
1. Return ONLY valid R code in a ```r code block, followed by a brief explanation.
2. The code must be self-contained and runnable.
3. Always include error handling with tryCatch.
4. Return results as a data.frame or list.
5. Add comments explaining key steps.
6. Do NOT install packages - only use library() calls."

  if (method == "rvest") {
    paste0(base_prompt, "

Use the 'rvest' and 'httr' packages for static page scraping.
Available functions: read_html(), html_elements(), html_text(), html_attr(),
html_table(), html_text2(), html_children(), html_name().")
  } else {
    paste0(base_prompt, "

Use the 'RSelenium' package for dynamic page scraping.
The user already has a Selenium session running. Use the provided 'remDr' object.
Available methods: remDr$navigate(), remDr$findElement(), remDr$findElements(),
remDr$getPageSource(), remDr$executeScript(), element$getElementText(),
element$getElementAttribute(), element$clickElement().
For waiting: Sys.sleep() for simple waits.")
  }
}


#' Build the user prompt with URL and instructions
#' @noRd
build_user_prompt <- function(url, instruction, page_source, method) {
  prompt <- sprintf("URL: %s\n\nInstruction: %s\n\nMethod: %s", url, instruction, method)

  if (!is.null(page_source)) {
    # Truncate page source to avoid token limits
    max_chars <- 8000
    if (nchar(page_source) > max_chars) {
      page_source <- substr(page_source, 1, max_chars)
      page_source <- paste0(page_source, "\n... [truncated]")
    }
    prompt <- paste0(prompt, "\n\nPage HTML (for context):\n", page_source)
  }

  prompt
}


#' Call the LLM API
#' @noRd
call_llm_api <- function(system_prompt, user_prompt, config) {
  if (config$provider == "anthropic") {
    call_anthropic_api(system_prompt, user_prompt, config)
  } else {
    call_openai_api(system_prompt, user_prompt, config)
  }
}


#' Call the Anthropic (Claude) API
#' @noRd
call_anthropic_api <- function(system_prompt, user_prompt, config) {
  body <- list(
    model = config$model,
    max_tokens = 4096,
    system = system_prompt,
    messages = list(
      list(role = "user", content = user_prompt)
    )
  )

  response <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key` = config$api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(response) != 200) {
    err <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("Anthropic API error (HTTP %s): %s",
                 httr::status_code(response), err))
  }

  parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  parsed$content$text[[1]]
}


#' Call the OpenAI API
#' @noRd
call_openai_api <- function(system_prompt, user_prompt, config) {
  body <- list(
    model = config$model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    max_tokens = 4096,
    temperature = 0.2
  )

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", config$api_key),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(response) != 200) {
    err <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("OpenAI API error (HTTP %s): %s",
                 httr::status_code(response), err))
  }

  parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  parsed$choices$message$content[[1]]
}


#' Parse LLM response to extract code and explanation
#' @noRd
parse_llm_response <- function(response_text) {
  # Extract code block
  code_pattern <- "```[rR]?\n(.*?)```"
  code_match <- regmatches(response_text, regexpr(code_pattern, response_text, perl = TRUE))

  if (length(code_match) == 0 || code_match == "") {
    # Try without language specifier
    code_pattern <- "```\n(.*?)```"
    code_match <- regmatches(response_text, regexpr(code_pattern, response_text, perl = TRUE))
  }

  if (length(code_match) > 0 && code_match != "") {
    # Remove the ``` markers
    code <- sub("^```[rR]?\n", "", code_match)
    code <- sub("\n?```$", "", code)
  } else {
    code <- response_text
  }

  # Extract explanation (everything outside the code block)
  explanation <- gsub(code_pattern, "", response_text, perl = TRUE)
  explanation <- trimws(explanation)

  list(code = code, explanation = explanation)
}
