# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║  NLP Web Scraper — Shiny App                                            ║
# ║  Describe what you want to scrape in plain English; Claude generates     ║
# ║  and executes the R code (rvest for static, RSelenium for dynamic).     ║
# ╚═══════════════════════════════════════════════════════════════════════════╝
#
# Launch:
#   shiny::runApp("nlp-scraper-app")
#
# Required packages: shiny, httr, jsonlite, rvest, R.utils, DT
# Optional:          RSelenium (for dynamic / JS-rendered pages)
# Environment:       ANTHROPIC_API_KEY must be set (or entered in the UI)

library(shiny)
library(httr)
library(jsonlite)
library(rvest)
library(R.utils)

# Source helper modules
source(file.path("R", "claude_api.R"), local = TRUE)
source(file.path("R", "scraper.R"),    local = TRUE)


# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

  # ── Header ───────────────────────────────────────────────────────────────
  tags$div(
    style = "background:#2c3e50; color:white; padding:18px 24px; margin:-15px -15px 20px;",
    tags$h3("NLP Web Scraper", style = "margin:0;"),
    tags$p("Give a URL + plain-English instruction → Claude writes & runs the R code",
           style = "margin:4px 0 0; opacity:.8;")
  ),

  sidebarLayout(

    # ── Sidebar: inputs ──────────────────────────────────────────────────────
    sidebarPanel(
      width = 4,

      textInput("url", "Target URL",
                placeholder = "https://example.com/page"),

      textAreaInput("instruction", "What do you want to extract?",
                    rows = 4,
                    placeholder = paste0(
                      "e.g.  Extract all article titles, dates, ",
                      "and links from the blog listing")),

      selectInput("method", "Scraping method",
                  c("Auto-detect" = "auto",
                    "Static (rvest)" = "static",
                    "Dynamic (RSelenium)" = "dynamic")),

      passwordInput("api_key", "Anthropic API key",
                    placeholder = "Leave blank → uses ANTHROPIC_API_KEY env var"),

      selectInput("model", "Claude model",
                  c("claude-sonnet-4-20250514",
                    "claude-haiku-4-5-20251001")),

      checkboxInput("execute", "Execute generated code", value = TRUE),

      actionButton("go", "Scrape",
                   class = "btn-success btn-lg w-100",
                   icon  = icon("wand-magic-sparkles")),

      tags$hr(),

      tags$h6("Quick examples"),
      actionButton("ex_wiki",  "Wikipedia table",   class = "btn-sm btn-outline-secondary w-100 mb-1"),
      actionButton("ex_hn",    "Hacker News links", class = "btn-sm btn-outline-secondary w-100 mb-1"),
      actionButton("ex_quotes","Quotes to Scrape",  class = "btn-sm btn-outline-secondary w-100")
    ),

    # ── Main panel: outputs ──────────────────────────────────────────────────
    mainPanel(
      width = 8,

      uiOutput("status_ui"),

      tabsetPanel(
        id = "tabs",

        # Tab 1 — generated code
        tabPanel("Generated Code",
          tags$pre(
            style = "background:#1e1e1e; color:#d4d4d4; padding:14px;
                     border-radius:6px; max-height:450px; overflow:auto;
                     font-size:13px; white-space:pre-wrap;",
            textOutput("code_out")
          )
        ),

        # Tab 2 — scraped data
        tabPanel("Results",
          tags$br(),
          uiOutput("result_ui")
        ),

        # Tab 3 — Claude's explanation
        tabPanel("Explanation",
          tags$br(),
          verbatimTextOutput("explanation_out")
        ),

        # Tab 4 — editable code + re-run
        tabPanel("Code Editor",
          tags$br(),
          tags$p("Tweak the code and re-run manually:"),
          textAreaInput("editor", NULL, rows = 15, width = "100%"),
          actionButton("run_editor", "Run", class = "btn-warning",
                       icon = icon("play")),
          tags$br(), tags$br(),
          uiOutput("editor_result_ui")
        )
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  rv <- reactiveValues(code = NULL, explanation = NULL, result = NULL,
                       status = NULL, editor_result = NULL)

  # ── Quick-example buttons ────────────────────────────────────────────────
  observeEvent(input$ex_wiki, {
    updateTextInput(session, "url",
      value = "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)")
    updateTextAreaInput(session, "instruction",
      value = "Extract the country names and population figures from the main table into a data.frame")
    updateSelectInput(session, "method", selected = "static")
  })

  observeEvent(input$ex_hn, {
    updateTextInput(session, "url", value = "https://news.ycombinator.com")
    updateTextAreaInput(session, "instruction",
      value = "Get each story title and its URL from the front page")
    updateSelectInput(session, "method", selected = "static")
  })

  observeEvent(input$ex_quotes, {
    updateTextInput(session, "url", value = "https://quotes.toscrape.com")
    updateTextAreaInput(session, "instruction",
      value = "Extract every quote text and its author name")
    updateSelectInput(session, "method", selected = "static")
  })

  # ── Main scrape action ──────────────────────────────────────────────────
  observeEvent(input$go, {
    req(input$url, input$instruction)
    rv$code <- rv$explanation <- rv$result <- rv$editor_result <- NULL

    # Resolve API key
    key <- if (nzchar(input$api_key)) input$api_key else Sys.getenv("ANTHROPIC_API_KEY")
    if (!nzchar(key)) {
      rv$status <- "Error: No API key. Set ANTHROPIC_API_KEY or enter it above."
      return()
    }

    # Determine method
    method <- input$method
    if (method == "auto") {
      rv$status <- "Detecting page type..."
      method <- detect_page_type(input$url)
      rv$status <- paste0("Detected: ", method, " page")
    }

    llm_method <- if (method == "static") "rvest" else "rselenium"

    # Fetch HTML context for Claude
    rv$status <- "Fetching page source..."
    page_html <- tryCatch(
      if (method == "static") fetch_page_source(input$url) else NULL,
      error = function(e) NULL
    )

    # Selenium session (dynamic only)
    sel <- NULL
    if (method == "dynamic") {
      rv$status <- "Starting headless browser..."
      sel <- tryCatch(start_selenium(), error = function(e) {
        rv$status <- paste("Selenium error:", e$message)
        NULL
      })
      req(sel)
      page_html <- tryCatch(selenium_navigate(sel$remDr, input$url),
                            error = function(e) NULL)
    }

    # Ask Claude
    rv$status <- "Sending instruction to Claude..."
    llm <- tryCatch(
      call_claude(
        url       = input$url,
        instruction = input$instruction,
        page_html = page_html,
        method    = llm_method,
        api_key   = key,
        model     = input$model
      ),
      error = function(e) {
        rv$status <- paste("Claude API error:", e$message)
        if (!is.null(sel)) stop_selenium(sel)
        NULL
      }
    )
    req(llm)

    rv$code        <- llm$code
    rv$explanation <- llm$explanation
    updateTextAreaInput(session, "editor", value = llm$code)

    # Execute
    if (input$execute) {
      rv$status <- "Executing generated code..."
      rv$result <- run_code(llm$code,
                            remDr = if (!is.null(sel)) sel$remDr else NULL,
                            timeout_sec = 30)
    }

    if (!is.null(sel)) stop_selenium(sel)
    rv$status <- "Done"
  })

  # ── Run edited code ────────────────────────────────────────────────────
  observeEvent(input$run_editor, {
    req(nzchar(input$editor))
    rv$editor_result <- run_code(input$editor)
  })

  # ── Outputs ─────────────────────────────────────────────────────────────
  output$status_ui <- renderUI({
    req(rv$status)
    tags$p(tags$em(rv$status), style = "color:#7f8c8d;")
  })

  output$code_out <- renderText(rv$code)

  output$explanation_out <- renderText(rv$explanation)

  output$result_ui <- renderUI({
    req(rv$result)
    render_result(rv$result, "main_table", output)
  })

  output$editor_result_ui <- renderUI({
    req(rv$editor_result)
    render_result(rv$editor_result, "editor_table", output)
  })
}


# ── Helper: render a result (data.frame → DT, error → alert, else str) ───
render_result <- function(res, table_id, output) {
  if (is.list(res) && identical(res$success, FALSE)) {
    return(tags$div(class = "alert alert-danger", res$error))
  }
  if (is.data.frame(res)) {
    output[[table_id]] <- DT::renderDataTable(
      DT::datatable(res, options = list(pageLength = 20, scrollX = TRUE))
    )
    return(DT::dataTableOutput(table_id))
  }
  tags$pre(paste(utils::capture.output(utils::str(res, max.level = 3)),
                 collapse = "\n"))
}


shiny::shinyApp(ui, server)
