# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║  NLP Web Scraper — Shiny App                                            ║
# ║  Describe what you want to scrape in plain English; Claude generates     ║
# ║  and executes the R code (rvest for static, RSelenium for dynamic).     ║
# ╚═══════════════════════════════════════════════════════════════════════════╝
#
# Launch:
#   shiny::runApp("nlp-scraper-app")
#
# Required packages: shiny, bslib, httr, jsonlite, rvest, R.utils, DT
# Optional:          RSelenium (for dynamic / JS-rendered pages)
# Environment:       ANTHROPIC_API_KEY must be set (or entered in the UI)

library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(rvest)
library(R.utils)
library(DT)

source(file.path("R", "claude_api.R"), local = TRUE)
source(file.path("R", "scraper.R"),    local = TRUE)


# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tags$head(tags$style(HTML("
    .app-header { background:#2c3e50; color:#fff; padding:18px 24px;
                  margin:-15px -15px 20px; }
    .app-header h3 { margin:0; }
    .app-header p  { margin:4px 0 0; opacity:.8; }
    .code-block { background:#1e1e1e; color:#d4d4d4; padding:14px;
                  border-radius:6px; max-height:450px; overflow:auto;
                  font-family:'Fira Code','Consolas',monospace;
                  font-size:13px; white-space:pre-wrap; }
    .result-box { border:1px solid #dee2e6; border-radius:6px; padding:15px; }
  "))),

  div(class = "app-header",
    h3("NLP Web Scraper"),
    tags$p("Give a URL + plain-English instruction. Claude writes & runs the R code.")
  ),

  layout_sidebar(
    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebar = sidebar(
      width = 380,

      textInput("url", "Target URL",
                placeholder = "https://example.com/page"),

      textAreaInput("instruction", "What do you want to extract?",
                    rows = 4,
                    placeholder = "e.g. Extract all article titles, dates, and links from the blog listing"),

      selectInput("method", "Scraping method",
                  c("Auto-detect" = "auto",
                    "Static (rvest)" = "static",
                    "Dynamic (RSelenium)" = "dynamic")),

      passwordInput("api_key", "Anthropic API key",
                    placeholder = "Leave blank to use ANTHROPIC_API_KEY env var"),

      selectInput("model", "Claude model",
                  c("claude-sonnet-4-20250514",
                    "claude-haiku-4-5-20251001")),

      checkboxInput("execute", "Execute generated code", value = TRUE),

      actionButton("go", "Scrape",
                   class = "btn-success btn-lg w-100",
                   icon  = icon("wand-magic-sparkles")),

      tags$hr(),

      tags$h6("Quick examples"),
      actionButton("ex_wiki",   "Wikipedia table",   class = "btn-sm btn-outline-secondary w-100 mb-1"),
      actionButton("ex_hn",     "Hacker News links", class = "btn-sm btn-outline-secondary w-100 mb-1"),
      actionButton("ex_quotes", "Quotes to Scrape",  class = "btn-sm btn-outline-secondary w-100")
    ),

    # ── Main area ────────────────────────────────────────────────────────────
    navset_card_tab(
      id = "tabs",

      # Tab 1 — Generated Code
      nav_panel("Generated Code",
        div(class = "code-block", verbatimTextOutput("code_out"))
      ),

      # Tab 2 — Results
      nav_panel("Results",
        uiOutput("result_header"),
        DTOutput("result_table"),
        uiOutput("result_other")
      ),

      # Tab 3 — Explanation
      nav_panel("Explanation",
        verbatimTextOutput("explanation_out")
      ),

      # Tab 4 — Code Editor
      nav_panel("Code Editor",
        tags$p("Tweak the generated code and re-run manually:"),
        textAreaInput("editor", NULL, rows = 15, width = "100%"),
        div(class = "d-flex gap-2 mb-3",
          actionButton("run_editor", "Run", class = "btn-warning", icon = icon("play"))
        ),
        DTOutput("editor_table"),
        uiOutput("editor_other")
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  rv <- reactiveValues(
    code        = NULL,
    explanation = NULL,
    result      = NULL,
    editor_result = NULL
  )

  # ── Quick examples ───────────────────────────────────────────────────────
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

  # ── Main scrape action ─────────────────────────────────────────────────
  observeEvent(input$go, {
    req(input$url, input$instruction)
    rv$code <- rv$explanation <- rv$result <- rv$editor_result <- NULL

    # Resolve API key
    key <- if (nzchar(input$api_key)) input$api_key else Sys.getenv("ANTHROPIC_API_KEY")
    if (!nzchar(key)) {
      showNotification("No API key. Set ANTHROPIC_API_KEY or enter it in the sidebar.",
                       type = "error", duration = 8)
      return()
    }

    withProgress(message = "Working...", value = 0, {

      # 1. Determine method
      method <- input$method
      if (method == "auto") {
        incProgress(0.1, detail = "Detecting page type")
        method <- detect_page_type(input$url)
      }
      llm_method <- if (method == "static") "rvest" else "rselenium"

      # 2. Fetch page source
      incProgress(0.2, detail = "Fetching page source")
      page_html <- NULL
      sel <- NULL

      if (method == "static") {
        page_html <- tryCatch(fetch_page_source(input$url), error = function(e) NULL)
      } else {
        sel <- tryCatch(start_selenium(), error = function(e) {
          showNotification(paste("Selenium error:", e$message), type = "error")
          NULL
        })
        if (!is.null(sel)) {
          page_html <- tryCatch(selenium_navigate(sel$remDr, input$url),
                                error = function(e) NULL)
        }
      }

      # 3. Call Claude
      incProgress(0.3, detail = "Asking Claude to generate code")
      llm <- tryCatch(
        call_claude(
          url         = input$url,
          instruction = input$instruction,
          page_html   = page_html,
          method      = llm_method,
          api_key     = key,
          model       = input$model
        ),
        error = function(e) {
          showNotification(paste("Claude API error:", e$message),
                          type = "error", duration = 10)
          if (!is.null(sel)) stop_selenium(sel)
          NULL
        }
      )

      if (is.null(llm)) return()

      rv$code        <- llm$code
      rv$explanation <- llm$explanation
      updateTextAreaInput(session, "editor", value = llm$code)

      # 4. Execute
      if (input$execute) {
        incProgress(0.3, detail = "Executing generated code")
        rv$result <- run_code(
          llm$code,
          remDr       = if (!is.null(sel)) sel$remDr else NULL,
          timeout_sec = 60
        )
      }

      if (!is.null(sel)) stop_selenium(sel)
      incProgress(0.1, detail = "Done")
    })

    # Auto-switch to Results tab if we executed
    if (input$execute) updateNavsetBar(session, "tabs", selected = "Results")
  })

  # ── Run edited code ──────────────────────────────────────────────────────
  observeEvent(input$run_editor, {
    req(nzchar(input$editor))
    rv$editor_result <- NULL
    withProgress(message = "Running code...", value = 0.5, {
      rv$editor_result <- run_code(input$editor)
    })
  })

  # ── Outputs: Generated Code ─────────────────────────────────────────────
  output$code_out <- renderText(rv$code %||% "Click 'Scrape' to generate code.")

  output$explanation_out <- renderText(rv$explanation %||% "")

  # ── Outputs: Results tab ────────────────────────────────────────────────
  output$result_header <- renderUI({
    res <- rv$result
    if (is.null(res)) return(tags$p(class = "text-muted", "No results yet."))
    if (is_error(res)) return(tags$div(class = "alert alert-danger", res$error))
    if (is.data.frame(res)) {
      tags$div(class = "d-flex justify-content-between align-items-center mb-2",
        tags$span(sprintf("%d rows x %d columns", nrow(res), ncol(res))),
        downloadButton("download_csv", "Download CSV", class = "btn-sm btn-outline-primary")
      )
    }
  })

  output$result_table <- renderDT({
    res <- rv$result
    req(is.data.frame(res))
    datatable(res, options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  })

  output$result_other <- renderUI({
    res <- rv$result
    req(!is.null(res), !is_error(res), !is.data.frame(res))
    tags$pre(class = "result-box",
             paste(utils::capture.output(utils::str(res, max.level = 3)),
                   collapse = "\n"))
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("scraped_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(rv$result, file, row.names = FALSE)
    }
  )

  # ── Outputs: Code Editor tab ───────────────────────────────────────────
  output$editor_table <- renderDT({
    res <- rv$editor_result
    req(is.data.frame(res))
    datatable(res, options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  })

  output$editor_other <- renderUI({
    res <- rv$editor_result
    if (is.null(res)) return(NULL)
    if (is_error(res)) return(tags$div(class = "alert alert-danger", res$error))
    if (!is.data.frame(res)) {
      tags$pre(class = "result-box",
               paste(utils::capture.output(utils::str(res, max.level = 3)),
                     collapse = "\n"))
    }
  })
}


# ── Helpers ──────────────────────────────────────────────────────────────────

is_error <- function(x) {
  is.list(x) && identical(x$success, FALSE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

shiny::shinyApp(ui, server)
