#' NLP Web Scraper - Shiny App
#'
#' Interactive Shiny application for NLP-powered web scraping.
#' Enter a URL and natural language instructions, and the app uses an LLM
#' to generate and execute R scraping code.
#'
#' Launch with: shiny::runApp("app.R")

library(shiny)
library(rpolecat)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .header { background: #2c3e50; color: white; padding: 20px; margin: -15px -15px 20px; }
      .header h2 { margin: 0; }
      .header p { margin: 5px 0 0; opacity: 0.8; }
      .code-output { background: #1e1e1e; color: #d4d4d4; padding: 15px;
                     border-radius: 5px; font-family: 'Fira Code', monospace;
                     font-size: 13px; white-space: pre-wrap; overflow-x: auto;
                     max-height: 400px; overflow-y: auto; }
      .result-box { background: #f8f9fa; border: 1px solid #dee2e6;
                    border-radius: 5px; padding: 15px; }
      .btn-scrape { background: #27ae60; border: none; font-size: 16px;
                    padding: 10px 30px; }
      .btn-scrape:hover { background: #219a52; }
      .status-msg { font-style: italic; color: #7f8c8d; }
    "))
  ),

  div(class = "header",
    h2("NLP Web Scraper"),
    p("Describe what you want to scrape in plain English")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      textInput("url", "Target URL",
                placeholder = "https://example.com/page-to-scrape"),

      textAreaInput("instruction", "What do you want to extract?",
                    placeholder = "e.g., Extract all article titles and their publication dates from the main content area",
                    rows = 4),

      selectInput("method", "Scraping Method",
                  choices = c("Auto-detect" = "auto",
                              "Static (rvest)" = "static",
                              "Dynamic (RSelenium)" = "dynamic")),

      selectInput("provider", "LLM Provider",
                  choices = c("Anthropic (Claude)" = "anthropic",
                              "OpenAI (GPT)" = "openai")),

      passwordInput("api_key", "API Key (or set env var)",
                    placeholder = "Leave blank to use environment variable"),

      checkboxInput("execute", "Execute generated code", value = TRUE),

      actionButton("scrape_btn", "Scrape", class = "btn-primary btn-scrape btn-block"),

      hr(),

      h4("Quick Examples"),
      actionButton("ex1", "Wikipedia table", class = "btn-sm btn-outline-secondary btn-block"),
      actionButton("ex2", "News headlines", class = "btn-sm btn-outline-secondary btn-block"),
      actionButton("ex3", "Links from page", class = "btn-sm btn-outline-secondary btn-block")
    ),

    mainPanel(
      width = 8,

      uiOutput("status"),

      tabsetPanel(
        id = "results_tabs",

        tabPanel("Generated Code",
          br(),
          div(class = "code-output",
            verbatimTextOutput("generated_code")
          )
        ),

        tabPanel("Results",
          br(),
          div(class = "result-box",
            uiOutput("result_display")
          )
        ),

        tabPanel("Explanation",
          br(),
          div(class = "result-box",
            verbatimTextOutput("explanation")
          )
        ),

        tabPanel("Code Editor",
          br(),
          p("Edit the generated code and re-run it manually:"),
          textAreaInput("code_editor", NULL, rows = 15, width = "100%"),
          actionButton("run_edited", "Run Edited Code", class = "btn-warning"),
          br(), br(),
          div(class = "result-box",
            uiOutput("edited_result_display")
          )
        )
      )
    )
  )
)


# --- Server ---
server <- function(input, output, session) {

  # Reactive values to store results
  rv <- reactiveValues(
    scrape_result = NULL,
    status = NULL,
    edited_result = NULL
  )

  # Quick example buttons
  observeEvent(input$ex1, {
    updateTextInput(session, "url",
                    value = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)")
    updateTextAreaInput(session, "instruction",
                        value = "Extract the country names and GDP values from the main ranking table")
    updateSelectInput(session, "method", selected = "static")
  })

  observeEvent(input$ex2, {
    updateTextInput(session, "url",
                    value = "https://news.ycombinator.com")
    updateTextAreaInput(session, "instruction",
                        value = "Extract all story titles and their URLs from the front page")
    updateSelectInput(session, "method", selected = "static")
  })

  observeEvent(input$ex3, {
    updateTextInput(session, "url",
                    value = "https://example.com")
    updateTextAreaInput(session, "instruction",
                        value = "Extract all hyperlinks with their text and href attributes")
    updateSelectInput(session, "method", selected = "static")
  })

  # Main scrape action
  observeEvent(input$scrape_btn, {
    req(input$url, input$instruction)

    rv$status <- "Generating scraping code..."
    rv$scrape_result <- NULL

    # Build LLM config
    api_key_val <- if (nzchar(input$api_key)) input$api_key else NULL
    config <- tryCatch(
      llm_config(provider = input$provider, api_key = api_key_val),
      error = function(e) {
        rv$status <- paste("Config error:", e$message)
        NULL
      }
    )
    req(config)

    # Run the scraper
    rv$status <- "Calling LLM API and scraping..."

    result <- tryCatch(
      nlp_scrape(
        url = input$url,
        instruction = input$instruction,
        method = input$method,
        execute = input$execute,
        config = config,
        verbose = FALSE
      ),
      error = function(e) {
        rv$status <- paste("Error:", e$message)
        NULL
      }
    )

    if (!is.null(result)) {
      rv$scrape_result <- result
      rv$status <- "Done!"

      # Populate code editor
      updateTextAreaInput(session, "code_editor", value = result$code)
    }
  })

  # Run edited code
  observeEvent(input$run_edited, {
    req(nzchar(input$code_editor))
    rv$edited_result <- tryCatch(
      execute_scraping_code(input$code_editor),
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )
  })

  # --- Outputs ---

  output$status <- renderUI({
    req(rv$status)
    div(class = "status-msg", p(rv$status))
  })

  output$generated_code <- renderText({
    req(rv$scrape_result)
    rv$scrape_result$code
  })

  output$explanation <- renderText({
    req(rv$scrape_result)
    rv$scrape_result$explanation
  })

  output$result_display <- renderUI({
    req(rv$scrape_result, rv$scrape_result$result)
    result <- rv$scrape_result$result

    if (is.list(result) && !is.null(result$success) && !result$success) {
      div(class = "alert alert-danger", result$error)
    } else if (is.data.frame(result)) {
      output$result_table <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 25, scrollX = TRUE))
      })
      DT::dataTableOutput("result_table")
    } else {
      pre(paste(utils::capture.output(utils::str(result, max.level = 3)),
                collapse = "\n"))
    }
  })

  output$edited_result_display <- renderUI({
    req(rv$edited_result)
    result <- rv$edited_result

    if (is.list(result) && !is.null(result$success) && !result$success) {
      div(class = "alert alert-danger", result$error)
    } else if (is.data.frame(result)) {
      output$edited_table <- DT::renderDataTable({
        DT::datatable(result, options = list(pageLength = 25, scrollX = TRUE))
      })
      DT::dataTableOutput("edited_table")
    } else {
      pre(paste(utils::capture.output(utils::str(result, max.level = 3)),
                collapse = "\n"))
    }
  })
}

shiny::shinyApp(ui = ui, server = server)
