# stock_dashboard_app.R
#
# This Shiny application builds an interactive stock dashboard using
# data from Yahoo Finance via the `yahoofinancer` package and news
# headlines from the NewsAPI via the `newsanchor` package. It displays
# the current price, daily percentage change, a simple suggested action
# (Buy More, Hold, or Sell) based on daily change thresholds, and a
# short list of recent news headlines with clickable links. This app
# demonstrates how to combine real‑time market data and news into a
# single view for educational or personal use. Note that the Yahoo
# Finance API and NewsAPI have usage terms and rate limits; the
# `yahoofinancer` package is intended for personal research purposes【168323328284411†L29-L41】, and
# `newsanchor` requires a free API key from newsapi.org【574323360341797†L8-L16】【574323360341797†L30-L38】.
#
# IMPORTANT: This script is provided for informational purposes only
# and does not constitute financial advice. Always do your own
# research or consult a professional before making investment
# decisions.

## Required packages
##
## Before running this application, install the following packages:
##
## install.packages(c("shiny", "DT"))
## # yahoofinancer is available on CRAN and GitHub
## install.packages("yahoofinancer")
## # newsanchor is available on CRAN
## install.packages("newsanchor")
##
## Optionally, install tidyverse for convenience
## install.packages("tidyverse")

library(shiny)
library(yahoofinancer)
library(newsanchor)
library(DT)
library(dplyr)
library(stringr)

# Set NewsAPI key
Sys.setenv(NEWS_API_KEY = "107eb8816c6948bc93f7fc18eb9bc3c3")

# UI definition
ui <- fluidPage(
  titlePanel("Interactive Stock Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "tickers",
        label   = "Enter Stock Tickers (comma separated):",
        value   = "AAPL, MSFT, GOOG"
      ),
      actionButton("update", "Update"),
      helpText(
        "Example input: AAPL, MSFT, GOOGL. Tickers are not case‑sensitive."
      ),
      helpText(
        "To retrieve news headlines, set your News API key via newsanchor::set_api_key() or \n",
        "Sys.setenv(NEWS_API_KEY = 'your_api_key')."
      )
    ),
    mainPanel(
      DTOutput("stock_table")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Attempt to initialise NewsAPI key from environment
  observe({
    api_key <- Sys.getenv("NEWS_API_KEY")
    if (nzchar(api_key)) {
      try(newsanchor::set_api_key(api_key), silent = TRUE)
    }
  })

  # Helper function to fetch ticker information
  fetch_ticker_info <- function(symbol) {
    # Convert to lower case as required by yahoofinancer
    sym <- tolower(symbol)
    # Create a Ticker object
    tick_obj <- Ticker$new(sym)

    # Retrieve current price and previous close using active bindings
    price <- tryCatch(tick_obj$regular_market_price, error = function(e) NA)
    prev  <- tryCatch(tick_obj$previous_close,        error = function(e) NA)
    # Compute daily percentage change
    daily_change <- NA
    if (!is.na(price) && !is.na(prev) && prev != 0) {
      daily_change <- (price - prev) / prev * 100
    }
    # Format daily change string
    daily_change_str <- if (!is.na(daily_change)) sprintf("%.2f%%", daily_change) else NA
    # Determine suggested action
    suggested <- if (is.na(daily_change)) {
      "Hold"
    } else if (daily_change <= -3) {
      "Buy More"
    } else if (daily_change >= 3) {
      "Sell"
    } else {
      "Hold"
    }
    # Map suggestion to an emoji label
    action_icon <- switch(
      suggested,
      "Buy More" = paste0("🟢 ", suggested),
      "Sell"     = paste0("🔴 ", suggested),
      "Hold"     = paste0("🟡 ", suggested)
    )
    # Fetch news headlines using newsanchor (optional)
    news_html <- NA
    # Attempt to query the News API; requires API key
    try({
      news_data <- get_headlines(query = symbol, page_size = 3)
      if (is.data.frame(news_data)) {
        # Combine top 3 titles with hyperlinks
        news_html <- news_data$articles |> 
          head(3) |> 
          mutate(link = paste0("<a href='", url, "' target='_blank'>", title, "</a>")) |>
          pull(link) |>
          paste(collapse = "<br/>")
      }
    }, silent = TRUE)
    # Return data frame row
    data.frame(
      Symbol        = toupper(symbol),
      Price         = if (!is.na(price)) price else NA,
      DailyChange   = daily_change_str,
      Action        = action_icon,
      News          = if (!is.na(news_html)) news_html else "",
      stringsAsFactors = FALSE
    )
  }

  # Reactive expression triggered by the update button
  stock_data <- eventReactive(input$update, {
    # Split input string into ticker symbols
    symbols <- str_split(input$tickers, ",")[[1]] |> str_trim() |> toupper()
    # Fetch info for each symbol and combine
    infos <- lapply(symbols, fetch_ticker_info)
    do.call(rbind, infos)
  }, ignoreNULL = FALSE)

  # Render data table
  output$stock_table <- renderDT({
    dat <- stock_data()
    # Use datatable with HTML escaping disabled for news links and action icons
    datatable(
      dat,
      escape = FALSE,
      options = list(
        pageLength = 10,
        rowCallback = JS(
          "function(row, data) {",
          "  // Colour the DailyChange column based on positive or negative sign",
          "  if (data[2].indexOf('-') !== -1) {",
          "    $('td:eq(2)', row).css('color', 'red');",
          "  } else {",
          "    $('td:eq(2)', row).css('color', 'green');",
          "  }",
          "}"
        )
      ),
      rownames = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)