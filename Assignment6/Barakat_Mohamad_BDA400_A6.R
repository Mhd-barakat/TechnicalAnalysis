# =====================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6 - Technical Analysis using R, Visualization Phase
# Student : Mohamad Barakat
# Script  : Barakat_Mohamad_BDA400_A06.R
# Purpose : Interactive R Shiny portfolio dashboard that fetches stock
#           data from Yahoo Finance, visualizes it (line, candlestick,
#           area), overlays technical indicators (Moving Averages, RSI,
#           MACD) and annotates Buy / Sell / Hold trading signals.
# =====================================================================

# ---------------------------------------------------------------------
# SECTION 1 - DATA COLLECTION AND SETUP
# ---------------------------------------------------------------------

# 1.1 Install (only if missing) and load the required packages
required_packages <- c("shiny", "ggplot2", "quantmod", "dplyr", "patchwork")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)     # web application framework
library(ggplot2)   # grammar-of-graphics plotting
library(quantmod)  # financial data download (also loads xts, zoo, TTR)
library(dplyr)     # data manipulation
library(patchwork) # stacks price, RSI and MACD panels on one chart

# 1.2 Default parameters - data source: Yahoo Finance
stock_symbol <- "AAPL"       # Replace "AAPL" with your desired stock symbol
start_date <- "2023-01-01"   # Replace with your desired start date
end_date <- "2023-07-01"     # Replace with your desired end date

# 1.3 Robust data-fetching function with exception handling
#     - Primary source : Yahoo Finance
#     - Backup source  : Alpha Vantage (only if a free API key is supplied)
#     - Missing values are filled with the last available observation
#     - Returns NULL (instead of crashing the app) if every source fails
fetch_stock_data <- function(symbol, from, to, av_key = "") {
  data <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to,
                                auto.assign = FALSE)),
    error = function(e) NULL)
  
  if (is.null(data) && nzchar(av_key)) {
    data <- tryCatch({
      av <- suppressWarnings(getSymbols(symbol, src = "av", api.key = av_key,
                                        output.size = "full", auto.assign = FALSE))
      av[paste0(from, "/", to)]
    }, error = function(e) NULL)
  }
  
  if (is.null(data) || NROW(data) == 0) return(NULL)
  
  data <- na.omit(na.locf(data))                    # handle missing data
  colnames(data) <- sub(".*\\.", "", colnames(data)) # "AAPL.Close" -> "Close"
  data
}

# 1.4 Fetch historical stock data (quick test outside the app)
stock_data <- fetch_stock_data(stock_symbol, start_date, end_date)
if (!is.null(stock_data)) print(head(stock_data))

# ---------------------------------------------------------------------
# SECTION 2 - VISUALIZING STOCK DATA
# ---------------------------------------------------------------------

# 2.1 Convert daily data to the time frame selected by the user
convert_time_frame <- function(x, time_frame) {
  switch(time_frame,
         "Weekly"  = to.weekly(x, indexAt = "endof", name = NULL),
         "Monthly" = to.monthly(x, indexAt = "endof", name = NULL),
         x)  # "Daily" = data unchanged
}

# 2.2 Build the base price chart (Line, Candlestick or Area)
plot_price <- function(df, chart_type, title) {
  df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
  ylims <- range(c(df$Low, df$High), na.rm = TRUE) * c(0.94, 1.04)
  
  p <- ggplot(df, aes(x = Date))
  
  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close), colour = "#1F4E79", linewidth = 0.8)
    
  } else if (chart_type == "Area") {
    p <- p +
      geom_area(aes(y = Close), fill = "#9DC3E6", alpha = 0.6) +
      geom_line(aes(y = Close), colour = "#1F4E79", linewidth = 0.7)
    
  } else {  # Candlestick
    bar_width <- 0.6 * as.numeric(median(diff(df$Date)))
    if (!is.finite(bar_width)) bar_width <- 0.6
    p <- p +
      geom_segment(aes(xend = Date, y = Low, yend = High), colour = "grey30") +
      geom_rect(aes(xmin = Date - bar_width / 2, xmax = Date + bar_width / 2,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Direction), colour = "grey30", linewidth = 0.2) +
      scale_fill_manual(values = c(Up = "#2E8B57", Down = "#C0392B"),
                        guide = "none")
  }
  
  p +
    coord_cartesian(xlim = range(df$Date), ylim = ylims) +
    labs(title = title, x = NULL, y = "Price (USD)", colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

# ---------------------------------------------------------------------
# SECTION 3 - OVERLAY TECHNICAL INDICATORS
# ---------------------------------------------------------------------

# 3.1 Calculate all indicators and return a tidy data frame
build_chart_data <- function(x, short_n = 20, long_n = 50, rsi_n = 14,
                             macd_fast = 12, macd_slow = 26, macd_sig = 9) {
  df <- data.frame(Date  = as.Date(index(x)),
                   Open  = as.numeric(Op(x)), High  = as.numeric(Hi(x)),
                   Low   = as.numeric(Lo(x)), Close = as.numeric(Cl(x)))
  n <- nrow(df)
  # safe() returns NA instead of an error when there are too few bars
  safe <- function(expr) tryCatch(as.numeric(expr),
                                  error = function(e) rep(NA_real_, n))
  
  df$ShortMA <- safe(SMA(df$Close, n = short_n))
  df$LongMA  <- safe(SMA(df$Close, n = long_n))
  df$RSI     <- safe(RSI(df$Close, n = rsi_n))
  
  macd <- tryCatch(MACD(df$Close, nFast = macd_fast, nSlow = macd_slow,
                        nSig = macd_sig, maType = "EMA"),
                   error = function(e) NULL)
  df$MACD       <- if (is.null(macd)) NA_real_ else as.numeric(macd[, "macd"])
  df$MACDSignal <- if (is.null(macd)) NA_real_ else as.numeric(macd[, "signal"])
  df$MACDHist   <- df$MACD - df$MACDSignal
  df
}

# 3.2 Moving Averages layer (drawn directly on the price chart)
add_ma_layers <- function(p, df, short_n, long_n) {
  short_lab <- paste0("SMA ", short_n)
  long_lab  <- paste0("SMA ", long_n)
  p +
    geom_line(data = df, aes(x = Date, y = ShortMA, colour = short_lab),
              linewidth = 0.9, na.rm = TRUE) +
    geom_line(data = df, aes(x = Date, y = LongMA, colour = long_lab),
              linewidth = 0.9, na.rm = TRUE) +
    scale_colour_manual(values = setNames(c("#F39C12", "#8E44AD"),
                                          c(short_lab, long_lab)))
}

# 3.3 RSI panel (0-100 scale, shown under the price chart)
plot_rsi <- function(df, rsi_low, rsi_high) {
  ggplot(df, aes(x = Date, y = RSI)) +
    annotate("rect", xmin = min(df$Date), xmax = max(df$Date),
             ymin = rsi_low, ymax = rsi_high, fill = "grey92") +
    geom_hline(yintercept = c(rsi_low, rsi_high), linetype = "dashed",
               colour = c("#2E8B57", "#C0392B")) +
    geom_line(colour = "#6C3483", linewidth = 0.8, na.rm = TRUE) +
    coord_cartesian(xlim = range(df$Date), ylim = c(0, 100)) +
    labs(x = NULL, y = "RSI") +
    theme_minimal(base_size = 12)
}

# 3.4 MACD panel (MACD line, signal line and histogram)
plot_macd <- function(df) {
  ggplot(df, aes(x = Date)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_col(aes(y = MACDHist, fill = MACDHist >= 0), na.rm = TRUE,
             show.legend = FALSE) +
    geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.8, na.rm = TRUE) +
    geom_line(aes(y = MACDSignal, colour = "Signal"), linewidth = 0.8,
              na.rm = TRUE) +
    scale_fill_manual(values = c(`TRUE` = "#2E8B57", `FALSE` = "#C0392B")) +
    scale_colour_manual(values = c(MACD = "#1F4E79", Signal = "#E67E22")) +
    coord_cartesian(xlim = range(df$Date)) +
    labs(x = NULL, y = "MACD", colour = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# ---------------------------------------------------------------------
# SECTION 4 - TRADING RULES AND ANNOTATIONS
# ---------------------------------------------------------------------

# 4.1 Basic rule from the instructions (Moving Average comparison):
#     short MA > long MA -> "Buy" ; short MA < long MA -> "Sell" ; else "Hold"
# 4.2 Event-based rules (signal only on the bar where a crossover happens):
#     - MA Crossover             : short MA crosses above / below long MA
#     - RSI                      : RSI leaves oversold (Buy) / overbought (Sell)
#     - MACD Crossover           : MACD crosses above / below its signal line
#     - MA Crossover + RSI Filter: MA crossover confirmed by the RSI level
generate_signals <- function(df, rule, rsi_low = 30, rsi_high = 70) {
  cross_up   <- function(a, b) a > b & dplyr::lag(a) <= dplyr::lag(b)
  cross_down <- function(a, b) a < b & dplyr::lag(a) >= dplyr::lag(b)
  low_line   <- rep(rsi_low,  nrow(df))
  high_line  <- rep(rsi_high, nrow(df))
  
  # Basic state-based rule (as given in the assignment instructions)
  df$Trend <- ifelse(df$ShortMA > df$LongMA, "Buy",
                     ifelse(df$ShortMA < df$LongMA, "Sell", "Hold"))
  df$Trend[is.na(df$Trend)] <- "Hold"
  
  buy <- switch(rule,
                "MA Crossover"              = cross_up(df$ShortMA, df$LongMA),
                "RSI"                       = cross_up(df$RSI, low_line),
                "MACD Crossover"            = cross_up(df$MACD, df$MACDSignal),
                "MA Crossover + RSI Filter" = cross_up(df$ShortMA, df$LongMA) &
                  df$RSI < rsi_high)
  sell <- switch(rule,
                 "MA Crossover"              = cross_down(df$ShortMA, df$LongMA),
                 "RSI"                       = cross_down(df$RSI, high_line),
                 "MACD Crossover"            = cross_down(df$MACD, df$MACDSignal),
                 "MA Crossover + RSI Filter" = cross_down(df$ShortMA, df$LongMA) &
                   df$RSI > rsi_low)
  
  buy[is.na(buy)]   <- FALSE
  sell[is.na(sell)] <- FALSE
  df$Signal <- ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
  df
}

# 4.3 Annotate the bars on the chart with the generated signals
add_signal_annotations <- function(p, df, show_hold = FALSE) {
  buys  <- df[df$Signal == "Buy", ]
  sells <- df[df$Signal == "Sell", ]
  
  if (nrow(buys) > 0) {
    p <- p +
      geom_point(data = buys, aes(x = Date, y = Low * 0.985), shape = 24,
                 size = 3.5, fill = "#27AE60", colour = "black") +
      geom_label(data = buys, aes(x = Date, y = Low * 0.96, label = Signal),
                 fill = "#27AE60", colour = "white", size = 3.2, fontface = "bold")
  }
  if (nrow(sells) > 0) {
    p <- p +
      geom_point(data = sells, aes(x = Date, y = High * 1.015), shape = 25,
                 size = 3.5, fill = "#C0392B", colour = "black") +
      geom_label(data = sells, aes(x = Date, y = High * 1.03, label = Signal),
                 fill = "#C0392B", colour = "white", size = 3.2, fontface = "bold")
  }
  if (show_hold) {
    holds <- df[df$Signal == "Hold", ]
    p <- p + geom_text(data = holds, aes(x = Date, y = Close, label = Signal),
                       vjust = 1.5, colour = "blue", size = 2.3, alpha = 0.6)
  }
  p
}

# ---------------------------------------------------------------------
# SECTION 5 - SHINY USER INTERFACE (UI)
# ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("BDA400 Portfolio Dashboard - Stock Technical Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data"),
      textInput("symbols", "Portfolio symbols (comma-separated):",
                value = "AAPL, MSFT, GOOGL"),
      passwordInput("av_key", "Alpha Vantage API key (optional backup):"),
      actionButton("load", "Fetch Data", icon = icon("download"),
                   class = "btn-primary"),
      hr(),
      selectInput("symbol", "Stock to analyze:", choices = stock_symbol),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      radioButtons("chart_type", "Chart Type:",
                   choices = c("Line", "Candlestick", "Area"),
                   selected = "Candlestick", inline = TRUE),
      hr(),
      h4("2. Technical Indicators"),
      checkboxGroupInput("technical_indicators", "Show / hide overlays:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      sliderInput("short_n", "Short MA period:", min = 5, max = 50, value = 20),
      sliderInput("long_n", "Long MA period:", min = 20, max = 200, value = 50),
      sliderInput("rsi_n", "RSI period:", min = 5, max = 30, value = 14),
      hr(),
      h4("3. Trading Rules"),
      selectInput("rule", "Trading rule:",
                  choices = c("MA Crossover", "RSI", "MACD Crossover",
                              "MA Crossover + RSI Filter")),
      sliderInput("rsi_bounds", "RSI oversold / overbought levels:",
                  min = 10, max = 90, value = c(30, 70)),
      checkboxInput("show_signals", "Annotate Buy / Sell signals", TRUE),
      checkboxInput("show_hold", "Also label 'Hold' bars", FALSE)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Price Chart",
                 plotOutput("stock_chart", height = "780px"),
                 h4("Most Recent Buy / Sell Signals"),
                 tableOutput("signal_table")),
        tabPanel("Portfolio Overview",
                 plotOutput("portfolio_chart", height = "450px"),
                 h4("Performance Summary"),
                 tableOutput("portfolio_table"))
      )
    )
  )
)

# ---------------------------------------------------------------------
# SECTION 6 - SHINY SERVER LOGIC
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 6.1 Fetch data for every portfolio symbol (re-runs on button / dates)
  #     A 5-year look-back is added so indicators are ready on day one.
  portfolio_data <- eventReactive(list(input$load, input$date_range), {
    syms <- toupper(trimws(unlist(strsplit(input$symbols, ","))))
    syms <- unique(syms[nzchar(syms)])
    validate(need(length(syms) > 0, "Please enter at least one stock symbol."),
             need(input$date_range[1] < input$date_range[2],
                  "The start date must be before the end date."))
    from <- as.Date(input$date_range[1]) - 5 * 365
    to   <- as.Date(input$date_range[2]) + 1
    
    data_list <- list()
    withProgress(message = "Fetching data from Yahoo Finance...", value = 0, {
      for (s in syms) {
        incProgress(1 / length(syms), detail = s)
        data_list[[s]] <- fetch_stock_data(s, from, to, input$av_key)
      }
    })
    
    failed <- setdiff(syms, names(data_list))
    if (length(failed) > 0) {
      showNotification(paste("Could not fetch:", paste(failed, collapse = ", ")),
                       type = "warning", duration = 8)
    }
    validate(need(length(data_list) > 0,
                  "No data fetched. Check the symbols or your internet connection."))
    data_list
  }, ignoreNULL = FALSE)
  
  # 6.2 Refresh the "Stock to analyze" list with the symbols that loaded
  observeEvent(portfolio_data(), {
    syms    <- names(portfolio_data())
    current <- isolate(input$symbol)
    updateSelectInput(session, "symbol", choices = syms,
                      selected = if (current %in% syms) current else syms[1])
  })
  
  # 6.3 Time frame + indicators + trading signals + date-range filter
  chart_data <- reactive({
    data_list <- portfolio_data()
    req(input$symbol %in% names(data_list))
    validate(need(input$short_n < input$long_n,
                  "The short MA period must be smaller than the long MA period."))
    
    x  <- convert_time_frame(data_list[[input$symbol]], input$time_frame)
    df <- build_chart_data(x, input$short_n, input$long_n, input$rsi_n)
    df <- generate_signals(df, input$rule, input$rsi_bounds[1], input$rsi_bounds[2])
    
    # Filter data based on the selected date range
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) > 1, "Not enough data in the selected date range."))
    df
  })
  
  # 6.4 Main chart: price + indicator layers (on/off) + signal annotations
  output$stock_chart <- renderPlot({
    df    <- chart_data()
    title <- paste0(input$symbol, " - ", input$time_frame, " ",
                    input$chart_type, " Chart | Rule: ", input$rule)
    
    p <- plot_price(df, input$chart_type, title)
    
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- add_ma_layers(p, df, input$short_n, input$long_n)
    }
    if (input$show_signals) {
      p <- add_signal_annotations(p, df, input$show_hold)
    }
    
    panels  <- list(p)
    heights <- 3
    if ("RSI" %in% input$technical_indicators) {
      panels  <- c(panels, list(plot_rsi(df, input$rsi_bounds[1], input$rsi_bounds[2])))
      heights <- c(heights, 1)
    }
    if ("MACD" %in% input$technical_indicators) {
      panels  <- c(panels, list(plot_macd(df)))
      heights <- c(heights, 1)
    }
    wrap_plots(panels, ncol = 1, heights = heights)
  })
  
  # 6.5 Table of the 10 most recent Buy / Sell signals
  output$signal_table <- renderTable({
    df  <- chart_data()
    sig <- df[df$Signal != "Hold",
              c("Date", "Close", "ShortMA", "LongMA", "RSI", "MACD", "Trend", "Signal")]
    if (nrow(sig) == 0) {
      return(data.frame(Message = "No Buy/Sell signals in this period (all bars = Hold)."))
    }
    sig <- head(sig[order(sig$Date, decreasing = TRUE), ], 10)
    sig$Date <- format(sig$Date, "%Y-%m-%d")
    sig
  }, digits = 2)
  
  # 6.6 Portfolio overview: performance of all symbols rebased to 100
  portfolio_df <- reactive({
    data_list <- portfolio_data()
    period    <- paste0(input$date_range[1], "/", input$date_range[2])
    do.call(rbind, lapply(names(data_list), function(s) {
      x <- data_list[[s]][period]
      if (NROW(x) == 0) return(NULL)
      close <- as.numeric(Cl(x))
      data.frame(Date = as.Date(index(x)), Symbol = s,
                 Close = close, Index = 100 * close / close[1])
    }))
  })
  
  output$portfolio_chart <- renderPlot({
    pdf <- portfolio_df()
    validate(need(!is.null(pdf) && nrow(pdf) > 0, "No portfolio data in this range."))
    equal_weight <- pdf %>%
      group_by(Date) %>%
      summarise(Index = mean(Index), .groups = "drop")
    
    ggplot(pdf, aes(x = Date, y = Index, colour = Symbol)) +
      geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
      geom_line(linewidth = 0.9) +
      geom_line(data = equal_weight, aes(x = Date, y = Index),
                inherit.aes = FALSE, colour = "black", linewidth = 1.3) +
      labs(title = "Portfolio Performance (start = 100)",
           subtitle = "Black line = equal-weight portfolio",
           x = NULL, y = "Rebased value", colour = "Symbol") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  })
  
  output$portfolio_table <- renderTable({
    pdf <- portfolio_df()
    req(pdf)
    pdf %>%
      group_by(Symbol) %>%
      summarise(`Start Price` = dplyr::first(Close),
                `End Price`   = dplyr::last(Close),
                `Return (%)`  = 100 * (dplyr::last(Close) / dplyr::first(Close) - 1),
                `Min Price`   = min(Close),
                `Max Price`   = max(Close),
                .groups = "drop")
  }, digits = 2)
}

# ---------------------------------------------------------------------
# SECTION 7 - RUN THE APPLICATION
# ---------------------------------------------------------------------
shinyApp(ui, server)
