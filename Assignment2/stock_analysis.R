# ==========================================================
# BDA400 - Assignment 2: Technical Analysis using R (Stage 1)
# Author: Mohamad Barakat
# ==========================================================

# ---------- 1. Install the required packages (run once) ----------
install.packages(c("quantmod", "TTR", "xts", "zoo"))

# ---------- 2. Load the packages ----------
library(quantmod)   # also loads xts, zoo and TTR
library(TTR)

# ---------- 3. Verify the installed versions ----------
packageVersion("quantmod")
packageVersion("TTR")

# ---------- Function 1: Import and load stock data ----------
load_stock_data <- function(file_path = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  # Read the symbols: one per line, remove spaces and blank lines
  symbols <- toupper(trimws(readLines(file_path, warn = FALSE)))
  symbols <- symbols[symbols != ""]
  
  stock_list <- list()
  for (sym in symbols) {
    tryCatch({
      xts_data <- getSymbols(sym, src = "yahoo", from = from, to = to,
                             auto.assign = FALSE)
      df <- data.frame(Date = index(xts_data), coredata(xts_data))
      colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      df <- df[!is.na(df$Close), ]   # remove missing prices
      stock_list[[sym]] <- df
      message("Loaded ", sym, ": ", nrow(df), " rows")
    }, error = function(e) {
      message("Could not load ", sym, ": ", conditionMessage(e))
    })
  }
  return(stock_list)
}

# Import the data for all symbols in portfolio.txt
stocks <- load_stock_data("portfolio.txt")

# Store each stock as a separate data frame (AAPL, MSFT, GOOGL, ...)
list2env(stocks, envir = .GlobalEnv)

# Check the result
names(stocks)
class(AAPL)
str(AAPL)

# ---------- Helper: statistical mode ----------
get_mode <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)   # prices are decimals, so round them first
  if (length(x) == 0) return(NA)
  freq <- table(x)                   # count how often each price occurs
  as.numeric(names(freq)[which.max(freq)])
}

# ---------- Function 2: Compute basic statistics ----------
calculate_statistics <- function(stock_df, ma_period = 20) {
  stock_df <- stock_df[!is.na(stock_df$Close), ]
  close <- stock_df$Close
  if (length(close) < ma_period) {
    stop("Not enough data to calculate the moving average")
  }
  
  # Moving average (Simple Moving Average from the TTR package)
  ma <- as.numeric(SMA(close, n = ma_period))
  ma_df <- data.frame(Date = stock_df$Date, Close = close, SMA = ma)
  
  # Summary statistics of the closing price
  summary_df <- data.frame(
    Mean       = round(mean(close), 2),
    Median     = round(median(close), 2),
    Mode       = get_mode(close),
    SD         = round(sd(close), 2),
    Latest_SMA = round(tail(ma, 1), 2)
  )
  
  # Structured result: a list with two data frames
  return(list(summary = summary_df, moving_average = ma_df))
}

# Apply calculate_statistics() to every stock (result: a named list)
all_stats <- lapply(stocks, calculate_statistics)

# Combine the summaries into one data frame (one row per stock)
stats_table <- do.call(rbind, lapply(names(all_stats), function(sym) {
  cbind(Symbol = sym, all_stats[[sym]]$summary)
}))
rownames(stats_table) <- NULL

# Check the result
stats_table
str(all_stats$AAPL)
tail(all_stats$AAPL$moving_average, 5)



# ---------- Function 3: Display the imported data ----------
display_stock_data <- function(stock_list, n = 6) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    cat("\n==========", sym, "==========\n")
    cat("Trading days:", nrow(df),
        "| From", format(min(df$Date)), "to", format(max(df$Date)), "\n")
    print(head(df, n), row.names = FALSE)
  }
}

# Display the data of all stocks
display_stock_data(stocks)

# Open one stock as a full table
View(AAPL)


# ---------- Function 4: Display the calculated statistics ----------
display_statistics <- function(stats_list) {
  for (sym in names(stats_list)) {
    cat("\n----- Statistics for", sym, "(closing price) -----\n")
    print(stats_list[[sym]]$summary, row.names = FALSE)
  }
}

# Display the statistics of every stock
display_statistics(all_stats)

# Display the comparison table of all stocks
print(stats_table, row.names = FALSE)
View(stats_table)


# ---------- Function 5: Plot closing price and moving average ----------
plot_moving_average <- function(stats_list, symbol) {
  ma_df <- stats_list[[symbol]]$moving_average
  plot(ma_df$Date, ma_df$Close, type = "l", col = "steelblue", lwd = 2,
       main = paste(symbol, "- Closing Price and 20-Day SMA"),
       xlab = "Date", ylab = "Price (USD)")
  lines(ma_df$Date, ma_df$SMA, col = "red", lwd = 2)
  legend("topleft", legend = c("Close", "SMA 20"),
         col = c("steelblue", "red"), lwd = 2)
}

# Plot one stock (change the symbol to plot another stock)
plot_moving_average(all_stats, "AAPL")

