# ------------------------------------------------------------
# Test script for all technical indicators - BDA400 Assignment 5
# Usage: set the working directory to the Assignment5 folder,
#        then run: source("test_indicators.R")
# ------------------------------------------------------------
rm(list = ls())

# Load all indicator functions (order matters for dependencies)
files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
           "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (f in files) source(f)

data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
data2 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

cat("\n--- SMA (period = 3) ---\n")
print(sma(data1, period = 3))
# Expected: 12.33 15.67 17.67 20.00 21.67 23.67 23.33

cat("\n--- EMA (period = 3) ---\n")
print(ema(data1, period = 3))
# Expected: 10 11 13 16.5 17.25 19.625 22.3125 23.15625 22.078125

cat("\n--- MACD (3, 5, 2) ---\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130),
           short_period = 3, long_period = 5, signal_period = 2))

cat("\n--- Standard Deviation ---\n")
print(stdev(data1))
# Expected: about 4.9466

cat("\n--- Linear Regression (length = 5, offset = 0) ---\n")
print(linreg(data1, regressionLength = 5, regressionOffset = 0))
# Expected: slope about 0.7429, intercept about 19.0667

cat("\n--- RSI (period = 5) ---\n")
print(rsi(data2, period = 5))
# Expected: NA x5, then about 50.53 68.93 71.84 78.21 66.86

cat("\n--- Stochastic RSI (period = 5, k = 3, d = 3) ---\n")
print(stoch_rsi(data2, period = 5, k_period = 3, d_period = 3))

cat("\n--- Stochastic RSI (assignment example: period = 14) ---\n")
print(suppressWarnings(stoch_rsi(data2, period = 14, k_period = 3, d_period = 3)))
# All NA: 10 data points are not enough for a 14-period RSI

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

cat("\n--- Crossover ---\n")
print(crossover(arr1, arr2))
# Expected: "Up" at position 4, "None" elsewhere

cat("\n--- Crossunder ---\n")
print(crossunder(arr1, arr2))
print(crossunder(arr2, arr1))
# Expected (second call): "True" at position 4

cat("\n--- Error handling tests ---\n")
print(tryCatch(sma(c(1, 2), 5), error = function(e) conditionMessage(e)))
print(tryCatch(crossover(1:3, 1:4), error = function(e) conditionMessage(e)))