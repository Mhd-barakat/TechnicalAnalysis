# ------------------------------------------------------------
# Stochastic RSI (StochRSI)
# Depends on: rsi() from rsi.R and sma() from sma.R
# ------------------------------------------------------------

# Load rsi() and sma() if they are not already available
if (!exists("rsi", mode = "function") && file.exists("rsi.R")) source("rsi.R")
if (!exists("sma", mode = "function") && file.exists("sma.R")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Calculate the StochRSI (normalize RSI between 0 and 1)
  if (all(is.na(rsi_values))) {
    # No valid RSI values (not enough data): keep NA values
    k_values <- rsi_values
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
    
    if (max_rsi == min_rsi) {
      # Avoid division by zero when all RSI values are equal
      k_values <- ifelse(is.na(rsi_values), NA, 0)
    } else {
      k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
    }
  }
  
  # Calculate the %K line (StochRSI smoothed with SMA)
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}