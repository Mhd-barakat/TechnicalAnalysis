# ------------------------------------------------------------
# Relative Strength Index (RSI) - Wilder's smoothing method
# Implemented using base R only (no external libraries)
# ------------------------------------------------------------
rsi <- function(data, period) {
  n <- length(data)
  
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)
  
  # Not enough data points: return a vector of NA values
  if (n <= period) {
    warning("Not enough data points to calculate RSI for this period")
    return(rsi_values)
  }
  
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)
  
  # Initialize two vectors to store the gains and losses (filled with zeros)
  gains  <- rep(0, length(diff_values))
  losses <- rep(0, length(diff_values))
  
  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate the average gains and average losses for the first 'period' points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    
    rs <- avg_gain / avg_loss          # if avg_loss = 0, rs = Inf and RSI = 100
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}