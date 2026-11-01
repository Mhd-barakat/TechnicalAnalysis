# ------------------------------------------------------------
# Exponential Moving Average (EMA)
# Implemented using base R only (no external libraries)
# ------------------------------------------------------------
ema <- function(data, period) {
  # Calculate the multiplier
  multiplier <- 2 / (period + 1)
  
  # Initialize the EMA vector
  ema_values <- numeric(length(data))
  
  # Loop through the data
  for (i in 1:length(data)) {
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}