# ------------------------------------------------------------
# Simple Moving Average (SMA)
# Implemented using base R only (no external libraries)
# ------------------------------------------------------------
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize a vector to store the SMA values
  n_windows <- length(data) - period + 1
  sma_values <- numeric(n_windows)
  
  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    # Extract the current window of 'period' data points
    current_window <- data[i:(i + period - 1)]
    # Mean of the window = sum of values / number of values
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}