# ------------------------------------------------------------
# Crossover function: detects when arr1 crosses above arr2
# Implemented using base R only (no external libraries)
# ------------------------------------------------------------
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  n <- length(arr1)
  if (n == 0) return(character(0))
  
  # Initialize a vector to store the crossover signals
  crossover_signals <- character(n)
  crossover_signals[1] <- "None"
  
  # Check for crossovers at each data point
  if (n > 1) {
    for (i in 2:n) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }
  
  return(crossover_signals)
}