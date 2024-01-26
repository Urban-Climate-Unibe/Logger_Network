fill_missing_temperatures <- function(temperature) {
  n <- length(temperature)
  is_na <- is.na(temperature)

  # Function to fill in the missing values
  fill_na <- function(i) {
    if (i == 1 || i == n) {
      return(NA)  # Edge cases where calculation is not possible
    }
    return(mean(c(temperature[i-1], temperature[i+1]), na.rm = TRUE))
  }

  for (i in 2:(n-1)) {
    if (is_na[i]) {
      # Check for consecutive NAs
      if (!is_na[i-1] && !is_na[i+1]) {
        temperature[i] <- fill_na(i)
      } else if (!is_na[i-1] && is_na[i+1] && i < n-1 && !is_na[i+2]) {
        temperature[i] <- fill_na(i)
        temperature[i+1] <- fill_na(i+1)
        i <- i + 1  # Skip the next index as it has been already handled
      }
    }
  }

  return(temperature)
}
