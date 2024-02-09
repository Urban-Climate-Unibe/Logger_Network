
fill_missing_temperatures <- function(x,max_gap) {

#' ToDo description
#'
#' @param x A vector
#' @returns A vector gapfilled
#' @examples



    # Length of the input vector
    n <- length(x)
    i = 2
    # Loop over each element in the vector
    while (i>1&&i<n) {
      # If the element is NA
      if (is.na(x[i])) {
        # Initialize a gap counter
        gap <- 0

        # Check the size of the gap
        for (j in i:n) {
          if (is.na(x[j])) {
            gap <- gap + 1
          } else {
            break
          }
        }

        # If the gap is within the allowed size, impute
        if (gap <= max_gap && i > 1 && j <= n) {
          # Calculate mean of the last known and next known values
          mean_value <- mean(c(x[i-1], x[j]), na.rm = TRUE)

          # Impute missing values with the calculated mean
          x[i:(i+gap-1)] <- mean_value

        }
        # Move the index past the gap
        i <- j
      }else{i = i+1}
    }

    return(x)
  }


