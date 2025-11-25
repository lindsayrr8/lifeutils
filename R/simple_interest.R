
#' Calculate Simple Interest and Total Amount: simple_interest
#'
#' This function computes the simple interest for a principal amount over a period of time,
#' given an annual interest rate. It also returns the total amount after the interest term.
#'
#' @param principal Numeric. The initial amount of money invested or borrowed.
#' @param rate Numeric. The annual interest rate (as a decimal, e.g., 0.08 for 8%).
#' @param time Numeric. The length of the interest period (in years by default or months if `time_unit = "m"`).
#' @param time_unit Character. Either "y" for years (default) or "m" for months.
#' @return A list with two elements:
#'   \item{interest}{The calculated simple interest, truncated to 2 decimals.}
#'   \item{total}{The total amount after interest, truncated to 2 decimals.}
#' @examples
#' simple_interest(principal = 1000, rate = 0.05, time = 2)
#' simple_interest(principal = 5000, rate = 0.08, time = 18, time_unit = "m")
#' @export
simple_interest <- function(principal, rate, time, time_unit = "y") {
  # function body ...
}


# Calculate simple interest and total amount
simple_interest <- function(principal, rate, time, time_unit = "y") {
  # Input checks
  if (!is.numeric(principal) || !is.numeric(rate) || !is.numeric(time)) {
    stop("Principal, rate, and time must all be numeric.")
  }
  if (principal < 0 || rate < 0 || time < 0) {
    stop("Principal, rate, and time must be non-negative.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' for months or 'y' for years.")
  }
  # Convert time to years if input is in months
  if (time_unit == "m") {
    time <- time / 12
  }
  # Calculate interest
  interest <- principal * rate * time
  # Total amount after interest
  total <- principal + interest
  # Truncate to 2 decimals
  interest <- trunc(interest * 100) / 100
  total <- trunc(total * 100) / 100
  # Return as a list
  return(list(
    interest = interest,
    total = total
  ))
}
