
#' Calculate Compound Interest and Total Amount: comp_interest
#'
#' Computes the compound interest for a principal amount over a specified period of time,
#' with optional compounding frequency, and returns both the interest earned and total amount.
#'
#' @param principal Numeric. The initial amount of money invested or borrowed.
#' @param rate Numeric. The annual interest rate as a decimal (e.g., 0.08 for 8%).
#' @param time Numeric. The duration of the investment or loan (in years by default, or months if `time_unit = "m"`).
#' @param n Numeric. Number of times interest is compounded per year (default is 1).
#' @param time_unit Character. "y" for years (default) or "m" for months.
#' @return A list with two elements:
#'   \item{interest}{The calculated compound interest, truncated to 2 decimals.}
#'   \item{total}{The total amount after compound interest, truncated to 2 decimals.}
#' @examples
#' comp_interest(principal = 1000, rate = 0.05, time = 2)
#' comp_interest(principal = 5000, rate = 0.08, time = 18, time_unit = "m", n = 4)
#' @export
comp_interest <- function(principal, rate, time, n = 1, time_unit = "y") {
  # function body ...
}


# Calculate compound interest and total amount
comp_interest <- function(principal, rate, time, n = 1, time_unit = "y") {
  # Input checks
  if (!is.numeric(principal) || !is.numeric(rate) || !is.numeric(time) || !is.numeric(n)) {
    stop("Principal, rate, time, and n must all be numeric.")
  }
  if (principal < 0 || rate < 0 || time < 0 || n <= 0) {
    stop("Principal, rate, time must be non-negative and n must be positive.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' for months or 'y' for years.")
  }
  # Convert time to years if input is in months
  if (time_unit == "m") {
    time <- time / 12
  }
  # Calculate total amount after compound interest
  total <- principal * (1 + rate / n)^(n * time)
  # Calculate compound interest
  interest <- total - principal
  # Truncate to 2 decimal places
  total <- trunc(total * 100) / 100
  interest <- trunc(interest * 100) / 100
  # Return as a list
  return(list(
    interest = interest,
    total = total
  ))
}
