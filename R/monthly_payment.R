
#' Calculate Monthly Payment for a Financed Purchase: monthly_payment
#'
#' Computes the monthly payment for a loan or financed purchase based on the total amount,
#' down payment, interest rate, and loan term. Returns a descriptive string with the monthly payment.
#'
#' @param total Numeric. Total amount of the purchase (must be non-negative).
#' @param term Numeric. Loan term in months or years (must be positive).
#' @param down_payment Numeric. Down payment amount (default 0, must be <= total).
#' @param interest_rate Numeric. Annual interest rate in percent (default 0 for no interest).
#' @param time_unit Character. "m" for months or "y" for years (default "m").
#'
#' @return Character string describing the monthly payment, truncated to 2 decimals.
#'
#' @examples
#' # Loan of $12,000 for 36 months with 5% annual interest and $2,000 down
#' monthly_payment(total = 12000, term = 36, down_payment = 2000, interest_rate = 5)
#'
#' # Loan of $5,000 for 2 years with no interest
#' monthly_payment(total = 5000, term = 2, time_unit = "y")
#'
#' @export
monthly_payment <- function(total, term, down_payment = 0, interest_rate = 0, time_unit = "m") {
  # Function body...
}


# Calculate monthly payment for financed purchase
monthly_payment <- function(total, term, down_payment = 0, interest_rate = 0, time_unit = "m") {
  # Input checks
  if (!is.numeric(total) || length(total) != 1 || total < 0) {
    stop("total must be a single non-negative numeric value.")
  }
  if (!is.numeric(down_payment) || length(down_payment) != 1 || down_payment < 0 || down_payment > total) {
    stop("down_payment must be numeric, non-negative, and <= total.")
  }
  if (!is.numeric(interest_rate) || length(interest_rate) != 1 || interest_rate < 0) {
    stop("interest_rate must be a non-negative numeric value (percent).")
  }
  if (!is.numeric(term) || length(term) != 1 || term <= 0) {
    stop("term must be a positive numeric value.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' (months) or 'y' (years).")
  }
  # Convert term to months if input is in years
  if (time_unit == "y") {
    term <- term * 12
  }
  # Amount financed
  principal <- total - down_payment
  # Convert interest_rate percent to decimal monthly rate
  r <- interest_rate / 100 / 12
  # Calculate monthly payment
  if (r == 0) {
    monthly <- principal / term
  } else {
    monthly <- principal * r / (1 - (1 + r)^(-term))
  }
  # Truncate to 2 decimals
  monthly <- trunc(monthly * 100) / 100
  # Return explanation
  return(paste0("Monthly payment: $", format(monthly, nsmall = 2)))
}
