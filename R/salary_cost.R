
#' Calculate Purchase as a Percentage of Salary: salary_cost
#'
#' Determines what percentage a specific purchase represents of an annual salary.
#' The result is truncated to two decimal places and returned as a descriptive string.
#'
#' @param purchase Numeric. Total cost of the purchase (non-negative). Required.
#' @param salary Numeric. Annual salary (positive). Required.
#'
#' @return A character string describing the percentage of the salary represented by the purchase.
#'
#' @examples
#' # Determine percentage of a $5,000 purchase for a $50,000 salary
#' salary_cost(purchase = 5000, salary = 50000)
#'
#' @export
salary_cost <- function(purchase, salary) {
  # Function body...
}


# Calculate what percent of a salary a purchase represents
salary_cost <- function(purchase, salary) {
  # Input checks
  if (!is.numeric(purchase) || length(purchase) != 1 || purchase < 0) {
    stop("purchase must be a single non-negative numeric value.")
  }
  if (!is.numeric(salary) || length(salary) != 1 || salary <= 0) {
    stop("salary must be a single positive numeric value.")
  }
  # Calculate percentage
  percent <- (purchase / salary) * 100
  # Truncate to 2 decimals
  percent <- trunc(percent * 100) / 100
  # Return descriptive string
  return(paste0("This purchase is ", format(percent, nsmall = 2), "% of your annual salary."))
}
