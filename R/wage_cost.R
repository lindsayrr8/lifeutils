
#' Calculate Hours Needed to Work for a Purchase: wage_cost
#'
#' Computes the number of hours a person must work at a given hourly wage
#' to afford a specified purchase amount. The result is truncated to two decimals
#' and returned as a descriptive string.
#'
#' @param purchase Numeric. Total cost of the purchase (non-negative). Required.
#' @param wage Numeric. Hourly wage of the worker (positive). Required.
#'
#' @return A character string describing the number of hours needed to work.
#'
#' @examples
#' # Calculate hours to work for a $50 purchase at $15/hour
#' wage_cost(purchase = 50, wage = 15)
#'
#' @export
wage_cost <- function(purchase, wage) {
  # Function body...
}


# Calculate hours needed to work to afford a purchase
wage_cost <- function(purchase, wage) {
  # Input checks
  if (!is.numeric(purchase) || length(purchase) != 1 || purchase < 0) {
    stop("purchase must be a single non-negative numeric value.")
  }
  if (!is.numeric(wage) || length(wage) != 1 || wage <= 0) {
    stop("hourly wage must be a single positive numeric value.")
  }
  # Calculate hours needed
  hours <- purchase / wage
  # Truncate to 2 decimals
  hours <- trunc(hours * 100) / 100
  # Return descriptive string
  return(paste0("Hours to work: ", format(hours, nsmall = 2)))
}
