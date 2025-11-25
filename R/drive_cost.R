
#' Calculate Total Fuel Cost for a Road Trip: drive_cost
#'
#' Computes the total gallons of fuel required and the total cost for a trip given the distance,
#' the vehicle's fuel efficiency (mpg), and the current gas price per gallon.
#'
#' @param distance Numeric. Total distance of the trip (must be positive).
#' @param mpg Numeric. Vehicle fuel efficiency in miles per gallon (must be positive).
#' @param gas_price Numeric. Price of gas per gallon (must be positive).
#'
#' @return A list with:
#' \describe{
#'   \item{total_gallons}{Numeric. Total gallons of fuel required for the trip.}
#'   \item{total_cost}{Numeric. Total cost of fuel for the trip.}
#' }
#'
#' @examples
#' # Example usage
#' drive_cost(distance = 300, mpg = 25, gas_price = 3.50)
#'
#' @export
drive_cost <- function(distance, mpg, gas_price) {
  # Function body...
}


# Calculate the total fuel cost for a road trip
drive_cost <- function(distance, mpg, gas_price) {
  # Input validation
  if (!is.numeric(distance) || !is.numeric(mpg) || !is.numeric(gas_price)) {
    stop("All inputs (distance, mpg, gas_price) must be numeric.")
  }
  if (distance <= 0 || mpg <= 0 || gas_price <= 0) {
    stop("All inputs must be positive numbers.")
  }
  # Calculate total gallons needed
  total_gallons <- distance / mpg
  # Calculate total cost
  total_cost <- total_gallons * gas_price
  # Truncate to 2 decimals for money format
  total_gallons <- trunc(total_gallons * 100) / 100
  total_cost <- trunc(total_cost * 100) / 100
  # Return as labeled list
  return(list(
    total_gallons = total_gallons,
    total_cost = total_cost
  ))
}
