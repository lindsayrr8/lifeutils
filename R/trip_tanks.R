
#' Calculate Tanks of Gas Needed for a Trip: trip_tanks
#'
#' Computes the number of gallons and tanks of gas required for a trip given the miles to travel.
#' Users can provide the car's mpg and tank size manually, or use a single row from the 'cars' data frame.
#'
#' @param miles Numeric. Total miles of the trip (non-negative). Required.
#' @param mpg Numeric. Optional if 'car_row' is provided. Miles per gallon of the vehicle.
#' @param tank_size Numeric. Optional if 'car_row' is provided. Size of the gas tank in gallons.
#' @param car_row Data frame row. Optional. A single row from the 'cars' data frame containing columns: car_type, mpg, tank_size. Do not mix with manual mpg/tank_size.
#'
#' @return A list with:
#' \describe{
#'   \item{car_type}{Character. Vehicle type if 'car_row' is used. Not returned for manual input.}
#'   \item{gallons_needed}{Numeric. Total gallons of gas. Required.}
#'   \item{tanks_needed}{Numeric. Total tanks of gas. Required.}
#' }
#'
#' @examples
#' # Manual input
#' trip_tanks(miles = 300, mpg = 25, tank_size = 16)
#'
#' # Using a row from the cars data frame
#' trip_tanks(miles = 300, car_row = cars[2, ])
#'
#' @export
trip_tanks <- function(miles, mpg = NULL, tank_size = NULL, car_row = NULL) {
  # Function body...
}


# Calculate how many tanks of gas are needed for a trip
trip_tanks <- function(miles, mpg = NULL, tank_size = NULL, car_row = NULL) {
  # Ensure miles is provided
  if (missing(miles) || !is.numeric(miles) || miles < 0) {
    stop("Please provide 'miles' as a non-negative numeric value for the trip.")
  }
  # Determine source: manual vs car_row (only check mpg/tank_size)
  if (!is.null(car_row) && (!is.null(mpg) || !is.null(tank_size))) {
    stop("Do not provide 'mpg' or 'tank_size' manually when using 'car_row'. Provide only 'miles'.")
  }
  # Use car_row if provided
  output <- list()
  if (!is.null(car_row)) {
    if (!is.data.frame(car_row) || !all(c("car_type", "mpg", "tank_size") %in% colnames(car_row))) {
      stop("car_row must be a single row from 'cars' with columns: car_type, mpg, tank_size.\nExample: trip_tanks(miles = 300, car_row = cars[1, ])")
    }
    if (nrow(car_row) != 1) stop("Please provide only one row from 'cars'.")
    mpg <- car_row$mpg
    tank_size <- car_row$tank_size
    output$car_type <- car_row$car_type
  }
  # Ensure mpg and tank_size are numeric and positive
  if (is.null(mpg) || !is.numeric(mpg) || mpg <= 0) stop("mpg must be a positive numeric value.")
  if (is.null(tank_size) || !is.numeric(tank_size) || tank_size <= 0) stop("tank_size must be a positive numeric value.")
  # Calculate gallons and tanks
  gallons_needed <- miles / mpg
  tanks_needed <- gallons_needed / tank_size
  # Truncate to 2 decimals
  gallons_needed <- trunc(gallons_needed * 100) / 100
  tanks_needed <- trunc(tanks_needed * 100) / 100
  # Add results
  output$gallons_needed <- gallons_needed
  output$tanks_needed <- tanks_needed

  return(output)
}
