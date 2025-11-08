

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








#' Calculate Travel Time in Days and Hours: travel_time
#'
#' Estimates the duration of a trip given the distance and average speed,
#' returning the time as full days and remaining hours.
#'
#' @param distance Numeric. Total distance of the trip (must be positive).
#' @param speed Numeric. Average speed during travel (must be positive).
#'
#' @return A character string describing the travel time in days and hours.
#'
#' @examples
#' # Example usage
#' travel_time(distance = 300, speed = 60)
#' travel_time(distance = 1500, speed = 50)
#'
#' @export
travel_time <- function(distance, speed) {
  # Function body...
}


# Calculate travel time in days and hours
travel_time <- function(distance, speed) {
  # Input validation
  if (!is.numeric(distance) || !is.numeric(speed)) {
    stop("Both distance and speed must be numeric.")
  }
  if (distance <= 0 || speed <= 0) {
    stop("Both distance and speed must be positive values.")
  }
  # Calculate total time in hours
  total_hours <- distance / speed
  # Convert to days and remaining hours
  days <- floor(total_hours / 24)
  hours <- total_hours %% 24
  # Truncate hours to 2 decimal places (not round)
  hours <- trunc(hours * 100) / 100
  # Return a formatted string
  message <- paste(days, "day(s) and", hours, "hour(s)")
  return(message)
}








#' Calculate Days Until a Given Date: days_until
#'
#' Returns the number of days from today until the specified future date.
#' If the date has already passed, a message is displayed.
#'
#' @param year Numeric. Four-digit year of the target date.
#' @param month Numeric. Month of the target date (1–12).
#' @param day Numeric. Day of the target date (1–31 as appropriate for the month).
#'
#' @return Numeric. The number of days until the specified date. Negative if the date has passed.
#'
#' @examples
#' # Days until Christmas 2025
#' days_until(2025, 12, 25)
#'
#' # Days until New Year 2026
#' days_until(2026, 1, 1)
#'
#' @export
days_until <- function(year, month, day) {
  # Function body...
}


# Calculate days until a given date
days_until <- function(year, month, day) {
  # Input validation - must be set up this way else R will treat input as a calculation by default
  if (missing(year) || missing(month) || missing(day)) {
    stop("Please enter the date as: days_until(year, month, day)\nExample: days_until(2025, 12, 25)")
  }
  # Ensure all inputs are numeric
  if (!is.numeric(year) || !is.numeric(month) || !is.numeric(day)) {
    stop("Year, month, and day must all be numeric.\nExample: days_until(2025, 12, 25)")
  }
  # Create date
  date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  today <- Sys.Date()
  diff <- as.numeric(date - today)
  # If date has passed, print message
  if (diff < 0) {
    message("That date has already passed.")
  }
  return(diff)
}








#' Calculate Days Since a Given Date: days_since
#'
#' Returns the number of days from a specified past date until today.
#' If the date is in the future, a message is displayed.
#'
#' @param year Numeric. Four-digit year of the target date.
#' @param month Numeric. Month of the target date (1–12).
#' @param day Numeric. Day of the target date (1–31 as appropriate for the month).
#'
#' @return Numeric. The number of days since the specified date. Negative if the date is in the future.
#'
#' @examples
#' # Days since May 15, 2020
#' days_since(2020, 5, 15)
#'
#' # Days since January 1, 2023
#' days_since(2023, 1, 1)
#'
#' @export
days_since <- function(year, month, day) {
  # Function body...
}


# Calculate days since a given date
days_since <- function(year, month, day) {
  # Input validation - must be set up this way else R will treat input as a calculation by default
  if (missing(year) || missing(month) || missing(day)) {
    stop("Please enter the date as: days_since(year, month, day)\nExample: days_since(2020, 5, 15)")
  }
  # Ensure all inputs are numeric
  if (!is.numeric(year) || !is.numeric(month) || !is.numeric(day)) {
    stop("Year, month, and day must all be numeric.\nExample: days_since(2020, 5, 15)")
  }
  # Create date
  date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  today <- Sys.Date()
  diff <- as.numeric(today - date)
  # If date is in the future, print message
  if (diff < 0) {
    message("That date has not yet occurred.")
  }
  return(diff)
}



