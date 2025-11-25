
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
