
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
