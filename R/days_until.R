
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
