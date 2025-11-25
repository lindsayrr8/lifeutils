
#' Calculate Savings Goal: savings_goal
#'
#' Determines how much a user needs to save per day, month, or year
#' to reach a target savings goal, optionally accounting for an existing balance.
#'
#' @param goal Numeric. The target savings amount.
#' @param have Numeric. Optional. Amount already saved (default 0).
#' @param time Numeric. Duration to reach the goal.
#' @param time_unit Character. Time unit of the `time` argument.
#'   Can be "d" for days, "m" for months, or "y" for years (default "y").
#'
#' @return A list with three elements:
#'   \item{daily}{Amount to save per day.}
#'   \item{monthly}{Amount to save per month.}
#'   \item{yearly}{Amount to save per year.}
#'   If the goal is already met or exceeded, all values will be 0 and a message is included.
#'
#' @examples
#' # Save $1000 over 6 months, starting from $200
#' savings_goal(goal = 1000, have = 200, time = 6, time_unit = "m")
#'
#' # Save $5000 over 2 years, starting from $0
#' savings_goal(goal = 5000, time = 2, time_unit = "y")
#'
#' @export
savings_goal <- function(goal, have = 0, time, time_unit = "y") {
  # function body ...
}


# Calculate savings goal
savings_goal <- function(goal, have = 0, time, time_unit = "y") {
  # Input checks
  if (!is.numeric(goal) || !is.numeric(have) || !is.numeric(time)) {
    stop("goal, have, and time must be numeric.")
  }
  if (goal < 0 || have < 0 || time <= 0) {
    stop("Amounts must be non-negative and time must be positive.")
  }
  if (!time_unit %in% c("d", "m", "y")) {
    stop("time_unit must be 'd' (days), 'm' (months), or 'y' (years).")
  }
  # Amount to save
  remaining <- goal - have
  if (remaining <= 0) {
    return(list(
      daily = 0,
      monthly = 0,
      yearly = 0,
      message = "Goal already met or exceeded."
    ))
  }
  # Convert time to years, months, days
  if (time_unit == "y") {
    years <- time
    months <- time * 12
    days <- time * 365
    # months
  } else if (time_unit == "m") {
    months <- time
    years <- time / 12
    days <- years * 365
    # days
  } else {
    days <- time
    years <- time / 365
    months <- years * 12
  }
  # Calculate savings per period
  daily <- trunc((remaining / days) * 100) / 100
  monthly <- trunc((remaining / months) * 100) / 100
  yearly <- trunc((remaining / years) * 100) / 100
  # Return as list
  return(list(
    daily = daily,
    monthly = monthly,
    yearly = yearly
  ))
}
