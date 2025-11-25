
#' Split a Bill Among Multiple People: split_bill
#'
#' Divides a total bill evenly among a specified number of people and returns the
#' amount each person should pay.
#'
#' @param bill Numeric. The total bill amount to be split.
#' @param people Integer. The number of people sharing the bill (default is 2).
#'
#' @return A named numeric vector giving the amount each person owes, truncated
#'   to two decimal places. Names are "Person 1", "Person 2", etc.
#'
#' @examples
#' # Split a $50 bill between 2 people
#' split_bill(50)
#'
#' # Split a $120 bill among 4 people
#' split_bill(120, people = 4)
#'
#' @export
split_bill <- function(bill, people = 2) {
  # Function body ...
}


# Split a bill among multiple people
split_bill <- function(bill, people = 2) {
  # Input checks
  if (!is.numeric(bill) || length(bill) != 1) {
    stop("bill must be a numeric value.")
  }
  if (bill < 0) stop("bill must be non-negative.")

  if (!is.numeric(people) || length(people) != 1 || people < 1 || people != round(people)) {
    stop("people must be a positive integer.")
  }
  # Calculate amount per person and replicate
  per_person <- rep(trunc((bill / people) * 100) / 100, people)
  # Create named vector
  names(per_person) <- paste("Person", 1:people)
  return(per_person)
}
