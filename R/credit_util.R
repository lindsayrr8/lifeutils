
#' Calculate Credit Utilization Brackets: credit_util
#'
#' Computes dollar amounts corresponding to common credit utilization percentages
#' (5%, 10%, 20%, 30%) for a given credit limit.
#'
#' @param limit Numeric. The total credit limit available.
#'
#' @return A named numeric vector, where names are the utilization percentages
#'   ("5%", "10%", "20%", "30%") and values are the corresponding dollar amounts.
#'
#' @examples
#' # Calculate utilization amounts for a $5000 credit limit
#' credit_util(5000)
#'
#' @export
credit_util <- function(limit) {
  # function body ...
}


# Calculate credit utilization brackets
credit_util <- function(limit) {
  # Input checks
  if (!is.numeric(limit) || length(limit) != 1) {
    stop("credit_limit must be a single numeric value.")
  }
  if (limit < 0) {
    stop("credit_limit must be non-negative.")
  }
  # Define brackets
  brackets <- c(5, 10, 20, 30) / 100
  # Calculate amounts
  amounts <- limit * brackets
  # Truncate to 2 decimals
  amounts <- trunc(amounts * 100) / 100
  # Return as named list
  names(amounts) <- paste0(brackets * 100, "%")
  return(amounts)
}
