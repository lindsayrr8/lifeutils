
#' Calculate Tip for a Bill or Selected Food Items: bill_tip
#'
#' Computes the tip amount and total bill including tip. The calculation can be based
#' on either a manual total amount or selected rows from a food menu data frame that
#' contains `price`, `discount`, and `tax` columns.
#'
#' @param amount Numeric. Total bill amount (optional if `menu_rows` is provided).
#' @param tip Numeric. Tip percentage as a decimal (default is 0.15 for 15%).
#' @param menu_rows Data frame. Optional subset of the `food` data frame to calculate
#'   the bill and tip automatically. Must include columns: `price`, `discount`, `tax`.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{tip_amount}: The calculated tip amount (truncated to 2 decimals).
#'     \item \code{total_with_tip}: The total bill including tip (truncated to 2 decimals).
#'   }
#'
#' @examples
#' # Using manual amount
#' bill_tip(amount = 50, tip = 0.2)
#'
#' # Using food menu rows
#' bill_tip(menu_rows = food[1:3, ])
#'
#' @export
bill_tip <- function(amount = NULL, tip = 0.15, menu_rows = NULL) {
  # Function body ...
}


# Calculate bill tip given total amount or selected food menu items
bill_tip <- function(amount = NULL, tip = 0.15, menu_rows = NULL) {
  # If food menu rows are provided, override amount
  if (!is.null(menu_rows)) {
    # Check it's a data.frame with required columns
    if (!is.data.frame(menu_rows) || !all(c("price", "discount", "tax") %in% colnames(menu_rows))) {
      stop("menu_rows must be a subset of the 'food' data frame with columns: price, discount, tax.\nExample: food[1:3, ]")
    }
    # Print the food menu rows being used
    cat("Calculating tip for the following items:\n")
    print(menu_rows)
    # Compute total per row: (price - discount) + tax
    row_totals <- (menu_rows$price - menu_rows$discount * menu_rows$price) * (1 + menu_rows$tax)
    amount <- sum(row_totals)
  }
  # Input checks for manual amount
  if (is.null(amount) || !is.numeric(amount) || !is.numeric(tip) || amount < 0 || tip < 0) {
    stop("Amount and tip must be numeric and non-negative. Either provide 'amount' or 'food' rows.")
  }
  # Calculate tip
  tip_amount <- amount * tip
  total_with_tip <- amount + tip_amount
  # Truncate to 2 decimals without rounding
  tip_amount <- trunc(tip_amount * 100) / 100
  total_with_tip <- trunc(total_with_tip * 100) / 100
  # Return a list with descriptive output
  return(list(
    tip_amount = tip_amount,
    total_with_tip = total_with_tip
  ))
}
