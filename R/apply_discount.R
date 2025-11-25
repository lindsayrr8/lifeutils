
#' Apply a Discount to a Price or Selected Items: apply_discount
#'
#' Calculates the discounted price for either a manually entered price or one/multiple
#' rows from a data frame (supports the 'food' or 'gifts' data frames). Returns a
#' descriptive string showing the final price after discount.
#'
#' @param price Numeric. The original price (used only if `item_row` is not provided).
#' @param discount Numeric between 0 and 1. The discount percentage to apply (default 0.10).
#' @param item_row Data frame row(s) from `food` or `gifts` containing columns
#'   `price` and `discount`. If provided, the function calculates the total and average discount.
#'
#' @return Character. A formatted string showing the price after discount, along with the original total.
#'
#' @examples
#' # Manual price and discount
#' apply_discount(price = 50, discount = 0.15)
#'
#' # Single item from food menu
#' apply_discount(item_row = food[1, ])
#'
#' # Multiple items from gifts
#' apply_discount(item_row = gifts[1:3, ])
#'
#' @export
apply_discount <- function(price = NULL, discount = NULL, item_row = NULL) {
  # Function body...
}


# Apply a discount to a price or multiple items, using manual input or a data frame (supports 'menu' and 'gifts')
apply_discount <- function(price = NULL, discount = NULL, item_row = NULL) {
  # Determine how many sources the user is using
  source_count <- 0
  if (!is.null(price) || !is.null(discount)) source_count <- source_count + 1
  if (!is.null(item_row)) source_count <- source_count + 1
  # Check that only one source is used
  if (source_count > 1) {
    stop("Please use only one source for the calculation: either manual price/discount OR a data frame row(s) (menu or gifts).")
  }
  # If using a data frame row(s)
  if (!is.null(item_row)) {
    # Check that required columns exist
    if (!all(c("price", "discount") %in% colnames(item_row))) {
      stop("item_row must have columns: price and discount. Example: menu[1, ] or gifts[3, ]")
    }
    # Ensure all prices and discounts are numeric
    if (!all(sapply(item_row$price, is.numeric)) || !all(sapply(item_row$discount, is.numeric))) {
      stop("All price and discount values in item_row must be numeric.")
    }
    if (any(item_row$price < 0) || any(item_row$discount < 0 | item_row$discount > 1)) {
      stop("Prices must be non-negative and discounts between 0 and 1.")
    }
    # Calculate totals across all selected rows
    price <- sum(item_row$price)
    discount <- mean(item_row$discount) # or could sum weighted discount; using mean for simplicity
  }
  # Manual input defaults
  if (is.null(price) || !is.numeric(price) || length(price) != 1) {
    stop("price must be a single numeric value.")
  }
  if (price < 0) stop("price must be non-negative.")
  # default discount is 10%
  if (is.null(discount)) discount <- 0.10
  if (!is.numeric(discount) || length(discount) != 1) {
    stop("discount must be a single numeric value.")
  }
  if (discount < 0 || discount > 1) {
    stop("discount must be between 0 and 1 (0% - 100%).")
  }
  # Calculate discounted price
  discounted_price <- price * (1 - discount)
  # Truncate to 2 decimals without rounding
  discounted_price <- trunc(discounted_price * 100) / 100
  # Return formatted explanation
  return(paste0("Price after discount: $", format(discounted_price, nsmall = 2),
                " (original total: $", format(price, nsmall = 2), ")"))
}
