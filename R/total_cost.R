
#' Calculate Total Cost with Discount and Sales Tax: total_cost
#'
#' Computes the total cost for either a manually entered price or multiple rows from a data frame.
#' Supports the 'food' and 'gifts' sample data frames. Returns the total price after applying
#' discounts and taxes. If using a data frame, prints the rows being used; for manual input, prints
#' the values provided.
#'
#' @param price Numeric. The base price (used only if `item_rows` is not provided).
#' @param discount Numeric between 0 and 1. The discount percentage (default 0 if manual input and not provided).
#' @param tax Numeric between 0 and 1. The sales tax percentage (default 0 if manual input and not provided).
#' @param item_rows Data frame rows from `food` or `gifts` containing columns `price`, `discount`, and `tax`.
#'   If provided, calculates the total across all rows and prints the items.
#'
#' @return Prints the total cost after discount and tax. (Truncated to 2 decimals.)
#'
#' @examples
#' # Manual calculation
#' total_cost(price = 50, discount = 0.1, tax = 0.08)
#'
#' # Using multiple items from food menu
#' total_cost(item_rows = food[1:3, ])
#'
#' # Using multiple items from gifts
#' total_cost(item_rows = gifts[1:2, ])
#'
#' @export
total_cost <- function(price = NULL, discount = NULL, tax = NULL, item_rows = NULL) {
  # Function body...
}


# Calculate total cost with discount and sales tax
# Supports either manual input (price, discount, tax) or multiple rows from a single data frame (supports food or gifts)
total_cost <- function(price = NULL, discount = NULL, tax = NULL, item_rows = NULL) {
  # Determine source type
  source_count <- 0
  if (!is.null(price) || !is.null(discount) || !is.null(tax)) source_count <- source_count + 1
  if (!is.null(item_rows)) source_count <- source_count + 1
  # Only one source allowed
  if (source_count > 1) {
    stop("Please use only one source for the calculation: either manual price/discount/tax OR a data frame (food or gifts).")
  }
  # If using a data frame
  if (!is.null(item_rows)) {
    # Required columns
    required_cols <- c("price", "discount", "tax")
    if (!all(required_cols %in% colnames(item_rows))) {
      stop(paste0("item_rows must have columns: ", paste(required_cols, collapse = ", "),
                  ". Example: food[1:3, ] or gifts[1:2, ]"))
    }
    # Print the rows being used
    cat("Calculating total for the following items:\n")
    print(item_rows)
    # Calculate total cost per row
    discounted_prices <- item_rows$price * (1 - item_rows$discount)
    totals <- discounted_prices * (1 + item_rows$tax)
    # Sum total across rows
    total <- sum(totals)
  } else {
    # Manual input checks
    if (is.null(price) || !is.numeric(price) || length(price) != 1 || price < 0) {
      stop("price must be a single non-negative numeric value.")
    }
    if (is.null(discount)) discount <- 0
    if (!is.numeric(discount) || length(discount) != 1 || discount < 0 || discount > 1) {
      stop("discount must be a numeric value between 0 and 1.")
    }
    if (is.null(tax)) tax <- 0
    if (!is.numeric(tax) || length(tax) != 1 || tax < 0 || tax > 1) {
      stop("tax must be a numeric value between 0 and 1.")
    }
    # Apply discount and tax
    total <- price * (1 - discount) * (1 + tax)
    # Print manual input info
    cat("Manual input:\n")
    cat("Price: ", price, ", Discount: ", discount, ", Tax: ", tax, "\n")
  }
  # Truncate to 2 decimals without rounding
  total <- trunc(total * 100) / 100
  # Print total price
  cat("Total price: $", format(total, nsmall = 2), "\n", sep = "")
}
