
#' Calculate Price Per Unit for Bulk Items: price_per_unit
#'
#' Computes the price per individual unit for bulk items. Supports either manual input
#' of `price` and `quantity` or a single row from the `bulk` data frame.
#' Prints the item details (from the data frame or manual input) and the per-unit price.
#'
#' @param price Numeric. Total price of the bulk package (non-negative). Required if `bulk_row` is not used.
#' @param quantity Numeric. Number of items in the package (positive integer). Required if `bulk_row` is not used.
#' @param bulk_row Data frame. Optional single row from the `bulk` data frame containing columns: `item`, `package_price`, `quantity`.
#'
#' @return Prints the item details and price per unit. Does not return a value.
#'
#' @examples
#' # Using manual input
#' price_per_unit(price = 20, quantity = 4)
#'
#' # Using a row from the bulk data frame
#' price_per_unit(bulk_row = bulk[1, ])
#'
#' @export
price_per_unit <- function(price = NULL, quantity = NULL, bulk_row = NULL) {
  # Function body...
}


# Calculate price per unit for bulk items (manual or bulk data frame)
price_per_unit <- function(price = NULL, quantity = NULL, bulk_row = NULL) {
  # Determine source type
  source_count <- 0
  if (!is.null(price) || !is.null(quantity)) source_count <- source_count + 1
  if (!is.null(bulk_row)) source_count <- source_count + 1
  # Check that only one source is used
  if (source_count > 1) {
    stop("Please use only one source: either manual price/quantity OR a row from the 'bulk' data frame.")
  }
  # If using a bulk row
  row_info <- NULL
  if (!is.null(bulk_row)) {
    # Check that required columns exist
    if (!all(c("item", "package_price", "quantity") %in% colnames(bulk_row))) {
      stop("bulk_row must have columns: item, package_price, and quantity. Example: bulk[1, ]")
    }
    # Ensure only one row at a time
    if (nrow(bulk_row) != 1) {
      stop("Please provide only one row at a time from the 'bulk' data frame.")
    }
    price <- bulk_row$package_price
    quantity <- bulk_row$quantity
    row_info <- bulk_row
  }
  # Manual input checks
  if (is.null(price) || !is.numeric(price) || length(price) != 1 || price < 0) {
    stop("price must be a single non-negative numeric value.")
  }
  if (is.null(quantity) || !is.numeric(quantity) || length(quantity) != 1 || quantity < 1 || quantity != round(quantity)) {
    stop("quantity must be a single positive integer.")
  }
  # Calculate per unit price
  unit_price <- price / quantity
  # Truncate to 2 decimals without rounding
  unit_price <- trunc(unit_price * 100) / 100
  # Prepare output
  if (!is.null(row_info)) {
    cat("Item details:\n")
    print(row_info)
  } else {
    cat("Manual input:\n")
    cat("Price: ", price, ", Quantity: ", quantity, "\n")
  }
  cat("Price per unit: $", format(unit_price, nsmall = 2), "\n", sep = "")
}
