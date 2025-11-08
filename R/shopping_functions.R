

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







#' Calculate Monthly Payment for a Financed Purchase: monthly_payment
#'
#' Computes the monthly payment for a loan or financed purchase based on the total amount,
#' down payment, interest rate, and loan term. Returns a descriptive string with the monthly payment.
#'
#' @param total Numeric. Total amount of the purchase (must be non-negative).
#' @param term Numeric. Loan term in months or years (must be positive).
#' @param down_payment Numeric. Down payment amount (default 0, must be <= total).
#' @param interest_rate Numeric. Annual interest rate in percent (default 0 for no interest).
#' @param time_unit Character. "m" for months or "y" for years (default "m").
#'
#' @return Character string describing the monthly payment, truncated to 2 decimals.
#'
#' @examples
#' # Loan of $12,000 for 36 months with 5% annual interest and $2,000 down
#' monthly_payment(total = 12000, term = 36, down_payment = 2000, interest_rate = 5)
#'
#' # Loan of $5,000 for 2 years with no interest
#' monthly_payment(total = 5000, term = 2, time_unit = "y")
#'
#' @export
monthly_payment <- function(total, term, down_payment = 0, interest_rate = 0, time_unit = "m") {
  # Function body...
}


# Calculate monthly payment for financed purchase
monthly_payment <- function(total, term, down_payment = 0, interest_rate = 0, time_unit = "m") {
  # Input checks
  if (!is.numeric(total) || length(total) != 1 || total < 0) {
    stop("total must be a single non-negative numeric value.")
  }
  if (!is.numeric(down_payment) || length(down_payment) != 1 || down_payment < 0 || down_payment > total) {
    stop("down_payment must be numeric, non-negative, and <= total.")
  }
  if (!is.numeric(interest_rate) || length(interest_rate) != 1 || interest_rate < 0) {
    stop("interest_rate must be a non-negative numeric value (percent).")
  }
  if (!is.numeric(term) || length(term) != 1 || term <= 0) {
    stop("term must be a positive numeric value.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' (months) or 'y' (years).")
  }
  # Convert term to months if input is in years
  if (time_unit == "y") {
    term <- term * 12
  }
  # Amount financed
  principal <- total - down_payment
  # Convert interest_rate percent to decimal monthly rate
  r <- interest_rate / 100 / 12
  # Calculate monthly payment
  if (r == 0) {
    monthly <- principal / term
  } else {
    monthly <- principal * r / (1 - (1 + r)^(-term))
  }
  # Truncate to 2 decimals
  monthly <- trunc(monthly * 100) / 100
  # Return explanation
  return(paste0("Monthly payment: $", format(monthly, nsmall = 2)))
}







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







#' Calculate Hours Needed to Work for a Purchase: wage_cost
#'
#' Computes the number of hours a person must work at a given hourly wage
#' to afford a specified purchase amount. The result is truncated to two decimals
#' and returned as a descriptive string.
#'
#' @param purchase Numeric. Total cost of the purchase (non-negative). Required.
#' @param wage Numeric. Hourly wage of the worker (positive). Required.
#'
#' @return A character string describing the number of hours needed to work.
#'
#' @examples
#' # Calculate hours to work for a $50 purchase at $15/hour
#' wage_cost(purchase = 50, wage = 15)
#'
#' @export
wage_cost <- function(purchase, wage) {
  # Function body...
}


# Calculate hours needed to work to afford a purchase
wage_cost <- function(purchase, wage) {
  # Input checks
  if (!is.numeric(purchase) || length(purchase) != 1 || purchase < 0) {
    stop("purchase must be a single non-negative numeric value.")
  }
  if (!is.numeric(wage) || length(wage) != 1 || wage <= 0) {
    stop("hourly wage must be a single positive numeric value.")
  }
  # Calculate hours needed
  hours <- purchase / wage
  # Truncate to 2 decimals
  hours <- trunc(hours * 100) / 100
  # Return descriptive string
  return(paste0("Hours to work: ", format(hours, nsmall = 2)))
}








#' Calculate Purchase as a Percentage of Salary: salary_cost
#'
#' Determines what percentage a specific purchase represents of an annual salary.
#' The result is truncated to two decimal places and returned as a descriptive string.
#'
#' @param purchase Numeric. Total cost of the purchase (non-negative). Required.
#' @param salary Numeric. Annual salary (positive). Required.
#'
#' @return A character string describing the percentage of the salary represented by the purchase.
#'
#' @examples
#' # Determine percentage of a $5,000 purchase for a $50,000 salary
#' salary_cost(purchase = 5000, salary = 50000)
#'
#' @export
salary_cost <- function(purchase, salary) {
  # Function body...
}


# Calculate what percent of a salary a purchase represents
salary_cost <- function(purchase, salary) {
  # Input checks
  if (!is.numeric(purchase) || length(purchase) != 1 || purchase < 0) {
    stop("purchase must be a single non-negative numeric value.")
  }
  if (!is.numeric(salary) || length(salary) != 1 || salary <= 0) {
    stop("salary must be a single positive numeric value.")
  }
  # Calculate percentage
  percent <- (purchase / salary) * 100
  # Truncate to 2 decimals
  percent <- trunc(percent * 100) / 100
  # Return descriptive string
  return(paste0("This purchase is ", format(percent, nsmall = 2), "% of your annual salary."))
}





