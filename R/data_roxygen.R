

#' Sample Food Menu Data: food
#'
#' A dataset containing sample menu items, their prices, applicable discounts, and tax rates.
#'
#' This dataset is intended for testing and demonstrating functions related to personal finance and shopping.
#'
#' @format A data frame with 10 rows and 4 columns:
#' \describe{
#'   \item{item}{Character: Name of the menu item.}
#'   \item{price}{Numeric: Price of the item in USD.}
#'   \item{discount}{Numeric: Discount rate for the item (0-1).}
#'   \item{tax}{Numeric: Tax rate applied to the item (0-1).}
#' }
#' @examples
#' \dontrun{
#' # Calculate total cost for first three menu items
#' total_cost(item_rows = food[1:3, ])
#'
#' # Apply discount to a single menu item
#' apply_discount(item_row = food[1, ])
#' }
#' @source Made up sample data for demonstration purposes
"food"





#' Sample Gifts Data: gifts
#'
#' A dataset containing sample gift items, their prices, applicable discounts, and tax rates.
#'
#' This dataset is intended for testing and demonstrating functions related to shopping and personal finance.
#'
#' @format A data frame with 10 rows and 4 columns:
#' \describe{
#'   \item{item}{Character: Name of the gift item.}
#'   \item{price}{Numeric: Price of the item in USD.}
#'   \item{discount}{Numeric: Discount rate for the item (0-1).}
#'   \item{tax}{Numeric: Tax rate applied to the item (0-1).}
#' }
#' @examples
#' \dontrun{
#' # Calculate total cost for first three gifts
#' total_cost(item_rows = gifts[1:3, ])
#'
#' # Apply discount to a single gift item
#' apply_discount(item_row = gifts[1, ])
#' }
#' @source Made up sample data for demonstration purposes
"gifts"







#' Sample Cars Data: cars
#'
#' A dataset containing sample car types, their fuel efficiency (mpg), and fuel tank sizes.
#'
#' This dataset is intended for testing and demonstrating functions related to travel and trip calculations.
#'
#' @format A data frame with 4 rows and 3 columns:
#' \describe{
#'   \item{car_type}{Character: Type of car (e.g., sedan, suv, truck, minivan).}
#'   \item{mpg}{Numeric: Miles per gallon fuel efficiency.}
#'   \item{tank_size}{Numeric: Fuel tank size in gallons.}
#' }
#' @examples
#' \dontrun{
#' # Calculate tanks needed for a 300-mile trip using the first car
#' trip_tanks(miles = 300, car_row = cars[1, ])
#' }
#' @source Made up sample data for demonstration purposes
"cars"






#' Sample Loans Data: loans
#'
#' A dataset containing sample loans with total amounts, down payments, interest rates, and terms.
#'
#' This dataset is intended for testing and demonstrating personal finance functions such as `compare_loans`.
#'
#' @format A data frame with 5 rows and 5 columns:
#' \describe{
#'   \item{loan_name}{Character: Name or identifier of the loan.}
#'   \item{total_amount}{Numeric: Total principal amount of the loan.}
#'   \item{down_payment}{Numeric: Initial down payment for the loan.}
#'   \item{interest_rate}{Numeric: Annual interest rate (as a decimal, e.g., 0.05 for 5\%).}
#'   \item{term_months}{Numeric: Loan term in months.}
#' }
#' @examples
#' \dontrun{
#' # Compare the first two sample loans
#' compare_loans(loan_row1 = loans[1, ], loan_row2 = loans[2, ])
#' }
#' @source Made up sample data for demonstration purposes
"loans"








#' Sample Bulk Items Data: bulk
#'
#' A dataset containing sample bulk items with package prices and quantities.
#'
#' This dataset is intended for testing and demonstrating functions such as `price_per_unit`.
#'
#' @format A data frame with 10 rows and 3 columns:
#' \describe{
#'   \item{item}{Character: Name of the bulk item.}
#'   \item{package_price}{Numeric: Price for the full package of the item.}
#'   \item{quantity}{Numeric: Number of individual units in the package.}
#' }
#' @examples
#' \dontrun{
#' # Calculate price per unit for the first bulk item
#' price_per_unit(bulk_row = bulk[1, ])
#' }
#' @source Generated sample data for demonstration purposes
"bulk"








# usethis::use_data(food, gifts, bulk, loans, cars, overwrite = TRUE)





