

#' Calculate Simple Interest and Total Amount: simple_interest
#'
#' This function computes the simple interest for a principal amount over a period of time,
#' given an annual interest rate. It also returns the total amount after the interest term.
#'
#' @param principal Numeric. The initial amount of money invested or borrowed.
#' @param rate Numeric. The annual interest rate (as a decimal, e.g., 0.08 for 8%).
#' @param time Numeric. The length of the interest period (in years by default or months if `time_unit = "m"`).
#' @param time_unit Character. Either "y" for years (default) or "m" for months.
#' @return A list with two elements:
#'   \item{interest}{The calculated simple interest, truncated to 2 decimals.}
#'   \item{total}{The total amount after interest, truncated to 2 decimals.}
#' @examples
#' simple_interest(principal = 1000, rate = 0.05, time = 2)
#' simple_interest(principal = 5000, rate = 0.08, time = 18, time_unit = "m")
#' @export
simple_interest <- function(principal, rate, time, time_unit = "y") {
  # function body ...
}


# Calculate simple interest and total amount
simple_interest <- function(principal, rate, time, time_unit = "y") {
  # Input checks
  if (!is.numeric(principal) || !is.numeric(rate) || !is.numeric(time)) {
    stop("Principal, rate, and time must all be numeric.")
  }
  if (principal < 0 || rate < 0 || time < 0) {
    stop("Principal, rate, and time must be non-negative.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' for months or 'y' for years.")
  }
  # Convert time to years if input is in months
  if (time_unit == "m") {
    time <- time / 12
  }
  # Calculate interest
  interest <- principal * rate * time
  # Total amount after interest
  total <- principal + interest
  # Truncate to 2 decimals
  interest <- trunc(interest * 100) / 100
  total <- trunc(total * 100) / 100
  # Return as a list
  return(list(
    interest = interest,
    total = total
  ))
}





#' Calculate Compound Interest and Total Amount: comp_interest
#'
#' Computes the compound interest for a principal amount over a specified period of time,
#' with optional compounding frequency, and returns both the interest earned and total amount.
#'
#' @param principal Numeric. The initial amount of money invested or borrowed.
#' @param rate Numeric. The annual interest rate as a decimal (e.g., 0.08 for 8%).
#' @param time Numeric. The duration of the investment or loan (in years by default, or months if `time_unit = "m"`).
#' @param n Numeric. Number of times interest is compounded per year (default is 1).
#' @param time_unit Character. "y" for years (default) or "m" for months.
#' @return A list with two elements:
#'   \item{interest}{The calculated compound interest, truncated to 2 decimals.}
#'   \item{total}{The total amount after compound interest, truncated to 2 decimals.}
#' @examples
#' comp_interest(principal = 1000, rate = 0.05, time = 2)
#' comp_interest(principal = 5000, rate = 0.08, time = 18, time_unit = "m", n = 4)
#' @export
comp_interest <- function(principal, rate, time, n = 1, time_unit = "y") {
  # function body ...
}


# Calculate compound interest and total amount
comp_interest <- function(principal, rate, time, n = 1, time_unit = "y") {
  # Input checks
  if (!is.numeric(principal) || !is.numeric(rate) || !is.numeric(time) || !is.numeric(n)) {
    stop("Principal, rate, time, and n must all be numeric.")
  }
  if (principal < 0 || rate < 0 || time < 0 || n <= 0) {
    stop("Principal, rate, time must be non-negative and n must be positive.")
  }
  if (!time_unit %in% c("m", "y")) {
    stop("time_unit must be 'm' for months or 'y' for years.")
  }
  # Convert time to years if input is in months
  if (time_unit == "m") {
    time <- time / 12
  }
  # Calculate total amount after compound interest
  total <- principal * (1 + rate / n)^(n * time)
  # Calculate compound interest
  interest <- total - principal
  # Truncate to 2 decimal places
  total <- trunc(total * 100) / 100
  interest <- trunc(interest * 100) / 100
  # Return as a list
  return(list(
    interest = interest,
    total = total
  ))
}






#' Compare Two Loans: compare_loans
#'
#' Calculates and compares the monthly payments and total costs of two loans: compare_loans
#' The function can accept either manual inputs for each loan or a single row
#' from a sample data frame called `loans`. Down payments are optional.
#'
#' @param principal1 Numeric. Total principal for the first loan (ignored if `loan_row1` is provided).
#' @param rate1 Numeric. Annual interest rate (as a decimal, e.g., 0.05 for 5%) for the first loan.
#' @param term1 Numeric. Loan term for the first loan (in months by default, or years if `term_unit = "y"`).
#' @param principal2 Numeric. Total principal for the second loan (ignored if `loan_row2` is provided).
#' @param rate2 Numeric. Annual interest rate for the second loan (as a decimal).
#' @param term2 Numeric. Loan term for the second loan.
#' @param down1 Numeric. Optional down payment for the first loan (default 0).
#' @param down2 Numeric. Optional down payment for the second loan (default 0).
#' @param term_unit Character. "m" for months (default) or "y" for years.
#' @param loan_row1 Data frame row. Optional single row from the `loans` data set to use for the first loan.
#' @param loan_row2 Data frame row. Optional single row from the `loans` data set to use for the second loan.
#'
#' @return A list with three elements:
#'   \item{loan1}{A list containing the monthly payment and total cost of the first loan.}
#'   \item{loan2}{A list containing the monthly payment and total cost of the second loan.}
#'   \item{comparison}{Character string indicating which loan is cheaper or if both are the same.}
#'
#' @examples
#' # Manual inputs
#' compare_loans(principal1 = 10000, rate1 = 0.05, term1 = 24,
#'               principal2 = 12000, rate2 = 0.07, term2 = 36)
#'
#' # Using sample data set rows
#' compare_loans(loan_row1 = loans[1, ], loan_row2 = loans[2, ])
#'
#' @export
compare_loans <- function(
    principal1 = NULL, rate1 = NULL, term1 = NULL,
    principal2 = NULL, rate2 = NULL, term2 = NULL,
    down1 = 0, down2 = 0,
    term_unit = "m",
    loan_row1 = NULL, loan_row2 = NULL
) {
  # function body ...
}


# Compare two loans - either manual input or optional sample data set rows from 'loans'
compare_loans <- function(
    principal1 = NULL, rate1 = NULL, term1 = NULL,
    principal2 = NULL, rate2 = NULL, term2 = NULL,
    down1 = 0, down2 = 0,
    term_unit = "m",  # "m" for months, "y" for years
    loan_row1 = NULL, loan_row2 = NULL
) {
  # Helper function to validate a single loan row
  validate_loan_row <- function(row, row_name) {
    required_cols <- c("total_amount", "down_payment", "interest_rate", "term_months")
    if (!is.data.frame(row) || nrow(row) != 1) {
      stop(paste0(
        "Please provide a single row from a data frame for ", row_name, ".\n",
        "Example usage: compare_loans(loan_row1 = loans[1, ], loan_row2 = loans[2, ])"
      ))
    }
    missing_cols <- setdiff(required_cols, colnames(row))
    if (length(missing_cols) > 0) {
      stop(paste0("The row for ", row_name, " is missing required columns: ",
                  paste(missing_cols, collapse = ", ")))
    }
  }
  # If sample data rows are provided, validate and extract values
  if (!is.null(loan_row1)) {
    validate_loan_row(loan_row1, "loan_row1")
    principal1 <- loan_row1$total_amount
    rate1 <- loan_row1$interest_rate
    term1 <- loan_row1$term_months
    down1 <- loan_row1$down_payment
    message("Using loan_row1 from sample data. Example: loans[1, ]")
  }
  if (!is.null(loan_row2)) {
    validate_loan_row(loan_row2, "loan_row2")
    principal2 <- loan_row2$total_amount
    rate2 <- loan_row2$interest_rate
    term2 <- loan_row2$term_months
    down2 <- loan_row2$down_payment
    message("Using loan_row2 from sample data. Example: loans[2, ]")
  }
  # Input checks for manual entries
  all_numeric <- c(principal1, rate1, term1, principal2, rate2, term2, down1, down2)
  if (!all(sapply(all_numeric, is.numeric))) stop("All inputs must be numeric")
  if (any(all_numeric < 0)) stop("All numeric inputs must be non-negative")
  if (!term_unit %in% c("m", "y")) stop("term_unit must be 'm' or 'y'")
  # Convert terms to months if given in years
  if (term_unit == "y") {
    term1 <- term1 * 12
    term2 <- term2 * 12
  }
  # Function to calculate monthly payment
  monthly_payment <- function(P, r_annual, n) {
    r_month <- r_annual / 12
    if (r_month == 0) return(P / n)
    P * (r_month * (1 + r_month)^n) / ((1 + r_month)^n - 1)
  }
  # Adjust principals for down payment
  loan1 <- principal1 - down1
  loan2 <- principal2 - down2
  # Monthly payments
  M1 <- monthly_payment(loan1, rate1, term1)
  M2 <- monthly_payment(loan2, rate2, term2)
  # Total cost
  total1 <- M1 * term1 + down1
  total2 <- M2 * term2 + down2
  # Truncate to 2 decimals
  M1 <- trunc(M1 * 100) / 100
  M2 <- trunc(M2 * 100) / 100
  total1 <- trunc(total1 * 100) / 100
  total2 <- trunc(total2 * 100) / 100
  # Comparison summary
  better <- if (total1 < total2) "Loan 1 is cheaper" else if (total2 < total1) "Loan 2 is cheaper" else "Both loans cost the same"
  return(list(
    loan1 = list(monthly_payment = M1, total_cost = total1),
    loan2 = list(monthly_payment = M2, total_cost = total2),
    comparison = better
  ))
}







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


