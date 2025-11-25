
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
