# lifeutils

## Description
Packaged collection of R functions for practical calculations in everyday life involving: personal finance, shopping, and travel.

## Link to Relevant Blog Post
[https://rlanguagejournal.blogspot.com/2025/11/the-lifeutils-r-package.html](https://rlanguagejournal.blogspot.com/2025/11/the-lifeutils-r-package.html)

## How to Run
1) Open project (based on (>= 3.5.0) R in RStudio)
2) Review included list of functions. Either input manual values or call examples from included data frames. All functions can receive manual input and run without utilizing a data frame. Some functions support included data sets. See documentation.
3) Outputs are produced in the console and can be saved locally if applicable.

## Additional Packages
- None
## Data - Source
- `bulk` - bulk items for purchase; made up; included with package.
- `cars` - a short list of cars with mpg and tank size; made up; included with package.
- `food` - a menu; made up; included with package.
- `gifts` - a short gift catalog; made up; included with package.
- `loans` - a small selection of loan plans; made up; included with package.

## List of Functions
#### Personal Finance:
`simple_interest` - calculates simple interest (and see the end result after the interest term).

`comp_interest` - calculates compound interest (and see the end result after the interest term).

`compare_loans` - compares two loan terms over a monthly time span, given a down payment if applicable, to see which term is more cost-effective (shows total cost and monthly payment amount for each loan).

`savings_goal` - calculates how much to save per each: day, month, or year to reach a desired amount (and shows each).

`credit_util` - calculates different bracket amounts for credit line utilization from a given credit limit (5%, 10%, 20%, 30%) and outputs them to user (ex: $500 is 30% of your credit utilization).

#### Shopping:
`bill_tip` - calculates the tip amount for a given bill, from a given tip percentage.

`split_bill` - calculates different ways to split a given bill (ex: between 2 people).

`apply_discount` - apply a given discount percentage to a cash price to see discounted price.

`total_cost` - calculate an item or transaction total cost with any discount and sales tax applied to see final price.

`monthly_payment` - calculates the monthly payment you will make on a financed purchase, with or without interest.

`price_per_unit` - get a per unit price for a bulk item (ex: how much does each cookie cost in a $10 bag containing 5 cookies?).

`wage_cost` - calculates how many hours you have to work to make a purchase, given your hourly wage and the purchase amount.

`salary_cost` - calculates what percent of a given salary that a given purchase is (ex: a $2,000 purchase is 4% of your annual salary).

#### Travel:
`trip_tanks` - calculates how many tanks of gas it will take to complete a road trip (given a car's mpg, gallon tank size, and miles in the trip).

`drive_cost` - calculates the total gas cost of a road trip (given a gas price, car mpg, and trip distance in miles).

`travel_time` - calculates the time it will take to travel a certain distance (given the distance in miles and average speed), output as days and hours.

`days_until` - checks the count of days until a certain date from the present date.

`days_since` - checks the count of days since a certain date from the present date.

#### Other:
`coin_toss` - heads or tails, for everything else in life!
