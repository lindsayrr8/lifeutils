

#' Simulate a Coin Toss: coin_toss
#'
#' Randomly returns either "Heads" or "Tails" to simulate a single coin flip.
#'
#' @return Character. Either "Heads" or "Tails".
#'
#' @examples
#' # Simulate a coin toss
#' coin_toss()
#'
#' # Toss multiple times using replicate
#' replicate(5, coin_toss())
#'
#' @export
coin_toss <- function() {
  result <- sample(c("Heads", "Tails"), size = 1)
  return(result)
}


# Simulate a coin toss
coin_toss <- function() {
  result <- sample(c("Heads", "Tails"), size = 1)
  return(result)
}
