#' Monty Hall problem
#'
#' This function simulates the probability that a game show contestant,
#' searching for a new car behind a set of three closed doors, will
#' win the prize upon switching their original door choice after a non-
#' winning door is revealed. The problem was first proposed by Steve 
#' Selvin.
#' 
#' @param n The number of games to simulate.
#' @param shift Whether to switch to a second door, a boolean value.
#' @export
#' @return A rational approximation of the probability that a player
#' will win a new car after switching their original door choice after
#' a non-winning door is revealed.
#' @examples monty(10000)

monty <- function(n, shift = TRUE) {
  wins <- 0
  doors <- c(1, 2, 3)
  for (i in 1:n) {
    car <- sample(doors, 1)
    first <- sample(doors, 1)
    if (first != car) {
      wins <- wins + 1
    }
  }
  if (!shift) {
    1 - wins / n
  }
  else {
    wins / n
  }
}