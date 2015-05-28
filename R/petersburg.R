#' St. Petersburg paradox
#'
#' This function simulates the average payout of a player in the lottery
#' game and consequent probability problem proposed by Nicolas
#' Bernoulli.
#' 
#' @param x The number of lotteries to simulate.
#' @param bet The price at which the player plays the game.
#' @export
#' @return The average payout across all simulated lotteries.
#' @examples petersburg(10000, bet = 15)

petersburg <- function(x, bet = 0) {
  winnings <- 0
  for (i in 1:x) {
    coin <- c('heads', 'tails')
    pot <- 2
    flip <- sample(coin, size = 1)
    while (flip == 'heads') {
      pot <- pot * 2
      flip <- sample(coin, size = 1)
    }
    winnings <- winnings + pot - bet
  }
  winnings / x
}