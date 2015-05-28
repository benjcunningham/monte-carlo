#' 100 prisoners problem
#'
#' This function simulates the probability that a group of 100
#' prisoners are able to find each of their own numbers in one of 100
#' drawers by following this algorithm:
#' 
#' 1. A prisoner first opens the drawer with her own number.
#' 2. If the drawer contains her number, she is finished.
#' 3. Otherwise, the drawer contains another number and the prisoner
#' opens the drawer with that number next.
#' 4. The prisoner repeats steps 2 and 3 until she finds her number
#' or has opened n / 2 drawers.
#' 
#' The problem was first proposed by Peter Bro Miltersen.
#' 
#' @param sims The number of survival situations to simulate.
#' @param pris The number of prisoners.
#' @param trys The ratio of attempts each prisoner gets relative to the
#' number of drawers.
#' @export
#' @return A rational approximation of the probability that a the group
#' of prisoners will find each of their own numbers.
#' @examples prisoners(1000)

prisoners <- function(sims, pris = 100, trys = 0.5) {
  deaths <- 0
  for (i in 1:sims) {
    keys <- 1:pris
    drawers <- sample(keys)
    for (p in 1:pris) {
      found <- FALSE
      n <- p
      for (attempt in 1:(pris * trys)) {
        if (p == drawers[n]) {
          found <- TRUE
          break
        }
        n <- drawers[n]
      }
      if (found == FALSE) {
        deaths <- deaths + 1
        break
      }
    }
  }
  1 - deaths / sims
}