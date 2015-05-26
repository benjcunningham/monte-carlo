#' Buffon's needle
#' 
#' This function simulates the mathematics problem, Buffon's needle,
#' proposed by Georges-Louis Leclerc, Comte de Buffon.
#' 
#' @param drops The number of needle drops to simulate.
#' @param w The width of the boards or distance between lines.
#' @param n The length of the needle. 
#' @export
#' @return A rational approximation of π, calculated as drops / hits.
#' @examples buffons_needle(10000)

buffons_needle <- function(drops, w = 2, n = 1) {
  hits <- 0
  for (i in 1:drops) {
    d <- runif(1, min = 0, max = w / 2)
    theta <- runif(1, min = 0, max = pi)
    if (d <= 0.5 * n * sin(theta)) {
      hits <- hits + 1
    }
  }
  drops / hits
}