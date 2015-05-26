#' Birthday problem
#'
#' This function simulates the probability that, in a set of n randomly
#' chosen people, some pair of them will have the same birthday. The
#' problem was first proposed by Richard von Mises.
#' 
#' @param n The number of people in the group.
#' @param g The number of groups to simulate
#' @export
#' @return A rational approximation of the probability that, in a set of
#' n randomly chosen people, some pair of them will have the same
#' birthday.
#' @examples birthday(23, g = 10000)

birthday <- function(n, g) {
  hits <- 0
  for (i in 1:g) {
    cal <- list()
    for (j in 1:n) {
      day <- round(runif(1, min = 1, max = 365))
      if (!is.na(match(day, cal))) {
        hits <- hits + 1
        break
      }
      cal <- c(cal, day)
    }
  }
  hits / g
}