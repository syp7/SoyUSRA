#' Calculate vector of compensator values for a Hawkes process
#'
#' Calculate the vector of compensator values (tau values) for a Hawkes process based
#' on the provided parameter vector and event times.
#'
#' @param vec A numeric vector containing the parameter values (lambda0, alpha, beta).
#' @param events A numeric vector containing the event times of the Hawkes process.
#' @param end The end time of the Hawkes process. Defaults to the maximum event time.
#' @return A numeric vector containing the compensator values calculated from the given parameter vector and event times.
#' @examples
#' vec <- c(0.1, 0.45, 0.5)
#' events <- c(0.2, 0.5, 1.0, 1.5, 2.0)
#' comp <- compensator_hp(vec, events, end = max(events))
#' print(comp)

compensator_hp <- function(vec, events, end = max(events)) {
  lambda0 <- vec[1]
  alpha <- vec[2]
  beta <- vec[3]
  t <- sort(events)
  r <- rep(0, length(t))
  comp <- numeric(length(t))
  comp[1] <- lambda0 * t[1]
  
  # Use recursion to calculate the r values and the compensator
  for(i in 2:length(t)) {
    r[i] <- exp(-beta * (t[i] - t[i-1])) * (1 + r[i-1])
    comp[i] <- lambda0 * (t[i] - t[i-1]) - (alpha / beta) * (r[i] - r[i-1] - 1)
  }
  
  return(comp)
}
