#' Calculate the negative log-likelihood for a Hawkes process
#'
#' Calculate the negative log-likelihood of a Hawkes process based on the provided
#' parameter vector and event times.
#'
#' @param vec A numeric vector containing the parameter values (lambda0, alpha, beta).
#' @param events A numeric vector containing the event times of the Hawkes process.
#' @param end The end time of the Hawkes process. Defaults to the maximum event time.
#' @return The negative log-likelihood value calculated from the given parameter vector and event times.
#' @examples
#' vec <- c(0.1, 0.45, 0.5)
#' events <- c(0.2, 0.5, 1.0, 1.5, 2.0)
#' -loglik <- negloglike_hp(vec, events, end = max(events))
#' print(-loglik)

negloglike_hp <- function(vec, events, end = max(events)) {
  lambda0 <- vec[1]
  alpha <- vec[2]
  beta <- vec[3]
  t <- sort(events)
  r <- rep(0, length(t))
  
  # Use recursion to calculate the r values
  for(i in 2:length(t)) {
    r[i] <- exp(-beta * (t[i] - t[i-1])) * (1 + r[i-1])
  }
  
  # Calculate the negative log-likelihood
  loglik <- -tail(t, 1) * lambda0
  loglik <- loglik + alpha/beta * sum(exp(-beta * (tail(t, 1) - t)) - 1)
  loglik <- loglik + sum(log(lambda0 + alpha * r))
  
  return(-loglik)
}
