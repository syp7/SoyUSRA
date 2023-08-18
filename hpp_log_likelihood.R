#' Calculate the negative log-likelihood for a Homogeneous Poisson process
#'
#' Calculate the negative log-likelihood of a Homogeneous Poisson process based
#' on the provided intensity parameter and event times.
#'
#' @param lambda The intensity parameter (lambda) of the Homogeneous Poisson process.
#' @param event_times A numeric vector containing the event times of the Homogeneous Poisson process.
#' @return The negative log-likelihood value calculated from the given lambda and event_times.
#' @examples
#' event_times <- c(0.2, 0.5, 1.0, 1.5, 2.0)
#' lambda_est <- hpp_lambda_hat(event_times)
#' -log_likelihood <- hpp_log_likelihood(lambda_est, event_times)
#' print(-log_likelihood)

hpp_log_likelihood <- function(lambda, event_times) {
  n <- length(event_times)
  total_time <- event_times[length(event_times)]
  log_likelihood <- n * log(lambda) - lambda * total_time
  return(-log_likelihood)
}
