#' Estimate the intensity parameter of a Homogeneous Poisson process
#'
#' Estimate the intensity parameter (lambda) of a Homogeneous Poisson process
#' based on the provided event times.
#'
#' @param event_times A numeric vector containing the event times of the Homogeneous Poisson process.
#' @importFrom stats any
#' @return The estimated intensity parameter (lambda) of the Homogeneous Poisson process.
#' @examples
#' event_times <- c(0.2, 0.5, 1.0, 1.5, 2.0)
#' lambda_est <- hpp_lambda_hat(event_times)
#' print(lambda_est)

hpp_lambda_hat <- function(event_times) {
  # Check for empty or negative event times
  if (length(event_times) == 0) {
    stop("Error: event_times should be a non-empty numeric vector.")
  }
  if (any(event_times < 0)) {
    stop("Error: event_times should be a vector of positive values.")
  }
  
  # Estimate intensity parameter
  num_events <- length(event_times)
  lambda_hat <- num_events / event_times[length(event_times)]
  return(lambda_hat)
}
