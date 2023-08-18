#' Calculate vector of compensator values for a Homogeneous Poisson process
#'
#' Calculate the vector of compensator values (tau values) for a Homogeneous
#' Poisson process based on the provided event times and estimated intensity parameter.
#'
#' @param event_times A numeric vector containing the event times of the Homogeneous Poisson process.
#' @param lambda_est The estimated intensity parameter (lambda) of the Homogeneous Poisson process.
#' @return A numeric vector containing the compensator values (tau values) calculated from event_times and lambda_est.
#' @examples
#' event_times <- c(0.2, 0.5, 1.0, 1.5, 2.0)
#' lambda_est <- hpp_lambda_hat(event_times)
#' vector_tau <- hpp_tau(event_times, lambda_est)
#' print(vector_tau)

hpp_tau <- function(event_times, lambda_est) {
  n <- length(event_times)
  vector_tau <- numeric(n)
  
  # Compensator value from 0 to first event time
  vector_tau[1] <- lambda_est * event_times[1]  
  
  for (i in 2:n) {
    interval <- event_times[i] - event_times[i-1]
    tau <- lambda_est * interval
    vector_tau[i] <- tau
  }
  
  return(vector_tau)
}
