#' Generate event times from a homogeneous Poisson process
#'
#' Generate event times from a homogeneous Poisson process with a specified 
#' number of events and event rate.
#'
#' @param num_events The number of events to generate.
#' @param rate The event rate for the Poisson process.
#' @importFrom stats rexp
#' @return A numeric vector containing the generated event times.
#' @examples
#' event_times <- hpp_event_times(num_events = 100, rate = 0.2)
#' print(event_times)

hpp_event_times <- function(num_events, rate) {
  # Function implementation...
  inter_event <- rexp(num_events, rate = rate)
  event_times <- cumsum(inter_event)
  return(event_times)
}
