dist_to_trace_discrete <- function(distribution, limits) {
  UseMethod("dist_to_trace_discrete")
}

dist_to_trace_continuous <- function(distribution, limits) {
  UseMethod("dist_to_trace_continuous")
}

dist_to_trace_discrete.d <- function(distribution, limits) {
  x <- discrete_x(
    type = "d",
    limits = limits
  )
}

dist_to_trace_discrete.p <- function(distribution, limits) {
  x <- discrete_x(
    type = "p",
    limits = limits
  )
}

dist_to_trace_discrete.q <- function(distribution, limits) {
  x <- discrete_x(
    type = "q",
    limits = limits
  )
}

dist_to_trace_continuous.d <- function(distribution, limits) {
  x <- continuous_x(
    type = "d",
    limits = limits
  )
}

dist_to_trace_continuous.p <- function(distribution, limits) {

}

dist_to_trace_continuous.q <- function(distribution, limits) {

}
