get_trace_x <- function(limits = c(0, 1), support = c(-Inf, Inf), discrete = FALSE, n = 100) {
  x_seq_start <- max(limits[1], support[1])
  x_seq_end <- min(limits[2], support[2])

  x_seq <- if (discrete) {
    x_seq_start <- ceiling(x_seq_start)
    x_seq_end <- floor(x_seq_end)
    if (x_seq_start > x_seq_end) return(numeric())
    x_seq_start:x_seq_end
  } else {
    seq(x_seq_start, x_seq_end, length.out = n)
  }
}

get_trace_y = function(distribution, x, type = c("d", "p", "s", "h", "ch")) {
  type <- match.arg(type)

  y_fun <- switch(
    type,
    "d" = stats::density,
    "p" = distributional::cdf,
    "s" = survival_fun,
    "h" = hazard_fun,
    "ch" = cumulative_hazard_fun
  )

  y_fun(distribution, x)
}

survival_fun <- function(d, x) {
  1 - distributional::cdf(d, x)
}

hazard_fun <- function(d, x) {
  stats::density(d, x) / survival_fun(d, x)
}

cumulative_hazard_fun <- function(d, x) {
  -log(survival_fun(d, x))
}
