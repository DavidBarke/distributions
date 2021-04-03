get_trace_x <- function(distribution,
                        limits = c(0, 1),
                        support = c(-Inf, Inf),
                        discrete = FALSE, n = 200,
                        alpha = 1e-16
) {
  x_seq_start <- max(limits[1], support[1])
  x_seq_end <- min(limits[2], support[2])

  if (discrete) {
    x_seq_start <- ceiling(x_seq_start)

    x_seq_end <- min(
      floor(x_seq_end),
      quantile(distribution, 1 - alpha)
    )

    if (x_seq_start > x_seq_end) return(numeric())
    x_seq_start:x_seq_end
  } else {
    x_seq_start_max <- max(
      x_seq_start,
      quantile(distribution, alpha)
    )

    x_seq_end_min <- min(
      x_seq_end,
      quantile(distribution, 1 - alpha)
    )

    x_seq <- seq(x_seq_start_max, x_seq_end_min, length.out = n)

    if (x_seq_start_max > x_seq_start) x_seq <- c(x_seq_start, x_seq)
    if (x_seq_end_min < x_seq_end) x_seq <- c(x_seq, x_seq_end)

    x_seq
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
