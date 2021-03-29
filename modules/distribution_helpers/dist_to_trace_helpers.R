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

get_trace_y = function(distribution, x, type = c("d", "p")) {
  type <- match.arg(type)

  y_fun <- if (type == "d") density else distributional::cdf

  y_fun(distribution, x)
}
