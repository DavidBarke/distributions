DistributionPlotter <- R6::R6Class(
  classname = "DistributionPlotter",
  public = list(
    initialize = function(distribution_helper) {
      private$distribution_helper <- distribution_helper
    },

    plot = function(distributions,
                    type = c("d", "p", "s", "h", "ch"),
                    limits = c(-5, 5),
                    n = 100,
                    shared_x = FALSE
    ) {
      type <- match.arg(type)

      p <- plotly::plot_ly(type = "scatter", mode = "lines")

      for (distribution in distributions) {
        p <- private$add_dist_trace(
          p = p,
          distribution = distribution,
          type = type,
          limits = limits,
          n = n,
          shared_x = shared_x
        )
      }

      p %>% private$add_layout(
        type = type,
        limits = limits
      )
    }
  ),
  private = list(
    distribution_helper = NULL,

    add_dist_trace = function(p,
                              distribution,
                              type = c("d", "p", "s", "h", "ch"),
                              limits = c(0, 10),
                              n = 100,
                              shared_x = FALSE
    ) {
      type <- match.arg(type)

      discrete <- private$distribution_helper$is_discrete(distribution)
      support <- private$distribution_helper$dist_to_support(distribution)

      # Determine plotting positions on x axis
      x <- if (shared_x) {
        seq(limits[1], limits[2], length.out = n)
      } else {
        private$get_trace_x(
          distribution = distribution,
          limits = limits,
          support = support,
          discrete = discrete,
          n = n
        )
      }

      y <- private$get_trace_y(
        distribution = distribution,
        x = x,
        type = type
      )

      plotly::add_trace(
        p,
        x = x,
        y = y,
        color = I(distribution$color),
        type = if (discrete) "bar" else "scatter",
        mode = if (discrete) NULL else "lines",
        name = private$distribution_helper$dist_to_trace_name(distribution)
      )
    },

    add_layout = function(p, type, limits) {
      xaxis <- list(
        title = list(
          text = "x"
        ),
        range = limits
      )

      yaxis <- list(
        title = list(
          text = switch(
            type,
            "d" = "f(x)",
            "p" = "F(x)",
            "s" = "S(x)",
            "h" = "\u03BB(x)",
            "ch" = "\u039B(x)"
          )
        )
      )

      plotly::layout(
        p = p,
        xaxis = xaxis,
        yaxis = yaxis
      )
    },

    get_trace_x = function(distribution,
                           limits = c(0, 1),
                           support = c(-Inf, Inf),
                           discrete = FALSE,
                           n = 200,
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
    },

    get_trace_y = function(distribution, x, type) {
      id <- private$distribution_helper$dist_to_id(distribution)

      dist_func <- private$distribution_helper$get_distribution_func(
        id = id,
        type = type
      )

      dist_func(distribution, x)
    }
  )
)
