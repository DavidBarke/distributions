DistributionPlotter <- R6::R6Class(
  classname = "DistributionPlotter",
  public = list(
    initialize = function(distribution_helper) {
      private$distribution_helper <- distribution_helper
    },

    plot = function(distributions,
                    type = c("d", "p", "q", "s", "h", "ch"),
                    limits = c(-5, 5),
                    n = 100
    ) {
      type <- match.arg(type)

      p <- plotly::plot_ly(type = "scatter", mode = "lines")

      for (i in seq_along(distributions)) {
        distribution <- distributions[[i]]
        p <- private$add_dist_trace(
          id = i,
          p = p,
          distribution = distribution,
          type = type,
          limits = limits,
          n = n
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
                              id,
                              distribution,
                              type = c("d", "p", "q", "s", "h", "ch"),
                              limits = c(0, 10),
                              n = 100
    ) {
      type <- match.arg(type)

      discrete <- private$distribution_helper$is_discrete(distribution)
      support <- private$distribution_helper$dist_to_support(distribution)

      # Determine plotting positions on x axis
      x <- if (type == "q") {
        if (discrete) {
          q <- private$get_discrete_x(distribution, min = 0, max = Inf)
          distributional::cdf(distribution, q)
        } else {
          seq(0, 1, length.out = n)
        }
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

      if (type == "q" && discrete) {
        name <-  private$distribution_helper$dist_to_trace_name(distribution)
        color <- I(distribution$color)

        p %>% plotly::add_segments(
          x = c(0, x),
          y = c(y, y[length(y)]),
          xend = c(x, 1),
          yend = c(y, y[length(y)]),
          color = color,
          name = name,
          legendgroup = id,
          hoverinfo = "skip"
        ) %>% plotly::add_trace(
          x = x,
          y = y,
          type = "scatter",
          mode = "markers",
          marker = list(
            symbol = "circle-open"
          ),
          color = color,
          showlegend = FALSE,
          legendgroup = id,
          name = name
        ) %>% plotly::add_trace(
          x = x,
          y = c(y[-1], y[length(y)]),
          type = "scatter",
          mode = "markers",
          marker = list(
            symcol = "circle"
          ),
          color = color,
          showlegend = FALSE,
          legendgroup = id,
          name = name
        )
      } else {
        plotly::add_trace(
          p,
          x = x,
          y = y,
          color = I(distribution$color),
          type = if (discrete) "bar" else "scatter",
          mode = if (discrete) NULL else "lines",
          name = private$distribution_helper$dist_to_trace_name(distribution)
        )
      }
    },

    add_layout = function(p, type, limits) {
      xaxis <- list(
        title = list(
          text = if (type == "q") "p" else "x"
        ),
        range = limits
      )

      yaxis <- list(
        title = list(
          text = switch(
            type,
            "d" = "f(x)",
            "p" = "F(x)",
            "q" = "Q(p)",
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

    get_discrete_x = function(distribution, min, max, alpha = 1e-16) {
      x_seq_start <- ceiling(min)

      x_seq_end <- min(
        floor(max),
        quantile(distribution, 1 - alpha)
      )

      if (x_seq_start > x_seq_end) return(numeric())

      x_seq_start:x_seq_end
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
        private$get_discrete_x(
          distribution = distribution,
          min = x_seq_start,
          max = x_seq_end,
          alpha = alpha
        )
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
