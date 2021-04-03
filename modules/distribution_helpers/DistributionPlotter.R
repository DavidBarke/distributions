DistributionPlotter <- R6::R6Class(
  classname = "DistributionPlotter",
  public = list(
    initialize = function(distribution_helper) {
      private$distribution_helper <- distribution_helper
    },

    plot = function(distributions,
                    type = c("d", "p", "s", "h", "ch"),
                    limits = c(-5, 5)
    ) {
      type <- match.arg(type)

      p <- plotly::plot_ly(type = "scatter", mode = "lines")

      for (distribution in distributions) {
        p <- private$add_dist_trace(
          p = p,
          distribution = distribution,
          type = type,
          limits = limits
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
                              limits = c(0, 10)
    ) {
      type <- match.arg(type)

      discrete <- private$distribution_helper$is_discrete(distribution)
      support <- private$distribution_helper$dist_to_support(distribution)

      # Determine plotting positions on x axis
      x <- get_trace_x(
        distribution = distribution,
        limits = limits,
        support = support,
        discrete = discrete
      )

      y <- get_trace_y(
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
    }
  )
)
