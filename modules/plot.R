plot_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    title = "Visualization",
    solidHeader = TRUE,
    status = "primary",
    shiny::selectInput(
      inputId = ns("type"),
      label = htmltools::tagList(
        "Distribution Function",
        popover_2(
          tag = shiny::actionLink(
            inputId = ns("type_info"),
            label = NULL,
            icon = shiny::icon("info-circle")
          ),
          title = "Distribution Functions",
          content = htmltools::tagList(
            dist_func_row(
              htmltools::tags$b("Name"),
              htmltools::tags$b("Definition")
            ),
            withMathJax(dist_func_row(
              "Probability Density Function",
              "$$f_{X}(x) = P(X = x)$$"
            )),
            dist_func_row(
              "Cumulative Distribution Function",
              "$$F_{X}(x) = \\int_{-\\infty}^{x} f(x) \\, \\mathrm{d}x = P (X \\le x)$$"
            ),
            dist_func_row(
              "Survival Function",
              "$$S_{X}(x) = \\int_{x}^{\\infty} f(x) \\, \\mathrm{d}x = P (X \\ge x)$$"
            ),
            dist_func_row(
              "Hazard Function",
              "$$\\lambda_{X}(x) = \\tfrac{f(x)}{S(x)}$$"
            ),
            dist_func_row(
              "Cumulative Hazard Function",
              "$$\\Lambda_{X}(x) = - \\log\\big(S(x)\\big)$$"
            )
          )
        )
      ),
      choices = distribution_helper$get_func_choices()
    ),
    plotly::plotlyOutput(
      outputId = ns("plot")
    ),
    x_limits_ui(
      id = ns("x_limits")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        n_x_ui(
          id = ns("n_x")
        )
      )
    )
  )
}

plot_server <- function(id, .values, distributions_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$type_info, {
        # Typeset MathJax for popover
        session$sendCustomMessage("mathjax-typeset", TRUE)
      })

      output$plot <- plotly::renderPlotly({
        distribution_helper$plot_dists(
          distributions = distributions_r(),
          input$type,
          limits = x_limits_return$limits_r(),
          n = n_x_return$n_r(),
          shared_x = FALSE # Currently there is no added value by shared_x = T
        )
      })

      x_limits_return <- x_limits_server(
        id = "x_limits",
        .values = .values
      )

      n_x_return <- n_x_server(
        id = "n_x",
        .values = .values
      )
    }
  )
}
