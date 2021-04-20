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
            dist_func_row(
              "Probability Density Function",
              "$$f_{X}(x) = P(X = x)$$"
            ),
            dist_func_row(
              "Cumulative Distribution Function",
              "$$F_{X}(x) = \\int_{-\\infty}^{x} f_{X}(x) \\, \\mathrm{d}x = P (X \\le x)$$"
            ),
            dist_func_row(
              "Quantile Function",
              "$$Q_{X}(p) = \\mathrm{inf}\\{x \\in \\mathbb{R} : p \\le F_{X}(x)\\} = F_{X}^{-1}(p)$$"
            ),
            dist_func_row(
              "Survival Function",
              "$$S_{X}(x) = \\int_{x}^{\\infty} f_{X}(x) \\, \\mathrm{d}x = P (X \\ge x)$$"
            ),
            dist_func_row(
              "Hazard Function",
              "$$\\lambda_{X}(x) = \\tfrac{f_{X}(x)}{S_{X}(x)}$$"
            ),
            dist_func_row(
              "Cumulative Hazard Function",
              "$$\\Lambda_{X}(x) = - \\log\\big(S_{X}(x)\\big)$$"
            )
          ),
          `data-template` = '
          <div class="popover wide-popover" role="tooltip">
            <div class="arrow"></div>
            <h3 class="popover-header"></h3>
            <div class="popover-body"></div>
          </div>'
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

      type_r <- shiny::reactive({
        input$type
      })

      output$plot <- plotly::renderPlotly({
        distribution_helper$plot_dists(
          distributions = distributions_r(),
          type_r(),
          limits = x_limits_return$limits_r(),
          n = n_x_return$n_r()
        )
      })

      x_limits_return <- x_limits_server(
        id = "x_limits",
        .values = .values,
        type_r = type_r
      )

      n_x_return <- n_x_server(
        id = "n_x",
        .values = .values
      )
    }
  )
}
