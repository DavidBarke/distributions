plot_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    title = "Visualization",
    solidHeader = TRUE,
    status = "primary",
    shiny::selectInput(
      inputId = ns("type"),
      label = "Distribution Function",
      choices = distribution_helper$get_func_choices()
    ),
    plotly::plotlyOutput(
      outputId = ns("plot")
    ),
    x_limits_ui(
      id = ns("x_limits")
    )
  )
}

plot_server <- function(id, .values, distributions_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$plot <- plotly::renderPlotly({
        distribution_helper$plot_dists(
          distributions = distributions_r(),
          input$type,
          limits = x_limits_return$limits_r()
        )
      })

      x_limits_return <- x_limits_server(
        id = "x_limits"
      )
    }
  )
}
