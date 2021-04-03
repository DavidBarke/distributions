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
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        n_x_ui(
          id = ns("n_x")
        )
      ),
      shiny::column(
        width = 6,
        shared_x_ui(
          id = ns("shared_x")
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

      output$plot <- plotly::renderPlotly({
        distribution_helper$plot_dists(
          distributions = distributions_r(),
          input$type,
          limits = x_limits_return$limits_r(),
          n = n_x_return$n_r(),
          shared_x = shared_x_return$shared_x_r()
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

      shared_x_return <- shared_x_server(
        id = "shared_x",
        .values = .values
      )
    }
  )
}
