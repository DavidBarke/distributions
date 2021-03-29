plot_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("type"),
      label = "Distribution Function",
      choices = c("Density" = "d", "Probability" = "p")
    ),
    plotly::plotlyOutput(
      outputId = ns("plot")
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
          limits = c(-5, 5)
        )
      })
    }
  )
}
