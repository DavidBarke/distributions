plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::column(
    width = 6,
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
      })
    }
  )
}
