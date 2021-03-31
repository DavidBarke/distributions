x_limits_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::numericInput(
        inputId = ns("min"),
        label = "Min",
        value = -5,
        step = 1
      )
    ),
    shiny::column(
      width = 6,
      shiny::numericInput(
        inputId = ns("max"),
        label = "Max",
        value = 5,
        step = 1
      )
    )
  )
}

x_limits_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      limits_r <- shiny::reactive({
        sort(c(shiny::req(input$min), shiny::req(input$max)))
      })

      return_list <- list(
        limits_r = limits_r
      )
    }
  )
}
