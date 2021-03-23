distribution_box_ui <- function(id, label = "Distribution") {
  ns <- shiny::NS(id)

  box <- bs4Dash::box(
    width = 12,
    collapsible = FALSE,
    title = htmltools::div(
      class = "flex",
      label,
      color_input(
        inputId = ns("color")
      )
    ),
    shiny::selectInput(
      inputId = ns("distribution"),
      label = NULL,
      choices = distributions()
    )
  )

  box$attribs$class <- paste(box$attribs$class, "small-card")

  box
}

distribution_box_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$color, {
        print(input$color)
      })
    }
  )
}
