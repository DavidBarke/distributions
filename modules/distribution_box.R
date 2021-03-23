distribution_box_ui <- function(id, label = "Distribution", color = "#fff") {
  ns <- shiny::NS(id)

  box <- bs4Dash::box(
    width = 12,
    collapsible = FALSE,
    title = htmltools::div(
      class = "flex",
      shiny::selectInput(
        inputId = ns("distribution"),
        label = NULL,
        choices = distributions$choices
      ),
      color_input(
        inputId = ns("color"),
        value = color
      )
    ),
    shiny::uiOutput(
      outputId = ns("params"),
      class = "flex"
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

      output$params <- shiny::renderUI({
        params <- distributions$params[[input$distribution]]

        purrr::map(names(params), ~ {
          distribution_param(
            name = htmltools::HTML(.),
            value = 1
          )
        })
      })
    }
  )
}
