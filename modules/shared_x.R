shared_x_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::checkboxInput(
    inputId = ns("shared_x"),
    label = htmltools::tagList(
      "Share x values",
      bs4Dash::popover(
        shiny::actionLink(
          inputId = ns("info"),
          label = NULL,
          icon = shiny::icon("info-circle")
        ),
        title = "Share x values",
        content = "If checked all continuous distribution functions are evaluated at the same x values. Otherwise x values are calculated for each continuous distribution function separately by taking into account specific quantiles."
      )
    )
  )
}

shared_x_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      return_list <- list(
        shared_x_r = shiny::reactive(input$shared_x)
      )
    }
  )
}
