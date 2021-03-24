distribution_modifier_ui <- function(id, current_distribution) {
  ns <- shiny::NS(id)

  print(current_distribution)

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("distribution"),
      label = "Distribution",
      choices = distributions$choices,
      selected = distributions$distribution_to_id(current_distribution)
    )
  )
}

distribution_modifier_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      return_list <- list(
        distribution_id_r = shiny::reactive(input$distribution)
      )

      return(return_list)
    }
  )
}
