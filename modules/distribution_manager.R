distribution_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

distribution_manager_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
