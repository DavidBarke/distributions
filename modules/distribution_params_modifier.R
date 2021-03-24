distribution_params_modifier_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("params")
  )
}

distribution_params_modifier_server <- function(id, .values, distribution_id_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      param_names_r <- shiny::reactive({
        distributions$params[[distribution_id_r()]]
      })

      output$params <- shiny::renderUI({
        purrr::map2(
          names(param_names_r()), seq_along(param_names_r()),
          function(name, index) {
            shiny::numericInput(
              inputId = ns("param" %_% index),
              label = htmltools::HTML(name),
              value = 1
            )
          }
        )
      })
    }
  )
}
