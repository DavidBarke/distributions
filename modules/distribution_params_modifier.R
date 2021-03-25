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
        as.character(distribution_helper$get_params(distribution_id_r()))
      })

      param_ids_r <- shiny::reactive({
        names(distribution_helper$get_params(distribution_id_r()))
      })

      output$params <- shiny::renderUI({
        purrr::map2(
          param_names_r(), seq_along(param_names_r()),
          function(name, index) {
            shiny::numericInput(
              inputId = ns("param" %_% index),
              label = htmltools::HTML(name),
              value = 1
            )
          }
        )
      })

      params_r <- shiny::reactive({
        x <- purrr::map2(
          param_names_r(), seq_along(param_names_r()),
          function(name, index) {
            input[["param" %_% index]]
        })

        names(x) <- param_ids_r()
        x
      })

      return_list <- list(
        params_r = params_r
      )

      return(return_list)
    }
  )
}
