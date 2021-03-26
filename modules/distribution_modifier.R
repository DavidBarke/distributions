distribution_modifier_ui <- function(id, current_distribution) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("distribution"),
      label = "Distribution",
      choices = distribution_helper$get_choices(),
      selected = distribution_helper$dist_to_id(current_distribution)
    ),
    distribution_params_modifier_ui(
      id = ns("distribution_params")
    )
  )
}

distribution_modifier_ui_proxy <- function(current_distribution) {
  distribution_modifier_ui(
    id = "distribution_modifier",
    current_distribution = current_distribution
  )
}

distribution_modifier_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      distribution_id_r <- shiny::reactive({
        input$distribution
      })

      params_return <- distribution_params_modifier_server(
        id = "distribution_params",
        .values = .values,
        distribution_id_r = distribution_id_r
      )

      distribution_r <- shiny::reactive({
        do.call(
          what = distribution_helper$get_func(distribution_id_r()),
          args = params_return$params_r()
        )
      })

      return_list <- list(
        error_r = params_return$error_r,
        distribution_r = distribution_r
      )

      return(return_list)
    }
  )
}
