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

distribution_modifier_ui_proxy_factory <- function(id) {
  force(id)
  function(current_distribution) {
    distribution_modifier_ui(
      id = id,
      current_distribution = current_distribution
    )
  }
}

distribution_modifier_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      current_distribution_rv <- shiny::reactiveVal(NULL)

      distribution_id_r <- shiny::reactive({
        shiny::req(input$distribution)
      })

      params_return <- distribution_params_modifier_server(
        id = "distribution_params",
        .values = .values,
        distribution_id_r = distribution_id_r,
        current_distribution_rv = current_distribution_rv
      )

      distribution_r <- shiny::reactive({
        do.call(
          what = distribution_helper$get_func(distribution_id_r()),
          args = params_return$params_r()
        )[[1]]
      })

      return_list <- list(
        error_r = params_return$error_r,
        distribution_r = distribution_r,
        # current_distribution_rv has to be set before the modal dialog is
        # displayed in order to get the selected values for the params
        current_distribution_rv = current_distribution_rv
      )

      return(return_list)
    }
  )
}
