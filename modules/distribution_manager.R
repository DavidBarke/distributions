distribution_manager_ui <- function(id, initial_size = 25) {
  ns <- shiny::NS(id)

  color_scale <- scales::col_numeric(palette(), c(1, initial_size))

  purrr::imap(color_scale(initial_size:1), function(color, index) {
    distribution_box_ui(
      id = ns("distribution" %_% index),
      color = color,
      index = index,
      value = distribution_helper$get_random_distribution()
    )
  })
}

distribution_manager_server <- function(id, .values, add_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      scale_size <- 20
      color_scale <- scales::col_numeric(palette(), c(0, scale_size - 1))

      distribution_indices_rv <- shiny::reactiveVal(0)

      envir <- new.env()
      envir$dist_return <- list()

      # One distribution_modifier for all distribution_boxes
      distribution_modifier_ui_proxy <- distribution_modifier_ui_proxy_factory(
        id = ns("distribution_modifier")
      )

      distribution_modifier_return <- distribution_modifier_server(
        id = "distribution_modifier",
        .values = .values
      )

      # New distribution_boxes
      shiny::observeEvent(add_r(), {
        index <- distribution_indices_rv() + 1
        shiny::insertUI(
          selector = .values$inactive_dz_id,
          where = "afterBegin",
          ui = distribution_box_ui(
            id = ns("distribution" %_% index),
            color = color_scale(index %% scale_size),
            index = index,
            value = distribution_helper$get_random_distribution()
          )
        )

        envir$dist_return[[index]] <- distribution_box_server(
          id = "distribution" %_% index,
          .values = .values,
          distribution_modifier_return = distribution_modifier_return,
          distribution_modifier_ui_proxy = distribution_modifier_ui_proxy
        )

        distribution_indices_rv(index)
      }, priority = 1)

      distributions_r <- shiny::reactive({
        distribution_indices_rv()

        distributions <- purrr::imap(envir$dist_return, function(x, i) {
          x$distribution_r()
        })

        do.call(c, distributions)
      })

      return_list <- list(
        distributions_r = distributions_r
      )

      return(return_list)
    }
  )
}
