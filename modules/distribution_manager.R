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

distribution_manager_server <- function(id,
                                        .values,
                                        add_r,
                                        active_distribution_ids_r,
                                        inactive_distribution_ids_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      scale_size <- 20
      color_scale <- scales::col_numeric(palette(), c(0, scale_size - 1))

      envir <- new.env()
      envir$dist_return <- list()
      names(envir$dist_return) <- character()
      # This counter is incremented every time a distribution is added.
      # It keeps track of all ever available distributions within a session
      # but is not adjusted when a distribution is removed
      distribution_counter_rv <- shiny::reactiveVal(0)

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
        index <- distribution_counter_rv() + 1
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

        envir$dist_return[[as.character(index)]] <- distribution_box_server(
          id = "distribution" %_% index,
          .values = .values,
          distribution_modifier_return = distribution_modifier_return,
          distribution_modifier_ui_proxy = distribution_modifier_ui_proxy
        )

        distribution_counter_rv(index)
      }, priority = 1)

      active_distributions_r <- shiny::reactive({
        active_returns <- envir$dist_return[active_distribution_ids_r()]

        distributions <- purrr::map(
          active_returns, function(x) {
            x$distribution_r()
          }
        )

        do.call(c, distributions)
      })

      inactive_distributions_r <- shiny::reactive({
        inactive_returns <- envir$dist_return[inactive_distribution_ids_r()]

        distributions <- purrr::map(
          inactive_returns, function(x) {
            x$distribution_r()
          }
        )

        do.call(c, distributions)
      })

      return_list <- list(
        active_distributions_r = active_distributions_r,
        inactive_distributions_r = inactive_distributions_r
      )

      return(return_list)
    }
  )
}
