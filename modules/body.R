body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        class = "overflow-scroll",
        rintrojs::introBox(
          data.step = 1,
          data.intro = "Prepare distributions",
          shiny::fluidRow(
            shiny::column(
              width = 6,
              class = "overflow-scroll",
              inactive_drop_zone_ui(
                id = ns("inactive_drop_zone")
              )
            ),
            shiny::column(
              width = 6,
              class = "overflow-scroll",
              active_drop_zone_ui(
                id = ns("active_drop_zone")
              )
            )
          )
        )
      ),
      shiny::column(
        width = 6,
        rintrojs::introBox(
          data.step = 2,
          data.intro = "Visualize distributions",
          plot_ui(
            id = ns("plot")
          )
        )
      )
    )
  )
}

body_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$distribution_manager <- distribution_manager_server(
        id = "distribution_manager",
        .values = .values,
        add_r = inactive_return$add_r,
        active_distribution_ids_r = active_return$distribution_ids_r,
        inactive_distribution_ids_r = inactive_return$distribution_ids_r
      )

      inactive_return <- inactive_drop_zone_server(
        id = "inactive_drop_zone",
        .values = .values
      )

      active_return <- active_drop_zone_server(
        id = "active_drop_zone",
        .values = .values
      )

      plot_server(
        id = "plot",
        .values = .values,
        distributions_r = .values$distribution_manager$active_distributions_r
      )
    }
  )
}
