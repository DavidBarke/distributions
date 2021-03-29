body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bin_drop_zone_ui(
          id = ns("bin_drop_zone")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            inactive_drop_zone_ui(
              id = ns("inactive_drop_zone")
            )
          ),
          shiny::column(
            width = 6,
            active_drop_zone_ui(
              id = ns("active_drop_zone")
            )
          )
        )
      ),
      shiny::column(
        width = 6,
        plot_ui(
          id = ns("plot")
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

      distribution_manager_return <- distribution_manager_server(
        id = "distribution_manager",
        .values = .values,
        add_r = inactive_return$add_r
      )

      inactive_return <- inactive_drop_zone_server(
        id = "inactive_drop_zone",
        .values = .values
      )

      active_return <- active_drop_zone_server(
        id = "active_drop_zone",
        .values = .values
      )

      active_distributions_r <- shiny::reactive({
        distribution_manager_return$distributions_r()[
          active_return$active_distribution_indices_r()
        ]
      })

      shiny::observe({
        print(active_distributions_r())
      })

      plot_server(
        id = "plot",
        .values = .values,
        distributions_r = active_distributions_r
      )
    }
  )
}
