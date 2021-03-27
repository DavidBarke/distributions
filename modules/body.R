body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::fluidRow(
      drag_from_ui(
        id = ns("drag_from"),
        # distribution_manager displays initial distributions, further
        # distributions are inserted to drag_from
        distribution_manager_ui(
          id = ns("distribution_manager")
        )
      ),
      drag_to_ui(
        id = ns("drag_to")
      ),
      shiny::column(
        width = 6
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
        add_r = drag_from_return$add_r
      )

      drag_from_return <- drag_from_server(
        id = "drag_from",
        .values = .values
      )

      drag_to_return <- drag_to_server(
        id = "drag_to",
        .values = .values
      )

      active_distributions_r <- shiny::reactive({
        distribution_manager_return$distributions_r()[
          drag_to_return$active_distribution_indices_r()
        ]
      })

      shiny::observe({
        print(active_distributions_r())
      })
    }
  )
}
