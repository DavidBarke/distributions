drag_to_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::column(
      width = 3,
      drop_zone(
        id = ns("drag_to"),
        label = "Active Distributions"
      )
    ),
    sortable::sortable_js(
      css_id = ns("drag_to"),
      options = sortable::sortable_options(
        group = list(
          name = "group",
          pull = TRUE,
          put = TRUE
        ),
        onSort = sortable::sortable_js_capture_input(ns("distributions"))
      )
    )
  )
}

drag_to_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      active_distribution_indices_r <- shiny::reactive({
        as.integer(input$distributions %||% integer())
      })

      return_list <- list(
        active_distribution_indices_r = active_distribution_indices_r
      )

      return(return_list)
    }
  )
}
