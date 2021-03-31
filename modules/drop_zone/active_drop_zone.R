active_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bs4Dash::box(
      width = NULL,
      collapsible = FALSE,
      title = "Active Distributions",
      htmltools::div(
        id = ns("dz")
      )
    ),
    sortable::sortable_js(
      css_id = ns("dz"),
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

active_drop_zone_server <- function(id, .values) {
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
