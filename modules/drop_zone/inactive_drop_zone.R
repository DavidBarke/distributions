inactive_drop_zone_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    drop_zone(
      id = ns("dz"),
      label = htmltools::tagList(
        "Inactive Distributions",
        htmltools::span(
          id = ns("add"),
          class = "distribution-add action-button",
          shiny::icon("plus")
        )
      ),
      ...
    ),
    sortable::sortable_js(
      css_id = ns("dz"),
      options = sortable::sortable_options(
        group = list(
          name = "group",
          pull = TRUE,
          put = TRUE
        )
      )
    )
  )
}

inactive_drop_zone_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$inactive_dz_id <- paste0("#", ns("dz"))

      return_list <- list(
        add_r = shiny::reactive(input$add)
      )

      return(return_list)
    }
  )
}
