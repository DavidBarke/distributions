drag_from_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::column(
      width = 3,
      drop_zone(
        id = ns("drag_from"),
        label = htmltools::tagList(
          "Inactive Distributions",
          htmltools::span(
            id = ns("add"),
            class = "distribution-add action-button",
            shiny::icon("plus")
          )
        ),
        ...
      )
    ),
    sortable::sortable_js(
      css_id = ns("drag_from"),
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

drag_from_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$drag_from_id <- paste0("#", ns("drag_from"))

      return_list <- list(
        add_r = shiny::reactive(input$add)
      )

      return(return_list)
    }
  )
}
