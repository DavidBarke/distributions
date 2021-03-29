bin_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::div(
      class = "drop-zone drop-zone-trash",
      htmltools::h3(
        class = "drop-zone-title",
        shiny::icon("trash")
      ),
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
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    )
  )
}

bin_drop_zone_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
