bin_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::div(
      class = "bin-drop-zone",
      shiny::icon("trash"),
      id = ns("dz")
    ),
    sortable::sortable_js(
      css_id = ns("dz"),
      options = sortable::sortable_options(
        ghostClass = "sortable-ghost-hidden",
        group = list(
          name = "group",
          pull = TRUE,
          put = TRUE
        ),
        onAdd = htmlwidgets::JS("function (e) { console.log('onAdd');this.el.removeChild(e.item); }")
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
