bin_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    rintrojs::introBox(
      data.step = 3,
      data.intro = "Remove distributions you don't need anymore by dragging them to the bin area.",
      htmltools::div(
        class = "relative",
        htmltools::div(
          class = "bin-drop-zone-overlay",
          shiny::icon("dumpster-fire"),
        ),
        htmltools::div(
          class = "bin-drop-zone",
          id = ns("dz")
        )
      )
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
        onAdd = htmlwidgets::JS("function (e) { this.el.removeChild(e.item); }"),
        direction = "vertical"
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
