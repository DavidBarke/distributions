body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        drop_zone(
          id = ns("drag_from"),
          label = "Inactive Distributions",
          purrr::map2(seq_len(size), color_scale(seq_len(size)), ~ {
            distribution_box_ui(
              id = ns("distribution" %_% .x),
              color = .y
            )
          })
        )
      ),
      shiny::column(
        width = 3,
        drop_zone(
          id = ns("drag_to"),
          label = "Active Distributions"
        )
      ),
      shiny::column(
        width = 6
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
    ),
    sortable::sortable_js(
      css_id = ns("drag_to"),
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

body_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      distribution_ui_proxy <- distribution_modifier_ui_proxy_factory(
        id = ns("distribution_modifier")
      )

      purrr::walk(seq_len(size), ~ {
        distribution_box_server(
          id = "distribution" %_% .,
          .values = .values,
          distribution_modifier_return = distribution_modifier_return,
          distribution_modifier_ui_proxy = distribution_ui_proxy
        )
      })

      distribution_modifier_return <- distribution_modifier_server(
        id = "distribution_modifier",
        .values = .values
      )
    }
  )
}
