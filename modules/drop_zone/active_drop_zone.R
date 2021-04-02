active_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bs4Dash::box(
      width = NULL,
      collapsible = FALSE,
      solidHeader = TRUE,
      status = "primary",
      title = "Active Distributions",
      htmltools::div(
        id = ns("dz")
      )
    ) %>% first_child(),
    sortable::sortable_js(
      css_id = ns("dz"),
      options = sortable::sortable_options(
        group = list(
          name = "group",
          pull = TRUE,
          put = TRUE
        ),
        onSort = sortable::sortable_js_capture_input(ns("distribution_ids"))
      )
    )
  )
}

active_drop_zone_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$active_dz_id <- paste0("#", ns("dz"))

      .values$update_active_ids <- update_sortable_factory(
        inputId = ns("distribution_ids"),
        selector = .values$active_dz_id
      )

      distribution_ids_r <- shiny::reactive({
        input$distribution_ids %||% character()
      })

      return_list <- list(
        distribution_ids_r = distribution_ids_r
      )

      return(return_list)
    }
  )
}
