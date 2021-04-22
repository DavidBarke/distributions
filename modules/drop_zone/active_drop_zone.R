active_drop_zone_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bs4Dash::box(
      width = NULL,
      collapsible = FALSE,
      solidHeader = TRUE,
      status = "primary",
      title = htmltools::tagList(
        "Active Distributions",
        popover_2(
          tag = shiny::actionLink(
            inputId = ns("title_info"),
            label = NULL,
            icon = shiny::icon("info-circle")
          ),
          title = "Active Distributions",
          content = htmltools::tagList(
            htmltools::p(
              "Drag an active distribution to the inactive distribution area to remove it from the visualization."
            ),
            htmltools::hr(),
            htmltools::p(
              "Modify a distribution by clicking on its type or its parameters."
            )
          )
        )
      ),
      htmltools::div(
        id = ns("dz")
      )
    ) %>% append_card_tools_right(
      remove_distributions_button_ui(
        id = ns("remove_distributions_button")
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

      remove_distributions_button_server(
        id = "remove_distributions_button",
        .values = .values,
        status = "active"
      )

      return_list <- list(
        distribution_ids_r = distribution_ids_r
      )

      return(return_list)
    }
  )
}
