inactive_drop_zone_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bs4Dash::box(
      width = NULL,
      collapsible = FALSE,
      solidHeader = TRUE,
      status = "primary",
      title = htmltools::tagList(
        "Inactive Distributions",
        popover_2(
          tag = shiny::actionLink(
            inputId = ns("title_info"),
            label = NULL,
            icon = shiny::icon("info-circle")
          ),
          title = "Inactive Distributions",
          content = htmltools::tagList(
            htmltools::p(
              "Drag an inactive distribution to the active distribution area to visualize it."
            ),
            htmltools::hr(),
            htmltools::p(
              "Add a new distribution by clicking the",
              shiny::icon("plus"),
              "icon."
            ),
            htmltools::hr(),
            htmltools::p(
              "Modify a distribution by clicking on its type or its parameters."
            )
          )
        ),
        htmltools::span(
          id = ns("add"),
          class = "distribution-add action-button",
          shiny::icon("plus")
        )
      ),
      htmltools::div(
        id = ns("dz")
      )
    ) %>% append_card_tools_right(
      remove_distributions_button_ui(
        id = ns("remove_distributions_button")
      )
    ) %>%first_child(),
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

inactive_drop_zone_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$inactive_dz_id <- paste0("#", ns("dz"))

      .values$update_inactive_ids <- update_sortable_factory(
        inputId = ns("distribution_ids"),
        selector = .values$inactive_dz_id
      )

      distribution_ids_r <- shiny::reactive({
        input$distribution_ids %||% character()
      })

      remove_distributions_button_server(
        id = "remove_distributions_button",
        .values = .values,
        status = "inactive"
      )

      return_list <- list(
        distribution_ids_r = distribution_ids_r
      )

      return_list <- list(
        add_r = shiny::reactive(input$add),
        distribution_ids_r = distribution_ids_r
      )

      return(return_list)
    }
  )
}
