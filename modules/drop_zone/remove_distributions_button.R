remove_distributions_button_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::span(
    id = ns("remove"),
    class = "distribution-add action-button",
    shiny::icon("times"),
    `data-toggle` = "tooltip",
    title = "Remove all distributions"
  )
}

remove_distributions_button_server <- function(id, .values, status) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      selector <- if (status == "active") paste(.values$active_dz_id, "*") else
        paste(.values$inactive_dz_id, "*")

      update_ids <- if (status == "active") .values$update_active_ids else
        .values$update_inactive_ids

      shiny::observeEvent(input$remove, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = "Remove all distributions",
          footer = htmltools::tagList(
            shiny::actionButton(
              inputId = ns("confirm_remove"),
              label = "Confirm"
            ),
            shiny::modalButton(
              "Dismiss"
            )
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        shiny::removeUI(
          selector = selector,
          multiple = TRUE,
          immediate = TRUE
        )

        update_ids()
      })
    }
  )
}
