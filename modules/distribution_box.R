distribution_box_ui <- function(id,
                                index,
                                label = "Distribution",
                                color = "#fff",
                                value = distributional::dist_normal()[[1]]
) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("distribution_box"),
    `data-rank-id` = index,
    class = "flex space-between distribution-box",
    htmltools::div(
      class = "flex",
      color_input(
        inputId = ns("color"),
        value = color,
        bg = ".distribution-box"
      ),
      distribution_input(
        inputId = ns("distribution"),
        value = value
      )
    )
    # remove_icon(
    #   inputId = ns("remove")
    # )
  )
}

distribution_box_server <- function(id,
                                    .values,
                                    distribution_modifier_ui_proxy,
                                    distribution_modifier_return
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      distribution_id_r <- shiny::reactive({
        shiny::req(input$distribution)$distribution_id
      })

      distribution_r <- shiny::reactive({
        param_names <- distribution_helper$get_params(distribution_id_r())
        params <- shiny::req(input$distribution)$distribution_param_values
        names(params) <- names(param_names)

        x <- do.call(
          distribution_helper$get_func(distribution_id_r()),
          as.list(params)
        )

        x[[1]]$color <- input$color

        x
      })

      distribution_el_r <- shiny::reactive({
        distribution_r()[[1]]
      })

      shiny::observeEvent(input$distribution_click, {
        distribution_modifier_return$current_distribution_rv(distribution_el_r())

        shiny::showModal(shiny::modalDialog(
          title = "Modify Distribution",
          distribution_modifier_ui_proxy(
            current_distribution = distribution_el_r()
          ),
          footer = htmltools::tagList(
            shiny::uiOutput(
              outputId = ns("confirm_btn")
            ),
            shiny::modalButton(
              label = "Close",
              icon = shiny::icon("times")
            )
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)

      output$confirm_btn <- shiny::renderUI({
        if (!distribution_modifier_return$error_r()) {
          shiny::actionButton(
            inputId = ns("confirm"),
            label = "Confirm",
            icon = shiny::icon("check")
          )
        }
      })

      shiny::observeEvent(input$confirm, {
        shiny::removeModal()

        update_distribution_input(
          inputId = "distribution",
          value = distribution_modifier_return$distribution_r()
        )
      })

      shiny::observeEvent(input$remove, {
        shiny::showModal(shiny::modalDialog(
          title = "Remove distribution",
          footer = htmltools::tagList(
            shiny::actionButton(
              inputId = ns("confirm_remove"),
              label = "Confirm",
              icon = shiny::icon("check")
            ),
            shiny::modalButton(
              label = "Dismiss",
              icon = shiny::icon("times")
            )
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        remove_distribution_input(
          inputId = "distribution"
        )
      })

      return_list <- list(
        distribution_r = distribution_r
      )

      return(return_list)
    }
  )
}
