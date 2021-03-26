distribution_box_ui <- function(id,
                                label = "Distribution",
                                color = "#fff",
                                value = distributional::dist_normal()
) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("distribution_box"),
    class = "flex distribution-box",
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
        input$distribution$distribution_id
      })

      distribution_r <- shiny::reactive({
        param_names <- distribution_helper$get_params(distribution_id_r())
        params <- rep(1, times = length(param_names))
        names(params) <- names(param_names)

        do.call(
          distribution_helper$get_func(distribution_id_r()),
          as.list(params)
        )
      })

      shiny::observeEvent(input$distribution_click, {
        shiny::showModal(shiny::modalDialog(
          title = "Modify Distribution",
          distribution_modifier_ui_proxy(
            current_distribution = distribution_r()
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
    }
  )
}
