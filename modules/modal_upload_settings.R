modal_upload_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("select")
    ),
    shiny::checkboxInput(
      inputId = ns("override"),
      label = "Override present distributions",
      value = FALSE
    )
  )
}

modal_upload_settings_server <- function(id, .values, content_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      is_distribution_r <- shiny::reactive({
        distributional::is_distribution(content_r())
      })

      output$select <- shiny::renderUI({
        if (is_distribution_r()) {
          text <- glue::glue(
            "Read vector of {n} distribution{if (n > 1) 's' else ''}.",
            n = length(content_r())
          )

          htmltools::tagList(
            htmltools::p(
              text
            ),
            shiny::selectInput(
              inputId = ns("include_as"),
              label = "Include as",
              choices = c(
                "Active" = "active",
                "Inactive" = "inactive"
              )
            )
          )
        } else {
          text <- glue::glue(
            "Read list of {n_active} active distribution{if (n_active > 1) 's' else ''}
            and {n_inactive} inactive distribution{if (n_inactive > 1) 's' else ''}",
            n_active = length(content_r()$active),
            n_inactive = length(content_r()$inactive)
          )

          htmltools::tagList(
            htmltools::p(
              text
            ),
            shiny::checkboxInput(
              inputId = ns("include_active"),
              label = "Include active",
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = ns("include_inactive"),
              label = "Include inactive",
              value = TRUE
            )
          )
        }
      })

      return_list <- list(
        override_r = shiny::reactive(input$override),
        include_active_r = shiny::reactive(input$include_active),
        include_inactive_r = shiny::reactive(input$include_inactive),
        to_r = shiny::reactive(input$include_as)
      )
    }
  )
}
