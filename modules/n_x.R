n_x_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::numericInput(
      inputId = ns("n"),
      label = htmltools::tagList(
        "Resolution of interpolation",
        bs4Dash::popover(
          shiny::actionLink(
            inputId = ns("info"),
            label = NULL,
            icon = shiny::icon("info-circle")
          ),
          title = "Resolution of interpolation",
          content = "For continuous distributions this is roughly equal to the number of unique x values."
        )
      ),
      min = 1,
      value = 100,
      step = 1
    ),
    shiny::uiOutput(
      outputId = ns("error")
    )
  )
}

n_x_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      n_rv <- shiny::reactiveVal(100)

      is_number_r <- shiny::reactive({
        !is.na(input$n) && !is.null(input$n)
      })

      n_r <- shiny::reactive({
        shiny::req(input$n)
      })

      is_integer_r <- shiny::reactive({
        n_r() == round(n_r())
      })

      is_positive_r <- shiny::reactive({
        n_r() > 0
      })

      output$error <- shiny::renderUI({
        shiny::validate(
          shiny::need(is_number_r(), "Please enter a positive integer.")
        )

        shiny::validate(
          shiny::need(is_integer_r(), "Please enter an integer."),
          shiny::need(is_positive_r(), "Please enter a positive integer.")
        )
      })

      error_r <- shiny::reactive({
        !is_number_r() || !is_integer_r() || !is_positive_r()
      })

      shiny::observeEvent(n_r(), {
        if (!error_r()) {
          n_rv(n_r())
        }
      })

      safe_n_r <- shiny::reactive({
        n_rv()
      }) %>% shiny::debounce(500)

      return_list <- list(
        n_r = safe_n_r
      )
    }
  )
}
