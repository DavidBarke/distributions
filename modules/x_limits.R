x_limits_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::numericInput(
        inputId = ns("min"),
        label = "Min",
        value = -5,
        step = 1
      )
    ),
    shiny::column(
      width = 6,
      shiny::numericInput(
        inputId = ns("max"),
        label = "Max",
        value = 5,
        step = 1
      )
    )
  )
}

x_limits_server <- function(id, .values, type_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$set_plot_min <- function(value) {
        shiny::updateNumericInput(
          inputId = "min",
          value = value,
          session = session
        )
      }

      .values$set_plot_max <- function(value) {
        shiny::updateNumericInput(
          inputId = "max",
          value = value,
          session = session
        )
      }

      shiny::observeEvent(type_r(), {
        if (type_r() == "q") {
          if (input$min < 0) {
            shiny::updateNumericInput(
              inputId = "min",
              value = 0
            )
          }

          if (input$max > 1) {
            shiny::updateNumericInput(
              inputId = "max",
              value = 1
            )
          }
        }
      })

      min_r <- shiny::reactive({
        if (is.null(input$min) || is.na(input$min)) return(0)

        if (type_r() == "q") {
          max(0, min(1, input$min))
        } else {
          input$min
        }
      })

      max_r <- shiny::reactive({
        if (is.null(input$max) || is.na(input$max)) return(1)

        if (type_r() == "q") {
          max(0, min(1, input$max))
        } else {
          input$max
        }
      })

      limits_r <- shiny::reactive({
        sort(c(min_r(), max_r()))
      })

      return_list <- list(
        limits_r = limits_r
      )
    }
  )
}
