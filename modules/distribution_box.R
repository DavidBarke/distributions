distribution_box_ui <- function(id, label = "Distribution", color = "#fff") {
  ns <- shiny::NS(id)

  index <- sample(seq_along(distributions$choices), 1)
  distribution <- names(distributions$choices[index])

  box <- bs4Dash::box(
    width = 12,
    collapsible = FALSE,
    title = htmltools::div(
      class = "flex",
      color_input(
        inputId = ns("color"),
        value = color,
        bg = ".distribution-box"
      ),
      badge_input(
        inputId = ns("distribution_badge"),
        distribution,
        lg = TRUE
      )
    ),
    shiny::uiOutput(
      outputId = ns("params"),
      class = "flex"
    )
  )

  box$children[[1]]$attribs$class <- paste(
    box$children[[1]]$attribs$class,
    "small-card", "distribution-box"
  )

  box
}

distribution_box_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      distribution_id_r <- shiny::reactive({
        distributions$choices[input$distribution_badge_text]
      })

      distribution_r <- shiny::reactive({
        param_names <- distributions$params[[distribution_id_r()]]
        params <- rep(1, times = length(param_names))
        names(params) <- param_names

        do.call(
          distributions$funcs[[distribution_id_r()]],
          as.list(params)
        )
      })

      output$params <- shiny::renderUI({
        params <- distributions$params[[distribution_id_r()]]

        purrr::map(names(params), ~ {
          distribution_param(
            name = htmltools::HTML(.),
            value = 1
          )
        })
      })

      shiny::observeEvent(input$distribution_badge, {
        shiny::showModal(shiny::modalDialog(
          title = "Modify Distribution",
          distribution_modifier_ui(
            id = ns("distribution_modifier"),
            current_distribution = distribution_r()
          ),
          footer = htmltools::tagList(
            shiny::actionButton(
              inputId = ns("confirm"),
              label = "Confirm",
              icon = shiny::icon("check")
            ),
            shiny::modalButton(
              label = "Close",
              icon = shiny::icon("times")
            )
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$confirm, {
        shiny::removeModal()
        updateBadgeInput(
          inputId = "distribution_badge",
          text = distributions$id_to_name(
            distribution_modifier_return$distribution_id_r()
          )
        )
      })

      distribution_modifier_return <- distribution_modifier_server(
        id = "distribution_modifier",
        .values = .values
      )
    }
  )
}
