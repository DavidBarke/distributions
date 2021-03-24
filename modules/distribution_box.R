distribution_box_ui <- function(id,
                                label = "Distribution",
                                color = "#fff",
                                value = distributional::dist_normal()
) {
  ns <- shiny::NS(id)

  distribution <- distributions$id_to_name(
    distributions$distribution_to_id(value)
  )

  distribution_params <- distributions$distribution_to_params(value)

  param_badges <- purrr::pmap(
    list(
      name = names(distribution_params),
      value = distribution_params,
      index = seq_along(distribution_params)
    ),
    function(name, value, index) {
      distribution_param(
        inputId = ns("param" %_% index),
        name = name,
        value = value
      )
    }
  )

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
    param_badges
  )

  box$children[[1]]$attribs$class <- paste(
    box$children[[1]]$attribs$class,
    "small-card", "distribution-box"
  )

  box
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

      shiny::observeEvent(input$distribution_badge, {
        shiny::showModal(shiny::modalDialog(
          title = "Modify Distribution",
          distribution_modifier_ui_proxy(
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
    }
  )
}
