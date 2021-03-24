distribution_box_ui <- function(id,
                                label = "Distribution",
                                color = "#fff",
                                value = distributional::dist_normal()
) {
  ns <- shiny::NS(id)

  distribution <- distribution_helper$dist_to_name(value)

  distribution_params <- distribution_helper$dist_to_params(value)

  param_badges <- purrr::pmap(
    list(
      name = names(distribution_params),
      value = distribution_params,
      index = seq_along(distribution_params)
    ),
    function(name, value, index) {
      distribution_param_input(
        inputId = ns("param" %_% index),
        name = name,
        value = value
      )
    }
  )

  htmltools::div(
    id = ns("distribution_box"),
    class = "flex distribution-box",
    color_input(
      inputId = ns("color"),
      value = color,
      bg = ".distribution-box"
    ),
    badge_input(
      inputId = ns("distribution_badge"),
      distribution,
      lg = TRUE
    ),
    htmltools::div(
      # param_badges
      dist_param_input(
        inputId = ns("param_badges"),
        distribution = value
      )
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
        distribution_helper$get_choices()[input$distribution_badge_text]
      })

      distribution_r <- shiny::reactive({
        param_names <- distribution_helper$get_params(distribution_id_r())
        params <- rep(1, times = length(param_names))
        names(params) <- param_names

        do.call(
          distribution_helper$get_func(distribution_id_r()),
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
          text = distribution_helper$id_to_name(
            distribution_modifier_return$distribution_id_r()
          )
        )

        params <- distribution_modifier_return$params_r()
        param_text <- paste(names(params), ":", params)

        purrr::walk2(seq_along(params), param_text, function(index, text) {
          updateBadgeInput(
            inputId = "param" %_% index,
            text = text
          )
        })
      })
    }
  )
}
