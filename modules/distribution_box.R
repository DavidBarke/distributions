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
        value = color
      ),
      badge(distribution, lg = TRUE)
    ),
    shiny::uiOutput(
      outputId = ns("params"),
      class = "flex"
    )
  )

  box$attribs$class <- paste(box$attribs$class, "small-card")

  box
}

distribution_box_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$params <- shiny::renderUI({
        index <- sample(seq_along(distributions$choices), 1)
        distribution <- distributions$choices[index]

        params <- distributions$params[[distribution]]

        purrr::map(names(params), ~ {
          distribution_param(
            name = htmltools::HTML(.),
            value = 1
          )
        })
      })
    }
  )
}
