remove_icon <- function(inputId) {
  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$script(
        src = "js/remove-icon.js"
      )
    ),
    htmltools::div(
      id = inputId,
      class = "remove-icon",
      shiny::icon("times")
    )
  )
}
