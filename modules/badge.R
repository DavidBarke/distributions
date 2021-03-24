badge_input <- function(inputId, text, lg = FALSE) {
  class <- "badge badge-input"

  if (lg) class <- paste(class, "badge-lg")

  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$script(
        src = "js/badge-input.js"
      )
    ),
    htmltools::span(
      id = inputId,
      class = class,
      htmltools::tags$div(
        id = paste(inputId, "text", sep = "_"),
        text
      )
    )
  )
}

updateBadgeInput <- function(session = shiny::getDefaultReactiveDomain(),
                             inputId,
                             text
) {
  session$sendInputMessage(paste(inputId, "text", sep = "_"), list(text = text))
}

badge <- function(text, lg = FALSE) {
  class <- "badge"

  if (lg) class <- paste(class, "badge-lg")

  htmltools::span(
    class = class,
    text
  )
}
