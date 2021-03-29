color_input <- function(inputId, label = NULL, value = "#fff", bg = NULL) {
  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$script(
        src = "js/color-input.js"
      )
    ),
    htmltools::div(
      class = "color-input",
      if (!is.null(label)) {
        htmltools::tags$label(
          `for` = inputId,
          "Label"
        )
      },
      htmltools::tags$input(
        type = "color",
        value = value,
        id = inputId,
        `data-bg` = if (!is.null(bg)) bg
      )
    )
  )
}
