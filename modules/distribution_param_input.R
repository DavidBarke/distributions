distribution_param_input <- function(inputId, name, value) {
  # paste would break rendering of greek letters
  text <- htmltools::tagList(name, ":", value)

  htmltools::span(
    class = "distribution-param",
    badge_input(inputId, text)
  )
}
