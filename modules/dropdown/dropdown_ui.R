dropdown_menu <- function(..., icon = NULL) {
  htmltools::tags$li(
    class = "nav-item dropdown",
    htmltools::a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      `aria-expanded` = "false",
      icon
    ),
    htmltools::div(
      class = "dropdown-menu dropdown-menu-sm",
      ...
    )
  )
}

dropdown_item <- function(inputId, label = NULL, icon = NULL, ...) {
  shiny::actionLink(
    inputId = inputId,
    label = label,
    icon = icon,
    class = "dropdown-item",
    ...
  )
}

dropdown_download_item <- function(outputId, label = "Download", ...) {
  shiny::downloadButton(
    outputId = outputId,
    label = label,
    class = "dropdown-item"
  )
}
