drop_zone <- function(..., id, label) {
  htmltools::div(
    class = "drop-zone",
    htmltools::div(
      class = "drop-zone-header",
      htmltools::h3(
        class = "drop-zone-title",
        label
      )
    ),
    htmltools::div(
      id = id,
      class = "drop-zone-body",
      ...
    )
  )
}
