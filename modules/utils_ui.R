append_card_tools_right <- function(x, child) {
  x$children[[1]]$children[[1]] <- htmltools::tagAppendChild(
    x$children[[1]]$children[[1]],
    htmltools::div(
      class = "card-tools float-right",
      child
    )
  )

  x
}
