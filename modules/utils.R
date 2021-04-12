`%_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

first_child <- function(x) {
  x$children[[1]]
}

is_distribution_or_null <- function(x) {
  is.null(x) || distributional::is_distribution(x)
}

popover_2 <- function(tag,
                      title = "",
                      content,
                      placement = c("top", "bottom", "left", "right")
) {
  tag <- htmltools::tagAppendAttributes(
    tag,
    title = title,
    `data-content` = content,
    `data-html` = htmlwidgets::JS("true"),
    `data-placement` = match.arg(placement),
    `data-container` = "body",
    `data-toggle` = "popover"
  )

  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$script(
        "$(function () {
          $(\"[data-toggle='popover']\").popover()
        })"
      )
    ),
    tag
  )
}

dist_func_row <- function(name, definition) {
  htmltools::div(
    class = "dist-func-row",
    htmltools::div(name),
    htmltools::div(definition)
  )
}
