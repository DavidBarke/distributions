popover_2 <- function(tag,
                      title = "",
                      content,
                      placement = c("top", "bottom", "left", "right"),
                      ...
) {
  tag <- htmltools::tagAppendAttributes(
    tag,
    title = title,
    `data-content` = content,
    `data-html` = htmlwidgets::JS("true"),
    `data-placement` = match.arg(placement),
    `data-container` = "body",
    `data-toggle` = "popover",
    ...
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
