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

dist_func_row <- function(name, definition) {
  htmltools::div(
    class = "dist-func-row",
    htmltools::div(name),
    htmltools::div(definition)
  )
}

dist_stat_row <- function(name, discrete, continous) {
  htmltools::div(
    class = "dist-func-row",
    htmltools::div(name),
    htmltools::div(discrete),
    htmltools::div(continous)
  )
}

text_color <- function(background_color) {
  background_rgb <- col2rgb(background_color)

  r <- background_rgb[1,]
  g <- background_rgb[2,]
  b <- background_rgb[3,]

  unname(ifelse((299 * r + 587 * g + 114 * b) / 1000 >= 128, "black", "white"))
}
