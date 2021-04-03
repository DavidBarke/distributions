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
