`%_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

first_child <- function(x) {
  x$children[[1]]
}
