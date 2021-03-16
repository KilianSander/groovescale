is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.character.vector <- function(x) {
  is.vector(x) && is.character(x)
}

is.character.or.numeric <- function(x) {
  is.character(x) || is.numeric(x)
}

is.na.or.null <- function(x) {
  is.na(x) || is.null(x)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}
