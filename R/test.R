#' Test function
#'
#' More description
#'
#' @param x Numeric value
#'
#' @return Character value
#'
#' @examples
#' x <- rnorm(n = 1, mean = 0, sd = 1)
#' test(x)
#'
#' @export
test <- function (x) {
  return(paste0("Your value is ", x))
}
