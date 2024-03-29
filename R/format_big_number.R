#' Format an object with a comma
#'
#' Format an object using a comma and rounding to the nearest integer.
#' @param x A vector that will be passed to [format()].
#' @param digits The number of digits you want to include
#'   when rounding and using `round(digits = digits)`.
#'
#' @export
#' @return Character values are returned using [format()].
#' @examples
#' # Remove tenth
#' format_big_number(1.1)
format_big_number <- function(x, digits = 0) {
  format(
    x = round(x, digits = digits),
    format = "f",
    big.mark = ","
  )
}
