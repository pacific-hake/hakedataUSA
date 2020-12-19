#' Get sum of data without `NA` values
#'
#' Get the sum of all values after excluding `NA` entries using
#' `na.rm = TRUE` inside of call to [sum].
#'
#' @param x An R object containing numeric values.
#' @return A single numerical value.

get_sum <- function(x) {
  sum(x, na.rm = TRUE)
}
