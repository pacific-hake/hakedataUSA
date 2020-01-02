#' Convert to Decimal Degrees
#' 
#' Convert a four- or five-digit number to decimal degrees, where it is assumed
#' that the last two digits of the input data are minutes.
#' @param x A vector of integer values, where the last two digits are assumed
#' to be the minute values that will be transformed into a decimal value.
#' @param rnd An integer value specifying how many digits to round to using
#' the argument \code{digits = rnd} in \code{round}.
#' @return A vector of numeric values.
convertToDecDeg <- function(x, rnd = 2) {
  deg <- floor(x / 100)
  minutes <- 100 * (x / 100 - deg) / 60
  return(round(deg + minutes, rnd))
}
