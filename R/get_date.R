#' Format a date to  a specific form
#'
#' Format a date object into a specific numeric form.
#'
#' @param data A vector of dates with class `POSIXct`.
#' @param formatout A character value specifying the format you want
#' the output to be in. See [base::strptime] for options.
#' @return A vector of numeric objects, such as months.
#' 
get_date <- function(data, formatout = c("%m", "%Y")) {
  stopifnot("POSIXct" %in% class(data) | "POSIXt" %in% class(data))
  out <- format(data, formatout)
  return(utils::type.convert(out, as.is = TRUE))
}
