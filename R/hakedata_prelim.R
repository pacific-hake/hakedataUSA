#' Find if date is preliminary data
#'
#' Find if the current date is prior to the data pull for the assessment or
#' if the date is after the typical final data pull.
#' Data should be complete the first week of January.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A logical.
hakedata_prelim <- function() {
  !format(Sys.Date(), "%m") %in% glue::glue("0{1:3}")
}
