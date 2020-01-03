#' Find last year of data for current assessment
#' 
#' Data is extracted the first Friday in January of the year
#' following the last year of data. Therefore, if data are extracted
#' from the databases in January, February, or March, then the terminal
#' year of data should be the previous year. Else, one would want the
#' most recent data in the current year as the last year of data included
#' in the extraction.
#' @author Kelli Faye Johnson
#' @return The last year of data you want as an integer.
#' @export
#' 
hakedata_year <- function() {
  as.numeric(format(Sys.Date(), "%Y")) - 
    ifelse(format(Sys.Date(), "%m") %in% c("01", "02", "03"),
    1, 0)
}