#' Find the date for a weekday
#'
#' Find calendar date for a weekday in a given month.
#' For example, you might want to know the date for the
#' second Wednesday of December in the current year.
#' This is the typical date for the meeting of the
#' Joint Technical Committee for the Pacific Hake
#' stock assessment.
#'
#' @param number The iteration you want, e.g.,
#'   `2` for second.
#' @param month An integer for the desired month.
#'   Options include values in `1:12`.
#' @param year The year of interest as a four-digit integer.
#' @param weekday The day of the week that you are interested in.
#'   For example, `"Wednesday"` is the default.
#' @author Kelli F. Johnson
#' @export
#'
#' @examples
#' # Find second Wednesday in December of current year
#' calc_calendar_date()
#' # Find third Wednesday in December of current year
#' calc_calendar_date(3)
#'
calc_calendar_date <- function(number = 2,
                               month = 12,
                               year = as.numeric(format(Sys.time(), "%Y")),
                               weekday = "Wednesday") {
  startdate <- as.Date(
    glue::glue("{year}-{month}-1"),
    "%Y-%m-%d"
  )
  date <- startdate

  while (as.numeric(format(date, format = "%m")) == month) {
    date <- date + 1
  }

  endmonth <- as.Date(format(date - 1, format = "%Y-%m-%d"))
  allmonth <- seq(
    startdate,
    endmonth,
    "day"
  )

  return(as.Date(allmonth[weekdays(allmonth) == weekday][number]))
}
