#' Find the working directory for `hake-data`
#'
#' Find the working directory called `hake-data` based on your user name.
#' `hake-data` stores all of the U.S. and Canadian data for the annual
#' stock assessment of Pacific Hake.
#' If you user name is not one of the default, pre-specified user names
#' available within this function, then `hakedata_wd()` will create a directory
#' on your `c:` drive called `stockassessment` and place `hake-data` in there.
#'
#' @return A full file path for the `hake-data` directory is returned.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' hakedata_wd()
#'
hakedata_wd <- function() {
  user <- Sys.info()["user"]
  if (Sys.info()["sysname"] == "Linux") {
    wd <- fs::path("/home", user, "github", "pacific-hake", "hake-assessment")
  }
  if (Sys.info()["sysname"] == "Windows") {
    wd <- switch(user,
      "Kelli.Johnson" = {
          fs::path("c:", "github", "pacific-hake", "hake-assessment", "data")
      },
      "Aaron.Berger" = {
          fs::path(
            "C:", "Users", "Aaron.Berger", "Documents",
            "GitHub", "hake-assessment", "data"
          )
      },
      fs::path("c:", "stockassessment", "hake-data")
    )
  }
  stopifnot(fs::dir_exists(wd))
  return(wd)
}

#' Find last year of data for current assessment
#' 
#' Data is extracted the first Friday in January of the year
#' following the last year of data. Therefore, if data are extracted
#' from the databases in January, February, or March, then the terminal
#' year of data should be the previous year. Else, one would want the
#' most recent data in the current year as the last year of data included
#' in the extraction.
#' @author Kelli F. Johnson
#' @return The last year of data you want as an integer.
#' @export
#'
hakedata_year <- function() {
  as.numeric(format(Sys.Date(), "%Y")) - 
    ifelse(format(Sys.Date(), "%m") %in% c("01", "02", "03"),
    1, 0)
}

#' Find username and passwords for databases
#'
#' Find the username and passwords specific given the username of the computer
#' and stored or entered passwords for accessing databases that store
#' confidential information about landings of Pacific Hake.
#'
#' @param database A vector of character values specifying which databases you
#'   want login information for.
#' @param file A file path specifying where to find the passwords.
#'   The path can be full or relative to your current working directory.
#'   If a path is provided, the file that it leads to
#'   must be for a text file with one password per line for each database
#'   in the `database` argument and in that order.
#'   The default for `database` means that the file would have two lines,
#'   where the first line is the NORPAC password and
#'   the second line is the PacFIN password.
#'   These passwords should not be surrounded with quotes.
#'   If a file name is not provided, which is the default behaviour, then
#'   the user will be prompted for their passwords. This also happens if
#'   the file cannot be found given the path provided.
#' 
#' @return A list with two entries, `usernames` and `passwords`.
#' Each element will have the same number of entries as the
#' input argument `database` and be named using the elements of `database`.
#' The list is invisibly returned to ensure that the passwords are not printed
#' to the screen. Thus, the function call should be assigned to an object.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' \dontrun{
#' # Prompted for passwords for each database
#' test <- hakedata_sql_password()
#' # Prompted for passwords for each database because file is not found
#' test <- hakedata_sql_password(file = "doesnotwork.txt")
#' # On Kelli Johnson's machine, the following will work
#' test <- hakedata_sql_password(file = "password.txt")
#' # Doesn't work because entry for database is not in the list
#' # of allowed databases, i.e., the default for `database`.
#' test <- hakedata_sql_password(database = "onedatabase")
#' # Only look for one password
#' test <- hakedata_sql_password(database = "NORPAC")
#' }
hakedata_sql_password <- function(database = c("NORPAC", "PacFIN"), file) {
  user <- Sys.info()["user"]
  database <- match.arg(database, several.ok = TRUE)
  name <- switch(user,
    "Kelli.Johnson" = {
      c("NORPAC" = "JOHNSONK", "PacFIN" = "kjohnson")[database]
    },
    "Aaron.Berger" = {
      c("NORPAC" = "BERGERA", "PacFIN" = "aberger")[database]
    },
    "Ian.Taylor" = {
      c("NORPAC" = "TAYLORI", "PacFIN" = "itaylor")[database]
    })
  stopifnot(!is.null(name))

  if (missing(file)) {
    file <- NULL
  } else {
    if (!file.exists(file)) file <- NULL
  }

  if (is.null(file)) {
    passwords <- rep(NA, length(database))
    for (ii in seq_along(database)) {
      passwords[ii] <- readline(
        prompt = glue::glue("
          Enter password for {database[ii]} database without quotes and \\
          hit Enter.

          "
        )
      )
    }
  } else {
    passwords <- readLines(file, warn = FALSE)
  }

  names(passwords) <- database
  invisible(list("username" = name, "password" = passwords))
}
