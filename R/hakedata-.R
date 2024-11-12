#' Find the working directory for `hake-assessment`
#'
#' Find the directory called `hake-assessment/data-tables`, which should be a
#' result of cloning \url{www.github.com/pacific-hake/hake-assessment}. The
#' location of the directory is found based on a set of rules for a given system
#' and user name of the computer you are on. This `data` directory stores
#' non-confidential data used in the assessment of Pacific Hake and is integral
#' in building the bridging files to go from one year of data to the next. If
#' the combination of known system and user names are not found then it will
#' default to using your current working directory.
#'
#' @return
#' A string specifying the full file path for the `hake-assessment/data`
#' directory. The default is your current working directory.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' hakedata_wd()
#'
hakedata_wd <- function() {
  user <- Sys.info()["user"]
  terminal_directory <- "data-tables"
  if (Sys.info()["sysname"] == "Linux") {
    wd <- fs::path(
      "/home", user,
      "github", "pacific-hake", "hake-assessment", terminal_directory
    )
  }
  if (Sys.info()["sysname"] == "Windows") {
    wd <- switch(user,
      "Kelli.Johnson" = {
        fs::path(
          "d:", "github", "pacific-hake",
          "hake-assessment", terminal_directory
        )
      },
      "Aaron.Berger" = {
        fs::path(
          "C:", "Users", "Aaron.Berger", "Documents",
          "GitHub", "hake-assessment", terminal_directory
        )
      },
      "Chantel.Wetzel" = {
        fs::path(
          "C:", "Users", "Chantel.Wetzel", "Documents",
          "github", "hake-assessment", terminal_directory
        )
      },
      {
        cli::cli_bullets(c(
          "x" = "Username not found",
          "i" = "Setting the directory to {getwd()}"
        ))
        getwd()
      }
    )
  }
  stopifnot(fs::dir_exists(wd))
  stopifnot(basename(wd) == "data-tables")
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
#' @return
#' The last year of data you want as an integer.
#' @export
#' @examples
#' hakedata_year()
#'
hakedata_year <- function() {
  as.numeric(format(Sys.Date(), "%Y")) -
    ifelse(format(Sys.Date(), "%m") %in% c("01", "02", "03"),
      1, 0
    )
}

#' Find username and passwords for databases
#'
#' Find the username and passwords specific given the username of the computer
#' and stored or entered passwords for accessing databases that store
#' confidential information about landings of Pacific Hake.
#'
#' @inheritParams pull_US_data
#'
#' @return
#' A list with two entries, `usernames` and `passwords`. Each entry contain a
#' named vector with one element for each element in the input argument
#' `database`. The list is invisibly returned to ensure that the passwords are
#' not printed to the screen. Thus, the function call should be assigned to an
#' object.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' \dontrun{
#' # Prompted for passwords for each database
#' test <- hakedata_sql_password()
#' # Prompted for passwords for each database because password_file is not found
#' test <- hakedata_sql_password(password_file = "doesnotwork.txt")
#' # On Kelli Johnson's machine, the following will work
#' test <- hakedata_sql_password(password_file = "password.txt")
#' # Doesn't work because entry for database is not in the list
#' # of allowed databases, i.e., the default for `database`.
#' test <- hakedata_sql_password(database = "onedatabase")
#' # Only look for one password
#' test <- hakedata_sql_password(database = "NORPAC")
#' }
hakedata_sql_password <- function(password_file,
                                  database = c("NORPAC", "PacFIN")) {
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
    },
    "Chantel.Wetzel" = {
      c("NORPAC" = "WETZELC", "PacFIN" = "cwetzel")[database]
    }
  )
  stopifnot(!is.null(name))
  stopifnot(all(names(name) %in% database))

  if (missing(password_file) || !file.exists(password_file)) {
    password_file <- NULL
  }

  if (is.null(password_file)) {
    passwords <- rep(NA, length(database))
    for (ii in seq_along(database)) {
      passwords[ii] <- readline(
        prompt = glue::glue("
          Enter password for {database[ii]} database without quotes and \\
          hit Enter.

          ")
      )
    }
  } else {
    passwords <- readLines(password_file, warn = FALSE)
    stopifnot(length(database) == length(passwords))
  }

  names(passwords) <- database
  invisible(list("username" = name, "password" = passwords))
}
