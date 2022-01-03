#' Find the working directory for `hake-data`
#'
#' Find the working directory called `hake-data` based on your user name.
#' `hake-data` stores all of the U.S. and Canadian data for the annual
#' stock assessment of Pacific Hake.
#' If you user name is not one of the default, pre-specified user names
#' available within this function, then `hakedatawd()` will create a directory
#' on your `c:` drive called `stockassessment` and place `hake-data` in there.
#'
#' @return A full file path for the `hake-data` directory is returned.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' hakedatawd()
#'
hakedatawd <- function() {
  user <- Sys.info()["user"]
  wd <- switch(user,
    "Kelli.Johnson" = {
        file.path("c:", "stockAssessment", "hake-data")
    },
    "Aaron.Berger" = {
        file.path("C:", "Users", "Aaron.Berger", "Documents", 
            "GitHub", "hake-data")
    },
    "Ian.Taylor" = {
        file.path("C:", "github", "hake-data")
    },
    file.path("c:", "stockassessment", "hake-data")
  )
  fs::dir_create(wd)
  wd
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
#' test <- hakedatasqlpw()
#' # Prompted for passwords for each database because file is not found
#' test <- hakedatasqlpw(file = "doesnotwork.txt")
#' # On Kelli Johnson's machine, the following will work
#' test <- hakedatasqlpw(file = "password.txt")
#' # Doesn't work because entry for database is not in the list
#' # of allowed databases, i.e., the default for `database`.
#' test <- hakedatasqlpw(database = "onedatabase")
#' # Only look for one password
#' test <- hakedatasqlpw(database = "NORPAC")
#' }
hakedatasqlpw <- function(database = c("NORPAC", "PacFIN"), file) {
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
