#' Find the Working Directory for hake-data
#' 
#' Find the working directory called `hake-data` based on your user name.
#' `hake-data` stores all of the U.S. and Canadian data for the annual
#' stock assessment of Pacific Hake.
#' If you user name is not one of the default, pre-specified user names
#' available within this function, then `hakedatawd` will create a directory
#' on your `c:` drive called `stockassessment` and place `hake-data` in there.
#' 
#' @return A full file path for the `hake-data` directory is returned.
#' @export
#' @author Kelli Faye Johnson
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

#' Find username for hake-data sql accounts
#' Find the username specific to the computer for
#' accessing the NORPAC or PacFIN data repository. 
#' 
#' @param database A vector of character values specifying which databases you
#' want login information for.
#' @param file A file path specifying where to find the passwords.
#' The path must be of a text file with two lines, where the first line
#' is the NORPAC password and the second line is the PacFIN password. 
#' These passwords should not be surrounded with quotes.
#' User will be prompted for passwords if a file is not specified, i.e.,
#' \code{file = NULL} or if the file that is specified cannot be found.
#' 
#' @return A list with two entries, usernames and passwords, for the 
#' databases you specified in the argument \code{database}.
#' @export
#' @author Kelli Faye Johnson
#' 
hakedatasqlpw <- function(database = c("NORPAC", "PacFIN"), file = NULL) {
  user <- Sys.info()["user"]
  database <- match.arg(database, several.ok = TRUE)
  name <- switch(user,
    "Kelli.Johnson" = {
      c("JOHNSONK", "kjohnson")
    },
    "Aaron.Berger" = {
      c("BERGERA", "aberger")
    },
    "Ian.Taylor" = {
      c("TAYLORI", "itaylor")
    })
  keep <- c("norpac", "pacfin") %in% tolower(database)
  if (length(keep) == 0) stop("The databases are not NORPAC or PacFIN")

  if (!file.exists(file)) file <- NULL

  if (is.null(file)) {
    passwords <- c(NA, NA)
    if ("norpac" %in% tolower(database)) {
      passwords[1] <- readline(prompt = "Enter NORPAC password without quotes\n")
    }
    if ("pacfin" %in% tolower(database)) {
      passwords[2] <- readline(prompt = "Enter PacFIN password without quotes\n")
    }
  } else {
    passwords <- readLines(file, warn = FALSE)
  }
  invisible(list("username" = name[keep], "password" = passwords[keep]))
}
