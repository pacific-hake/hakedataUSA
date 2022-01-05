#' Load an object from an `.Rdat` file
#'
#' Create a file path from `dir` and `file` using [file.path()] and
#' load the object into the local environment while returning it.
#' `loadlocal()` assumes that there is only one object in the `.Rdat` file.
#' The utility of this function comes from being able to quickly load
#' an R object that was saved as an `.Rdat` into a function argument without
#' having to save it in your work space.
#' @template file
#' @template dir
#' @author Kelli F. Johnson
#' @export
loadlocal <- function(file,
                      dir = file.path(hakedatawd(), "extractedData")) {
  env <- new.env()
  nm <- load(file.path(dir, file), env)
  env[[nm]]
}
