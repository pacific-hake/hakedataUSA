#' Copy Catch Files to Assessment
#'
#' Copy the catch files from the US Catches directory to the assessment
#' data directory.
#'
#' @template dirout
#' @author Kelli F. Johnson
datatoassessment_catch <- function(dirout) {
  mydir <- hakedatawd()
  catchdir <- file.path(mydir, "Catches")
  patterns <- dir(catchdir, pattern = "PacFIN_Sector|us-.*-catch-by-month")
  result <- file.copy(file.path(catchdir, patterns), file.path(dirout, patterns),
    overwrite = TRUE
  )
  if (any(!result)) {
    warning(paste(patterns[result], collapse = ",\n"), "\nwere not copied to ", dirout)
  }
  return(result)
}
