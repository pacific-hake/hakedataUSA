#' Copy U.S. composition data to the hake-assessment data directory
#'
#' @details
#' TODO: Write these files out directly to hake-assessment rather than writing
#'       to a temporary file. Make sure that no code depends on the temporary
#'       files before doing this. Eliminate this function. Write documentation
#'       in the function that makes these files that the original files in
#'       hake-assessment are overwritten.
#' TODO: Make a message that encourages a commit in hake-assessment to update
#'       the data.
#'
#' @template dirout
#' @export
datatoassessment_comp <- function(dirout) {
  # Deal with not wanting scientific notation in output
  oldoptions <- options()
  options(scipen = 999)
  on.exit(options(sciepen = oldoptions[["scipen"]]), add = TRUE)

  dircomp <- file.path(hakedatawd(), "Catches", "Comps")
  aa <- utils::read.csv(file.path(dircomp, "CP.Age.Only", "comps.csv"))
  colnames(aa)[1:3] <- c("year", "n.fish", "n.hauls")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa,
    file = file.path(dirout, "us-cp-age-data.csv"), sep = ",",
    row.names = FALSE)
  aa <- utils::read.csv(file.path(dircomp, "MS.Age.Only", "comps.csv"))
  colnames(aa)[1:3] <- c("year", "n.fish", "n.hauls")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa,
    file = file.path(dirout, "us-ms-age-data.csv"), sep = ",",
    row.names = FALSE)
  aa <- utils::read.csv(file.path(dircomp, "Shoreside.Age.Only", "shoresideAgeComps.csv"))
  colnames(aa)[1:3] <- c("year", "n.fish", "n.trips")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa, 
    file = file.path(dirout, "us-shore-age-data.csv"), sep = ",",
    row.names = FALSE)
}
