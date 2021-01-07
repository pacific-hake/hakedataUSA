#' Copy composition data in the assessment data directory
#'
#' @template dirout
#' @export
datatoassessment_comp <- function(dirout) {
  dircomp = file.path(hakedatawd(), "Catches", "Comps")
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
