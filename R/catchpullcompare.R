#' Compare NORPAC and PacFIN At-Sea Catches
#' 
#' The hake stock assessment uses NORPAC catches as data, but it is a
#' useful exercise to ensure that the catches from NORPAC match those
#' coming from PacFIN. The databases should match within 3 days according
#' to Vanessa Tuttle. 
#' 
#' @param pcatchatsea A data frame of catches from PacFIN of the 
#' at-sea sectors by year and month. If \code{NULL}, then the function 
#' will import the \code{.Rdat} file. 
#' 
#' @export
#' @author Kelli Faye Johnson
#' @return A data frame of comparisons between the at-sea catches generated
#' from NORAPC and PacFIN. The "diff" column are the differences by month 
#' and year between the two data bases. 
#' 
catchpullcompare <- function(pcatchatsea = NULL) {

  mydir <- hakedatawd()

  if (is.null(pcatchatsea)) {
    load(file.path(mydir, "extractedData", "pcatchatsea.Rdat"))
  }

  out <- aggregate(MT ~ YEAR + SECTOR, data = pcatchatsea, sum)
 
  diffs <- merge(
    aggregate(MT ~ YEAR + MONTH, data = pcatchatsea, sum), 
    read.csv(file.path(mydir, "Catches", "NorpacDomesticCatchesByMonth.csv")),
    by.x = c("YEAR", "MONTH"), by.y = c("Year", "Month"), all = TRUE)
  diffs$diff <- diffs$MT - diffs$Catch.MT
  diffs <- diffs[order(diffs$YEAR, diffs$MONTH), ]

  write.csv(diffs,
    file = file.path(mydir, "Catches", "catchpullcompare.csv"),
    row.names = FALSE)
  
  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(pcatchatsea$MT, list(pcatchatsea$MONTH,pcatchatsea$YEAR), sum)),
    c("Sector", "Month", "Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "PacfinAtSeaCatchesByMonth.csv"),
    row.names = FALSE)
  
  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(MT ~ MONTH + YEAR, 
      data = pcatchatsea[pcatchatsea$SECTOR == "CATCHER PROCESSOR", ], sum)),
    c("Sector", "Month", "Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "CP_pacFIN_CatchesByMonth.csv"),
    row.names = FALSE)

  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(MT ~ MONTH + YEAR, 
      data = pcatchatsea[pcatchatsea$SECTOR == "MOTHERSHIP", ], sum)),
    c("Sector", "Month", "Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "MS_pacFIN_withoutTribal_CatchesByMonth.csv"),
    row.names = FALSE)

  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(MT ~ MONTH + YEAR, 
      data = pcatchatsea[pcatchatsea$SECTOR != "CATCHER PROCESSOR", ], sum)),
    c("Sector", "Month", "Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "MS_pacFIN_withTribal_CatchesByMonth.csv"),
    row.names = FALSE)

  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(MT ~ MONTH + YEAR, 
      data = pcatchatsea[pcatchatsea$SECTOR == "TRIBAL", ], sum)),
    c("Sector", "Month", "Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "TRIBAL_pacFIN_CatchesByMonth.csv"),
    row.names = FALSE)

  write.csv(setNames(cbind("DomesticAtSea", 
    aggregate(pcatchatsea$MT, list(pcatchatsea$YEAR), sum)),
    c("Sector","Year", "Catch.MT")),
    file = file.path(mydir, "Catches", "PacfinAtSeaCatchesByYear.csv"),
    row.names = FALSE)

  return(diffs)
}
