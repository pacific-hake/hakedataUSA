#' Clean catches extracted from PacFIN
#' 
#' Remove foreign catches and fix column names of catches from PacFIN.
#' 
#' @template pcatch
#' @param addtribal A numeric value in metric tons that will be added
#' to the US shoreside catch to account for tribal fish tickets not
#' being in PacFIN yet.
#' 
#' @importFrom stats aggregate
#' @importFrom utils write.table
#' @return A list of three data frames,
#' \enumerate{
#'   \item tribal catches,
#'   \item removed foreign catches, and
#'   \item catches by year and sector.
#' } are returned and the following files are saved to the disk:
#'   * PacFIN_Fleet.csv
#'   * PacFIN_Sector.cs
#'   * USshoreCatchByPeriodComp_ft.csv
#'   * us-shore-catch-by-month.csv
#'   * us-research-catch-by-month.csv
#'   * us-shore-startdate-by-dahl.csv
#'   * us-ti-catch-by-month.csv
#'
pacfincatches <- function(pcatch = NULL, addtribal = 0) {
  
  mydir <- hakedatawd()

  if (is.null(pcatch)) {
    base::load(file.path(mydir, "extractedData", "Pacfincomp_ft_taylorCatch.Rdat"))
  }

  pcatch <- rbind(pcatch,
    data.frame("YEAR" = 2019, "FLEET" = "TI", "AGID" = "W", "GRID" = "MDT",
      "TDATE" = "2019-12-31", "ARID" = "3B", "PCID" = "WPT",
      "PORT" = 295, "IFQ_LANDING" = FALSE,
      "OVERAGE" = FALSE, "PROC" = 128363, "DAHL_SECTOR" = 17, "FTID" = "UNKNOWN",
      "DRVID" = "UNKNOWN", "COUNT_LE_PERMITS" = 0, "PARTICIPATION_GROUP_CODE" = "I",
      "LBS" = addtribal * 2204.62, "MT" = addtribal, "RMT" = addtribal))
  utils::write.table(tapply(pcatch$MT,list(pcatch$YEAR, pcatch$FLEET), sum),
    file = file.path(mydir, "Catches", "PacFIN_Fleet.csv"),
    sep = ",", quote = FALSE, row.names = TRUE, col.names = NA)

  # FLEET XXX is in the hake assessment as shore-based catches,
  # although 1986 differs from data used
  # database  1986 3431.9436
  # assesment 1986 3465.00
  xxcatch <- pcatch[pcatch$FLEET == "XX", ]

  pcatch$Date <- as.Date(pcatch$TDATE)
  pcatch[, "month"] <- get_date(pcatch[, "Date"], "%m")
  colnames(pcatch)[colnames(pcatch) == "YEAR"] <- "year"
  pcatch <- pcatch[order(pcatch$Date), ]
  pcatch$sector <- ifelse(grepl("^R", pcatch[, "FLEET"]), "USresearch", "USshore")

  utils::write.table(tapply(pcatch$MT, list(pcatch$year, pcatch$sector), sum), 
    file = file.path(mydir, "Catches", "PacFIN_Sector.csv"),
    sep = ",", quote = FALSE, row.names = TRUE, col.names = NA)
  pcatch.yr.per <- stats::aggregate(list("catch" = pcatch$MT),
    list("sector" = pcatch$sector, "month" = pcatch$month, "year" = pcatch$year), 
    FUN = sum)
  pcatch.yr.per <- pcatch.yr.per[order(pcatch.yr.per$sector), ]
  pcatch.yr.per$catch <- round(pcatch.yr.per$catch, 5)
  utils::write.table(pcatch.yr.per,
    file = file.path(mydir, "Catches", "USshoreCatchByPeriodComp_ft.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)
  utils::write.table(pcatch.yr.per[pcatch.yr.per$sector == "USshore", -1],
    file = file.path(mydir, "Catches", "us-shore-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)
  research <- pcatch[pcatch$sector == "USresearch", ]
  research <- stats::aggregate(list("catch" = research$MT),
    list("month" = research$month, "year" = research$year), 
    FUN = sum)
  research$catch <- sprintf("%.9f", research$catch)
  utils::write.table(research,
    file = file.path(mydir, "Catches", "us-research-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)
  utils::write.table(aggregate(Date ~ year + DAHL_SECTOR, data = pcatch, min),
    file = file.path(mydir, "Catches", "us-shore-startdate-by-dahl.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)

  #Look at tribal catch in shoreside (already added in above)
  tribal <- pcatch[pcatch$FLEET == "TI", ]
  tribal.yr.per <- stats::aggregate(list("catch" = tribal$MT),
      list("month" = tribal$month, "year" = tribal$year),
      FUN = sum)
  utils::write.table(tribal.yr.per,
    file = file.path(mydir, "Catches", "us-ti-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)

  invisible(list("PacFINTribal" = tribal, "Foreign" = xxcatch,
    "PacFINbySMY" = pcatch.yr.per))

}
