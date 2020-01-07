#' Clean catches extracted from PacFIN
#' 
#' Remove foreign catches and fix column names of catches from PacFIN.
#' 
#' @template pcatch
#' 
#' @importFrom stats aggregate
#' @importFrom utils write.table
#' @return A list of three data frames,
#' \enumerate{
#'   \item tribal catches,
#'   \item removed foreign catches, and
#'   \item catches by year and sector.
#' }
pacfincatches <- function(pcatch = NULL) {
  
  mydir <- hakedatawd()

  if (is.null(pcatch)) {
    base::load(file.path(mydir, "extractedData", "Pacfincomp_ft_taylorCatch.Rdat"))
  }
  
  utils::write.table(tapply(pcatch$MT,list(pcatch$YEAR, pcatch$FLEET), sum),
    file = file.path(mydir, "Catches", "PacFIN_Fleet.csv"),
    sep = ",", quote = FALSE, row.names = TRUE, col.names = NA)

  #todo: determine if these catches are foreign?
  # are they already accounted for in Canada?
  xxcatch <- pcatch[pcatch$FLEET == "XX", ]
  pcatch <- pcatch[pcatch$FLEET != "XX",]

  pcatch$Date <- as.Date(pcatch$TDATE)
  pcatch$month <- as.numeric(substr(pcatch$TDATE,6,7))
  colnames(pcatch)[colnames(pcatch) == "YEAR"] <- "year"
  pcatch <- pcatch[order(pcatch$Date),]
  pcatch$sector <- "USshore"
  pcatch$sector[grep("R[[:space:]]*", pcatch$FLEET)] <- "USresearch"

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

  #Look at tribal catch in shoreside (already added in above)
  tribal <- pcatch[pcatch$FLEET == "TI", ]
  tribal.yr.per <- stats::aggregate(list("catch" = tribal$MT),
      list("month" = tribal$month, "year" = tribal$year),
      FUN = sum)
  utils::write.table(tribal.yr.per,
    file = file.path(mydir, "Catches", "PacFIN_Tribal.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)

  invisible(list("PacFINTribal" = tribal, "Foreign" = xxcatch,
    "PacFINbySMY" = pcatch.yr.per))

}
