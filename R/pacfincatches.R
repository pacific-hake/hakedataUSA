#' 
#' 
#' @importFrom stats aggregate
pacfincatches <- function(pcatch = NULL) {
  
  mydir <- hakedatawd()
  summaryfile <- file.path(mydir, "extractedData", 
    paste0("summary_catchPacFIN_", format(Sys.time(), "%Y.%m.%d"), ".txt"))
  on.exit(suppressWarnings(sink(file = NULL)), add = TRUE)
  sink(summaryfile)
  cat("Summary of PacFIN catches created on", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  sink()

  if (is.null(pcatch)) {
    base::load(file.path(mydir, "extractedData", "Pacfincomp_ft_taylorCatch.Rdat"))
  }
  
  sink(summaryfile, append = TRUE)
  cat("Summary of PacFIN catches by year and fleet.\n")
  print(tapply(pcatch$MT,list(pcatch$YEAR, pcatch$FLEET),sum))
  cat("\n\nThe following XX catches (mt) were removed.\n")
  xxcatch <- pcatch[pcatch$FLEET == "XX", ]
  print(tapply(xxcatch$MT, xxcatch$YEAR, sum))
  sink()
  #todo: determine if these catches are foreign?
  # are they already accounted for in Canada?
  pcatch <- pcatch[pcatch$FLEET != "XX",]

  pcatch$Date <- as.Date(pcatch$TDATE)
  pcatch$Month <- as.numeric(substr(pcatch$TDATE,6,7))
  pcatch <- pcatch[order(pcatch$Date),]
  pcatch$Sector <- "USshore"
  pcatch$Sector[grep("R[[:space:]]*", pcatch$FLEET)] <- "USresearch"

  pcatch.yr <- tapply(pcatch$MT,list(pcatch$YEAR,pcatch$Sector),sum)
  write.csv(pcatch.yr, 
    file = file.path("Catches", "USshoreCatchByYearComp_ft.csv"))
  pcatch.yr.per <- aggregate(pcatch$MT,
    list(pcatch$Sector, pcatch$Month, pcatch$YEAR), sum)
  names(pcatch.yr.per) <- c("Sector","Period","Year","MT")
  pcatch.yr.per <- pcatch.yr.per[order(pcatch.yr.per$Sector), ]
  write.csv(pcatch.yr.per,
    file = file.path("Catches", "USshoreCatchByPeriodComp_ft.csv"),
    row.names = FALSE)

  #Look at tribal catch in shoreside (already added in above)
  tribal <- pcatch[pcatch$FLEET == "TI", ]
  tribal.yr.per <- setNames(
    aggregate(tribal$MT,list(tribal$Month,tribal$YEAR),sum),
    c("Period","Year","MT"))
  sink(summaryfile, append = TRUE)
  cat("\nSummary of tribal catch by year.\n")
  print(stats::aggregate(MT ~ YEAR, data = tribal, sum))
  sink()

  invisible(list("PacFINTribal" = tribal, "Foreign" = xxcatch,
    "PacFINbySMY" = pcatch.yr.per))

}
