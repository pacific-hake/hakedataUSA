#' Move US Catch Information to Assessment Files
#' 
#' Integrate the latest catch information from the US hake fishery
#' into the hake-assessment files used in the building of the
#' stock assessment.
#' 
#' @param dirout A directory that houses the \code{.csv} data files
#' for the stock assessment.
#' @param year The year of data that you are particularly interested
#' in updating, although most years get updated since 2008.
#' @param filedat The Stock Synthesis dat file that you want to 
#' integrate catches into. The default is \code{NULL}, which allows
#' you to run the function without writing a new dat file.
#' 
#' @return todo: document what this function returns.
#' @author Kelli Faye Johnson
#' @export
#' @import r4ss utils

datatoassessment <- function(dirout, year, filedat = NULL) {
  mydir <- hakedatawd()
  catchdir <- file.path(mydir, "Catches")

  out <- utils::read.csv(file.path(catchdir, "depth_USbottom_atsea.csv"))
  utils::write.table(x = out, 
    file = file.path(dirout, "depth-us-atsea-bottom.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  out <- utils::read.csv(file.path(catchdir, "depth_USfishing_atsea.csv"))
  utils::write.table(x = out, 
    file = file.path(dirout, "depth-us-atsea-fishing.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

  cp <- utils::read.csv(file.path(catchdir, "CP_CatchesByMonth.csv"))
  out <- cp[, -which(colnames(cp) == "Sector")]
  colnames(out) <- c("month", "year", "catch")
  out$catch <- round(out$catch, digits = 5)
  utils::write.table(x = out, 
    file = file.path(dirout, "us-cp-catch-by-month.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  ms <- utils::read.csv(file.path(catchdir, "MS_CatchesByMonth.csv"))
  out <- ms[, -which(colnames(ms) == "Sector")]
  colnames(out) <- c("month", "year", "catch")
  out$catch <- round(out$catch, digits = 5)
  utils::write.table(x = out, 
    file = file.path(dirout, "us-ms-catch-by-month.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

  sh <- utils::read.csv(file.path(catchdir, "USshoreCatchByPeriodComp_ft.csv"))
  out <- sh[sh$Sector == "USshore", -which(colnames(ms) == "Sector")]
  colnames(out) <- c("month", "year", "catch")
  out$catch <- round(out$catch, digits = 5)
  utils::write.table(x = out, 
    file = file.path(dirout, "us-shore-catch-by-month.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  out <- sh[sh$Sector == "USresearch", -which(colnames(ms) == "Sector")]
  colnames(out) <- c("month", "year", "catch")
  out$catch <- sprintf("%.9f", out$catch)
  utils::write.table(x = out, 
    file = file.path(dirout, "us-research-catch-by-month.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

  inc <- utils::read.csv(file.path(dirout, "landings-tac-history.csv"))
  all <- merge(merge(cp, ms, all = TRUE), setNames(sh, colnames(ms)), all = TRUE)
  all$Sector <- as.character(all$Sector)
  all[all$Sector == "USshore", "Sector"] <- "US_shore"
  inc <- merge(
    data.frame("Year" = unique(all[!all$Year %in% inc$Year, c("Year")])),
    inc, all = TRUE)
  ags <- aggregate(Catch.MT ~ Year + Sector, data = all, sum)
  for (ii in unique(ags$Sector)) {
    inc[match(ags$Year[ags$Sector == ii], inc$Year), match(ii, colnames(inc))] <-
    ags$Catch.MT[ags$Sector == ii]
  }
  inc[, "Ustotal"] <- apply(inc[, grepl("^US|_US", colnames(inc), ignore.case = FALSE)], 
    1, sum, na.rm = TRUE)
  inc[, "TOTAL"] <- apply(inc[, grepl(".total", colnames(inc), ignore.case = TRUE)], 
    1, sum, na.rm = TRUE)
  inc[is.na(inc)] <- ""

  tar <- utils::read.csv(file.path(dirout, "catch-targets-biomass.csv"))
  tar[tar$Year == year, "Realized"] <- inc[inc$Year == year, "TOTAL"]
  tar[tar$Year == year, "X.Realized"] <- tar[tar$Year == year, "Realized"] / 
    tar[tar$Year == year, "TAC"]
  colnames(tar)[which(colnames(tar) == "X.Realized")] <- "%Realized"
  tar[is.na(tar)] <- ""

  # apc <- utils::read.csv(file.path(dirout, "us-ap-catch.csv"))
  # apc[which(apc[, 1] == "Catch"), "CP"] <- floor(
  #   ags[ags$Year == year & ags$Sector == "atSea_US_CP", "Catch.MT"])
  # apc[which(apc[, 1] == "Catch"), "MS"] <- floor(
  #   ags[ags$Year == year & ags$Sector == "atSea_US_MS", "Catch.MT"])
  # # NEED SS and Tribal
  # apc[which(apc[, 1] == "Catch"), "Total"] <- sum(apc[which(apc[, 1] == "Catch"), 2:5])
  # apc[which(apc[, 1] == "Remaining"), 2:6] <- apc[2, 2:6] - apc[which(apc[, 1] == "Catch"), 2:6]
  # apc[5, 2:6] <- round((apc[which(apc[, 1] == "Catch"), 2:6] / apc[1, 2:6]) * 100, 0)
  # apc[6, 2:6] <- round((apc[which(apc[, 1] == "Catch"), 2:6] / apc[2, 2:6]) * 100, 0)
  # colnames(apc)[1] <- ""

  # Dat file
  if (!is.null(filedat)) {
    # dat <- r4ss::SS_readdat(filedat, version = 3.3, verbose = FALSE)
    # dat$catch$Fishery <- round(inc[match(dat$catch$year, inc$Year), "TOTAL"], 0)
    # r4ss::SS_writedat(dat, filedat, overwrite = TRUE)
    alt <- readLines(filedat)
    top <- grep("#Year Seas Fleet Catch   Catch_SE", alt)
    while (substr(alt[top], 1, 4) != 1966) top <- top + 1
    bottom <- grep("CPUE_and_surveyabundance_observations", alt)
    while (!grepl("[[:digit:]]{4}", substr(alt[bottom], 1, 4))) bottom <- bottom - 1
    newcatch <- apply(data.frame(inc$Year, 1, 1, 
      round(inc$TOTAL, 0), 
      as.numeric(tail(strsplit(alt[top], "\\s")[[1]], 1))), 1, paste, collapse = " ")
    alt <- alt[-(top:bottom)]
    alt <- append(alt, after = top - 1, newcatch)
    writeLines(alt, filedat)
  }

  # Write files back to the disk
  utils::write.table(x = inc, 
    file = file.path(dirout, "landings-tac-history.csv"),
    row.names = FALSE, sep = ",", quote = FALSE)
  utils::write.table(x = tar, 
    file = file.path(dirout, "catch-targets-biomass.csv"),
    row.names = FALSE, sep = ",", quote = FALSE)

}