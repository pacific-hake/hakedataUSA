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
#' @importFrom stats aggregate

datatoassessment <- function(dirout, year, filedat = NULL) {
  mydir <- hakedatawd()
  catchdir <- file.path(mydir, "Catches")

  inc <- utils::read.csv(file.path(dirout, "landings-tac-history.csv"))
  sh <- utils::read.csv(file.path(catchdir, "PacFIN_Sector.csv"))
  inc$US_shore[match(sh$X, inc$Year)] <- sh$USshore
  inc$USresearch[match(sh$X[!is.na(sh$USresearch)], inc$Year)] <- sh$USresearch[!is.na(sh$USresearch)]
  cp <- stats::aggregate(catch ~ year, 
    data = utils::read.csv(file.path(catchdir, "us-cp-catch-by-month.csv")), sum)
  inc$atSea_US_CP[match(cp$year, inc$Year)] <- cp$catch
  ms <- aggregate(catch ~ year, 
    data = utils::read.csv(file.path(catchdir, "us-ms-catch-by-month.csv")), sum)
  inc$atSea_US_MS[match(ms$year, inc$Year)] <- ms$catch
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
    dat <- r4ss::SS_readdat(filedat, version = 3.3, verbose = FALSE)
    dat$catch[match(inc$Year, dat$catch[dat$catch$fleet == 1, "year"]), "catch"] <- inc$TOTAL
    r4ss::SS_writedat(dat, filedat, overwrite = TRUE, verbose = FALSE)
  }

  # Write files back to the disk
  utils::write.table(x = inc, 
    file = file.path(dirout, "landings-tac-history.csv"),
    row.names = FALSE, sep = ",", quote = FALSE)
  utils::write.table(x = tar, 
    file = file.path(dirout, "catch-targets-biomass.csv"),
    row.names = FALSE, sep = ",", quote = FALSE)

}