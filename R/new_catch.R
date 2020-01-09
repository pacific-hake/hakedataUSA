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

new_catch <- function(dirout, year, filedat = NULL) {

  inc <- utils::read.csv(file.path(dirout, "landings-tac-history.csv"))
  sh <- utils::read.csv(file.path(dirout, "PacFIN_Sector.csv"))
  inc$US_shore[match(sh$X, inc$Year)] <- sh$USshore
  inc$USresearch[match(sh$X[!is.na(sh$USresearch)], inc$Year)] <- sh$USresearch[!is.na(sh$USresearch)]
  cp <- stats::aggregate(catch ~ year,
    data = utils::read.csv(file.path(dirout, "us-cp-catch-by-month.csv")), sum)
  inc$atSea_US_CP[match(cp$year, inc$Year)] <- cp$catch
  ms <- aggregate(catch ~ year,
    data = utils::read.csv(file.path(dirout, "us-ms-catch-by-month.csv")), sum)
  inc$atSea_US_MS[match(ms$year, inc$Year)] <- ms$catch
  inc[, "Ustotal"] <- apply(inc[, grepl("^US|_US", colnames(inc), ignore.case = FALSE)],
    1, sum, na.rm = TRUE)

  # update CAN catch
  can.l <- lapply(
    dir(dirout, pattern = "can.*catch-by-month.csv", full.names = TRUE),
    function(x) {
      out <- read.csv(x, header = TRUE, check.names = FALSE)
      out$sector <- gsub(".*can-([a-z]{2})-.*", "\\1", x)
      out$sector[out$sector == "ft"] <- "CAN_FreezeTrawl"
      out$sector[out$sector == "jv"] <- "CAN_JV"
      out$sector[out$sector == "ss"] <- "CAN_Shoreside"
      colnames(out) <- gsub("^([1-9]{1})", "catch_\\1", colnames(out))
      return(out)
    })
  can.l <- reshape(do.call(rbind, can.l), direction = "long",
    idvar = c("year", "sector"), sep = "_", timevar = "month",
    varying = grep("[0-9]", colnames(can.l[[1]])))
  can.l <- aggregate(catch ~ year + sector, data = can.l, sum)
  can.l <- reshape(can.l, direction = "wide", idvar = c("year"), timevar = "sector")
  colnames(can.l) <- gsub("catch\\.", "", colnames(can.l))
  can.l[is.na(can.l)] <- 0
  inc[match(can.l$year, inc$Year),
    c("CAN_FreezeTrawl", "CAN_JV", "CAN_Shoreside")] <- can.l[,
    c("CAN_FreezeTrawl", "CAN_JV", "CAN_Shoreside")]
  inc[, "CANtotal"] <- apply(inc[, grepl("^CAN_", colnames(inc), ignore.case = FALSE)],
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
