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
#' @template digits
#' @param lastassessmentvals A vector of three values,
#' * estimate of spawning stock biomass for terminal year
#' in thousands of metric tons,
#' * estimate of depletion in percent with one decimal place,
#' * estimate of TAC for the first year of the forecast period
#' in metric tons rounded to the nearest whole number.
#'
#' @return todo: document what this function returns.
#' @author Kelli F. Johnson
#' @export
#' @import r4ss utils
#' @importFrom stats aggregate
#' @examples
#' \dontrun{
#' new_catch(file.path("hake-assessment", "data"),
#'   year = 2020, lastassessmentvals = c(1.379, 65.0, 66458))
#' }
new_catch <- function(dirout, year, filedat = NULL,
  digits = 5,
  lastassessmentvals = c(NA, NA, NA)) {

  file.apc <- file.path(dirout, "us-ap-catch.csv")
  file.lan <- file.path(dirout, "landings-tac-history.csv")
  file.tar <- file.path(dirout, "catch-targets-biomass.csv")

  inc <- utils::read.csv(file.lan)
  sh <- utils::read.csv(file.path(dirout, "PacFIN_Sector.csv"))
  inc$US_shore[match(sh$X, inc$Year)] <- round(
    x = sh$USshore, digits = digits)
  inc$USresearch[match(sh$X[!is.na(sh$USresearch)], inc$Year)] <- round(
    sh$USresearch[!is.na(sh$USresearch)], digits = digits)
  cp <- stats::aggregate(catch ~ year,
    data = utils::read.csv(file.path(dirout, "us-cp-catch-by-month.csv")), sum)
  inc$atSea_US_CP[match(cp$year, inc$Year)] <- cp$catch
  ms <- aggregate(catch ~ year,
    data = utils::read.csv(file.path(dirout, "us-ms-catch-by-month.csv")), sum)
  ti <- aggregate(catch ~ year,
    data = utils::read.csv(file.path(dirout, "us-ti-catch-by-month.csv"),
      header = TRUE),
    FUN = sum)
  inc$atSea_US_MS[match(ms$year, inc$Year)] <- ms$catch
  inc[, "Ustotal"] <- apply(inc[, grepl("^US|_US", colnames(inc), ignore.case = FALSE)],
    1, sum, na.rm = TRUE)

  # update CAN catch
  can.l <- dplyr::bind_rows(can.l) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("catch"),
      names_to = "month",
      names_prefix = "catch_"
    ) %>%
    dplyr::group_by(Year, sector) %>%
    dplyr::summarize(catch = sum(value)) %>%
    tidyr::pivot_wider(names_from = sector, values_from = catch) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("CAN"),
      tidyr::replace_na,
      0
    ))
  inc[match(can.l$Year, inc$Year),
    c("CAN_FreezeTrawl", "CAN_JV", "CAN_Shoreside")] <- can.l[,
    c("CAN_FreezeTrawl", "CAN_JV", "CAN_Shoreside")]
  inc[, "CANtotal"] <- apply(inc[, grepl("^CAN_", colnames(inc), ignore.case = FALSE)],
    1, sum, na.rm = TRUE)
  inc[, "TOTAL"] <- apply(inc[, grepl(".total", colnames(inc), ignore.case = TRUE)],
    1, sum, na.rm = TRUE)
  inc[is.na(inc)] <- ""

  tar <- utils::read.csv(file.tar, check.names = FALSE)
  if (!year %in% tar$Year) {
    tar <- rbind(tar,
      c(year, rep(NA, NCOL(tar) - 1)))
  }
  tar[, "Realized catch"] <- sprintf("%.0f", inc[
    match(tar$Year, inc$Year),
    "TOTAL"])
  tar[, "TAC"] <- inc[
    match(tar[, "Year"], inc[, "Year"]),
    "TAC"]
  if (!all(is.na(lastassessmentvals))) {
    tar[
      tar[, "Year"] == year,
      c("Biomass estimate", "Depletion", "Assessment TAC")] <-
      lastassessmentvals
  }
  tar[is.na(tar)] <- ""

  # todo: don't subtract tribal from shoreside when they are not
  # added together
  apc <- utils::read.csv(file.apc, check.names = FALSE)
  apc[2, grep("shore", x = colnames(apc), ignore.case = TRUE)] <-
    sprintf("%.0f", inc[inc[, "Year"] == year, "US_shore"] - 
      sum(ti[ti[, "year"] == year, "catch"]))
  apc[2, grep("CP", x = colnames(apc), ignore.case = TRUE)] <-
    sprintf("%.0f", inc[inc[, "Year"] == year, "atSea_US_CP"])
  apc[2, grep("MS", x = colnames(apc), ignore.case = TRUE)] <-
    sprintf("%.0f", inc[inc[, "Year"] == year, "atSea_US_MS"])
  apc[2, grep("TAC", x = colnames(apc), ignore.case = TRUE)] <-
    sprintf("%.0f", inc[inc[, "Year"] == year,
      grep("UStotal", colnames(inc), ignore.case = TRUE)])
  apc[2, grep("Tribal", x = colnames(apc), ignore.case = TRUE)] <-
    sprintf("%.0f", sum(ti[ti[, "year"] == year, "catch"]))
  apc[3, -1] <- sprintf("%2.1f%%",
    type.convert(apc[2, -1], as.is = TRUE) /
    type.convert(apc[1, -1], as.is = TRUE) * 100
    )
  colnames(apc)[1] <- ""

  # Dat file
  if (!is.null(filedat)) {
    dat <- r4ss::SS_readdat(filedat, verbose = FALSE)
    ind <- !(dat$catch$year > 0)
    dat$catch <- rbind(dat$catch[ind, ],
      data.frame("year" = inc$Year, "seas" = 1, "fleet" = 1,
        catch = inc$TOTAL, catch_se = 0.01))
    dat$endyr <- max(inc$Year)
    r4ss::SS_writedat(dat, filedat, overwrite = TRUE, verbose = FALSE)
  }

  # Write files back to the disk
  utils::write.table(x = apc,
    file = file.apc,
    row.names = FALSE, sep = ",", quote = FALSE)
  utils::write.table(x = inc,
    file = file.lan,
    row.names = FALSE, sep = ",", quote = FALSE)
  utils::write.table(x = tar,
    file = file.tar,
    row.names = FALSE, sep = ",", quote = FALSE)

}
