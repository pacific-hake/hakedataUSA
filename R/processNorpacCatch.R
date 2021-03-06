#' Process Raw Data Extracted From NORPAC
#'
#' Raw data consists of sampled and unsampled hauls with the total weight
#' including bycatch weight. For sampled hauls, \code{EXTRAPOLATED_WEIGHT}
#' is the weight of the catch only. For unsampled hauls that have no knowledge
#' of the amount of bycatch included in the haul, \code{OFFICIAL_TOTAL_CATCH}
#' must be used along with a estimated bycatch rate to calculate
#' \code{EXTRAPOLATED_WEIGHT}. Monthly bycatch rates are estimated from
#' the sampled hauls and multiplied by the \code{OFFICIAL_TOTAL_CATCH} to get
#' an estimate of the amount of bycatch that should be subtracted from the total
#' weight to get just the weight of the species of interest.
#'
#' @details Definitions
#' \enumerate{
#'   \item @template official_total_catch
#'   \item @template extrapolated_weight
#' }
#'
#' @details Assumptions
#' \enumerate{
#'   \item sampled hauls have an entry in the \code{SPECIES} column,
#'   i.e., hake are denoted with 206
#'   \item an unsampled haul is represented by one unique record
#' }
#'
#' @template ncatch
#' @template species
#' @param outfname A file path to save the output to.
#' @template verbose
#' @importFrom stats aggregate
#' @author Andi Stephens
#' @return
#' \enumerate{
#'   \item A data frame of monthly catches in metric tons.
#'   \item A csv file is saved if \code{outfname} is specified.
#' }
#'
processNorpacCatch <- function(ncatch, species = 206, outfname = NULL,
    verbose = FALSE) {

  col_hauljoin <- grep("HAUL(.)+JOIN", colnames(ncatch))
  if (length(col_hauljoin) != 1) {
    stop("The word HAUL_JOIN was either not found or found in multiple columns.")
  }
  colnames(ncatch)[col_hauljoin] <- "HAULJOIN"
  ncatch$SPECIFICHAUL <- paste(format(ncatch$HAULJOIN, digits = 19),
    ncatch$HAUL, sep = "_")

  ncatch$Date <- as.Date(ncatch$RETRIEVAL_DATE, f = "%Y-%m-%d")
  ncatch$month <- get_date(ncatch$RETRIEVAL_DATE, "%m")
  ncatch$Month <- format(ncatch$RETRIEVAL_DATE, format = "%b")
  ncatch$fMonth <- factor(ncatch[, "Month"],
    levels = month.abb[type.convert(sort(unique(format(
      ncatch[, "Date"],"%m"))), as.is = TRUE)]
    )
  ncatch$Month <- factor(ncatch[, "Month"],
    levels = month.abb[type.convert(sort(unique(format(
      ncatch[,"Date"],"%m"))), as.is = TRUE)]
    )
  ncatch$year <- get_date(ncatch$RETRIEVAL_DATE, "%Y")
  ncatch$hrs <- ncatch$DURATION_IN_MIN/60
  ncatch$crate <- ncatch$EXTRAPOLATED_WEIGHT / 1000 / ncatch$hrs
  ncatch$FISHING_DEPTH_M <- measurements::conv_unit(
    ncatch$FISHING_DEPTH_FATHOMS, from = "fathom", to = "m")
  ncatch$BOTTOM_DEPTH_M <- measurements::conv_unit(
    ncatch$BOTTOM_DEPTH_FATHOMS, from = "fathom", to = "m")
  ncatch$vesseltype <- norpac_vesseltype(ncatch$VESSEL_TYPE)
  ncatch$Sector <- "DomesticAtSea"

  ncatch$sampled <- ifelse(yes = 0, no = 1,
    is.na(ncatch$HAUL_SAMPLED_BY) | ncatch$HAUL_SAMPLED_BY == 0)
  # Unsampled hauls will have a SPECIES == NA and EXTRAPOLATED_WEIGHT == NA
  ncatch$SPECIES <- ifelse(ncatch$sampled == 0, species, ncatch$SPECIES)
  ncatch$OFFICIAL_TOTAL_CATCHkg <- ncatch$OFFICIAL_TOTAL_CATCH * 1000
  ncatch$ByCatch <- ncatch$OFFICIAL_TOTAL_CATCHkg - ncatch$EXTRAPOLATED_WEIGHT

  # Find monthly rate for un-sampled tows and multiply times OFFICIAL_CATCH
  # to get the amount of hake in the tow based on average bycatch rate
  # bycatch rates are VESSEL_TYPE specific as of 2019 assessment
  ncatch$bycatchrate <- stats::ave(
    ifelse(ncatch$sampled == 1, ncatch$ByCatch, 0),
    ncatch$year, ncatch$month, ncatch$SPECIES == species, ncatch$VESSEL_TYPE,
    FUN = get_sum) /    stats::ave(
    ifelse(ncatch$sampled == 1, ncatch$OFFICIAL_TOTAL_CATCHkg, 0),
    ncatch$year, ncatch$month, ncatch$SPECIES == species, ncatch$VESSEL_TYPE,
    FUN = get_sum)
  ncatch$Catch.MT <- ifelse(ncatch$sampled == 0,
    ncatch$OFFICIAL_TOTAL_CATCHkg * (1 - ncatch$bycatchrate),
    ncatch$EXTRAPOLATED_WEIGHT) / 1000
  ncatch$ByCatch <- ifelse(ncatch$sampled == 0,
    ncatch$OFFICIAL_TOTAL_CATCHkg - ncatch$Catch.MT / 1000,
    ncatch$ByCatch)

  # Export summary information
  ncatch$catch <- round(ncatch$Catch.MT, digits = 5)
  nsampledhauls <- stats::aggregate(month ~ sampled + vesseltype + year,
    data = ncatch[ncatch$SPECIES == species, ], length)
  bycatchrates <- stats::aggregate(bycatchrate ~ year + month + vesseltype, 
    data = ncatch[ncatch$SPECIES == species & ncatch$sampled == 0,], mean)
  catches <- stats::aggregate(Catch.MT ~ Sector + month + year,
    data = ncatch[ncatch$SPECIES == species, ], sum, na.rm = TRUE)
  catchout <- stats::aggregate(catch ~ Sector + vesseltype + month + year,
    data = ncatch[ncatch$SPECIES == species, ], get_sum)
  TRout <- ncatch[
    ncatch$CDQ_CODE=="M01" &
    !is.na(ncatch$CDQ_CODE) &
    ncatch$SPECIES == species, ]
  TRout$Sector <- "DomesticAtSeaTribal"
  TRout.yr <- stats::aggregate(list("catch" = TRout$catch),
    list("sector" = TRout$Sector, "year" = TRout$year), FUN = get_sum)

  if (!is.null(outfname)) {
    utils::write.table(nsampledhauls,
      file = file.path(outfname, "NORPAC_DomesticAtSea_nsampledhauls.csv"),
      row.names = FALSE, sep = ",", append = FALSE)
    utils::write.table(bycatchrates,
      file = file.path(outfname, "NORPAC_DomesticAtSea_bycatchrate.csv"),
      row.names = FALSE, sep = ",", append = FALSE)
    utils::write.table(catches, 
      file = file.path(outfname, "us-catch-by-month.csv"), col.names = TRUE,
      row.names = FALSE, sep = ",", append = FALSE)
    utils::write.table(catchout[catchout$vesseltype == "CP", -(1:2)],
      file = file.path(outfname, "us-cp-catch-by-month.csv"),
      sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
    utils::write.table(catchout[catchout$vesseltype == "MS", -(1:2)],
      file = file.path(outfname, "us-ms-catch-by-month.csv"),
      sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
    utils::write.table(TRout.yr,
      file = file.path(outfname, "NORPAC_DomesticAtSea_tribal.csv"),
      sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  invisible(ncatch)
}
