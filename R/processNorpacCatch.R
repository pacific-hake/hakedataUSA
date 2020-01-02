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
#' @param species A numeric value that denotes the species of interest in the
#' column \code{SPECIES}.
#' @param outfname A file path to save the output to.
#' @template verbose
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
  ncatch$Month <- utils::type.convert(format(ncatch$RETRIEVAL_DATE, "%m"))
  ncatch$Year <- utils::type.convert(format(ncatch$RETRIEVAL_DATE, "%Y"))
  my.sum <- function(x) sum(x, na.rm = TRUE)

  ncatch$sampled <- ifelse(yes = 0, no = 1,
    is.na(ncatch$HAUL_SAMPLED_BY) | ncatch$HAUL_SAMPLED_BY == 0)
  ncatch$SPECIES <- ifelse(ncatch$sampled == 0, species, ncatch$SPECIES)
  ncatch$OFFICIAL_TOTAL_CATCHkg <- ncatch$OFFICIAL_TOTAL_CATCH * 1000
  ncatch$ByCatch <- ncatch$OFFICIAL_TOTAL_CATCHkg - ncatch$EXTRAPOLATED_WEIGHT

  # Find monthly rate for un-sampled tows and multiply times OFFICIAL_CATCH
  # to get the amount of hake in the tow based on average bycatch rate
  ncatch$MonthlyByCatch <- stats::ave(ifelse(ncatch$sampled == 1, ncatch$ByCatch, 0),
    ncatch$Year, ncatch$Month, ncatch$SPECIES == species, FUN = my.sum)
  ncatch$MonthlyTotal <- stats::ave(ifelse(ncatch$sampled == 1, ncatch$OFFICIAL_TOTAL_CATCHkg, 0),
    ncatch$Year, ncatch$Month, ncatch$SPECIES == species, FUN = my.sum)
  ncatch$ByCatchRate <- ncatch$MonthlyByCatch / ncatch$MonthlyTotal
  ncatch$Catch.MT <- ifelse(ncatch$sampled == 0,
    ncatch$OFFICIAL_TOTAL_CATCHkg * (1 - ncatch$ByCatchRate),
    ncatch$EXTRAPOLATED_WEIGHT) / 1000
  ncatch$ByCatch <- ifelse(ncatch$sampled == 0,
    ncatch$OFFICIAL_TOTAL_CATCHkg - ncatch$Catch.MT / 1000,
    ncatch$ByCatch)

  catches <- data.frame("Sector" = "DomesticAtSea",
    stats::aggregate(Catch.MT ~ Month + Year,
    data = ncatch[ncatch$SPECIES == species, ], sum, na.rm = TRUE))
  if (!is.null(outfname)) {
    utils::write.table(catches, file = outfname, col.names = TRUE,
      row.names = FALSE, sep = ",", append = FALSE)
  }
  invisible(catches)
}
