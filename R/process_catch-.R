#' Process NORPAC catches
#'
#' Summarize and plot NORPAC catches for the at-sea hake fishery. Fleet types
#' are assigned to create summaries by fleet and year.
#'
#' @details # Tribal catches
#' The column `CDQ_CODE` contains strings starting with an upper-case followed
#' by and the last two digits of the the CDQ code of the vessel. The code was
#' created in 1995 and is used to track tribal catches in the hake fishery. The
#' following codes exist:
#' * C51 -- Aleutian Pribilof Island Community Development Association
#' * C52 -- Bristol Bay Economic Development Corporation
#' * C53 -- Central Bering Sea Fishermen's Association
#' * C54 -- Coastal Villages Fishing Cooperative
#' * C55 -- Norton Sound Economic Development Corporation
#' * C56 -- Yukon Delta Fisheries Development Association
#' * R10 -- Research Permit - Alaska Fisheries Science Center Catch Estimation
#' * M01 -- Makah Tribe Whiting Association
#' * R11 -- Research Permit - WA Sea Grant bird/longline interaction
#' * R12 -- Research Permit - WA Sea Grant bird/longline interaction
#'
#' @details # Raw data
#' Raw data consists of sampled and unsampled hauls with the total weight
#' including bycatch weight. For sampled hauls, `EXTRAPOLATED_WEIGHT` is the
#' weight of the catch only. For unsampled hauls that have no knowledge of the
#' amount of bycatch included in the haul, `OFFICIAL_TOTAL_CATCH` must be used
#' along with a estimated bycatch rate to calculate `EXTRAPOLATED_WEIGHT`.
#' Monthly fleet-specific bycatch rates are estimated from the sampled hauls and
#' multiplied by the `OFFICIAL_TOTAL_CATCH` to get an estimate of the amount of
#' bycatch that should be subtracted from `OFFICIAL_TOTAL_CATCH` to get just the
#' weight of Pacific Hake.
#'
#' @details # Definitions
#' 1. `OFFICIAL_TOTAL_CATCH` is measured in metric tons (mt) and includes both
#'    retained and discarded species. Thus, this measurement sums the targeted
#'    species, prohibited species, and non-allocated species.
#' 1. `EXTRAPOLATED_WEIGHT` is measured in kilograms (kg) and includes just the
#'    weight of the relevant `SPECIES` included in that row of data, e.g., if
#'    `SPECIES == 206`, then `EXTRAPOLATED_WEIGHT` is the kg of Pacific Hake in
#'    that haul.
#'
#' @details # Assumptions
#' * sampled hauls have an entry in the `SPECIES` column, i.e., hake are
#'   denoted with 206
#' * an unsampled haul is represented by one unique record because there no
#'   additional lines of catch for the bycatch species given it was not sampled
#'
#' @param ncatch A data frame with catch information extracted from the NORPAC
#'   database. The default is to read the `.Rdat` file from the disk using
#'   [get_local()]. Else, users can just pass the R object itself, which would
#'   typically be `hakedata[["ncatch"]]`.
#' @param nyears The number of years for plotting.
#'
#' @import ggplot2 grDevices
#' @export
#' @author Kelli F. Johnson
#'
#' @return Figures and csv files are saved to the disk regarding
#' catch rates and depth (fishing and bottom depths).
#' In the `Figures` directory, the following png files are saved:
#'   * fishDepthsUS.png
#'   * fishDepthsUSallyears.png
#'   * fish_FISHING_DEPTH_FATHOMS_US.png
#'   * fish_BOTTOM_DEPTH_FATHOMS_US.png
#'   * fishBottomDepthByVesselUS.png
#'   * fishCatchRatesUSByYear.png
#'   * fishDepthByYearUS.png
#'   * ../doc/main-figures/fishCatchRatesUS.png
#'   * fishCatchRatesUSnolog.png
#'
#' In the `Catches` directory, the following csv files are saved:
#'   * depth-us-atsea-bottom.csv
#'   * depth-us-atsea-fishing.csv
#'
process_catch_norpac <- function(ncatch = get_local(file = "norpac_catch.Rdat"),
                                 nyears = 5,
                                 savedir = hakedata_wd()) {
  # Setup the environment
  species <- 206
  args <- list(
    width = 6.5, height = 4.5, pointsize = 10, units = "in", res = 600
  )
  oldop <- options()$warn
  options(warn = -1)
  on.exit(options(warn = oldop), add = TRUE)

  # File management
  fs::dir_create(
    path = fs::path(savedir, "Figures"),
    recurse = TRUE
  )
  data("quotas")

  ncatch_formatted <- ncatch %>%
    dplyr::mutate(
      Date = f_date(RETRIEVAL_DATE, format = "%Y-%m-%d"),
      month = f_date(RETRIEVAL_DATE, format = "%m"),
      Month = f_date(RETRIEVAL_DATE, format = "%b", factor = TRUE),
      year = f_date(RETRIEVAL_DATE, "%Y"),
      # Create catch rate in mt/hr
      crate = EXTRAPOLATED_WEIGHT / 1000 / HRS,
      FISHING_DEPTH_M = FISHING_DEPTH_FATHOMS * fathom_to_meter,
      BOTTOM_DEPTH_M = BOTTOM_DEPTH_FATHOMS * fathom_to_meter,
      vesseltype = f_vessel_type(VESSEL_TYPE),
      Sector = "DomesticAtSea",
      sampled = ifelse(is.na(HAUL_SAMPLED_BY) | HAUL_SAMPLED_BY == 0, 0, 1),
      # Unsampled hauls will have a SPECIES == NA and EXTRAPOLATED_WEIGHT == NA
      SPECIES = ifelse(sampled == 0, species, SPECIES),
      OFFICIAL_TOTAL_CATCHkg = OFFICIAL_TOTAL_CATCH * 1000,
      # ByCatch (kg)
      ByCatch = OFFICIAL_TOTAL_CATCHkg - EXTRAPOLATED_WEIGHT
    )
  outncatch <- ncatch_formatted %>%
    dplyr::group_by(year, month, SPECIES == species, VESSEL_TYPE) %>%
    dplyr::mutate(
      bycatchrate = sum(ifelse(sampled == 1, ByCatch, 0), na.rm = TRUE) /
        sum(ifelse(sampled == 1, OFFICIAL_TOTAL_CATCHkg, 0), na.rm = TRUE),
      Catch.MT = ifelse(
        test = sampled == 0,
        yes = OFFICIAL_TOTAL_CATCHkg * (1 - bycatchrate),
        no = EXTRAPOLATED_WEIGHT
      ) / 1000,
      ByCatch = ifelse(
        test = sampled == 0,
        yes = OFFICIAL_TOTAL_CATCHkg - Catch.MT / 1000,
        no = ByCatch
      ),
      catch = round(Catch.MT, digits = 5)
    ) %>%
    dplyr::ungroup()

  # Find monthly rate for un-sampled tows and multiply times OFFICIAL_CATCH
  # to get the amount of hake in the tow based on average bycatch rate
  # bycatch rates are VESSEL_TYPE specific as of 2019 assessment
  catchout <- outncatch %>%
    dplyr::filter(SPECIES == species) %>%
    dplyr::group_by(Sector, vesseltype, month, year) %>%
    dplyr::summarize(catch = sum(catch, na.rm = TRUE)) %>%
    dplyr::ungroup()

  cp <- catchout[catchout$vesseltype == "CP", -(1:2)]
  plot_catchvmonthbyyear(
    data = cp,
    Yrs = as.character((max(cp$year) - nyears + 1):max(cp$year)),
    title = "U.S. CP Catches",
    quotas = quotas[1, -1],
    file = file.path(savedir, "Figures", "CpCatchMonthYear.png")
  )
  utils::write.table(cp %>% dplyr::arrange(year, month),
    file = fs::path(savedir, "us-cp-catch-by-month.csv"),
    sep = ",", row.names = FALSE, quote = FALSE
  )
  ms <- catchout[catchout$vesseltype == "MS", -(1:2)]
  plot_catchvmonthbyyear(
    data = ms,
    file = file.path(savedir, "Figures", "MsCatchMonthYear.png"),
    Yrs = as.character((max(ms$year) - nyears + 1):max(ms$year)),
    title = "U.S. MS Catches",
    quotas = quotas[2, -1]
  )

  utils::write.table(
    ms %>% dplyr::arrange(year, month),
    file = fs::path(savedir, "us-ms-catch-by-month.csv"),
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )

  hcatch <- outncatch %>%
    dplyr::filter(SPECIES == species) %>%
    dplyr::mutate(vcolumn = tidyr::replace_na(CATCHER_BOAT_ADFG, var(VESSEL)))
  # Unique vessel (either catcher boat if not NA or mothership)
  keeptheseyears <- utils::tail(1:max(hcatch$year, na.rm = TRUE), nyears)

  #### Figures: depths
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year"))
  stopifnot(
    "Yearly summaries are not confidential" =
      all(
        stats::aggregate(ngroups ~ year, data = hcatch, unique)[, "ngroups"] > 2
      )
  )
  gg <- plot_boxplot(
    data = stats::reshape(
      data = dplyr::filter(
        hcatch,
        year %in% keeptheseyears & ngroups > 2
      ) %>%
        data.frame(),
      direction = "long",
      varying = c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS"),
      v.names = "fathoms", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")
    ),
    xvar = c("year", "type"), showmedian = TRUE,
    yvar = "fathoms", ylab = "Depth (fathoms)", mlab = "",
    incolor = "year", scales = "free",
    file = file.path(savedir, "Figures", "fishDepthsUS.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]]
  )

  cols <- c(
    "year",
    grep(value = TRUE, "lower|median|upper", colnames(gg[["data"]]))
  )
  utils::write.csv(
    x = gg[["data"]][grepl("Bottom", gg[["data"]][, "type"]), cols],
    file = file.path(savedir, "depth-us-atsea-bottom.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    x = gg[["data"]][grepl("Fishing", gg[["data"]][, "type"]), cols],
    file = file.path(savedir, "depth-us-atsea-fishing.csv"),
    row.names = FALSE
  )
  hcatch[, "months"] <- droplevels(factor(hcatch$month,
    levels = 1:12,
    labels = rep(paste(seq(1, 11, by = 2), seq(2, 12, by = 2), sep = "-"),
      each = 2
    )
  ))

  #### Figure: catch rate
  hcatch <- get_confidential(
    hcatch,
    yvar = "vcolumn",
    xvar = c("year", "month")
  )
  ylabelexpression <- expression(
    Unstandardized ~ catch ~ rates ~ (mt ~ "*" ~ hr^{
      -1
    })
  )
  gg <- mapply(plot_boxplot,
    data = list(
      dplyr::filter(hcatch, year %in% keeptheseyears & ngroups > 2) %>%
        data.frame(),
      dplyr::filter(hcatch, year %in% keeptheseyears & ngroups > 2) %>%
        data.frame()
    ),
    file = list(
      file.path(
        dirname(savedir),
        "doc",
        "main-figures",
        "fishCatchRatesUS.png"
      ),
      file.path(
        savedir,
        "Figures",
        "fishCatchRatesUSnolog.png"
      )
    ),
    yscale = list(
      "log10",
      "identity"
    ),
    mlab = list(
      "U.S. at-sea unstandardized yearly catch-rates",
      "U.S. at-sea unstandardized yearly catch-rates"
    ),
    MoreArgs = list(
      xvar = c("Month"),
      showmedian = TRUE,
      incolor = "year",
      yvar = "crate",
      ylab = ylabelexpression,
      legend.position = c(0.5, 0.95),
      legend.direction = "horizontal",
      width = args[["width"]],
      height = args[["height"]],
      units = args[["units"]],
      dpi = args[["res"]]
    )
  )
  return(outncatch)
}

#' Print summaries and figures for catches from PacFIN
#'
#' @template pcatch
#' @template nyears
#' @template savedir
#'
#' @return The following files are saved to the disk:
#' * PacFIN_Fleet.csv
#' * PacFIN_Sector.cs
#' * USshoreCatchByPeriodComp_ft.csv
#' * us-shore-catch-by-month.csv
#' * us-research-catch-by-month.csv
#' * us-shore-startdate-by-dahl.csv
#' * us-ti-catch-by-month.csv
#'
process_catch_pacfin <- function(pcatch = get_local(file = "pacfin_catch.Rdat"),
                                 nyears = 5,
                                 savedir = hakedata_wd()) {
  # File management
  data("quotas")

  # FLEET XXX is in the hake assessment as shore-based catches,
  # although 1986 differs from data used
  # database  1986 3431.9436
  # assesment 1986 3465.00
  pcatch.yr.per <- stats::aggregate(list("catch" = pcatch$MT),
    list(
      "sector" = pcatch$sector,
      "month" = pcatch$month,
      "year" = pcatch$year
    ),
    FUN = sum
  )
  pcatch.yr.per <- pcatch.yr.per[order(pcatch.yr.per$sector), ]
  pcatch.yr.per$catch <- round(pcatch.yr.per$catch, 5)
  plot_catchvmonthbyyear(
    data = pcatch.yr.per %>%
      dplyr::filter(sector == "USshore") %>%
      dplyr::select(-sector),
    Yrs = as.character(
      (max(pcatch.yr.per$year) - nyears + 1):max(pcatch.yr.per$year)
    ),
    quotas = quotas[3, ],
    title = "U.S. Shoreside Catches",
    file = file.path(savedir, "Figures", "shoresideCatchMonthYear.png")
  )

  utils::write.table(pcatch.yr.per[pcatch.yr.per$sector == "USshore", -1],
    file = file.path(savedir, "us-shore-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
  research <- pcatch[pcatch$sector == "USresearch", ]
  research <- stats::aggregate(list("catch" = research$MT),
    list("month" = research$month, "year" = research$year),
    FUN = sum
  )
  research$catch <- sprintf("%.9f", research$catch)
  utils::write.table(research,
    file = file.path(savedir, "us-research-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )

  # Look at tribal catch in shoreside (already added in above)
  utils::write.table(
    x = pcatch %>%
      dplyr::filter(FLEET == "TI") %>%
      dplyr::group_by(month, year) %>%
      dplyr::summarize(catch = sum(MT)) %>%
      dplyr::arrange(year, month) %>%
      dplyr::ungroup(),
    file = file.path(savedir, "us-ti-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
}
