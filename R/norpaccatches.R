#' Workup NORPAC Catches
#'
#' Summarize and plot NORPAC catches for the at-sea hake fishery.
#' Fleet types are assigned to create summaries by fleet and year.
#'
#' @details *Tribal catches*
#' @template cdq_code
#' 
#' @details # Raw data
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
#' @details # Definitions
#' \enumerate{
#'   \item @template official_total_catch
#'   \item @template extrapolated_weight
#' }
#'
#' @details # Assumptions
#' \enumerate{
#'   \item sampled hauls have an entry in the \code{SPECIES} column,
#'   i.e., hake are denoted with 206
#'   \item an unsampled haul is represented by one unique record
#' }
#'
#' @param ncatch An R object with catch information extracted from the
#'   NORPAC database. The default is to read the `.Rdat` file
#'   from the disk using [loadlocal()]. Else, users can just pass the R object
#'   itself, which would typically be `hakedata[["ncatch"]]`.
#' @param nyears The number of years for plotting.
#' @template species
#'
#' @import ggplot2 grDevices
#' @export
#' @author Kelli F. Johnson
#'
#' @return Figures and csv files are saved to the disk regarding
#' catch rates and depth (fishing and bottom depths).
#' In the `Figures` directory, the following png or csv files are saved:
#'   * fishDepthsUS.png
#'   * fishDepthsUSallyears.png
#'   * fish_FISHING_DEPTH_FATHOMS_US.png
#'   * fish_BOTTOM_DEPTH_FATHOMS_US.png
#'   * fishBottomDepthByVesselUS.png
#'   * fishCatchRatesUSByYear.png
#'   * fishDepthByYearUS.png
#'   * fishCatchRatesUS.png
#'   * fishCatchRatesUSnolog.png
#'   * NORPAC_DomesticAtSea_bdepthfathom_range.csv
#'   * NORPAC_DomesticAtSea_bdepthfathom_deeper1500f.csv
#'   * us-cp-startdate.csv
#'   * us-ms-startdate.csv
#'
#' In the `Catches` directory, the following csv files are saved:
#'   * depth-us-atsea-bottom.csv
#'   * depth-us-atsea-fishing.csv
#'
norpaccatches <- function(ncatch = loadlocal(file = "norpac_catch.Rdat"),
                          nyears = 5,
                          species = 206,
                          savedir = hakedatawd()) {
  # Setup the environment
  args <- list(width = 6.5, height = 4.5,
    pointsize = 10, units = "in", res = 600)
  oldop <- options()$warn
  options(warn = -1)
  on.exit(options(warn = oldop), add = TRUE)

  # File management
  fs::dir_create(
    path = file.path(savedir, "Figures", "CONFIDENTIAL"),
    recurse = TRUE
  )
  fs::dir_create(
    path = file.path(savedir, "Catches"),
    recurse = TRUE
  )
  quotas <- utils::read.csv(
    file = file.path(savedir, "Catches", "quotas.csv"),
    sep = ",", header = TRUE, check.names = FALSE
  )

  outncatch <- ncatch %>%
    dplyr::rename_at(
      ggplot2::vars(dplyr::matches("HAUL.+JOIN")), ~ "HAULJOIN"
    ) %>%
    dplyr::mutate(
      SPECIFICHAUL = paste(format(HAULJOIN, digits = 19), sep = "_"),
      Date = as.Date(RETRIEVAL_DATE, f = "%Y-%m-%d"),
      month = get_date(RETRIEVAL_DATE, "%m"),
      Month = droplevels(factor(format(Date, format = "%b"), month.abb)),
      year = get_date(RETRIEVAL_DATE, "%Y"),
      crate = EXTRAPOLATED_WEIGHT / 1000 / HRS,
      FISHING_DEPTH_M = FISHING_DEPTH_FATHOMS * fathom_to_meter,
      BOTTOM_DEPTH_M = BOTTOM_DEPTH_FATHOMS * fathom_to_meter,
      vesseltype = norpac_vesseltype(VESSEL_TYPE),
      Sector = "DomesticAtSea",
      sampled = ifelse(is.na(HAUL_SAMPLED_BY) | HAUL_SAMPLED_BY == 0, 0, 1),
      # Unsampled hauls will have a SPECIES == NA and EXTRAPOLATED_WEIGHT == NA
      SPECIES = ifelse(sampled == 0, species, SPECIES),
      OFFICIAL_TOTAL_CATCHkg = OFFICIAL_TOTAL_CATCH * 1000,
      ByCatch = OFFICIAL_TOTAL_CATCHkg - EXTRAPOLATED_WEIGHT
    ) %>%
    dplyr::group_by(year, month, SPECIES == species, VESSEL_TYPE) %>%
    dplyr::mutate(
      bycatchrate = sum(ifelse(sampled == 1, ByCatch, 0), na.rm = TRUE) / 
        sum(ifelse(sampled == 1, OFFICIAL_TOTAL_CATCHkg, 0), na.rm = TRUE),
      Catch.MT = ifelse(sampled == 0,
        OFFICIAL_TOTAL_CATCHkg * (1 - bycatchrate),
        EXTRAPOLATED_WEIGHT) / 1000,
      ByCatch = ifelse(sampled == 0,
        OFFICIAL_TOTAL_CATCHkg - Catch.MT / 1000,
        ByCatch
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

  utils::write.table(
    x = stats::aggregate(month ~ sampled + vesseltype + year,
      data = outncatch[outncatch$SPECIES == species, ], length
    ),
    file = file.path(
      savedir, "Catches",
       "NORPAC_DomesticAtSea_nsampledhauls.csv"
    ),
    row.names = FALSE, sep = ",", append = FALSE)
  utils::write.table(
    x = stats::aggregate(bycatchrate ~ year + month + vesseltype, 
      data = outncatch[outncatch$SPECIES == species & outncatch$sampled == 0,],
      mean
    ),
    file = file.path(
      savedir, "Catches",
       "NORPAC_DomesticAtSea_bycatchrate.csv"
    ),
    row.names = FALSE, sep = ",", append = FALSE
  )
  utils::write.table(
    x = stats::aggregate(Catch.MT ~ Sector + month + year,
      data = outncatch[outncatch$SPECIES == species, ],
      sum, na.rm = TRUE
    ),
    file = file.path(
      savedir, "Catches",
      "us-catch-by-month.csv"
    ),
    row.names = FALSE, sep = ",", append = FALSE
  )
  cp <- catchout[catchout$vesseltype == "CP", -(1:2)]
  plot_catchvmonthbyyear(
    data = cp,
    Yrs = as.character((max(cp$year) - nyears + 1):max(cp$year)),
    title = paste(
      "U.S. CP Catches",
      ifelse(hakedata_prelim(), "(preliminary)", "")),
    quotas = quotas[1, -1],
    file = file.path(savedir, "Figures", "CpCatchMonthYear.png")
  )
  utils::write.table(cp,
    file = file.path(
      savedir, "Catches",
       "us-cp-catch-by-month.csv"
    ),
    sep = ",", row.names = FALSE, quote = FALSE
  )
  ms <- catchout[catchout$vesseltype == "MS", -(1:2)]
 plot_catchvmonthbyyear(
    data = ms,
    file = file.path(savedir, "Figures", "MsCatchMonthYear.png"),
    Yrs = as.character((max(ms$year) - nyears + 1):max(ms$year)),
    title = paste(
      "U.S. MS Catches",
      ifelse(hakedata_prelim(), "(preliminary)", "")),
    quotas = quotas[2, -1]
  )
  utils::write.table(ms,
    file = file.path(
      savedir, "Catches",
       "us-ms-catch-by-month.csv"
    ),
    sep = ",", row.names = FALSE, quote = FALSE
  )

  utils::write.table(
    x = outncatch %>%
      dplyr::filter(
        CDQ_CODE == "M01",
        !is.na(CDQ_CODE),
        SPECIES == species
      ) %>%
      dplyr::mutate(Sector = "DomesticAtSeaTribal") %>%
      dplyr::group_by(Sector, year) %>%
      dplyr::summarize(catch = sum(catch, na.rm = TRUE)) %>%
      dplyr::ungroup(),
    file = file.path(
      savedir, "Catches",
      "NORPAC_DomesticAtSea_tribal.csv"),
    sep = ",", row.names = FALSE, quote = FALSE
  )

  hcatch <- outncatch %>%
    dplyr::filter(SPECIES == species) %>%
    dplyr::mutate(vcolumn = tidyr::replace_na(CATCHER_BOAT_ADFG,var(VESSEL)))
  # Unique vessel (either catcher boat if not NA or mothership)
  keeptheseyears <- utils::tail(1:max(hcatch$year, na.rm = TRUE), nyears)

  #### Figures: depths
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year"))
  stopifnot("Yearly summaries are not confidential" =
    all(stats::aggregate(
      ngroups ~ year,
      data = hcatch,
      unique
    )[, "ngroups"] > 2)
    )
  gg <- plot_boxplot(
    data = stats::reshape(
      data = dplyr::filter(hcatch,
        year %in% keeptheseyears & ngroups > 2
      ) %>%
      data.frame,
      direction = "long",
      varying = c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS"),
      v.names = "fathoms", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("year", "type"), showmedian = TRUE,
    yvar = "fathoms", ylab = "Depth (fathoms)", mlab = "",
    incolor = "year", scales = "free",
    file = file.path(savedir, "Figures", "fishDepthsUS.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- plot_boxplot(
    data = stats::reshape(hcatch %>% data.frame,
      direction = "long",
      varying = c("FISHING_DEPTH_M", "BOTTOM_DEPTH_M"),
      v.names = "meters", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("year", "type"), showmedian = TRUE,
    yvar = "meters", ylab = "Depth (m)", mlab = "",
    incolor = "year", scales = "free",
    file = file.path(savedir, "Figures", "fishDepthsUSallyears.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  cols <- c("year",
    grep(value = TRUE, "lower|median|upper", colnames(gg[["data"]])))
  utils::write.csv(
    x = gg[["data"]][grepl("Bottom", gg[["data"]][, "type"]), cols],
    file = file.path(savedir, "Catches", "depth-us-atsea-bottom.csv"),
    row.names = FALSE)
  utils::write.csv(
    x = gg[["data"]][grepl("Fishing", gg[["data"]][, "type"]), cols],
    file = file.path(savedir, "Catches", "depth-us-atsea-fishing.csv"),
    row.names = FALSE)
  hcatch[, "months"] <- droplevels(factor(hcatch$month, levels = 1:12,
    labels = rep(paste(seq(1, 11, by = 2), seq(2, 12, by = 2), sep = "-"),
    each = 2)))
  hcatch <- get_confidential(hcatch,
    yvar = "vcolumn",
    xvar = c("year", "months")
  )
  gg <- plot_boxplot(hcatch[hcatch[, "ngroups"] > 2, ],
    xvar = c("months", "year"), showmedian = TRUE,
    yvar = "FISHING_DEPTH_FATHOMS", ylab = "Fishing depth (fathoms)", mlab = "",
    file = file.path(savedir, "Figures", "fishFISHING_DEPTH_FATHOMS_US.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- plot_boxplot(hcatch[hcatch[, "ngroups"] > 2, ],
    xvar = c("months", "year"), showmedian = TRUE,
    yvar = "BOTTOM_DEPTH_FATHOMS", ylab = "Bottom depth (fathoms)", mlab = "",
    file = file.path(savedir, "Figures", "fishBOTTOM_DEPTH_FATHOMS_US.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])

  gg <- plot_boxplot(hcatch[!is.na(hcatch$VESSEL) &
    hcatch$year %in% keeptheseyears, ],
    xvar = c("VESSEL", "year"), showmedian = TRUE,
    yvar = "BOTTOM_DEPTH_FATHOMS", ylab = "Bottom depth (fathoms)", mlab = "",
    file = file.path(
      savedir,
      "Figures",
      "CONFIDENTIAL",
      "fishBottomDepthByVesselUS.png"
    ),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  hcatch <- get_confidential(hcatch,
    yvar = "vcolumn",
    xvar = c("year", "vesseltype")
  )
  gg <- plot_boxplot(
    data = stats::reshape(
      data = dplyr::filter(hcatch, year %in% keeptheseyears & ngroups > 2) %>% data.frame,
      direction = "long",
      varying = c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS"),
      v.names = "fathoms", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("vesseltype", "type", "year"), xlab = "At-Sea sector",
    showmedian = TRUE,
    yvar = "fathoms", ylab = "Depth (fathoms)", mlab = "",
    scales = "free", nrow = 2,
    file = file.path(savedir, "Figures", "fishDepthByYearUS.png"),
    width = 7, height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])

  #### Figure: catch rate
  hcatch <- get_confidential(
    hcatch,
    yvar = "vcolumn",
    xvar = c("year", "month")
  )
  ylabelexpression <- expression(
    Unstandardized~catch~rates~ (mt~'*'~hr^{-1})
  )
  gg <- plot_boxplot(data = hcatch,
    xvar = c("month", "year"),
    yvar = "crate",
    ylab = ylabelexpression,
    mlab = "U.S. at-sea unstandardized yearly catch-rates.",
    file = file.path(
      savedir,
      "Figures",
      "Confidential",
      "fishCatchRatesUSByYear.png"
    ),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- mapply(plot_boxplot,
    data = list(
      dplyr::filter(hcatch, year %in% keeptheseyears) %>% data.frame,
      dplyr::filter(hcatch, year %in% keeptheseyears & ngroups > 2) %>% data.frame,
      dplyr::filter(hcatch, year %in% keeptheseyears & ngroups > 2) %>% data.frame
    ),
    file = list(
      file.path(savedir, "Figures", "Confidential", "fishCatchRatesUS.png"),
      file.path(savedir, "Figures", "fishCatchRatesUS.png"),
      file.path(savedir, "Figures", "fishCatchRatesUSnolog.png")),
    yscale = list("log10", "log10", "identity"),
    mlab = list(
      "U.S. at-sea unstandardized yearly catch-rates (confidential)",
      "U.S. at-sea unstandardized yearly catch-rates",
      "U.S. at-sea unstandardized yearly catch-rates"
      ),
    MoreArgs = list(
      xvar = c("Month"), showmedian = TRUE,
      incolor = "year",
      yvar = "crate",
      ylab = ylabelexpression,
      legend.position = c(0.5, 0.95),
      legend.direction = "horizontal",
      width = args[["width"]], height = args[["height"]],
      units = args[["units"]], dpi = args[["res"]]
      ))

  #### Summaries
  # Summarize catches by depth
  utils::write.table(t(do.call("rbind",
    tapply(
      dplyr::pull(hcatch, BOTTOM_DEPTH_FATHOMS) ,
      dplyr::pull(hcatch, year),
      range, na.rm = TRUE, simplify = TRUE))),
    file = file.path(
      savedir,
      "Catches",
      "NORPAC_DomesticAtSea_bdepthfathom_range.csv"
    ),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  # Number of tows deeper than 1500 fathoms
  temp <- get_confidential(hcatch[hcatch$BOTTOM_DEPTH_FATHOMS > 1500, ],
    yvar = "vcolumn",
    xvar = c("month", "year"))
  utils::write.table(x = setNames(aggregate(BOTTOM_DEPTH_FATHOMS ~ year + month,
    drop = TRUE,
    data = temp[temp[, "ngroups"] > 2, ],
    FUN = length), c("year", "month", "ntowsdeeper1500f")),
    file = file.path(
      savedir,
      "Catches",
      "NORPAC_DomesticAtSea_bdepthfathom_deeper1500f.csv")
    )
  utils::write.table(
    aggregate(Date ~ year, data = hcatch[hcatch$VESSEL_TYPE == 1, ], min),
    file = file.path(savedir, "Catches", "us-cp-startdate.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)
  utils::write.table(
    aggregate(Date ~ year, data = hcatch[hcatch$VESSEL_TYPE == 2, ], min),
    file = file.path(savedir, "Catches", "us-ms-startdate.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)

}
