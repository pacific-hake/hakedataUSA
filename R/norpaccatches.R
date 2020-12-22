#' Workup NORPAC Catches
#'
#' Summarize and plot NORPAC catches for the at-sea hake fishery.
#' Fleet types are assigned to create summaries by fleet and year.
#'
#' @details *Tribal catches*
#' @template cdq_code
#'
#' @param ncatch An R object with NORPAC catches. If \code{NULL},
#' which is the default, then the \code{.Rdat} file will be read
#' from the disk.
#' @param nyears The number of years for plotting.
#' @template species
#'
#' @import ggplot2 grDevices
#' @export
#' @author Kelli Faye Johnson
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
norpaccatches <- function(ncatch = NULL, nyears = 5,
  species = 206) {

  #### Internal functions
  #' @param x A named list of values by year, more than likely
  #' made by tapply or split.
  #' @param country The country the data is coming from, which
  #' is only relevant if you are saving the data to the disk
  #' for naming purposes.
  #' @param type A character value that will be used for the name
  #' of the file.
  #' The first portion should be the sector such as "atsea".
  #' The second portion should be "fishing" if the file is not
  #' bottom depth, which what the Canadians export.
  #' @param dir A directory where you want to save the data. If left
  #' \code{NULL} then no data will be saved and just a data frame will
  #' be returned.
  exportdepth <- function(x, country = c("US", "CAN"),
    type = c("sector-bottom"), dir = NULL) {
   
    country <- match.arg(country, several.ok = FALSE)
    # Consult US or Canadian counterparts before changing this
    # code! The function is duplicated across repositories
    # to access confidential data. The same quantiles must
    # be exported by each country.
    aa <- sapply(lapply(x, boxplot.stats), "[[", "stats")
    aa[1, ] <- sapply(x, quantile, probs = c(0.025), na.rm = TRUE)
    aa[5, ] <- sapply(x, quantile, probs = c(0.975), na.rm = TRUE)
    rownames(aa) <- c("lower95", "lowerhinge", "median",
      "upperhinge", "upper95")
    aa <- t(aa)
    aa <- data.frame("year" = as.numeric(rownames(aa)), aa)
    if (!is.null(dir)) {
      utils::write.csv(aa, file = file.path(dir,
        paste0("depth-", tolower(country),"-", type, ".csv")),
        row.names = FALSE)
    }
    return(aa)
  }

  #### Setup the environment
  args <- list(width = 6.5, height = 4.5,
    pointsize = 10, units = "in", res = 600)
  oldop <- options()$warn
  options(warn = -1)
  on.exit(options(warn = oldop), add = TRUE)

  mydir <- hakedatawd()
  dir.create(file.path(mydir, "Figures", "CONFIDENTIAL"),
    showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(file.path(mydir, "Figures"))) {
    stop("The folder 'CONFIDENTIAL' doesn't exist in 'Figures' in ", mydir)
  }

  if (is.null(ncatch)) {
    load(file.path(mydir, "extractedData", "NORPACdomesticCatch.Rdat"))
  }
  outncatch <- processNorpacCatch(ncatch,
    outfname = file.path(mydir, "Catches"))

  hcatch <- outncatch[outncatch$SPECIES == species, ]
  # Unique vessel (either catcher boat if not NA or mothership)
  hcatch[, "vcolumn"] <- ifelse(is.na(hcatch[, "CATCHER_BOAT_ADFG"]),
    hcatch[, "VESSEL"],
    hcatch[, "CATCHER_BOAT_ADFG"])
  keeptheseyears <- utils::tail(1:max(hcatch$year, na.rm = TRUE), nyears)

  #### Figures: depths
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year"))
  stopifnot("Yearly summaries are not confidential" =
    all(stats::aggregate(ngroups ~ year, data = hcatch, unique)[, "ngroups"] > 2)
    )
  gg <- plot_boxplot(
    data = stats::reshape(
      data = hcatch[hcatch[, "year"] %in% keeptheseyears &
                    hcatch[, "ngroups"] > 2, ],
      direction = "long",
      varying = c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS"),
      v.names = "fathoms", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("year", "type"), showmedian = TRUE,
    yvar = "fathoms", ylab = "Depth (fathoms)", mlab = "",
    incolor = "year", scales = "free",
    file = file.path(mydir, "Figures", "fishDepthsUS.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- plot_boxplot(
    data = stats::reshape(hcatch,
      direction = "long",
      varying = c("FISHING_DEPTH_M", "BOTTOM_DEPTH_M"),
      v.names = "meters", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("year", "type"), showmedian = TRUE,
    yvar = "meters", ylab = "Depth (m)", mlab = "",
    incolor = "year", scales = "free",
    file = file.path(mydir, "Figures", "fishDepthsUSallyears.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  cols <- c("year",
    grep(value = TRUE, "lower|median|upper", colnames(gg[["data"]])))
  utils::write.csv(
    x = gg[["data"]][grepl("Bottom", gg[["data"]][, "type"]), cols],
    file = file.path(mydir, "Catches", "depth-us-atsea-bottom.csv"),
    row.names = FALSE)
  utils::write.csv(
    x = gg[["data"]][grepl("Fishing", gg[["data"]][, "type"]), cols],
    file = file.path(mydir, "Catches", "depth-us-atsea-fishing.csv"),
    row.names = FALSE)

  hcatch[, "months"] <- droplevels(factor(hcatch$month, levels = 1:12,
    labels = rep(paste(seq(1, 11, by = 2), seq(2, 12, by = 2), sep = "-"),
    each = 2)))
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year", "months"))
  gg <- plot_boxplot(hcatch[hcatch[, "ngroups"] > 2, ],
    xvar = c("months", "year"), showmedian = TRUE,
    yvar = "FISHING_DEPTH_FATHOMS", ylab = "Fishing depth (fathoms)", mlab = "",
    file = file.path(mydir, "Figures", "fishFISHING_DEPTH_FATHOMS_US.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- plot_boxplot(hcatch[hcatch[, "ngroups"] > 2, ],
    xvar = c("months", "year"), showmedian = TRUE,
    yvar = "BOTTOM_DEPTH_FATHOMS", ylab = "Bottom depth (fathoms)", mlab = "",
    file = file.path(mydir, "Figures", "fishBOTTOM_DEPTH_FATHOMS_US.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])

  gg <- plot_boxplot(hcatch[!is.na(hcatch$VESSEL) &
    hcatch$year %in% keeptheseyears, ],
    xvar = c("VESSEL", "year"), showmedian = TRUE,
    yvar = "BOTTOM_DEPTH_FATHOMS", ylab = "Bottom depth (fathoms)", mlab = "",
    file = file.path(mydir, "Figures", "CONFIDENTIAL", "fishBottomDepthByVesselUS.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year", "vesseltype"))
  gg <- plot_boxplot(
    data = stats::reshape(
      data = hcatch[hcatch[, "year"] %in% keeptheseyears &
                    hcatch[, "ngroups"] > 2, ],
      direction = "long",
      varying = c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS"),
      v.names = "fathoms", timevar = "type",
      times = c("(a) Fishing depth", "(b) Bottom depth")),
    xvar = c("vesseltype", "type", "year"), xlab = "At-Sea sector",
    showmedian = TRUE,
    yvar = "fathoms", ylab = "Depth (fathoms)", mlab = "",
    scales = "free", nrow = 2,
    file = file.path(mydir, "Figures", "fishDepthByYearUS.png"),
    width = 7, height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])

  #### Figure: catch rate
  hcatch <- get_confidential(hcatch, yvar = "vcolumn", xvar = c("year", "month"))
  gg <- plot_boxplot(data = hcatch,
    xvar = c("month", "year"),
    yvar = "crate",
    ylab = "Unstandardized catch-per-hour (mt/hour)",
    mlab = "U.S. at-sea unstandardized yearly catch-rates.",
    file = file.path(mydir, "Figures", "Confidential", "fishCatchRatesUSByYear.png"),
    width = args[["width"]], height = args[["height"]],
    units = args[["units"]], dpi = args[["res"]])
  gg <- mapply(plot_boxplot,
    data = list(
      hcatch[hcatch[, "year"] %in% keeptheseyears, ],
      hcatch[hcatch[, "year"] %in% keeptheseyears & hcatch[, "ngroups"] > 2, ],
      hcatch[hcatch[, "year"] %in% keeptheseyears & hcatch[, "ngroups"] > 2, ]),
    file = list(
      file.path(mydir, "Figures", "Confidential", "fishCatchRatesUS.png"),
      file.path(mydir, "Figures", "fishCatchRatesUS.png"),
      file.path(mydir, "Figures", "fishCatchRatesUSnolog.png")),
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
      ylab = "Unstandardized catch-per-hour (mt/hour)",
      legend.position = c(0.1, 0.23),
      width = args[["width"]], height = args[["height"]],
      units = args[["units"]], dpi = args[["res"]]
      ))

  #### Summaries
  # Summarize catches by depth
  utils::write.table(t(do.call("rbind",
    tapply(hcatch[, "BOTTOM_DEPTH_FATHOMS"], hcatch[, "year"],
      range, na.rm = TRUE, simplify = TRUE))),
    file = file.path(mydir, "Catches", "NORPAC_DomesticAtSea_bdepthfathom_range.csv"),
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  # Number of tows deeper than 1500 fathoms
  temp <- get_confidential(hcatch[hcatch$BOTTOM_DEPTH_FATHOMS > 1500, ],
    yvar = "vcolumn",
    xvar = c("month", "year"))
  utils::write.table(x = setNames(aggregate(BOTTOM_DEPTH_FATHOMS ~ year + month,
    drop = TRUE,
    data = temp[temp[, "ngroups"] > 2, ],
    FUN = length), c("year", "month", "ntowsdeeper1500f")),
    file = file.path(mydir, "Catches", "NORPAC_DomesticAtSea_bdepthfathom_deeper1500f.csv"))
  utils::write.table(
    aggregate(Date ~ year, data = hcatch[hcatch$VESSEL_TYPE == 1, ], min),
    file = file.path(mydir, "Catches", "us-cp-startdate.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)
  utils::write.table(
    aggregate(Date ~ year, data = hcatch[hcatch$VESSEL_TYPE == 2, ], min),
    file = file.path(mydir, "Catches", "us-ms-startdate.csv"),
    sep = ",", quote = FALSE, row.names = FALSE)

}
