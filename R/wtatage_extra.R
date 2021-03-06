# Extra Calculations Related to Weight-At-Age
#
# This script depends on stuff created in the script WtAtAgeCollate.R
# which in turn depends on functions in wtatage_calculations.R 
#' todo: give a better description
#' 
#' @param dir The directory where the data is stored. 
#' It can either be relative or absolute, but no working
#' directory will be changed. Instead, the \code{dir} is
#' just used to import data and save resulting plots in.
#' @template dirmod
#' @param outliers A logical value if outliers should be looked for
#' in the individual files or if a summary file that was previously 
#' generated using \code{weightatage} should be loaded.
#' @param maxage The age of the plus group used for the stock assessment.
#' @param yrs A vector of years to search for recent data. Typically,
#' the vector starts with 2008 and ends with the most recent year
#' of data. 
#' @param navgyears The number of early and late years to average since
#' 1975 and \code{max(yrs)} for the early and late analysis asked for
#' by the Scientific Review Group in 2017. The argument can be a single
#' value or a vector of two values, where in the latter case the second
#' value will be used for the most recent time period. 
#' @template nforecast
#' 
#' @import ggplot2 r4ss utils
#' @importFrom stats aggregate
#' @export
#' @author Ian Taylor
#' @return todo: document return
#' 
wtatage_extra <- function(dir, dirmodel = NULL, outliers = TRUE, maxage = 15,
  yrs = 2008:hakedata_year(),
  navgyears = c(5, 3), nforecast = 3) {

  # from maturity.ogives$maturity
  maturity <- c(0.000, 0.000, 0.261, 0.839, 0.961, 0.920, 
    0.928, 0.926, 0.957, 0.944, 0.980, 
    0.962, 1.000, 0.958, 0.955, 0.900, 
    0.900, 0.900, 0.900, 0.900, 0.900) 
  if (length(navgyears) == 1) navgyears <- rep(navgyears, 2)
  if (outliers) {
    dat <- get_wtatagecsv(file = file.path(dir, "LWAdata_1975to2007.csv"),
      outlierplot = TRUE,
      outlierPlotName = file.path(dir, "plots", "wtAgeOutliers1975to2007.png"),
      elimUnsexed = FALSE)
    # Eliminate all pre 2019 Canadian data from compiled files
    # 2019 data comes from a separate csv file and is brought into the
    # year file read in below.
    dat <- dat[!grepl("Can[_a]", dat$Source, ignore.case = TRUE), ]
    #Canadian data
    canall <- wtatage_can(file.path(dir,
      "LengthWeightAge_data_1975to2017_IncludeCanada.csv"))
    #todo: calculate outlier for can data
    canall$outlierL <- FALSE
    dat <- rbind(dat, canall)
    for (yr in yrs) {
      filename <- file.path(dir, paste0("LWAdata_", yr, ".csv"))
      if (!file.exists(filename)) next
      thisyr <- get_wtatagecsv(file = filename,
        outlierplot = TRUE,
        outlierPlotName = file.path(dir, "plots", paste0("wtAgeOutliers", yr, ".png")),
        elimUnsexed = FALSE)
      remove <- (grepl("Can[_a]", thisyr$Source, ignore.case = TRUE) & 
        thisyr$Year < 2019)
      thisyr <- thisyr[!remove, ]
      dat <- rbind(dat, thisyr)
    }
    if (any(is.na(dat$Year))) {
      stop("Year was not read in correctly for some weight-at-age data\n",
        " probably Canadian Acoustic Data.")
    }
    utils::write.csv(dat,
      file = file.path(dir, paste0("LWAdata_1975to", max(yrs), ".csv")))
  } else {
    dat <- utils::read.csv(
      file = file.path(dir, paste0("LWAdata_1975to", max(yrs), ".csv")))
  }
  ### calculating average weight for early or late period for 2018 SRG request
  early <- min(dat$Year):(min(dat$Year) + navgyears[1] - 1)
  late <- (max(yrs) - navgyears[2] + 1):(max(yrs))

  # copied into wtatage.ss file for
  # alternative model developed during 2018 SRG

  # separate data into acoustic (ac) and fishery (fs) subsets
  # note that "Acoustic Poland" is excluded from both sets,
  # not sure if that was intentional
  dat$cat <- gsub("Acoustic [CU].+", "Acoustic", 
    ifelse(grepl("acoustic", dat$Source, ignore.case = TRUE), 
      as.character(dat$Source), "Trawl"))

  mtable <- stats::aggregate(Weight_kg ~ cat + Year + Age_yrs, 
    data = dat[dat$Age_yrs %in% 1:10, ],
    mean, na.rm = TRUE)
  ggplot2::ggplot(data.frame(mtable[!grepl("Poland", mtable$cat), ]), 
    ggplot2::aes(x = Year, y = Weight_kg, col = factor(Age_yrs))) + 
  ggplot2::geom_line() + ggplot2::geom_point() + 
  ggplot2::facet_grid(cat ~ .) +
  ggplot2::xlab("Year") + ggplot2::ylab("Mean weight-at-age (kg)") + 
  ggplot2::labs(col = "Age\n(years)") + 
  ggplot2::scale_color_manual(values = rich.colors.short(20)[1:10]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))
  ggplot2::ggsave(filename = file.path(dir, "meanweightatage_survey.png"))

  # Includes acoustic samples from Poland in 1977
  ggplot2::ggplot(stats::aggregate(Weight_kg ~ Year + Age_yrs, 
    data = dat[dat$Age_yrs %in% 1:maxage, ],
    mean, na.rm = TRUE), 
    ggplot2::aes(x = Year, y = Weight_kg, col = factor(Age_yrs))) + 
  ggplot2::geom_line() + ggplot2::geom_point() + 
  ggplot2::xlab("Year") + ggplot2::ylab("Mean weight-at-age (kg)") + 
  ggplot2::labs(col = "Age\n(years)") + 
  ggplot2::scale_color_manual(values = rich.colors.short(20)[1:maxage]) + 
  ggplot2::theme_bw()
  ggplot2::ggsave(filename = file.path(dir, "meanweightatage_all.png"))
  if (dev.cur() > 1) dev.off()

  #### making input files for SS with the holes still present
  # NULL months keeps the Poland data
  wtage_All <- make_wtage_matrix(dat,fleetoption=2,
    months = NULL) # make matrix
  wtage_All_wMean <- make_wtage_matrix(dat,fleetoption=2,getmean=TRUE,
    yearsearly = unique(dat$Year),
    months = NULL)

  avgearl <- make_wtage_matrix(dat,fleetoption=2,getmean=TRUE,
    yearsearly = early,
    months = NULL)[1, ]
  avgearl[2, ] <- avgearl[1, ] * c(rep(1, 6), maturity)
  avgearl[2, "fleet"] <- -2
  utils::write.table(avgearl,
    file = file.path(dir, paste0("wtatage.ss_early", navgyears[1], ".txt")),
    sep = " ", row.names = FALSE, col.names = FALSE)

  #### making alternative data.frame with mean lengths
  lenage_All_wMean <- make_wtage_matrix(dat,fleetoption=2, value="length", getmean=TRUE,
    months = NULL) # make matrix

  # repeat but return sample sizes instead of mean weights
  counts_All_wMean <- make_wtage_matrix(dat,fleetoption=2,getmean=TRUE, value="count",
    months = NULL)
  utils::write.csv(
    setNames(counts_All_wMean, gsub("#", "", colnames(counts_All_wMean))),
    file.path(dir, "wtatage_all_samplesize.csv"),
    row.names = FALSE)

  # new method does only linear interpolation within each age (only works with all data)
  wtageInterp1_All         <- dointerpSimple(wtage_All)

  #### do 2nd interpolation (actually an extrapolation at the edges)
  ### there is no 23rd column to remove, but the commands work anyway
  # all data
  wtageInterp2_All <- fill_wtage_matrix(wtageInterp1_All[,-23])
  wtageInterp2_All$Note <- fill_wtage_matrix(wtage_All[,-23])$Note

  # write output combining all fleets closer to format used by SS
  wtage_All_wMean$Note <- c(paste("# Mean from ",min(dat$Year),"-",max(dat$Year),sep=""),wtageInterp2_All$Note)
  wtageInterp2_All <- rbind(wtage_All_wMean[1,], wtageInterp2_All)
  mat_Interp2_All <- t(as.matrix(wtageInterp2_All[,
    -grep("^[^a]|Note", colnames(wtageInterp2_All))]))

  # matrices for plotting
  make_wtatage_plots(plots=1:6, 
    data = wtage_All_wMean, 
    counts = counts_All_wMean, 
    lengths = lenage_All_wMean,
    dir = dir, year = max(yrs), maxage = maxage, verbose = FALSE)

  # adding ages 16-20 as repeats of age 15
  wtage_extended <- wtageInterp2_All[, -grep("Note", colnames(wtageInterp2_All))]
  wtage_extended <- wtage_extended[, c(1:ncol(wtage_extended), 
    rep(ncol(wtage_extended), times = sum(!(1:length(maturity)-1) %in% 0:maxage)))]
  wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))] <- 
    round(wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))], 4)
  colnames(wtage_extended)[grep("^a", colnames(wtage_extended))] <- 
    paste0("a", seq_along(maturity) - 1)

  ## Add forecast average
  withforecast <- rbind(wtage_extended,
    setNames(data.frame(max(late):(max(late)+nforecast - 1)+1, matrix(c(1, 1,1, 1, 0,
    apply(wtage_extended[wtage_extended[, 1] %in% late, -c(1:6)], 2, mean)),
    ncol = NCOL(wtage_extended)-1, nrow = nforecast, byrow = TRUE)),
    colnames(wtage_extended)))

  filenameforss <- file.path(dir, paste0("wtatage_", max(yrs), "created_",
    format(Sys.time(),"%d-%b-%Y_%H.%M"),".ss"))
  if (file.exists(filenameforss)) file.remove(filenameforss)
  write_wtatage_file(file = filenameforss, data = withforecast, maturity = maturity)
  if (!is.null(dirmodel)) {
    file.copy(filenameforss, file.path(dirmodel, "wtatage.ss"),
      overwrite = TRUE)
  }

  save(dat,mat_Interp2_All, wtage_All, wtage_All_wMean, withforecast,
    file = file.path(dir, "LWAdata.Rdata"))
}
