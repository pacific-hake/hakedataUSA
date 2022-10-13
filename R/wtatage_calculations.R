#' Create weight-at-age files for hake assessment
#'
#' Create weight-at-age files for the hake stock assessment.
#' Updated csv or rds files must exist prior to running.
#'
#' @param dir The directory where the data is stored.
#' It can either be relative or absolute because the working
#' directory will not be changed. Instead, \code{dir} is
#' used to import data and export results to.
#' @param maxage The age of the plus group used for the stock assessment.
#' This will correspond to the maximum age group in the data, not in the
#' model because SS can model many ages when there is only information in
#' the data for a few ages.
#' @param yrs A vector of years to search for recent data. Typically,
#' the vector starts with 2008 and ends with the most recent year
#' of data. This will allow files created from `wtatage_collate()` to
#' be included in the analysis, i.e., recent US data. Typically, you
#' should not have to change this value from the default entry.
#' @param navgyears The number of early and late years to average since
#' 1975 and \code{max(yrs)} for the early and late analysis asked for
#' by the Scientific Review Group in 2017. The argument can be a single
#' value or a vector of two values, where in the latter case the second
#' value will be used for the most recent time period.
#' @param nforecast The number of years to forecast into the future.
#' Typically, this is three for the hake assessment and will lead to
#' this many rows of mean weight-at-age data being copied to the data frame
#' beyond the last year of empirical data.
#' @param maturity A vector of maturity values from the maturity ogive.
#' The length needs to be the same as the number of ages in the model,
#' not the number of ages in the data.
#'
#' @import ggplot2 r4ss utils
#' @export
#' @author Ian Taylor
#' @return todo: document return
#'
data_wtatage <- function(
  dir,
  maxage = 15,
  yrs = 2008:hakedata_year(),
  navgyears = c(5, 5),
  nforecast = 3,
  maturity = c(
    0.000, 0.000, 0.261, 0.839, 0.961,
    0.920, 0.928, 0.926, 0.957, 0.944,
    0.980, 0.962, 1.000, 0.958, 0.955,
    0.900, 0.900, 0.900, 0.900, 0.900, 0.900)
  ) {

  if (length(navgyears) == 1) navgyears <- rep(navgyears, 2)
  fs::dir_create(path = file.path(dir, "plots"))

  # Data provided by CG on 2021-01-09 in google drive #703
  # filtered by area rather than month and provided as rds rather
  # than csv to save on size, contains all US samples in
  # LWAdata_1975to2007.csv, so eliminated that file.
  dat <- get_wtatagecsv(
    file = file.path(dir, "LengthWeightAge_data.rds"),
    verbose = TRUE,
    outlierplot = TRUE,
    outlierPlotName = file.path(dir, "plots", "wtAgeOutliers1975to2007.png"),
    elimUnsexed = FALSE)
  # Fix some names
  dat[, "Source"] <- gsub("jv", "JV", dat[, "Source"])
  dat[, "Source"] <- gsub("Poland_acoustic", "Acoustic Poland", dat[, "Source"])
  # Eliminate all post 2007 US data b/c read in below
  dat <- dat[
    !(grepl("US_", dat[, "Source"], ignore.case = TRUE) &
      dat[, "Year"] > 2007),
    ]
  for (yr in yrs) {
    filename <- file.path(dir, paste0("LWAdata_", yr, ".csv"))
    if (!file.exists(filename)) next
    thisyr <- get_wtatagecsv(file = filename,
      outlierplot = TRUE,
      outlierPlotName = file.path(dir, "plots", paste0("wtAgeOutliers", yr, ".png")),
      elimUnsexed = FALSE)
    # Remove the CAN data because it is in dat
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
    file = file.path(dir, paste0("LWAdata_1975to", max(yrs), ".csv"))
  )

  ### calculating average weight for early or late period for 2018 SRG request
  early <- min(dat$Year):(min(dat$Year) + navgyears[1] - 1)
  late <- (max(yrs) - navgyears[2] + 1):(max(yrs))
  # separate data into acoustic (ac) and fishery (fs) subsets
  dat$cat <- ifelse(grepl("acoustic", dat$Source, ignore.case = TRUE),
      "Survey", "Fishery")
  mtable <- stats::aggregate(Weight_kg ~ cat + Year + Age_yrs,
    data = dat[dat[, "Age_yrs"] %in% 1:10, ],
    mean, na.rm = TRUE)
  gg <- ggplot2::ggplot(data.frame(mtable),
    ggplot2::aes(x = Year, y = Weight_kg, col = factor(Age_yrs))) +
  ggplot2::geom_line() + ggplot2::geom_point() +
  ggplot2::facet_grid(cat ~ .) +
  ggplot2::xlab("Year") + ggplot2::ylab("Mean weight-at-age (kg)") +
  ggplot2::labs(col = "Age\n(years)") +
  ggplot2::scale_color_manual(values = rich.colors.short(20)[1:10]) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))
  ggplot2::ggsave(gg,
    width = 7, height = 7, units = "in",
    filename = file.path(dir, "plots", "meanweightatage_source.png"))

  gg <- ggplot2::ggplot(stats::aggregate(Weight_kg ~ Year + Age_yrs,
    data = dat[dat[, "Age_yrs"] %in% 1:maxage, ],
    mean, na.rm = TRUE),
    ggplot2::aes(x = Year, y = Weight_kg, col = factor(Age_yrs))) +
  ggplot2::geom_line() + ggplot2::geom_point() +
  ggplot2::xlab("Year") + ggplot2::ylab("Mean weight-at-age (kg)") +
  ggplot2::labs(col = "Age\n(years)") +
  ggplot2::scale_color_manual(values = rich.colors.short(20)[1:maxage]) +
  ggplot2::theme_bw()
  ggplot2::ggsave(gg,
    width = 7, height = 7, units = "in",
    filename = file.path(dir, "plots", "meanweightatage_all.png"))

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
    file.path(dirname(normalizePath(dir)), "wtatage_all_samplesize.csv"),
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
  make_wtatage_plots(plots = 1:6,
    data = wtage_All_wMean,
    counts = counts_All_wMean,
    lengths = lenage_All_wMean,
    dir = file.path(dir, "plots"),
    year = max(yrs), maxage = maxage, verbose = FALSE)

  # adding ages 16-20 as repeats of age 15
  wtage_extended <- wtageInterp2_All[, -grep("Note", colnames(wtageInterp2_All))]
  wtage_extended <- wtage_extended[, c(1:ncol(wtage_extended),
    rep(ncol(wtage_extended), times = sum(!(1:length(maturity)-1) %in% 0:maxage)))]
  wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))] <-
    round(wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))], 4)
  colnames(wtage_extended)[grep("^a", colnames(wtage_extended))] <-
    paste0("a", seq_along(maturity) - 1)

browser()

  ## Add forecast average
  withforecast <- rbind(wtage_extended,
    setNames(data.frame(max(late):(max(late)+nforecast - 1)+1, matrix(c(1, 1,1, 1, 0,
    apply(wtage_extended[wtage_extended[, 1] %in% late, -c(1:6)], 2, mean)),
    ncol = NCOL(wtage_extended)-1, nrow = nforecast, byrow = TRUE)),
    colnames(wtage_extended)))
  # Create data frame for "last year's data with this year's analysis"
  # because sometimes the way that the data is worked up changes and
  # you can't just subtract a year of data because of the forecast average
  # used in bridging.
  withforecast_old <- rbind(wtage_extended[-NROW(wtage_extended), ],
    setNames(data.frame(
      max(late):(max(late)+nforecast - 1),
      matrix(c(1, 1,1, 1, 0,
    apply(
      X = wtage_extended[wtage_extended[, 1] %in% (late - 1), -c(1:6)],
      MARGIN = 2, FUN = mean)),
    ncol = NCOL(wtage_extended)-1,
    nrow = nforecast, byrow = TRUE)),
    colnames(wtage_extended)))

  filenameforss <- file.path(dir, paste0("wtatage_", max(yrs), "created_",
    format(Sys.time(),"%d-%b-%Y_%H.%M"),".ss"))
  filenameforbridge <- gsub("created_.+\\.ss", "createdforbridge.ss", filenameforss)
  unlink(x = c(filenameforss, filenameforbridge))
  write_wtatage_file(file = filenameforss, data = withforecast, maturity = maturity)
  write_wtatage_file(file = filenameforbridge,
    data = withforecast_old, maturity = maturity)

  save(dat,mat_Interp2_All, wtage_All, wtage_All_wMean, withforecast,
    file = file.path(dir, "LWAdata.Rdata"))
}

#' Remove Weight-At-Age Outliers
#'
#' Get saved weight-at-age data and remove any outliers and/or plot
#' the data to visualize outliers.
#' Outliers are screened using two methods. 
#' First, data in September of 2003 are screened for fish less than 0.52 kg
#' and greater than 45 cm in length.
#' Second, data are screen for a weight-length relationship beyond that deemed
#' to be normal, i.e., 20e-6*length^3 and 2e-6*length^3.
#' Started using this method in 2011. 
#'
#' @param file The file path, full or relative, to the \code{.csv} file you 
#' are importing. The file should have the following seven columns:
#' Source, Weight_kg, Sex, Age_yrs, Length_cm, Month, and Year.
#' @param removeOutliers A logical value specifying whether or not outliers
#' should be removed from the data.
#' @param outlierplot A logical value specifying whether or not plots should
#' be generated. If \code{outlierplot = TRUE} and \code{outlierPlotName = TRUE},
#' then a text file under a similar name to \code{outlierPlotName} will also be
#' generated that contains a table of outliers by data source.
#' @param outlierPlotName A file path to a \code{.png} file that will be used
#' to save the plots to the disk. If \code{NULL}, then the plots will be printed
#' to the screen rather than saved to the disk.
#' @param elimUnsexed A logical value specifying if unsexed fish should be 
#' eliminated from the data set. 
#' @param elimAge A vector of ages to eliminate. The default is to eliminate all
#' fish with an age of 99. 
#' Use \code{elimAge = NULL} to not eliminate fish of any age.
#' @template verbose
#'  
#' @export 
#' @author Ian Taylor
#' @return A cleaned data frame of weight-at-age information for hake.
#' The data frame includes the following columns:
#' \enumerate{
#'   \item Source
#'   \item Weight_kg
#'   \item Sex
#'   \item Age_yrs
#'   \item Length_cm
#'   \item Month
#'   \item Year
#'   \item OutlierL
#' }.
#' @seealso wtatage_collate
#'
get_wtatagecsv <- function(file, removeOutliers = TRUE, outlierplot = FALSE,
  outlierPlotName = "wt_at_age_outliers.png", elimUnsexed = FALSE,
  elimAge = 99,
  verbose = FALSE) {

  if (tools::file_ext(file) == "csv") {
    h <- read.csv(file)
  } else {
    h <- readRDS(file)
  }
  if (verbose) cat("Looking for outliers in", file, "\n")
  if (!is.null(outlierPlotName) & outlierplot) {
    outlierTxtName <- gsub("\\.png", "\\.txt", outlierPlotName)
    ignore <- file.create(outlierTxtName)
  }

  outlierL <- rep(FALSE, nrow(h))
  outlierL[
    h$Year == 2003 & h$Month == 9 & 
    h$Weight_kg < 0.52 & h$Length_cm > 45] <- TRUE
  outlierL[h$Weight_kg > (20e-6)*h$Length_cm^3] <- TRUE
  outlierL[h$Weight_kg < (2e-6)*h$Length_cm^3] <- TRUE

  if (exists("outlierTxtName")) {
    sink(outlierTxtName, append = TRUE)
    cat("Table of outliers by data set generated from\n", file, "\n")
    print(table(h$Source, outlierL))
    sink()
  }

  if (basename(file) == "LengthWeightAge_data.rds") {
    old <- c(CAN_acoustic = 5719, CAN_jv = 136, CAN_JV = 507,
      CAN_polish = 487, CAN_shoreside = 636, Poland_acoustic = 2092,
      US_acoustic = 15718, US_atsea = 27939, US_foreign = 29767,
      US_JV = 29421, US_shore = 35824)
    testthat::expect_equal(old, 
      c(table(h[h$Year < 2008, "Source"])))
  } else {
  sourcetable <- table(h$Source)
  # Some more samples added Dec 2018 / Jan 2019
  old <- c("Acoustic Poland" = 2094, "US_JV" = 29431, "US_FOREIGN" = 29778)
  if (any(grepl("poland", names(sourcetable), ignore.case = TRUE))) {
    poland <- grep("poland", names(sourcetable), ignore.case = TRUE)
    testthat::expect_equal(sourcetable[poland], old["Acoustic Poland"],
      check.attributes = FALSE)
    rm(poland)
  } 
  if (any(grepl("US_JV", names(sourcetable), ignore.case = TRUE))) {
    testthat::expect_equal(sourcetable["US_JV"], old["US_JV"])
  } 
  if (any(grepl("US_FOREIGN", names(sourcetable), ignore.case = TRUE))) {
    foreign <- grep("US_FOREIGN", names(sourcetable), ignore.case = TRUE)
    testthat::expect_equal(sourcetable[foreign], old["US_FOREIGN"],
      check.attributes = FALSE)
    rm(foreign)
  }
  }

  # plot of outliers
  if (outlierplot){
    x <- 0:150
    if (!is.null(outlierPlotName)) {
      png(outlierPlotName, width = 10, height = 7, units = "in", res = 400)
    } else { dev.new() }
    par(mfrow = c(1, 3))
    plot(h$Length_cm,h$Weight_kg,
      pch = 16, col = rgb(0, 0, 0, 0.2), xlab = "Length (cm)", ylab = "Weight (kg)",
      main = "All outliers")
    points(h$Length_cm[outlierL],h$Weight_kg[outlierL], pch = 16, col = 2)
    lines(x, 2e-6*x^3, col = 4)
    lines(x, 20e-6*x^3, col = 4)
    #todo: think about age-0 fish
    #todo: change the colours to match those used in hake-assessment
    plot(Weight_kg ~ Age_yrs, data = h[h$Age_yrs > 0, ],
      pch = 16, col = rgb(0,0,0,.2), xlab = "Age", ylab = "Weight (kg)",
      main = "Weight vs Age (log space)", log = "xy")
    points(h$Age_yrs[outlierL], h$Weight_kg[outlierL], pch = 16, col = 2)
    plot(h$Age_yrs, h$Length_cm, 
      pch = 16, col = rgb(0, 0, 0, 0.2), xlab = "Age", ylab = "Length (cm)",
      main = "Length vs Age")
    points(h$Age_yrs[outlierL], h$Length_cm[outlierL], pch = 16, col = 2)
    if (!is.null(outlierPlotName)) dev.off()
    if (verbose) cat("Plot saved to", outlierPlotName, "\n")
  }
  h <- cbind(h, outlierL)
  if (removeOutliers) {
    h <- h[!outlierL, ]
  }

  # eliminate unsexed fish (a tiny fraction),
  if (elimUnsexed) {
    aa <- table(h$Sex, h$Source)
    bb <- paste("fraction unsexed =", 
      format(mean(!h$Sex %in% c("F","M")), digits = 4), "\n")
    if (verbose) cat("eliminating all unsexed fish (a tiny fraction),\n",
        "almost all are Poland Acoustic, which was only in 1977\n")
    if (verbose) print(aa)
    if (verbose) cat(bb)
    if (exists("outlierTxtName")) {
      sink(outlierTxtName, append = TRUE)
      cat("\nThe below unsexed fish were eliminated\n")
      print(aa)
      cat(bb)
      sink()
    }
    h <- h[h$Sex %in% c("F", "M"), ]
  }

  h$Source <- as.factor(as.character(h$Source))

  # eliminate age 99 fish (not necessarily present)
  if (!any(is.null(elimAge))) {
    if (exists("outlierTxtName")) {
      sink(outlierTxtName, append = TRUE)
      cat("\nThe below fish were eliminated if their age fell within\n",
        "the following ages: ", paste(elimAge, collapse = ", "), "\n")
      print(table(h$Age_yrs))
      sink()
    }
    h$Age_yrs[h$Age_yrs %in% elimAge] <- NA
  }

  invisible(h)
}

#' Make Weight-At_Age Matrix
#' 
#' Make input file \code{wtatage.ss} for hake.
#' 
#' @param dat A data frame created from \code{\link{get_wtatagecsv}} or one that
#' has the following column names: 
#' Source, Weight_kg, Sex, Age_yrs, Length_cm, Month, and Year.
#' @param fleetoption An integer. 
#' A value of 1 separates the acoustic-survey data from the rest of the data.
#' A value of 2 pools all the samples.
#' @param value A character value specifying which type of data you are 
#' interested in. The options include \code{"weight"}, \code{"length"},
#' and \code{"count"}, where the latter will supply the sample size.
#' @param months A vector of months to include in the analysis.
#' @param getmean A logical value. If \code{TRUE}, then the first row of the
#' resulting data frame will be the mean across all years. This mean entry will
#' have a negative year of 1940.
#' @param maxage The age of the plus-group bin. The default is 15 years old.
#' 
#' @export 
#' @author Ian Taylor
#' @seealso [get_wtatagecsv()]
#' @return A data frame with the first six columns pertaining to metadata, 
#' i.e., #Yr, seas, gender, GP, bseas, and fleet, and additional columns
#' pertaining to each age starting with age zero up to the maximum age
#' supplied with the argument \code{maxage}. Ages for which there were no 
#' samples are filled with \code{NA}. If \code{value = "count"}, then 
#' the data frame will return sample sizes for each age rather than mean 
#' weight- or length-at-age. 
#' 
make_wtage_matrix <- function(dat, fleetoption = 1, value = "weight",
  months = 1:12, getmean = FALSE, maxage = 15,
  yearsearly = unique(dat$Year)) {

  ha <- dat[!is.na(dat$Age_yrs), ]
  if (!is.null(months)) ha <- ha[ha$Month %in% months, ]

  agebinspop  <- 0:maxage
  N_agebins <- length(agebinspop)
  Ncols <- N_agebins + 6
  fleetinfo <- create_fleetnames(option = fleetoption)
  if (length(unique(
    ha$Source[which(!ha$Source %in% fleetinfo$name_WLdata)]
    )) > 0) {
    warning("Weight-at-age 'Source' contains entries other than:\n",
      paste0("'", fleetinfo$name_WLdata, "'", collapse = "\n"))
  }
  # create empty data.frame
  wtage <- data.frame(matrix(NA, nrow = 0, ncol = Ncols))

  # column name for quantity to return
  colname <- switch(value,
    "length" = "Length_cm",
    "Weight_kg")

  # The below code does the same thing as the loop, but doesn't filter
  # the data ... todo: think about how to simplify the code
  # applylist <- list(
  #     Year = ha$Year,
  #     Age = ifelse(ha$Age_yrs > maxage, maxage, ha$Age_yrs)
  #     )
  # tapply(ha[,colname], applylist, mean)
  # tapply(ha[,colname],applylist, length)
  useyears <- c(switch(getmean + 1, NULL, -1940), sort(unique(ha$Year)))
  for (ID in unique(fleetinfo$ID)) {
    for (y in useyears) {
      htemp <- ha[
        as.character(ha$Source) %in% fleetinfo$name_WLdata[fleetinfo$ID == ID], ]
      if (y > 0) {
        htemp <- htemp[htemp$Year == y, ]
      } else {
        htemp <- htemp[htemp$Year %in% yearsearly, ]
      }
      # make empty vectors to hold value-by-age for this year
      sampsizes <- rep(0, N_agebins)
      means <- rep(NA, N_agebins)

      for (iage in 1:N_agebins) {
        vals <- htemp[htemp$Age_yrs %in% seq(agebinspop[iage], 
          ifelse(iage < N_agebins, agebinspop[iage], max(ha$Age_yrs)), 1), colname]
        n <- length(vals)
        if (n > 0) {
          sampsizes[iage] <- n
          means[iage] <- mean(vals, na.rm=TRUE)
        }
      }
      if (sum(sampsizes) > 0) {
        wtage <- rbind(wtage, data.frame(t(c(y, 1, 1, 1, 1, ID, 
          switch(value, "count" = sampsizes, means)))))
      }
    } # end year loop
  } # end fleet loop
  names(wtage) <- c("#Yr", "seas", "gender", "GP", "bseas", "fleet",
    paste("a", agebinspop, sep = ""))
  rownames(wtage) <- 1:nrow(wtage)
  return(wtage)
}

fill_wtage_matrix <- function(wtage, option = c("row", "age")){
  # fills in NA values with average of adjacent years
  option <- paste0("i", match.arg(option, several.ok = FALSE))
  if (!"Note" %in% colnames(wtage)) wtage$Note <- ""
  nages <- ncol(wtage) - 6
  for(irow in 1:nrow(wtage)){
    isNA <- (1:nages)[is.na(wtage[irow,-(1:6)])]
    if(length(isNA)>0){
      wtage$Note[irow] <- paste(wtage$Note[irow],
        "# interpolated ages", paste(isNA - 1, collapse = ","))
      for(iage in isNA){
        get(option)
        if(irow>1) earliervals <- wtage[1:(irow-1),iage+6] else earliervals <- NA
        if(irow<nrow(wtage))
          latervals <- wtage[(irow+1):nrow(wtage),iage+6] else latervals <- NA
        lastearlier <- rev(earliervals[!is.na(earliervals)])[1]
        firstlater <- latervals[!is.na(latervals)][1]
        if(is.na(lastearlier)) lastearlier <- firstlater
        if(is.na(firstlater)) firstlater <- lastearlier

        wtage[irow,iage+6] <- mean(lastearlier,firstlater,na.rm=TRUE)
      }
    }
  }
  return(wtage)
}

fill_wtage_matrix <- function(wtage, option = c("row", "age")){
  # fills in NA values with average of adjacent years
  option <- paste0("i", match.arg(option, several.ok = FALSE))
  if (!"Note" %in% colnames(wtage)) wtage$Note <- ""
  nages <- ncol(wtage) - ifelse("Note" %in% colnames(wtage), 1, 0) - 6
  for(irow in 1:nrow(wtage)){
    isNA <- (1:nages)[is.na(wtage[irow,-(1:6)])]
    if(length(isNA)>0){
      wtage$Note[irow] <- paste(wtage$Note[irow],
        "# interpolated ages", paste(isNA - 1, collapse = ","))
      for(iage in isNA){
        if (get(option) > 1) {
          if (option == "irow") earliervals <- wtage[1:(irow-1),iage+6]
          if (option == "iage") earliervals <- wtage[irow,(1:(iage-1))+6]
          } else earliervals <- NA
        if(get(option) < ifelse(option == "irow", nrow(wtage), nages)) {
          if (option == "irow") latervals <- wtage[(irow+1):nrow(wtage),iage+6] 
          if (option == "iage") latervals <- wtage[irow,((iage+1):nages)+6]
        } else { latervals <- NA }
        lastearlier <- rev(earliervals[!is.na(earliervals)])[1]
        firstlater <- latervals[!is.na(latervals)][1]
        if(is.na(lastearlier)) lastearlier <- firstlater
        if(is.na(firstlater)) firstlater <- lastearlier
        wtage[irow,iage+6] <- mean(lastearlier,firstlater,na.rm=TRUE)
      }
    }
  }
  return(wtage)
}

rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

#need to update the yrvec here because not an argument in makewtatagaeplots function
makeimage <- function(agevec=0:15,yrvec=1975:2017,
                      mat, # matrix of values by age and year
                      meanvec=NULL, # vector of mean value by age across years
                      Ntext=FALSE, # switch to have text show sample size rather than value
                      Nsamp.mat=NULL, # matrix of sample sizes 
                      Nsamp.meanvec=NULL, # sum of sample sizes by age across years
                      interpmat=NULL, # matrix indicating which values are interpolations
                      main="", # title for plot
                      dofont=TRUE, # use bold font to indicate interpolation
                      dorect=FALSE, # use shaded rectangles to indicate interpolation
                      margins=c(4.2,4.2,4,1)+.1){
  # if meanvec is not a separate input, assume that it's the first row
  if(is.null(meanvec)){
    meanvec <- mat[,1]
    mat <- mat[,-1]
    # same calculation for the sample sizes
    # (need to add them as an input)
    if(Ntext){
      Nsamp.meanvec <- Nsamp.mat[,1]
      Nsamp.mat <- Nsamp.mat[,-1]
    }
  }
  par(mar=margins)
  yrvec2 <- c(1973,1974,yrvec)
  mat2 <- cbind(meanvec,NA,mat)
  if(Ntext){
    Nsamp.mat2 <- cbind(Nsamp.meanvec,NA,Nsamp.mat)
  }
  if(max(mat, na.rm=TRUE) < 4){ # assume weights
    breaks <- seq(0,4,length=51)
    digits <- 2 # round weights to 2 digits
  }else{ # assume length
    breaks <- seq(10,80,length=51)
    digits <- 1 # round lengths to 1 digit
  }
  image(x=0:15,y=yrvec2,z=mat2,axes=FALSE,xlab='Age',ylab='Year',
        col=rainbow(60)[1:50], main=main, breaks=breaks)
  # add text
  zdataframe <- expand.grid(yr=yrvec2,age=agevec)
  zdataframe$z <- c(t(mat2))
  if(Ntext){
    zdataframe$Nsamp <- c(t(Nsamp.mat2))
  }
  if(!is.null(interpmat)){
    interpmat2 <- cbind(meanvec,NA,interpmat)
    zdataframe$interp <- c(t(interpmat2))
  }else{
    zdataframe$interp <- 0
  }
  zdataframe$font <- 1
  if(dofont) zdataframe$font <- ifelse(is.na(zdataframe$interp),2,1)

  if(!Ntext){
    ztext <- format(round(zdataframe$z, digits))
    ztext[ztext=="  NA"] <- ""
    ztext[ztext=="   NA"] <- ""
    text(x=zdataframe$age,y=zdataframe$yr,label=ztext,font=zdataframe$font,cex=.7)
  }else{
    ztext <- zdataframe$Nsamp
    text(x=zdataframe$age,y=zdataframe$yr,label=ztext,font=zdataframe$font,cex=.7)
  }
  interp <- zdataframe[is.na(zdataframe$interp) & zdataframe$yr!=1974,]
  if(dorect)
    rect(interp$age-.5,interp$yr-.5,
         interp$age+.5,interp$yr+.5,col=rgb(0,0,0,.3),density=20)
  # finish plot
  axis(1,at=0:15,cex.axis=.7);
  axis(2,at=c(1973,yrvec),
       labels=c("mean",yrvec),las=1,cex.axis=.7)
}

dointerpSimple <- function(df,skipcols=1:6){
  cols <- setdiff(1:ncol(df),skipcols)
  n <- nrow(df)
  for(icol in cols){
    df[,icol] <- approx(x=1:n, xout=1:n, y=df[,icol])$y
  }
  return(df)
}

#' Write Weight-At-Age Plots to Disk
#' 
#' todo: write a description for make_wtatage_plots
#' 
#' @param plots A vector of digits that specifies which plots to create.
#' @param data A data frame of weight at age data generated from
#' \code{make_wtage_matrix(..., getmean = TRUE)}.
#' @param counts A data frame of weight at age counts generated from
#' \code{make_wtage_matrix(..., getmean = TRUE, value = "count")}.
#' @param lengths A data frame of weight at length generated from
#' \code{make_wtage_matrix(..., getmean = TRUE, value = "length")}.
#' The default for this argument is \code{NULL}, and the function can be ran
#' without supplying length data.
#' @param dir The directory that you want to save the plots in. The default 
#' is to save the plots in your current working directory.
#' @param year A character value giving the current year that will be used in
#' the file names for each plot.
#' @param maxage The maximum age of fish modelled in the stock assessment, i.e.,
#' what is the age at which all fish are grouped into a plus group.
#' @template verbose
#' 
#' @export
#' @import grDevices
#' @author Ian Taylor
#' 
make_wtatage_plots <- function(plots=1:6, data, counts, lengths = NULL,
  dir = getwd(), year = format(Sys.Date(), "%Y"), maxage = 15, verbose = FALSE){
  # make plots
  # plot of all data with mean

  on.exit(grDevices::graphics.off())
  data <- data[, !grepl("Note", colnames(data), ignore.case = TRUE)]
  agecols <- grep(paste0("^a", 0:maxage, collapse = "|"), colnames(data))
  meanvec <- as.numeric(data[1, agecols])
  mat <- t(as.matrix(data[-1, agecols]))
  wt1 <- dointerpSimple(data[-1, ])
  temp <- fill_wtage_matrix(wt1[, agecols])
  mat2 <- t(as.matrix(temp[, !grepl("Note", colnames(temp))]))
  rm(temp)
  Nsamp.meanvec <- as.numeric(counts[1, agecols])
  Nsamp.mat <- t(as.matrix(counts[-1, agecols]))

  if (verbose) message("make plots in dir:\n", dir)

  # plot without extrapolation
  if(1 %in% plots){
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_1_nointerp.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat,meanvec=meanvec,main="Mean weight at age (all data)",
      yrvec = (year - ncol(mat) + 1):year)
    dev.off()
  # plot showing sample sizes
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_1B_nointerp_numbers.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat,meanvec=meanvec, Ntext=TRUE,
              Nsamp.meanvec=Nsamp.meanvec, Nsamp.mat=Nsamp.mat,
              main="Mean weight at age (colors) with sample sizes (numbers)",
      yrvec = (year - ncol(mat) + 1):year)
    dev.off()
  }

  if(2 %in% plots){
    mat1 <- t(as.matrix(wt1[, agecols]))
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_2_interp.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat1,meanvec=meanvec,
      main="Mean weight at age with interpolation (all data)",
      yrvec = (year - ncol(mat1) + 1):year)
    dev.off()
  }

  if(3 %in% plots){
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_3_interp_extrap.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat2,meanvec=meanvec,
      main="Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year)
    dev.off()
  }

  if(4 %in% plots){
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_4_interp_extrap_shade.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat2,interpmat=mat,dofont=FALSE,dorect=TRUE,
      meanvec=meanvec,
      main="Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year)
    dev.off()
  }

  if(5 %in% plots){
    fileplot <- file.path(dir, 
      paste0("empirical_wtatage_", year, "_alldata_5_interp_extrap_bold.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=mat2,interpmat=mat,dofont=TRUE,dorect=FALSE,
      meanvec=meanvec,
      main="Mean weight at age with interpolation & extrapolation (all data)",
      yrvec = (year - ncol(mat2) + 1):year)
    dev.off()
  }

  # Mean length plots
  if(6 %in% plots & !is.null(lengths)){
    fileplot <- file.path(dir, 
      paste0("empirical_lenatage_", year, "_alldata_6_nointerp.png"))
    len.meanvec <- as.numeric(lengths[1,agecols])
    len.mat <- t(as.matrix(lengths[-1, agecols]))

    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=len.mat,meanvec=len.meanvec,
              main="Mean length at age (all data, cm)",
      yrvec = (year - ncol(len.mat) + 1):year)
    dev.off()

    fileplot <- file.path(dir, 
      paste0("empirical_lenatage_", year, "_6B_nointerp_numbers.png"))
    grDevices::png(fileplot,width=7,height=9,units="in",res=400)
    makeimage(mat=len.mat,meanvec=len.meanvec, Ntext=TRUE,
      Nsamp.meanvec=Nsamp.meanvec, Nsamp.mat=Nsamp.mat,
      main="Mean length at age (colors) with sample sizes (numbers)",
      yrvec = (year - ncol(len.mat) + 1):year)
    dev.off()

    fileplot <- file.path(dir, paste0("EWAforDoc_Numbers.eps"))
    grDevices::cairo_ps(fileplot, width = 7, height = 9, pointsize = 12)
    makeimage(mat = len.mat, meanvec = len.meanvec, Ntext = TRUE,
      Nsamp.meanvec = Nsamp.meanvec, Nsamp.mat = Nsamp.mat,
      main = "Mean length at age (colors) with sample sizes (numbers)",
      yrvec = (year - ncol(len.mat) + 1):year)
    dev.off()
  }

  # write image to EPS format for inclusion in document
  cairo_ps(filename = file.path(dir, "EWAforDoc.eps"),
           width = 7, height = 9, pointsize = 12)
  makeimage(mat=mat2,interpmat=mat,dofont=TRUE,dorect=FALSE,
    meanvec=meanvec,main="", margins=c(4.2,4.2,1,1)+.1,
    yrvec = (year - ncol(mat2) + 1):year)
  dev.off()
}

#' Write weight-at-age file
#' 
#' @param file A filename that you want to save the information as. The default is to have
#' an extension of \code{.ss} such that the file can be used for Stock Synthesis.
#' The file path can either be relative or absolute.
#' @param data Weight at age matrix.
#' @param maturity A vector of maturity at age.
#' @template verbose
#' 
write_wtatage_file <- function(
  file = paste0("wtatage_",format(Sys.time(),"%Y"),"created_",format(Sys.time(),"%d-%b-%Y_%H.%M"),".ss"), 
  data, maturity,
  verbose = FALSE){
  # stuff copied from SS_writedat for printing tables
  on.exit({if(sink.number()>0) sink()}) # only needed if this is put into a function

  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  on.exit(options(width = oldwith), add = TRUE)
  on.exit(options(max.print = oldmax.print))
  options(width=5000,max.print=9999999)

  if (verbose) cat("opening connection to", file, "\n")
  zz <- file(file, open="at")
  on.exit(close(zz), add = TRUE)
  sink(zz)

  nrows_per_matrix <- nrow(data)
  nrows_total <- 1 + 4*nrows_per_matrix

  header <- c("# empirical weight-at-age Stock Synthesis input file for hake",
              "# created by code in the R script: wtatage_calculations.R",
              paste("# creation date:",Sys.time()),
              "###################################################",
              "20 # Maximum age",
              "",
              "#Maturity x Fecundity: Fleet = -2 (Values maturity unchanged from 2012 Stock Assessment)",
              "#Maturity x Fecundity: Fleet = -2 (are maturity * wtatage)",
              "")
  writeLines(header)

  # Make fleet -2 for most recent years
  fleetn2 <- cbind(data[, c(1:6)],
    t(apply((data[, -c(1:6)]),1, function(x) x*maturity)))
  fleetn2$fleet <- -2
  printdf(fleetn2)

  writeLines("#All matrices below use the same values, pooled across all data sources")

  for(ifleet in -1:2){
    data$fleet <- ifleet
    if(ifleet==-1) note <- "#Weight at age for population in middle of the year: Fleet = -1"
    if(ifleet==0)  note <- "#Weight at age for population at beginning of the year: Fleet = 0"
    if(ifleet==1)  note <- "#Weight at age for Fishery: Fleet = 1"
    if(ifleet==2)  note <- "#Weight at age for Survey: Fleet = 2"

    writeLines(c("",note))
    printdf(data)
  }

  # terminator line
  terminator <- 0*fleetn2[1, ]
  terminator[, 1] <- -9999
  terminator[, "fleet"] <- 2
  writeLines("")
  writeLines("# terminator line")
  printdf(terminator)

  writeLines("# End of wtatage.ss file")

  # restore defaults
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  if (verbose) cat("file written to", file, "\n")
}

#' Generate a Date Frame of Fleet Names and IDs
#' 
#' Implement consistency in fleet names and IDs for the hake fishery. The 
#' function returns a data frame that is useful for plotting and table names.
#'
#' @param option An integer value, where 1 leads to fishery and survey being
#' identified as 1 and 2 in the \code{ID} column, 2 leads to a single fleet 
#' identified as 1, and 3 leads to two columns with the second column of a single
#' fleet as \code{ID0}.
#' 
#' @export
#' @author Kelli F. Johnson
#' @return A data frame with data-specific fleet names and fleet names
#' that are standardized for plotting purposes. The fishery is typically identified
#' as 1 and the survey is identified as 2, but all data can be identified as 0 if desired.
#' 
create_fleetnames <- function(option = 1) {
  if (!option %in% 1:3) stop("Option must be 1, 2, or 3.")
  fishery <- c(
    "Acoustic Poland",
    "US_FOREIGN",
    "US_foreign",
    "US_JV",
    "ATSEA",
    "US_atsea",
    "US_shore",
    "SHORE",
    "",
    "CAN_JV",
    "CAN_shoreside",
    "CAN_domestic",
    "CAN_polish"
    )
  survey <- c(
    "Acoustic U.S.",
    "Acoustic Canada",
    "CAN_acoustic",
    "US_acoustic"
    )
  fleetinfo <- data.frame(
    "ID" = c(
      rep(1, length(fishery)),
      rep(2, length(survey))
      ),
    "name_WLdata" = c(fishery, survey),
    "name_model" = c(
      rep("Fishery", length(fishery)),
      rep("Acoustic_Survey", length(survey))
      )
    )
  if (option == 2) fleetinfo$ID <- 0
  if (option == 3) fleetinfo$ID0 <- 0
  return(fleetinfo)
}
