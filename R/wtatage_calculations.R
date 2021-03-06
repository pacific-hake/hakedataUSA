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

  h <- read.csv(file)
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

  # plot of outliers
  if (outlierplot){
    x <- 0:150
    if (!is.null(outlierPlotName)) {
      png(outlierPlotName, width = 10, height = 7, units = "in", res = 400)
    } else { dev.new() }
    par(mfrow = c(1, 3))
    plot(h$Length_cm,h$Weight_kg,
      pch = 16, col = rgb(0, 0, 0, 0.2), xlab = "Length", ylab = "Weight",
      main = "All outliers")
    points(h$Length_cm[outlierL],h$Weight_kg[outlierL], pch = 16, col = 2)
    lines(x, 2e-6*x^3, col = 4)
    lines(x, 20e-6*x^3, col = 4)
    #todo: think about age-0 fish
    #todo: change the colours to match those used in hake-assessment
    plot(Weight_kg ~ Age_yrs, data = h[h$Age_yrs > 0, ],
      pch = 16, col = rgb(0,0,0,.2), xlab = "Age", ylab = "Weight",
      main = "Weight vs Age (log space)", log = "xy")
    points(h$Age_yrs[outlierL], h$Weight_kg[outlierL], pch = 16, col = 2)
    plot(h$Age_yrs, h$Length_cm, 
      pch = 16, col = rgb(0, 0, 0, 0.2), xlab = "Age", ylab = "Length",
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

  # create empty data.frame
  wtage <- data.frame(matrix(NA, nrow = 0, ncol = Ncols))

  # column name for quantity to return
  colname <- switch(value,
    "length" = "Length_cm",
    "Weight_kg")

  useyears <- c(switch(getmean + 1, NULL, -1940), sort(unique(ha$Year)))
  for (ID in unique(fleetinfo$ID)) {
    for (y in useyears) {
      htemp <- ha[
        as.character(ha$Source) %in% fleetinfo$name_WLdata[fleetinfo$ID == ID], ]
      if (y > 0) {
        htemp <- ha[ha$Year == y, ]
      } else {
        htemp <- ha[ha$Year %in% yearsearly, ]
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
#' @author Kelli Faye Johnson
#' @return A data frame with data-specific fleet names and fleet names
#' that are standardized for plotting purposes. The fishery is typically identified
#' as 1 and the survey is identified as 2, but all data can be identified as 0 if desired.
#' 
create_fleetnames <- function(option = 1) {
  if (!option %in% 1:3) stop("Option must be 1, 2, or 3.")
  fleetinfo <- data.frame(
    "ID" = c(rep(1, 7), 2, 2),
    "name_WLdata" = c("US_FOREIGN", "US_JV", "ATSEA", "SHORE", "",
      "CAN_JV", "CAN_domestic", "Acoustic U.S.", "Acoustic Canada"),
    "name_model" = c(rep("Fishery",7), "Acoustic_Survey", "Acoustic_Survey")
    )
  if (option == 2) fleetinfo$ID <- 0
  if (option == 3) fleetinfo$ID0 <- 0
  return(fleetinfo)
}
