#' Map age samples
#'
#' @template atsea.ages
#' @template ncatch
#' @param savepng A logical specifying if the png plots of mapfun should
#' be saved to the disk. If \code{FALSE} the plots will neither be created
#' or saved. 
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A data frame of min and max weights per year.
#' 
mappingagesamples <- function(atsea.ages = NULL, ncatch = NULL,
  savepng = FALSE) {
  mydir <- hakedatawd()
  if (is.null(atsea.ages)) {
    load(file.path(mydir, "extractedData", "atsea.ages.Rdat"))
  }  
  if (is.null(ncatch)) {
    load(file.path(mydir, "extractedData", "NORPACdomesticCatch.Rdat"))
  }

  ncatch$YEAR <- as.numeric(format(ncatch$RETRIEVAL_DATE,"%Y"))
  ncatch$HaulID <- paste(ncatch$CRUISE, ncatch$HAUL, sep=".") 
  atsea.ages$HaulID <- paste(atsea.ages$CRUISE, atsea.ages$HAUL_OFFLOAD, sep=".")

  age.info <- dplyr::left_join(
    x = atsea.ages, 
    y = ncatch[ncatch$SPECIES == 206, ],
    by = c(
      "BOTTOM_DEPTH_FATHOMS", "CRUISE", "CDQ_CODE", "FISHING_DEPTH_FATHOMS", 
      "LATDD_END", "LATDD_START", "LONDD_END", "LONDD_START", 
      "PERMIT", "SPECIES", "YEAR", "HaulID"))
  age.info <- age.info[!is.na(age.info$AGE), ]
  age.info$RETRIEVAL_DATE2 <- as.Date(age.info$RETRIEVAL_DATE)

  # diffs <- tapply(age.info$RETRIEVAL_DATE2, age.info$YEAR, 
  #     function(x) table(x %in% (min(x) + 0:diff(range(x)))))
  # if (is.list(diffs)) {
  #   stop("There are days that are not present in the NORPAC ages.",
  #     "\nOne needs to check age.info inside mappingagesamples.")
  # }

  weights <- do.call("rbind", 
    tapply(age.info$EXTRAPOLATED_WEIGHT/1000, age.info$YEAR, range))
  colnames(weights) <- c("min", "max")
  # With 2017 data it was 0.38855 137.95097
  if (savepng) mapfun(data = age.info, doPNG=TRUE)
  return(weights)
}

#' Plot Map of Fishing Locations for Each Age Sample
#' 
#' todo: add more details here
#' 
#' @param data A data frame created in mappingagesamples.
#' @param doPNG A logical value specifying whether or not to print the 
#' plots to png files.
#' @param dir A file path specifying the base directory for which you
#' have the hake data. Plots will be created in nested folders based on 
#' your input value for this argument.
#' 
#' @return Plots or saved png files.
#' 
mapfun <- function(data, doPNG=FALSE, dir = getwd()){
  width <- 9
  height <- 13
  if(doPNG){
    dirpng <- file.path(dir, "Figures", "CONFIDENTIAL", "age_sample_maps_atsea")
    dir.create(dirpng, recursive = TRUE, showWarnings = FALSE)
  } else {windows(width = width, height = height)}
  #todo: the above will not work on linux machines

  scale <- .8
  age.subset <- seq(1,11,2)
  sizes <- c(10,20,40,80,160)
  alldata <- data
  data <- data[!is.na(data$RETRIEVAL_DATE2), ]
  for (iyear in unique(data$YEAR)) {
    localdata <- data[data$YEAR == iyear, ]
    localdiff <- diff(range(localdata$RETRIEVAL_DATE2))
    days <- min(localdata$RETRIEVAL_DATE2) + 0:localdiff
  for(iday in 1:length(days)){
    day <- days[iday]
    if(doPNG){
      png(file.path(dirpng, paste0("CONFIDENTIAL_age_sample_atsea_",
        iyear, "_day_", sprintf("%03d", iday), ".png")),
          res=150, width=width, height=height, units="in")
    }
    # make map of coast
    coastmap(ylim=c(40.8,49), xlim=c(-128,-122))
    mtext(side=3, line=.2, "CONFIDENTIAL", cex=3)
    # legend showing vessel type, size, and age
    # Go back in history to get individual legends per type
    legend("bottomright", pch = c(22:23, rep(22, each = 4)),
           pt.bg = rainbow(15, alpha = 0.6)[age.subset],
           col= rainbow(15, alpha = 0.7)[age.subset],
           pt.cex = c(rep(scale*sqrt(sizes)[1], 2), 
            scale*sqrt(sizes)),
           legend = 
           paste0(paste0("  age ", age.subset), "\n  [",
            c(c("CP","MS"), rep("", 4)), 
            c("", "", "10 t","20 t","50 t","100 t"),
            "]\n"),
           bty="n", 
           xpd = TRUE, 
           title = "[type or t per haul]")
    # line and text showing date
    xval <- -123.8
    rect(xval, 44.95, par()$usr[2], 45)
    bar.end <- xval + (iday/length(days))*(par()$usr[2] - xval)
    rect(xval, 44.95, bar.end, 45, col='red')
    text(xval, 44.85, labels=day, pos=4, font=2)
    text(xval, 45.15, labels="Date", pos=4, font=2)
    localdata.day <- localdata[localdata$RETRIEVAL_DATE2 == day, ]
    localdata.day$LONDD_START <- ifelse(
      is.na(localdata.day$LONDD_START), 
      localdata.day$LONDD_END, localdata.day$LONDD_START)
    localdata.day$LATDD_START <- ifelse(
      is.na(localdata.day$LATDD_START), 
      localdata.day$LATDD_END, localdata.day$LATDD_START)
    # add points showing fishing location
    points(x = localdata.day$LONDD_START,
           y = localdata.day$LATDD_START,
           bg = rainbow(15, alpha = .2)[localdata.day$AGE],
           col = rainbow(15, alpha = .5)[localdata.day$AGE],
           pch = 21+localdata.day$VESSEL_TYPE,
           cex = scale*sqrt(localdata.day$EXTRAPOLATED_WEIGHT/1000))
    box()
    if(doPNG){
      dev.off()
    }else{
      Sys.sleep(.2)
    }
    }
  }
}

