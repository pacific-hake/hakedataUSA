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
#' @param writecsv A logical specifying if csv files should be
#' written to the disk.
#' @param colour A logical value specifying if the plots should be 
#' made in colour or gray scale, where the latter occurs if \code{FALSE}.
#' @param nyears The number of years for plotting.
#' 
#' @import ggplot2 grDevices
#' @export
#' @author Kelli Faye Johnson
#' 
#' @return Saves files to the disk
#' todo: summarize return in more detail
#' 
norpaccatches <- function(ncatch = NULL, writecsv = TRUE, colour = TRUE,
  nyears = 5) {
  
  # Plot could be used to replace Figure 6 in 2015 assessment, requires
  # more info on confidentiality rules as the July and August values
  # represent fewer than 3 motherships (but likely more catcher boats)
  # log-scale plot comparing 2015 and 2016 and 2017
  catchRatesFig <- function(data, log = c("y"), 
    years = 2015:2017, months = 5:11, colors) {
    if (log == "y") {
      ylim <- c(.7,200)
      yvec <- c(1,2,5,10,20,30,40,50,100,200)
    } else {
      ylim <- c(.7,100)
      yvec <- seq(0,100,20)
    }
    plot(1, type="n", axes=FALSE, xlab="Month",
         ylab="Unstandardized catch-per-hour (t/hour)",
         xaxs="i", xlim=range(months)+c(-.5,.5),
         log=log,
         main="U.S. at-sea unstandardized catch-rate",
         ylim=ylim)
    #abline(h=yvec, col="grey", lty=3)
    dists <- rev(seq(-0.55, 0.33, length.out = length(years) + 1))
    for (ii in years) {
      temp <- data[[which(names(splits) == ii)]]
      box95(temp, add = TRUE, 
        log="y",yaxt="n",
        at=as.numeric(names(temp))-dists[match(ii, years)], 
        boxwex=.2, col=colors[match(ii, years)], axes=FALSE)
    }
    legend("topright", bty="n", fill=colors, legend=years)
    axis(2, at=yvec, las=1)
    axis(1, at=months, month.abb[months], tick=FALSE)
    axis(1, at=1:13 - 0.5, lab=rep("", length(1:13)))
    for(imonth in seq(4,10,2)){
      rect(imonth-.5, .01, imonth+.5, 300, col=rgb(0,0,0,0.05), border=FALSE)
    }
    box()
  }

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

  args <- list(width = 6.5, height = 4.5, pointsize = 10)
  args2 <- c(args, units = "in", res = 300)
  hake.theme <- plottheme()
  oldop <- options()$warn
  options(warn = -1)
  on.exit(options(warn = oldop), add = TRUE)

  mydir <- hakedatawd()
  summaryfile <- file.path(mydir, "extractedData", 
    paste0("summary_catchNORPAC_", format(Sys.time(), "%Y.%m.%d"), ".txt"))
  on.exit(suppressWarnings(sink(file = NULL)), add = TRUE)
  sink(summaryfile)
  cat("Summary of NORPAC catches created on", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  sink()

  if (is.null(ncatch)) {
    load(file.path(mydir, "extractedData", "NORPACdomesticCatch.Rdat"))
  }

  #MS and CP combined
  #VESSEL_TYPE:  A one-digit numeric code that indicates whether the vessel
  # processes fish or delivers it to a processing plant where:
  # 1 = a catcher processor vessel,
  # 2 = a mothership or a ship that receives unsorted codends from other vessels,
  # 3 = a catcher only vessel that delivers unprocessed fish to a shoreside
  #     or floating plant or vessel,
  # 4 = a mothership that receives sorted codends,
  # 5 = a vessel that sells the majority of their catch over the side to other
  #     fishing vessels who will utilize the fish for bait,
  # 6 = vessels that discard all catch from a haul.

  # Calculate bycatch rate to determine hake weight in unsampled hauls
  out <- processNorpacCatch(ncatch,
    outfname = switch(writecsv + 1,
    NULL, file.path(mydir, "Catches", "NorpacDomesticCatchesByMonth.csv")))
  out.yr <- aggregate(Catch.MT ~ Sector + Year, data = out, sum)
  if(writecsv) write.csv(out.yr,
    file = file.path(mydir, "Catches", "NorpacDomesticCatchesByYear.csv"),
    row.names = FALSE)

  # Vessel type
  typedate <- addmargins(margin = 2,
    table(ncatch$VESSEL_TYPE, format(ncatch$RETRIEVAL_DATE, "%Y")))
  sink(summaryfile, append = TRUE)
  cat("\nSummary of vessel type by year \n")
  print(typedate)
  for (ii in c("VESSEL", "PERMIT", "CDQ_CODE")) {
    temp <- table(ncatch[, ii], ncatch$VESSEL_TYPE)
    cat("\nSummary of vessel type by", tolower(ii), "\n")
    print(temp)
  }
  sink()
  #MS and CP only
  for (vtype in 1:2) {
    temp <- processNorpacCatch(ncatch[ncatch$VESSEL_TYPE == vtype, ])
    if (vtype == 1) temp$Sector <- "atSea_US_CP"
    if (vtype == 2) temp$Sector <- "atSea_US_MS"
    if (writecsv) {
      write.csv(temp,
        file.path(mydir, "Catches", 
          paste0(strsplit(temp$Sector[1], "_")[[1]][3], "_CatchesByMonth.csv")),
        row.names = FALSE)
      yearly <- aggregate(Catch.MT ~ Sector + Year, data = temp, sum)
      write.csv(yearly,
        file.path(mydir, "Catches", 
          paste0(strsplit(temp$Sector[1], "_")[[1]][3], "_CatchesByYear.csv")),
        row.names = FALSE)
    }
  }
 # Working here
  cdqcodedate <- table(ncatch$CDQ_CODE,format(ncatch$RETRIEVAL_DATE, "%Y"))
  if (nrow(cdqcodedate) != 1) stop("There are ", nrow(cdqcodedate), 
    " cdq codes in the data and there should be one.")
  if (!all(cdqcodedate[names(cdqcodedate) %in% 2008:2017] == 
    c(1938, 2065, 2496, 1636, 19, 0, 0, 0, 0))) stop("The tribal catches",
    " do not match what was pulled in 2017\n")
  #TRIBAL
  TRout <- processNorpacCatch(ncatch[ncatch$CDQ_CODE=="M01",])
  TRout$Sector <- "atSea_tribal"
  TRout.yr <- aggregate(TRout$Catch,list(TRout$Sector,TRout$Year),sum)
  sink(summaryfile, append = TRUE)
  cat("\nSummary of CDQ_CODE by year \n")
  print(cdqcodedate)
  cat("\nSummary of Makah catches by year \n")
  print(TRout.yr)
  sink()

  sink(summaryfile, append = TRUE)
  cat("\n\nSummary of hake-only catches")
  sink()

  hcatch <- ncatch[ncatch$SPECIES == 206, ]
  hcatch$Year <- as.numeric(format(hcatch$RETRIEVAL_DATE, "%Y"))
  hcatch$Month <- as.numeric(format(hcatch$RETRIEVAL_DATE, "%m"))
  hcatch$hrs <- hcatch$DURATION_IN_MIN/60
  hcatch$crate <- hcatch$EXTRAPOLATED_WEIGHT / 1000 / hcatch$hrs
  hcatch$FISHING_DEPTH_M <- hcatch$FISHING_DEPTH_FATHOMS * 1.8288
  hcatch$BOTTOM_DEPTH_M <- hcatch$BOTTOM_DEPTH_FATHOMS * 1.8288
  splits <- lapply(split(hcatch, hcatch$Year), 
    function(x) split(x[, "crate"], x[, "Month"]))
  catch.yr <- split(hcatch,hcatch$Year)

  # todo: Find a way to ensure that these deep depths are not confidential
  # todo: fix las to match between US and Canada
  keeptheseyears <- tail(1: max(hcatch$Year, na.rm = TRUE), nyears)
  exportdepth(split(hcatch$FISHING_DEPTH_M, hcatch$Year),
    country = "US", type = "fishing_atsea", 
    dir = file.path(mydir, "Catches"))
  exportdepth(split(hcatch$BOTTOM_DEPTH_M, hcatch$Year),
    country = "US", type = "bottom_atsea", 
    dir = file.path(mydir, "Catches"))
  if (colour) {
    colors <- plotcolour(length(sort(unique(hcatch$Year))))
  } else {colors <- rep("grey", length(sort(unique(hcatch$Year))))}
  for (idev in c("eps", "png")) {
    filei <- file.path(mydir, "Figures", paste0("fishDepthsUS.", idev))
    if (idev == "eps") {
      do.call("cairo_ps", c(args, file = filei))
    }
    if (idev == "png") {
      do.call("png", c(args2, file = filei))
    }
  par(mfrow=c(1,2),mar=c(4,4,3,1))  
  out <- box95(lapply(catch.yr[names(catch.yr) %in% 
    keeptheseyears],
    function(x)x$FISHING_DEPTH_FATHOMS),
    main="Fishing Depth",ylab="Fathoms",las=1, col = tail(colors, nyears))
  out <- box95(lapply(catch.yr[names(catch.yr) %in% 
    keeptheseyears],
    function(x)x$BOTTOM_DEPTH_FATHOMS),
    main="Bottom Depth",las=1, col = tail(colors, nyears))
  axis(2,at=seq(100,400,100),las=1)
  mtext("Year",side=1,outer=T,line=-1.5)
  dev.off()
  }

  pdf(file = file.path(mydir, "Figures", "fishCatchRatesUSByYear.pdf"))
  for (iyear in as.numeric(names(splits))) {
    box95(splits[[which(names(splits) == iyear)]],
      ylab="Unstandardized catch-per-hour (mt/hour)",
      xlab="Month",
      main=paste("U.S. At-sea unstandardized", iyear, "catch-rate (preliminary)"),
      log="y",yaxt="n")
  }
  dev.off()

  for (ii in 1:4) {
    do.call(c("cairo_ps", rep("png", 2), "cairo_ps")[ii], 
      c(get(ifelse(ii %in% c(1,4), "args", "args2")), 
        file = file.path(mydir, "Figures", 
          paste0("fishCatchRatesUS", 
            c(".eps", ".png", "nolog.png", "nolog.eps")[ii]))))
    catchRatesFig(data = splits, log = c(rep("y", 2), "", "")[ii],
      years = tail(names(splits), nyears), colors = tail(colors, nyears))
    dev.off()
  }

  # Summarize catches by depth
  sink(summaryfile, append = TRUE)
  cat("\nSummary of depth ranges (min and max fathoms) of catch per year.\n")
  print(sapply(lapply(catch.yr, "[[", "BOTTOM_DEPTH_FATHOMS"), 
    range, na.rm = TRUE))
  sink()

# check these tables to make sure at least 3 vessels were fishing in each month
# todo: find a way to print year / month combos that are true
  hcatch$groups <- factor(hcatch$Month, levels = 1:12, 
    labels = rep(c("12", "34", "56", "78", "910", "1112"), each = 2))
  confid <- apply(table(
    hcatch$CATCHER_BOAT_ADFG, hcatch$groups, hcatch$Year, useNA = "ifany"), 
    3, function(x) apply(x, 2, function(y) any(y %in% 1:2)))
  confid <- apply(table(
    hcatch$CATCHER_BOAT_ADFG, hcatch$Month, hcatch$Year, useNA = "ifany"), 
    3, function(x) apply(x, 2, function(y) any(y %in% 1:2)))
  confid <- apply(table(
    hcatch$VESSEL, hcatch$Month, hcatch$Year, useNA = "ifany"), 
    3, function(x) apply(x, 2, function(y) any(y %in% 1:2)))
  with(hcatch[hcatch$Year == 2017, ], 
    table(CATCHER_BOAT_ADFG, Month, useNA="ifany"))

  sink(summaryfile, append = TRUE)
  cat("\nSummary of bottom depth by vessel by month by year\n",
    "for depths greater than 1500.\n")
  temp <- sapply(catch.yr, function(x) {
      x <- x[x$BOTTOM_DEPTH_FATHOMS > 1500, ]
      table(x$VESSEL, x$Month)
    })
  print(temp[which(sapply(temp, dim)[2, ] != 0)])
  cat("\nSummary of bottom depth by vessel by year\n",
    "for depths greater than 1999.\n")
  print(sapply(catch.yr, 
    function(x) table(x[x$BOTTOM_DEPTH_FATHOMS > 1999, "VESSEL"])))
  sink()

  hcatch$Monthf <- droplevels(factor(hcatch$Month, levels = 1:12, 
    labels = rep(paste(seq(1, 11, by = 2), seq(2, 12, by = 2), sep = "-"),
    each = 2)))
  temp <- merge(hcatch, setNames(
    aggregate(VESSEL ~ Monthf + Year, 
      data = hcatch[!is.na(hcatch$Month) & 
      hcatch$Year %in% tail(1:max(hcatch$Year, na.rm = TRUE), nyears), ], 
    function(x) ifelse(length(unique(x)) %in% 1:2, 0, 1)),
    c("Monthf", "Year", "sumn")))
  for (iname in c("FISHING_DEPTH_FATHOMS", "BOTTOM_DEPTH_FATHOMS")){
    temp$y <- temp[, iname]
    g <- ggplot2::ggplot(temp[temp$sumn == 1, ], 
      ggplot2::aes(x = Monthf, y = y)) + hake.theme + 
      ggplot2::geom_boxplot(outlier.shape = NA) + 
      ggplot2::facet_wrap(Year ~ ., nrow = 2) + 
      ggplot2::xlab("month") + 
      ggplot2::ylab(tolower(gsub("_DEPTH_FATHOMS", " depth (fathoms)", iname))) 
    ggplot2::ggsave(g, file = file.path("FIGURES", paste0("fish", iname, "_US.png")),
      width = 7, height = 7)
  }

  g <- ggplot2::ggplot(hcatch[!is.na(hcatch$VESSEL) & 
      hcatch$Year %in% tail(1:max(hcatch$Year, na.rm = TRUE), nyears), ], 
    ggplot2::aes(x = VESSEL, y = BOTTOM_DEPTH_FATHOMS)) + hake.theme + 
  ggplot2::geom_boxplot() + 
  ggplot2::facet_wrap(Year ~ ., nrow = 2) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::xlab("vessel") + 
  ggplot2::ylab("bottom depth (fathoms)") 
  ggplot2::ggsave(g, file = file.path("FIGURES", "CONFIDENTIAL", "fishBottomDepthByVesselUS.png"),
    width = 7, height = 7)
  # todo: label the vessel types as CP and MS but I don't know what 3 is.
  if (!any(table(hcatch$Year, hcatch$VESSEL_TYPE) %in% 1:2)) {
    g <- ggplot2::ggplot(reshape(hcatch[!is.na(hcatch$Year) & 
      hcatch$Year %in% tail(1:max(hcatch$Year, na.rm = TRUE), nyears), ],
      varying = c("BOTTOM_DEPTH_FATHOMS", "FISHING_DEPTH_FATHOMS"),
      v.names = "depth",
      timevar = "dtype",
      times = c("bottom", "fishing"),
      direction = "long"), 
      ggplot2::aes(x = factor(VESSEL_TYPE), y = depth)) + hake.theme + 
      ggplot2::geom_boxplot(outlier.shape = NA) + 
      ggplot2::facet_grid(dtype ~ Year, scales = "free_y") + 
      ggplot2::xlab("sector") + 
      ggplot2::ylab("depth (fathoms)")
    ggplot2::ggsave(g, file = file.path("FIGURES", "fishDepthByYearUS.png"),
      width = 7, height = 7)
  }
  while (dev.cur() > 1) dev.off()
}
