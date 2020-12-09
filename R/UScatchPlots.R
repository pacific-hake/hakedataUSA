#' Plots of Catch for US Only
#'
#' Plots for the JTC of US catch data.
#' todo: Make CP and MS use NORPAC data instead of PacFIN.
#' 
#' @param doPNG A logical specifying if png files should be saved.
#' @param nyears The number of years you want to plot. The maximum year
#' will be found and \code{nyears} will be sequenced into history until
#' the desired range is found. Years will be included in this range even
#' if they do not have any data.
#' @param preliminary A logical value specifying if the data represents
#' preliminary data available prior to the first week of January when
#' the final data pull is normally extracted from the data warehouses.
#' The default, of \code{TRUE}, adds \code{"(preliminary)"} to the title
#' of each plot.
#' @param cex.title Font size for the main title placed above the figure.
#' Originally, the default was 1.3, this has since been changed to reflect
#' that an additional title is not printed on the slide for the JTC meeting.
#' Thus, a larger title was needed and the default is now .
#' @export
#' @author Kelli Faye Johnson
#'
#' @return todo: document return of UScatchPlots
#' 
UScatchPlots <- function(doPNG = TRUE, nyears = 5, preliminary = TRUE,
  cex.title = c(3, 1.3)[1]) {
  mydir <- hakedatawd()
  args <- list(height = 5, width = 10, units = "in", pointsize = 10, res = 300)
  args2 <- list(mfrow=c(2,2),mar=c(0.5,4.1,0.5,0.5),
    oma=c(4,0.1,ceiling(cex.title),0.1),
    mgp=c(2.1, 0.75, 0), las = 1)
  args3 <- list(outer=TRUE,side=3,line=-0.1,cex=cex.title)
  args4 <- list(outer=TRUE,side=1,line=1.5,cex=1.3)

  quotas <- read.csv(file.path(mydir, "Catches", "quotas.csv"), 
    sep = ",", header = TRUE, check.names = FALSE)
  shore <- read.csv(
    file.path(mydir, "Catches", "USshoreCatchByPeriodComp_ft.csv"))
  cp <- data.frame("sector" = "CP", 
    read.csv(file.path(mydir, "Catches", "us-cp-catch-by-month.csv")))
  ms <- data.frame("sector" = "MS", 
    read.csv(file.path(mydir, "Catches", "us-ms-catch-by-month.csv")))
  Yrs <- as.character((max(shore$year) - nyears + 1):max(shore$year))
  Yrs <- as.character((max(cp$year) - nyears + 1):max(cp$year))
  cols <- plotcolour(length(Yrs))
  lineTypes <- rep(1,length(Yrs))
  lineWds <- rep(2,length(Yrs))
  lineWds[length(Yrs)] <- 3

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path(mydir, "Figures", "shoresideCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(shore[shore$sector=="USshore",],
    Yrs=Yrs,quotas=quotas[3,],lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3,
    text = paste(
      "U.S. Shoreside Catches",
      ifelse(preliminary, " (preliminary)", "")))
  )
  do.call("mtext", c(args4, text = "Month"))
  if (doPNG) dev.off()

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path(mydir, "Figures", "CpCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(cp,
    Yrs=Yrs,quotas=quotas[1,-1],lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3,
    text = paste(
      "U.S. CP Catches",
      ifelse(preliminary, " (preliminary)", "")))
  )
  do.call("mtext", c(args4, text = "Month"))
  if(doPNG) dev.off()  

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path(mydir, "Figures", "MsCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(ms,
    Yrs=Yrs,quotas=quotas[2,-1],lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3,
    text = paste(
      "U.S. MS Catches",
      ifelse(preliminary, " (preliminary)", "")))
  )
  do.call("mtext", c(args4, text = "Month"))
  if(doPNG) dev.off()  

  tmp <- tapply(cp$catch,cp$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 136960, tolerance = 1e-04,
    info = "2017 US catcher-processor catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="CP","2017"]),
    0.9978726, tolerance = 1e-06,
    info = "Ratio of 2017 catch to catcher-processor quota.")  

  tmp <- tapply(ms$catch,ms$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 66427.88, tolerance = 1e-04,
    info = "2017 US mothership catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="MS","2017"]),
    0.6856435, tolerance = 1e-06,
    info = "Ratio of 2017 catch to mothership quota.")
      
  tmp <- tapply(shore$catch,shore$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 150841.18, tolerance = 1e-04,
    info = "2017 US shoreside catch in mt.")
  testthat::expect_equal(
    as.numeric(tmp/quotas[quotas$Fleet=="Shore","2017"]), 
    0.8896718, tolerance = 1e-06,
    info = "Ratio of 2017 catch to shoreside quota.")

}
