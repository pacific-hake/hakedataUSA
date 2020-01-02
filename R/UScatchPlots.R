#' Plots of Catch for US Only
#'
#' Plots for the JTC of US catch data.
#' todo: Make CP and MS use NORPAC data instead of PacFIN.
#' 
#' @param doPNG A logical specifying if png files should be saved.
#' @param nyears The number of years you want to plot.
#' @export
#' @author Kelli Faye Johnson
#'
#' @return todo: document return of UScatchPlots
#' 
UScatchPlots <- function(doPNG = TRUE, nyears = 5) {
  mydir <- hakedatawd()
  args <- list(height = 5, width = 10, units = "in", pointsize = 10, res = 300)
  args2 <- list(mfrow=c(2,2),mar=c(4,4.1,0.5,0.5),oma=c(0,0.1,2,0.1),
    mgp=c(2.1, 0.75, 0), las = 1)
  args3 <- list(outer=TRUE,side=3,line=-0.3,cex=1.3)

  quotas <- read.csv(file.path(mydir, "Catches", "quotas.csv"), header = TRUE)
  colnames(quotas) <- gsub("^X", "", colnames(quotas))
  shore <- read.csv(
    file.path(mydir, "Catches", "USshoreCatchByPeriodComp_ft.csv"))
  cp <- read.csv(file.path(mydir, "Catches", "CP_CatchesByMonth.csv"))
  ms <- read.csv(file.path(mydir, "Catches", "MS_CatchesByMonth.csv"))
  colnames(shore) <- colnames(cp) <- colnames(ms) <- 
    c("Fleet", "Month", "Year", "Catch.MT")
  Yrs <- as.character((max(shore$Year) - nyears + 1):max(shore$Year))
  Yrs <- as.character((max(cp$Year) - nyears + 1):max(cp$Year))
  cols <- plotcolour(length(Yrs))
  lineTypes <- rep(1,length(Yrs))
  lineWds <- rep(2,length(Yrs))
  lineWds[length(Yrs)] <- 3

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path("Figures", "shoresideCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(shore[shore$Fleet=="USshore",],
    Yrs=Yrs,quotas=quotas[3,],lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3, text = "U.S. Shoreside Catches (preliminary)"))
  if (doPNG) dev.off()

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path("Figures", "CpCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  tmp <- quotas[1,-1]
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(cp,
    Yrs=Yrs,quotas=tmp,lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3, text = "U.S. CP Catches (preliminary)"))
  if(doPNG) dev.off()  

  if (doPNG) {
    do.call("png", c(args, 
      file = file.path("Figures", "MsCatchMonthYear.png")))
  } else {windows(height = args$height, width = args$width)}
  tmp <- quotas[2,-1]
  do.call("par", args2)
  plotHakeCatchMonthYear.fn(ms,
    Yrs=Yrs,quotas=tmp,lineWds=lineWds,lineTypes=lineTypes,
    cols=cols,leg.cex=0.7)
  do.call("mtext", c(args3, text = "U.S. MS Catches (preliminary)"))
  if(doPNG) dev.off()  

  tmp <- tapply(cp$Catch.MT,cp$Year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 136960, tolerance = 1e-04,
    info = "2017 US catcher-processor catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="CP","2017"]),
    0.9978726, tolerance = 1e-06,
    info = "Ratio of 2017 catch to catcher-processor quota.")  

  tmp <- tapply(ms$Catch.MT,ms$Year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 66427.88, tolerance = 1e-04,
    info = "2017 US mothership catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="MS","2017"]),
    0.6856435, tolerance = 1e-06,
    info = "Ratio of 2017 catch to mothership quota.")
      
  tmp <- tapply(shore$Catch.MT,shore$Year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 150841.18, tolerance = 1e-04,
    info = "2017 US shoreside catch in mt.")
  testthat::expect_equal(
    as.numeric(tmp/quotas[quotas$Fleet=="Shore","2017"]), 
    0.8896718, tolerance = 1e-06,
    info = "Ratio of 2017 catch to shoreside quota.")

}
