
#'
comps_plot <- function() {
  mydir <- hakedatawd()
  datfile <- file.path(mydir, "Catches", "Comps", "AgeCompsForSS.txt")
  if (!file.exists(datfile)) stop("The file, ", datfile, " was not found.\n",
    "Must combine composition data for SS to run comps_plot.")
  dat <- read.table(file = datfile)
  yrs <- dat$V1
  comps <- dat[, paste0("V", 10:ncol(dat))]
  x <- c(rep(yrs, ncol(comps)), 0)
  y <- c(rep(1:ncol(comps), each = length(yrs)), -1)
  z <- sqrt(c(unlist(comps), 100))

  for (minyear in c(min(yrs), max(yrs) - 15)) {
    png(
      file = file.path(mydir, "Catches", "Comps", 
        paste0("propnatage_fish_minyear", minyear, ".png")),
      height=6, width=15, units = "in", res = 600)
    par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
    symbols(x, y, circles = z,
      inches = 0.2, xlim = range(yrs[yrs >= minyear]),
      xaxt = "n", yaxt = "n", ylim = c(1, ncol(comps)),
      bg = rgb(0,0,1,0.6), 
      xlab = "Year", ylab = "Age",
      las = 1)
    axis(1,at=yrs)
    axis(2,at=1:ncol(comps),las=1)
    title(main="Proportion of numbers-at-age (coastwide fishery)")
    dev.off()
  }

}
