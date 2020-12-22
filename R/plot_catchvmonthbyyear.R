#' Plot a line of time-series data
#'
#' Plot a line of time-series data by month.
#'
#' @param x A vector of months in numeric form.
#' Also, commonly known as the `period`.
#' @param y Dependent variable in a vector that is the same length as `x`.
#' Typically, this will be monthly catches for a given year.
#' @param plotType The type of plot you wish to create, where the y-axis
#' type will change based on the entry. The default of `default` will lead
#' to the raw data being plotted. `proportion` plots the proportion and
#' `cumulative` plots the cumulative value for that year.
#' @param ... Arguments passed to [graphics::lines]
#' @return A line of the dependent variable by month
#' is added to the current figure.
#'
lines.bymonth <- function(x, y,
  plotType = c("default", "proportion", "cumulative"),
  ...) {

  plotType <- match.arg(plotType, several.ok = FALSE)
  if (plotType[1] == "proportion") {
    y <- cumsum(y)/sum(y)
  }
  if (plotType[1] == "cumulative") {
    y <- cumsum(y)
  }
  xx <- 1:12
  yy <- rep(NA, 12)
  for (i in xx) {
    if (any(x == i)) { #is the month present in the data
      yy[i] <- y[which(x == i)]
    } else {
      if (i == 1) {
        yy[i] <- 0
      } else {
        if (plotType[1] == "default") {
          yy[i] <- 0
        } else {
          yy[i] <- yy[i - 1]
        }
      }
    }
  }

  graphics::lines(xx, yy, type = "b", pch = 20, ...)
}

#' todo:document plot_catchvmonthbyyear
#'
#' todo:document plot_catchvmonthbyyear
#' @template data
#' @param Yrs todo: document
#' @param quotas todo: document
#' @param lineWds todo: document
#' @param lineTypes todo: document
#' @param cols todo: document
#' @param leg.cex todo: document
#' @param divisor todo: document
#' @param plotNum todo: document
#'
plot_catchvmonthbyyear <- function(data,
  Yrs = range(dat$year), quotas = NULL,
  lineWds, lineTypes, cols, leg.cex = 1,
  divisor = 1000, plotNum = 1:4) {
#Take hake dataframe of Fleet, Month, Year, MT and plots year specific catches by month
#Does not discriminate by fleet
#Assumes that there is one observation of Month in each year

  plot.base <- function(ylim = c(0, 1.05), ylab) {
    plot(1, 1, xlim = c(1, 12), ylim = ylim,
      type = "n", xaxt = "n",
      xlab = "", ylab = ylab, yaxs = "i")
  }
  units <- paste0("(", ifelse(divisor != 1, paste(divisor, " "), ""), "mt)")
  dat.yr <- split(data[data[, "year"] %in% Yrs,], data$year[data[, "year"] %in% Yrs])
  maxYlim <- 
  if (1 %in% plotNum) {
    plot.base(ylab = paste("Catch", units),
      ylim=c(0,max(data[data[, "year"] %in% Yrs, "catch"]) / divisor)
      )
    for (i in 1:length(Yrs)) {
      lines.bymonth(
        dat.yr[[Yrs[i]]]$month,
        dat.yr[[Yrs[i]]]$catch/divisor,
        col = cols[i], lwd = lineWds[i], lty = lineTypes[i])
    }
    axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)
  }

  if(2 %in% plotNum) {
    plot.base(ylab = paste0("Cumulative Catch", units),
      ylim=c(0, max(unlist(lapply(dat.yr,function(x){sum(x$catch)}))[Yrs],
        na.rm = TRUE) / divisor)
      )
    for(i in 1:length(Yrs)) {
      lines.bymonth(
        dat.yr[[Yrs[i]]]$month,
        dat.yr[[Yrs[i]]]$catch/divisor,
        plotType = "cumulative",
        col = cols[i], lwd = lineWds[i], lty = lineTypes[i])
    }
    legend("topleft", legend = Yrs, col = cols,
      lty = lineTypes, lwd = lineWds, cex = leg.cex, bty = "n")
    axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)
  }

  if(1 %in% plotNum) {
    plot.base(ylab = "Proportion of Total Catch")
    abline(h = 1, col = gray(0.5))
    for (i in 1:length(Yrs)) {
      lines.bymonth(
        dat.yr[[Yrs[i]]]$month,
        dat.yr[[Yrs[i]]]$catch / divisor,
        plotType = "proportion",
        col = cols[i], lwd = lineWds[i], lty = lineTypes[i])
    }
    axis(side = 1)
  }

  if(!is.null(quotas) & 1 %in% plotNum) {
    plot.base(ylab = "Proportion of Sector Quota")
    abline(h = 1, col = gray(0.5))
    for(i in 1:length(Yrs)) {
      if (is.null(dim(dat.yr[[Yrs[i]]])[1])) next
      dat.yr[[Yrs[i]]] <- rbind(dat.yr[[Yrs[i]]],
        c(NA, 13,
          as.numeric(Yrs[i]),
          quotas[[Yrs[i]]] - sum(dat.yr[[Yrs[i]]][,"catch"])))
      lines.bymonth(
        dat.yr[[Yrs[i]]]$month,
        dat.yr[[Yrs[i]]]$catch/divisor,
        plotType="proportion",
        col = cols[i], lwd = lineWds[i], lty = lineTypes[i])
    }
    axis(side = 1)
  }
  mtext(side = 1, outer = TRUE, "Month", line = 1.5)
}
