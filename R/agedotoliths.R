#' Summary of NORPAC Otoliths by Year
#' 
#' Create a proportion table of age samples that have been aged
#' for the NORPAC data. This information is normally given to the
#' stakeholders at the December JTC meeting.
#' 
#' @param agedata A data frame of NORPAC ages, often called atsea.ages.
#' 
#' @author Kelli Faye Johnson
#' @return A data frame of proportions by year and month. The 
#' function also saves a summary file (csv format) to the disk
#' with sample sizes from which proportions can be calculated. 
#' 
agedotoliths <- function(agedata) {
  agedata$Month <- format(agedata$HAUL_OFFLOAD_DATE, "%m")
  agedata$Year <- format(agedata$HAUL_OFFLOAD_DATE, "%Y")
  agedata$isaged <- is.na(agedata$AGE)
  asum <- table(agedata$Year, agedata$Month, 
    agedata$isaged, useNA = "ifany")
  endcol <- apply(asum, 3, rowSums)
  all <- apply(asum, 1:2, sum)
  temp <- asum[, , "FALSE"] / all
  colnames(temp) <- format(as.Date(
    paste("2000", colnames(temp), "1", sep = "/")), "%b")
  keep <- cbind(temp, "Total" = prop.table(endcol, 1)[, 1]) * 100
  
  saveme <- rbind(
    cbind(asum[,,1], "Total" = endcol[, 1]),
    cbind(asum[,,2], "Total" = endcol[, 2]))
  saveme <- rbind(saveme, "Total" = colSums(saveme))
  dups <- duplicated(rownames(saveme))
  rownames(saveme)[dups] <- paste0(rownames(saveme)[dups], "notaged")
  saveme <- rbind(saveme, 
    structure(keep, 
      dimnames = list(paste0(rownames(keep), "perc"), colnames(keep))))
  write.csv(saveme, 
    file = file.path(hakedatawd(), "Catches", "Comps", "NORPACOtolithSum.csv"))

  return(keep)
}
