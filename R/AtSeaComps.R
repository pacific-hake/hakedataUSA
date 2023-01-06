#' Workup At-Sea Composition Data for Pacific Hake
#'
#' todo: add more information about this function here.
#'
#' @template atsea.ages
#' @template ncatch
#' @param endyear The final year of the data, where the data will start
#' in 2008.
#' @param ages A vector of ages to be included in the composition data.
#' The default is to include ages from one to fifteen.
#' @template savedir
#'
#' @return A list object with summary information regrading the composition
#' data. Multiple files are saved to the disk as well. These saved files
#' include summary information in ...report.txt files and comps.csv files.
#' @export
#' @author Kelli F. Johnson
#'
atseacomps <- function(atsea.ages = loadlocal(file = "atsea.ages.Rdat"),
                       ncatch = loadlocal(file = "NORPACdomesticCatch.Rdat"),
                       endyear = hakedata_year(),
                       ages = 1:15,
                       savedir = hakedatawd()) {
  Yearlist <- 2008:endyear

  # 3 types (expanded, MS only, CP only)
  direc <- file.path(
    savedir, "Catches", "Comps",
    c("AtSea.Age.Only", "MS.Age.Only", "CP.Age.Only")
  )
  ignore <- sapply(direc, dir.create, recursive = TRUE, showWarnings = FALSE)

  # Previously in agedotoliths
  # todo: change to dplyr and see if ever used
  asum <- table(atsea.ages$Year, atsea.ages$Month,
    is.na(atsea.ages$AGE),
    useNA = "ifany"
  )
  endcol <- apply(asum, 3, rowSums)
  temp <- asum[, , "FALSE"] / apply(asum, 1:2, sum)
  colnames(temp) <- format(as.Date(
    paste("2000", colnames(temp), "1", sep = "/")
  ), "%b")
  keep <- cbind(temp, "Total" = prop.table(endcol, 1)[, 1]) * 100
  saveme <- rbind(
    cbind(asum[, , 1], "Total" = endcol[, 1]),
    cbind(asum[, , 2], "Total" = endcol[, 2])
  )
  saveme <- rbind(saveme, "Total" = colSums(saveme))
  dups <- duplicated(rownames(saveme))
  rownames(saveme)[dups] <- paste0(rownames(saveme)[dups], "notaged")
  saveme <- rbind(
    saveme,
    structure(keep,
      dimnames = list(paste0(rownames(keep), "perc"), colnames(keep))
    )
  )
  write.csv(saveme,
    file = file.path(savedir, "Catches", "Comps", "NORPACOtolithSum.csv")
  )

  for (ii in seq_along(direc)) {
    comps <- matrix(NA,
      ncol = length(ages) + 2,
      nrow = length(Yearlist),
      dimnames = list(
        Yearlist,
        c("Nfish", "NHauls", paste("Age", ages, sep = ""))
      )
    )
    i <- 0
    for (Yr in Yearlist) {
      i <- i + 1
      report.filename <- file.path(direc[ii], paste0(
        Yr,
        c(".Atsea.Ages.report.txt", ".MS.Ages.report.txt", ".CP.Ages.report.txt")[ii]
      ))
      dat <- atsea.ages[atsea.ages$YEAR == Yr, ]

      ncatch.yr <- ncatch[
        substring(ncatch$RETRIEVAL_DATE, 1, 4) == Yr &
          ncatch$SPECIES == 206,
      ]

      if (ii != 1) {
        vesseltype <- ifelse(ii == 2, 2, 1)
      } else {
        vesseltype <- NULL
      }
      if (all(is.na(dat$AGE))) next
      tmp <- atseaComp.yr(dat, ncatch.yr,
        BY_AGE = TRUE, BY_MONTH = FALSE, BY_GENDER = FALSE,
        lbin.sizes = NULL, minAge = min(ages), maxAge = max(ages),
        vesselType = vesseltype,
        in.season = NULL, in.pctl = 0.95,
        which = "pct", min_Haul = 0, min_T_weight = 0,
        minSampleSize = 1, remove_sparse = FALSE, NO_LENGTH = FALSE,
        report.filename = report.filename, DEBUG = FALSE
      )
      if (is.null(tmp)) next

      # this works because it is essentially only age and no other dimensions
      comps[i, ] <- c(tmp$nFish, tmp$nHauls, as.vector(apply(tmp$comps, 2, function(x) {
        x / sum(x, na.rm = T)
      })))
    }
    write.csv(comps, file = file.path(direc[ii], "comps.csv"))
  }
  dat <- atsea.ages[!is.na(atsea.ages$AGE), ]
  dat <- table(dat$YEAR, dat$AGE)
  daterange <- range(as.Date(substring(
    atsea.ages$HAUL_OFFLOAD_DATE[!is.na(atsea.ages$AGE)],
    1, 10
  )))
  if (daterange[1] != "2008-05-15") {
    stop(
      "The ages extracted in",
      " atsea.ages\nshould start in 2008-05-15 and not ",
      daterange[1]
    )
  }
  if (3 %in% c(
    unique(apply(table(ncatch$VESSEL_TYPE, ncatch$CRUISE) > 0, 2, sum)),
    unique(apply(table(ncatch$VESSEL_TYPE, ncatch$VESSEL) > 0, 2, sum))
  )) {
    stop(
      "Vessel_Type 3 sells unprocessed fish, which  may be accounted",
      "\nfor in shoreside sampling. Code suggests that all three types",
      "\nare present in a cruise or vessel when only two should be."
    )
  }

  summary <- list(
    "NAages" = sum(is.na(atsea.ages$AGE)),
    "atseaagesbyYrAge" = dat,
    "atseaagesbyYr" = rowSums(dat)
  )
  utils::write.table(summary$atseaagesbyYrAge,
    file = file.path(savedir, "Catches", "Comps", "PacFIN_ages_n.csv"),
    sep = ",", col.names = NA, row.names = TRUE
  )

  return(summary)
}
