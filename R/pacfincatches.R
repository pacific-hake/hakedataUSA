#' Print summaries and figures for catches from PacFIN
#'
#' @template pcatch
#' @template nyears
#' @template savedir
#'
#' @return The following files are saved to the disk:
#' * PacFIN_Fleet.csv
#' * PacFIN_Sector.cs
#' * USshoreCatchByPeriodComp_ft.csv
#' * us-shore-catch-by-month.csv
#' * us-research-catch-by-month.csv
#' * us-shore-startdate-by-dahl.csv
#' * us-ti-catch-by-month.csv
#'
pacfincatches <- function(pcatch = loadlocal(file = "pacfin_catch.Rdat"),
                          nyears = 5,
                          savedir = hakedatawd()) {
  # File management
  data("quotas")

  utils::write.table(
    x = tapply(pcatch$MT, list(pcatch$YEAR, pcatch$FLEET), sum),
    file = file.path(savedir, "Catches", "PacFIN_Fleet.csv"),
    sep = ",", quote = FALSE, row.names = TRUE, col.names = NA
  )

  # FLEET XXX is in the hake assessment as shore-based catches,
  # although 1986 differs from data used
  # database  1986 3431.9436
  # assesment 1986 3465.00

  utils::write.table(tapply(pcatch$MT, list(pcatch$year, pcatch$sector), sum),
    file = file.path(savedir, "Catches", "PacFIN_Sector.csv"),
    sep = ",", quote = FALSE, row.names = TRUE, col.names = NA
  )
  pcatch.yr.per <- stats::aggregate(list("catch" = pcatch$MT),
    list("sector" = pcatch$sector, "month" = pcatch$month, "year" = pcatch$year),
    FUN = sum
  )
  pcatch.yr.per <- pcatch.yr.per[order(pcatch.yr.per$sector), ]
  pcatch.yr.per$catch <- round(pcatch.yr.per$catch, 5)
  utils::write.table(pcatch.yr.per,
    file = file.path(savedir, "Catches", "USshoreCatchByPeriodComp_ft.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
  plot_catchvmonthbyyear(
    data = pcatch.yr.per %>%
      dplyr::filter(sector == "USshore") %>%
      dplyr::select(-sector),
    Yrs = as.character(
      (max(pcatch.yr.per$year) - nyears + 1):max(pcatch.yr.per$year)
    ),
    quotas = quotas[3, ],
    title = paste(
      "U.S. Shoreside Catches",
      ifelse(hakedata_prelim(), "(preliminary)", "")
    ),
    file = file.path(savedir, "Figures", "shoresideCatchMonthYear.png")
  )

  utils::write.table(pcatch.yr.per[pcatch.yr.per$sector == "USshore", -1],
    file = file.path(savedir, "Catches", "us-shore-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
  research <- pcatch[pcatch$sector == "USresearch", ]
  research <- stats::aggregate(list("catch" = research$MT),
    list("month" = research$month, "year" = research$year),
    FUN = sum
  )
  research$catch <- sprintf("%.9f", research$catch)
  utils::write.table(research,
    file = file.path(savedir, "Catches", "us-research-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
  utils::write.table(
    x = stats::aggregate(Date ~ year + DAHL_SECTOR, data = pcatch, min),
    file = file.path(savedir, "Catches", "us-shore-startdate-by-dahl.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )

  # Look at tribal catch in shoreside (already added in above)
  utils::write.table(
    x = pcatch %>%
      dplyr::filter(FLEET == "TI") %>%
      dplyr::group_by(month, year) %>%
      dplyr::summarize(catch = sum(MT)) %>%
      dplyr::arrange(year, month) %>%
      dplyr::ungroup(),
    file = file.path(savedir, "Catches", "us-ti-catch-by-month.csv"),
    sep = ",", quote = FALSE, row.names = FALSE
  )
}
