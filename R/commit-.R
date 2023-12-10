#' Updates the most recent catch time series in the SS3 data file
#'
#' Integrate the latest catch information from the U.S. and Canadian Pacific
#' Hake fisheries into the Stock Synthesis data file. Information on catches is
#' found in `.csv` files saved in the hake-assessment repository.
#'
#' @param dir_data A directory that houses the `.csv` data files for the stock
#'   assessment. These are typically found within the `data` directory of the
#'   hake-assessment repository.
#' @template digits
#' @param lastassessmentvals A vector of three values,
#'   * estimate of spawning stock biomass for terminal year in thousands of
#'     metric tons,
#'   * estimate of depletion in percent with one decimal place,
#'   * estimate of TAC for the first year of the forecast period in metric tons
#'     rounded to the nearest whole number.
#'
#' @return The following two files are updated in the
#' pacific-hake/hake-assessment repository:
#' * landings-tac-history.csv
#' * catch-targets-biomass.csv
#' @author Kelli F. Johnson
#' @export
commit_catch <- function(dir_data = hakedata_wd(),
                         digits = 5,
                         lastassessmentvals = c(NA, NA, NA)) {
  # Files that are read in and written back out
  file_lan <- fs::path(dir_data, "landings-tac-history.csv")
  file_tar <- fs::path(dir_data, "catch-targets-biomass.csv")
  # Files that are just read in
  file_sh <- fs::path(dir_data, "us-shore-catch-by-month.csv")
  file_re <- fs::path(dir_data, "us-research-catch-by-month.csv")
  file_cp <- fs::path(dir_data, "us-cp-catch-by-month.csv")
  file_ms <- fs::path(dir_data, "us-ms-catch-by-month.csv")
  file_ti <- fs::path(dir_data, "us-ti-catch-by-month.csv")
  file_can <- fs::dir_ls(
    gsub("-tables", "", dir_data),
    regexp = "can.*catch_by_month_df.rda"
  ) %>%
    `names<-`(gsub(".*can_([a-z]{2})_.*", "\\1", .))

  # Read in and prepare data sets
  sh <- dplyr::full_join(
    utils::read.csv(file_sh),
    utils::read.csv(file_re),
    by = c("year", "month")
  ) %>%
    dplyr::rename(US_shore = "catch.x", USresearch = "catch.y") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(dplyr::across(dplyr::starts_with("US"), sum, na.rm = TRUE)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("US"),
        ~ as.numeric(round(., digits = 5))
      )
    )
  cp <- utils::read.csv(file_cp) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(atSea_US_CP = sum(catch))
  ms <- utils::read.csv(file_ms) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(atSea_US_MS = sum(catch))
  ti <- utils::read.csv(file_ti) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(catch = sum(catch))
  temp.env <- new.env()
  can_names <- purrr::map(
    file_can,
    .f = \(x) load(x, envir = temp.env)
  )
  can <- purrr::map_dfr(
    can_names,
    .f = \(x) get(x, envir = temp.env),
    .id = "sector"
  ) %>%
    dplyr::mutate(sector = dplyr::case_when(
      sector == "ss" ~ "CAN_Shoreside",
      sector == "ft" ~ "CAN_FreezeTrawl",
      sector == "jv" ~ "CAN_JV"
    )) %>%
    dplyr::group_by(year, sector) %>%
    dplyr::summarize(catch = sum(catch)) %>%
    tidyr::pivot_wider(names_from = sector, values_from = catch) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("CAN"), tidyr::replace_na, 0)
    )
  inc <- list(sh, cp, ms, can) %>%
    purrr::reduce(dplyr::full_join, by = "year") %>%
    dplyr::rename(
      Year = year,
      `U.S. Mothership` = atSea_US_MS,
      `U.S. Catcher-processor` = atSea_US_CP,
      `U.S. Shore-based` = US_shore,
      `U.S. Research` = USresearch,
      `Canada Shoreside` = CAN_Shoreside,
      `Canada Freezer-trawler` = CAN_FreezeTrawl
    ) %>%
    coalesce_join(
      utils::read.csv(file_lan, check.names = FALSE),
      .,
      by = "Year"
    ) %>%
    # dplyr::mutate(dplyr::across(.fns = tidyr::replace_na, replace = 0)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("TAC"), as.character),
      dplyr::across(dplyr::matches("TAC"), ~ tidyr::replace_na(., "")),
      dplyr::across(dplyr::matches("[FJMCSR][oaer]"), ~ tidyr::replace_na(., 0))
    )

  check_year <- utils::read.csv(file_tar, check.names = FALSE)
  tar <- utils::read.csv(file_tar, check.names = FALSE) |>
    dplyr::select(!dplyr::matches("^TAC$|Total TAC")) |>
    dplyr::right_join(
      inc %>%
        dplyr::filter(Year >= min(check_year[, "Year"])) %>%
        dplyr::mutate(
          TOTAL = rowSums(
            dplyr::across(
              .cols = c(-Year, -`U.S. TAC`, -`Canada TAC`, -`Total TAC`)
            ),
            na.rm = TRUE
          )
        ) |>
        dplyr::select(Year, TOTAL, `Total TAC`),
      by = "Year"
    ) |>
    dplyr::mutate(`Realized catch` = sprintf("%.0f", TOTAL)) %>%
    dplyr::select(-TOTAL) %>%
    dplyr::relocate(`Realized catch`, `Total TAC`, .after = Year) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(~ tidyr::replace_na(., ""))


  if (!all(is.na(lastassessmentvals))) {
    tar[
      tar[, "Year"] == max(tar[, "Year"]),
      c("Biomass estimate", "Depletion", "TAC")
    ] <- lastassessmentvals
  }

  # Write files back to the disk
  utils::write.table(
    x = inc,
    file = file_lan,
    row.names = FALSE,
    sep = ",",
    quote = FALSE
  )
  utils::write.table(
    x = tar,
    file = file_tar,
    row.names = FALSE,
    sep = ",",
    quote = FALSE
  )
}
