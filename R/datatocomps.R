datatocomps <- function(dirdata, dirmod) {

  # todo:
  # 1. change to feeding data rather than raw files so it is clear what is being used
  # 2. weight-at-age file needs to be updated prior to creating catch-at-age because
  #    the average of the last five years that was in the file for the previous year
  #    is being used rather than this years data b/c it is already in the file
  # 3. Weight-at-age is summed over all ages 0-20 but ages are not available for 16+?

  options(default.stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)

  wtatage <- r4ss::SS_readwtatage(file = file.path(dirmod, "wtatage.ss"),
    verbose = FALSE) %>%
    dplyr::filter(Fleet == 1) %>%
    dplyr::rename(Year = "Yr") %>%
    dplyr::select(Year, dplyr::matches("^[0-9]+$")) %>%
    tidyr::pivot_longer(names_to = "age", values_to = "weight", -Year)

  final <- get_catchatage(dirdata) %>%
    tidyr::pivot_longer(
      names_prefix = "a",
      names_to = "age",
      values_to = "numbers",
      cols = dplyr::matches("^a[0-9]+$")
    ) %>%
    dplyr::left_join(wtatage, by = c("age", "Year")) %>%
    dplyr::group_by(Year, Sector) %>%
    dplyr::mutate(
      prop = (catch * numbers) / sum(numbers * weight)
    ) %>%
    dplyr::group_by(Year, age) %>%
    dplyr::summarize(
      nTrips = sum(Nsamples, na.rm = TRUE),
      prop = sum(prop, na.rm = TRUE)
    ) %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(prop = prop.table(prop) * 100) %>%
    dplyr::filter(!is.na(prop)) %>%
    dplyr::arrange(age = as.numeric(age)) %>%
    tidyr::pivot_wider(names_from = age, values_from = prop) %>%
    dplyr::mutate(
      Month = 7,
      Fleet = 1,
      Sex = 0,
      Partition = 0,
      AgeErr = max(Year) - 1972,
      LbinLo = -1,
      LbinHi = -1,
      .after = Year
    )

  ssdat <- r4ss::SS_readdat(
    file.path(dirmod, "hake_data.ss"),
    verbose = FALSE
  )
  new <- setNames(final[final[, "Year"] >= 2008, ], colnames(ssdat$agecomp))
  ssdat$agecomp <- rbind(
    ssdat$agecomp[!(ssdat$agecomp$Yr >= 2008 & ssdat$agecomp$FltSvy == 1), ],
    new
  )

  # Increment ageing error matrix
  while (ssdat$N_ageerror_definitions != max(final$AgeErr)) {
    ssdat$N_ageerror_definitions <- ssdat$N_ageerror_definitions + 1
    bias <- ssdat$ageerror[seq(2, NROW(ssdat$ageerror), by = 2), ]
    ssdat$ageerror <- rbind(
      ssdat$ageerror,
      ageerror_new(which(bias[NROW(bias)-1, ] / bias[NROW(bias), ] == 0.55) + 1)
    )
  }
  r4ss::SS_writedat(ssdat, file.path(dirmod, "hake_data.ss"),
    overwrite = TRUE, verbose = FALSE)
  return(final)
}

get_catchatage <- function(datapath) {
  source("https://raw.githubusercontent.com/pacific-hake/hake-assessment/master/R/load-data.R")

  candata <- load.can.age.data(file.path(datapath, "can-age-data.csv"))
  can <- mapply(function(x,y) cbind(x, Nsamples = y),
    setNames(candata[1:3], c("CAN_Shoreside", "CAN_FreezeTrawl", "CAN_JV")),
    lapply(candata[4:6], as.numeric)
  ) %>%
    lapply(tibble::as_tibble, rownames = "Year", .name_repair = "minimal") %>%
    dplyr::bind_rows(.id = "Sector") %>%
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::rename_with(~ gsub("^([0-9]+)$", "a\\1", .))

  usa <- list(
    "atSea_US_CP" = read.csv(file.path(datapath, "us-cp-age-data.csv")),
    "atSea_US_MS" = read.csv(file.path(datapath, "us-ms-age-data.csv")),
    "US_shore" = read.csv(file.path(datapath, "us-shore-age-data.csv"))
  ) %>%
    dplyr::bind_rows(.id = "Sector") %>%
    dplyr::mutate(Year = year, .keep = "unused") %>%
    dplyr::mutate(Nsamples = ifelse(is.na(n.trips), n.hauls, n.trips)) %>%
    dplyr::select(-n.trips, -n.hauls)

  catch <- utils::read.csv(file.path(datapath, "landings-tac-history.csv")) %>%
    dplyr::select(-dplyr::matches("total|TAC", ignore.case = TRUE)) %>%
    tidyr::pivot_longer(names_to = "Sector", values_to = "catch", -Year)

  dplyr::full_join(can, usa, by = colnames(can)) %>%
    dplyr::right_join(catch)
}
