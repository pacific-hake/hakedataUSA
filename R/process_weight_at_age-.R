#' Collate some of the age-weight data for the most recent year
#'
#' This script contains commands to format the data on weight, length, and age
#' from different sources, including the acoustic survey, the U.S. at-sea fishery,
#' and the U.S. shoreside fishery. Canadian fishery data does not appear to be included.
#'
#' The resulting compiled data for the most recent year are written to a file
#' with a name like /LengthWeightAge/LWAdata_2017.csv.
#'
#' @details
#' TODO: Ensure that this is done from 2008 to present every year to
#'       because there may be changes to previous weight-at-age data.
#' @param year An integer value specifying the year of data you would like to summarize.
#'
#' @export
#' @author Ian G. Taylor
#' @import utils
#' @return A list of weight-at-age information that includes the following items:
#' \enumerate{
#'   \item acousticmean: Mean weight-at-age from the survey, if the s
#' survey operated in that year.
#'   \item usatseamean: mean weight-at-age from the US-at-sea data.
#'   \item usshorebasedmean: mean weight-at-age from the US-shore-based data.
#'   \item outliers: A table of outliers by Agency for the US-shore-based fishery.
#'   \item largefishtable: A table of large fish (i.e., greater than 10 kg)
#' by data source.
#'   \item dat: The data that was saved to the disk for the given year.
#'   \item survey: A logical value specifying if it was a survey year.
#'   \item year: The year for which the data was compiled.
#'   \item file: The file path used to save the `dat`.
#' }
#'
process_weight_at_age_US <- function(year = hakedata_year(),
                                     savedir = hakedata_wd()) {
  info <- list()
  acdir <- file.path(savedir, "AcousticSurvey", "BioData", "csvFiles")

  # Determine if a survey occurred
  if (length(dir(acdir, pattern = as.character(year))) > 0) {
    Ac_survYear <- TRUE
    # Shimada
    datUS <- readxl::read_excel(
      path = dir(
        acdir,
        pattern = glue::glue("{year}.+specimen_{{0,1}}[AGES]{{0,4}}\\."),
        full.names = TRUE
      )
    ) %>%
      dplyr::mutate(
        Sex2 = dplyr::case_when(
          sex == 1 ~ "M",
          sex == 2 ~ "F",
          sex == 3 ~ "U"
        )
      ) %>%
      dplyr::left_join(
        readxl::read_excel(
          path = dir(
            acdir,
            pattern = glue::glue("{year}.+haul\\."),
            full.names = TRUE
          )
        ) %>% dplyr::select(haul, hb_date_time),
        by = "haul"
      ) %>%
      dplyr::transmute(
        Source = "Acoustic U.S.",
        Weight_kg = weight,
        Sex = Sex2,
        Age_yrs = age,
        Length_cm = length,
        Month = as.numeric(format(as.Date(hb_date_time, f = "%m/%d/%Y"), "%m")),
        Year = as.numeric(format(as.Date(hb_date_time, f = "%m/%d/%Y"), "%Y"))
      )
    datCAN <- readxl::read_excel(
      path = dir(
        acdir,
        pattern = glue::glue("{year}.+_specimen_CAN[_AGES]{{0,5}}\\."),
        full.names = TRUE
      )
    ) %>%
      dplyr::mutate(
        Sex2 = dplyr::case_when(
          sex == 1 ~ "M",
          sex == 2 ~ "F",
          sex == 3 ~ "U"
        )
      ) %>%
      dplyr::left_join(
        readxl::read_excel(
          path = dir(
            acdir,
            pattern = glue::glue("{year}.+biodata_haul_CAN\\."),
            full.names = TRUE
          )
        ) %>% dplyr::select(haul, eq_date_time),
        by = "haul"
      ) %>%
      dplyr::transmute(
        Source = "Acoustic U.S.",
        Weight_kg = weight,
        Sex = Sex2,
        Age_yrs = age,
        Length_cm = length,
        Month = as.numeric(format(as.Date(eq_date_time, f = "%m/%d/%Y"), "%m")),
        Year = as.numeric(format(as.Date(eq_date_time, f = "%m/%d/%Y"), "%Y"))
      )
    dat <- rbind(datUS, datCAN) %>%
      dplyr::filter(!is.na(Age_yrs) & !is.na(Weight_kg))
    info$acousticmean <- tapply(dat$Weight_kg, list(dat$Age_yrs), mean)
  } else {
    Ac_survYear <- FALSE
    info$acousticmean <- NULL
  }

  # US at sea fishery
  base::load(file.path(savedir, "extractedData", "atsea.ages.Rdat")) # atsea.ages
  tmp <- atsea.ages[
    !is.na(atsea.ages$AGE) &
      !is.na(atsea.ages$WEIGHT) &
      atsea.ages$Year %in% year,
  ]
  info$usatseamean <- tapply(tmp$WEIGHT, list(tmp$AGE), mean)
  tmp <- data.frame(
    Source = "ATSEA", Weight_kg = tmp$WEIGHT,
    Sex = tmp$SEX, Age_yrs = tmp$AGE,
    Length_cm = tmp$LENGTH,
    Month = tmp$Month,
    Year = tmp$Year
  )

  if (Ac_survYear) {
    dat <- rbind(dat, tmp)
  } else {
    dat <- tmp
  }
  rm(tmp)

  # US Shore-based fishery
  base::load(file.path(savedir, "extractedData", "page.Rdat"))
  page.worked <- page[
    !is.na(page$FISH_AGE_YEARS_FINAL) &
      !is.na(page$FISH_WEIGHT) &
      page$SAMPLE_YEAR %in% year,
  ]
  page.worked$SEX <- factor(page.worked$SEX)
  info$usshorebasedmean <- tapply(
    page.worked$FISH_WEIGHT / 1000,
    list("AGE" = page.worked$FISH_AGE_YEARS_FINAL), mean
  )
  tmp <- data.frame(
    Source = "SHORE",
    Weight_kg = page.worked$FISH_WEIGHT / 1000,
    Sex = page.worked$SEX, Age_yrs = page.worked$FISH_AGE_YEARS_FINAL,
    Length_cm = page.worked$FISH_LENGTH / 10,
    Month = page.worked$SAMPLE_MONTH,
    Year = page.worked$SAMPLE_YEAR
  )
  dat <- rbind(dat, tmp)
  rm(tmp)

  fs::dir_create(file.path(savedir, "LengthWeightAge"))
  fileout <- file.path(
    savedir, "LengthWeightAge",
    paste0("LWAdata_", year, ".csv")
  )
  bad <- dat[dat$Weight_kg > 10, ]
  info$outliers <- NULL
  if (NROW(bad) > 0) {
    info$outliers <- bad
    dat[dat$Weight_kg < 10, ]
  }
  utils::write.csv(dat, file = fileout, row.names = FALSE)

  info$largefishtable <- aggregate(Weight_kg ~ Source + I(Weight_kg > 10),
    data = dat, length
  )
  info$wtatage <- dat
  info$survey <- Ac_survYear
  info$year <- year
  info$file <- fileout
  invisible(info)
}

#' Create weight-at-age files for hake assessment
#'
#' Create weight-at-age files for the hake stock assessment.
#' Updated csv or rds files must exist prior to running.
#'
#' @param dir The directory where the data is stored.
#' It can either be relative or absolute because the working
#' directory will not be changed. Instead, `dir` is
#' used to import data and export results to.
#' @param maxage The age of the plus group used for the stock assessment.
#' This will correspond to the maximum age group in the data, not in the
#' model because SS can model many ages when there is only information in
#' the data for a few ages.
#' @param yrs A vector of years to search for recent data. Typically,
#' the vector starts with 2008 and ends with the most recent year
#' of data. This will allow files created from `process_weight_at_age_US()` to
#' be included in the analysis, i.e., recent US data. Typically, you
#' should not have to change this value from the default entry.
#' @param navgyears The number of early and late years to average since
#' 1975 and \code{max(yrs)} for the early and late analysis asked for
#' by the Scientific Review Group in 2017. The argument can be a single
#' value or a vector of two values, where in the latter case the second
#' value will be used for the most recent time period.
#' @param nforecast The number of years to forecast into the future.
#' Typically, this is three for the hake assessment and will lead to
#' this many rows of mean weight-at-age data being copied to the data frame
#' beyond the last year of empirical data.
#' @param maturity A vector of maturity values from the maturity ogive. The
#'   length needs to be the same as the number of ages in the model, not the
#'   number of ages in the data. The default is to use the maturity ogive stored
#'   in the package.
#'
#' @import ggplot2 r4ss utils
#' @export
#' @author Ian G. Taylor
#' @return todo: document return
#'
process_weight_at_age <- function(dir = file.path(hakedata_wd(), "LengthWeightAge"),
                                  maxage = 15,
                                  yrs = 2008:hakedata_year(),
                                  navgyears = 5,
                                  nforecast = 4,
                                  maturity = maturity_at_age) {
  fs::dir_create(path = file.path(dir, "plots"))

  # LengthWeightAge_data.rds provided by CG on 2021-01-09 in google drive #703
  # filtered by area rather than month and provided as rds rather than csv to
  # save on size, contains all US samples in LWAdata_1975to2007.csv, so
  # eliminated that file.
  files_weights <- fs::path(
    dir,
    c(
      "LengthWeightAge_data.rds",
      "can-weight-at-age-and-length.csv",
      glue::glue("LWAdata_{yrs}.csv")
    )
  )
  dat <- purrr::map_dfr(
    files_weights[fs::file_exists(files_weights)],
    .f = weight_at_age_read
  ) %>%
    weight_at_age_outlier(filter = FALSE, drop = FALSE)
  test_weight_at_age(dat)

  late <- (max(yrs) - navgyears + 1):(max(yrs))

  gg <- plot_weight_at_age(
    data = dplyr::filter(dat, Age_yrs <= 10, outlier == FALSE),
    maxage = maxage
  )
  ggplot2::ggsave(
    gg,
    width = 7, height = 7, units = "in",
    filename = file.path(dir, "plots", "meanweightatage_source.png")
  )
  gg <- plot_weight_at_age(
    data = dplyr::filter(dat, Age_yrs <= maxage, outlier == FALSE),
    maxage = maxage
  ) +
    ggplot2::facet_grid(cat ~ .)
  ggplot2::ggsave(gg,
    width = 7, height = 7, units = "in",
    filename = file.path(dir, "plots", "meanweightatage_all.png")
  )

  #### making input files for SS with the holes still present
  # NULL months keeps the Poland data
  wtage_All <- weight_at_age_wide(dat)
  wtage_All_wMean <- dplyr::bind_rows(
    weight_at_age_wide(dat %>% dplyr::mutate(Year = -1940)),
    weight_at_age_wide(dat)
  )

  #### making alternative data.frame with mean lengths
  lenage_All_wMean <- dplyr::bind_rows(
    weight_at_age_wide(
      dat %>%
        dplyr::mutate(Year = -1940) %>%
        dplyr::filter(!is.na(Length_cm)),
      value = "length"
    ),
    weight_at_age_wide(
      dat %>%
        dplyr::filter(!is.na(Length_cm)),
      value = "length"
    )
  )
  # repeat but return sample sizes instead of mean weights
  counts_All_wMean <- dplyr::bind_rows(
    weight_at_age_wide(
      dat %>% dplyr::mutate(Year = -1940),
      value = "count"
    ) %>%
      replace(is.na(.), 0),
    weight_at_age_wide(
      dat,
      value = "count"
    ) %>%
      replace(is.na(.), 0)
  )
  utils::write.csv(
    setNames(counts_All_wMean, gsub("#", "", colnames(counts_All_wMean))),
    file.path(dirname(normalizePath(dir)), "wtatage_all_samplesize.csv"),
    row.names = FALSE
  )
  # new method does only linear interpolation within each age (only works with all data)
  wtageInterp1_All <- dointerpSimple(wtage_All)

  #### do 2nd interpolation (actually an extrapolation at the edges)
  wtageInterp2_All <- fill_wtage_matrix(wtageInterp1_All)
  wtageInterp2_All$Note <- fill_wtage_matrix(wtage_All)$Note

  # write output combining all fleets closer to format used by SS
  wtage_All_wMean$Note <- c(paste("# Mean from ", min(dat$Year), "-", max(dat$Year), sep = ""), wtageInterp2_All$Note)
  wtageInterp2_All <- rbind(wtage_All_wMean[1, ], wtageInterp2_All)

  # matrices for plotting
  make_wtatage_plots(
    plots = 1:6,
    data = wtage_All_wMean,
    counts = counts_All_wMean,
    lengths = lenage_All_wMean,
    dir = file.path(dir, "plots"),
    year = max(yrs),
    maxage = maxage
  )

  # adding ages 16-20 as repeats of age 15
  wtage_extended <- wtageInterp2_All[, -grep("Note", colnames(wtageInterp2_All))]
  wtage_extended <- wtage_extended[, c(
    1:ncol(wtage_extended),
    rep(ncol(wtage_extended), times = sum(!(1:length(maturity) - 1) %in% 0:maxage))
  )]
  wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))] <-
    round(wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))], 4)
  colnames(wtage_extended)[grep("^a", colnames(wtage_extended))] <-
    paste0("a", seq_along(maturity) - 1)

  ## Add forecast average
  withforecast <- dplyr::bind_rows(
    wtage_extended,
    wtage_extended %>%
      dplyr::filter(`#Yr` %in% late) %>%
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with("a"), mean),
        `#Yr` = max(`#Yr`) + 1:NROW(.)
      ) %>%
      dplyr::filter(
        rep(c(TRUE, FALSE), times = c(nforecast, NROW(.) - nforecast))
      )
  )
  write_wtatage_file(
    file = fs::path(dirname(dir), "wtatage.ss"),
    data = withforecast,
    maturity = maturity
  )
  save(
    dat, wtage_All, wtage_All_wMean, withforecast,
    file = fs::path(dir, "LWAdata.Rdata")
  )

  return(withforecast)
}
