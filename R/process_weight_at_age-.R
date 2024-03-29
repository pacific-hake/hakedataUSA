#' Get the weight-at-age data from the NWFSC server
#'
#' @param savedir A string specifying the path of interest.
#'
#' @export
#' @author Kelli F. Johnson
#'
process_weight_at_age_survey <- function(savedir = hakedata_wd()) {
  year <- hakedata_year()
  # NWFSC Acoustic Survey path to biological data since 1995
  server_path <- fs::path(
    "//nwcfile", "fram", "Survey.Acoustics", "Survey Time Series Analysis",
    "Historical Summary (for Kriging)", "Biological"
  )

  # Search for the annual directories, then search for biological_specimen and
  # the haul files. Merge the two files together then row bind all of the years.
  main_tibble <- fs::dir_ls(
    path = server_path,
    type = "dir",
    recurse = TRUE,
    regexp = paste0(
      .Platform[["file.sep"]], "[0-9]{4}",
      .Platform[["file.sep"]], c("US$", "CAN$"),
      collapse = "|"
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      bio_file = purrr::map_chr(
        value,
        .f = \(x) fs::dir_ls(x, regexp = "biodata_specimen.+x")[1]
      ),
      haul_file = purrr::map_chr(
        value,
        .f = \(x) fs::dir_ls(x, regexp = "[hH][aA][uU][lL][_CANand]{0,7}\\.")[1]
      ),
      data = purrr::map2(bio_file, haul_file, .f = read_weight_at_age_nwfsc)
    ) |>
    dplyr::pull(data) |>
    dplyr::bind_rows()

  # Save the data after combining with old data
  file_path <- fs::path(savedir, "survey-weight-at-age.csv")
  old_data <- utils::read.csv(file_path) |>
    dplyr::filter(Year < 1993) |>
    dplyr::mutate(
      Source = dplyr::case_when(
        Source == "US_acoustic" ~ "U.S. Acoustic",
        Source == "CAN_acoustic" ~ "Canada Acoustic",
        TRUE ~ Source
      )
    )
  final_data <- dplyr::bind_rows(
    old_data,
    main_tibble
  ) |>
    dplyr::arrange(Source, Year, Month) |>
    as.data.frame()

  utils::write.csv(
    x = final_data,
    file = file_path,
    quote = FALSE,
    row.names = FALSE
  )
  return(invisible(final_data))
}

process_weight_at_age_us <- function(savedir = hakedata_wd()) {
  base::load(file.path(savedir, "extractedData", "atsea.ages.Rdat"))
  base::load(file.path(savedir, "extractedData", "page.Rdat"))
  tmp <- dplyr::bind_rows(
    US_atsea = atsea.ages %>%
      dplyr::rename(
        Weight_kg = "WEIGHT",
        Sex = "SEX",
        Age_yrs = "AGE",
        Length_cm = "LENGTH"
      ) %>%
      dplyr::mutate(
        Year = as.numeric(Year, as.is = TRUE),
        Month = as.numeric(Month, as.is = TRUE)
      ) %>%
      dplyr::select(Weight_kg, Sex, Age_yrs, Length_cm, Month, Year),
    US_shore = page %>%
      dplyr::rename(
        Sex = "SEX",
        Age_yrs = "AGE",
        Month = "SAMPLE_MONTH",
        Year = "SAMPLE_YEAR"
      ) %>%
      dplyr::mutate(
        Weight_kg = FISH_WEIGHT / 1000,
        Length_cm = FISH_LENGTH / 10
      ) %>%
      dplyr::select(Weight_kg, Sex, Age_yrs, Length_cm, Month, Year),
    .id = "Source"
  ) %>%
    dplyr::filter(
      !is.na(Age_yrs),
      !is.na(Weight_kg)
    )
  final_data <- utils::read.csv(
    file = fs::path(savedir, "us-weight-at-age.csv")
  ) %>%
    dplyr::filter(Year < 2008) %>%
    dplyr::bind_rows(tmp) |>
    dplyr::arrange(Source, Year, Month)
  write.csv(
    x = final_data |> as.data.frame(),
    file = fs::path(savedir, "us-weight-at-age.csv"),
    quote = FALSE,
    row.names = FALSE
  )
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
#' @export
#' @author Ian G. Taylor
#' @return todo: document return
#'
process_weight_at_age <- function(dir = hakedata_wd(),
                                  maxage = 15,
                                  yrs = 2008:hakedata_year(),
                                  navgyears = 5,
                                  nforecast = 4,
                                  maturity = maturity_at_age) {
  fs::dir_create(path = file.path(dir, "plots"))

  # length-weight-age_data.rds provided by CG on 2021-01-09 in google drive #703
  # filtered by area rather than month and provided as rds rather than csv to
  # save on size, contains all US samples in LWAdata_1975to2007.csv, so
  # eliminated that file.
  files_weights <- fs::path(
    ext = "csv",
    dir,
    c("survey-weight-at-age", "us-weight-at-age", "can-weight-at-age")
  )
  dat <- purrr::map_dfr(
    files_weights,
    .f = weight_at_age_read
  ) %>%
    weight_at_age_outlier(filter = FALSE, drop = FALSE)

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
  dat <- dplyr::filter(dat, !outlier)
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
    file.path(normalizePath(dir), "wtatage-all-samplesize.csv"),
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
