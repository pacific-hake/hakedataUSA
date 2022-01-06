#' Collate some of the age-weight data for the most recent year
#'
#' This script contains commands to format the data on weight, length, and age
#' from different sources, including the acoustic survey, the U.S. at-sea fishery,
#' and the U.S. shoreside fishery. Canadian fishery data does not appear to be included.
#'
#' The resulting compiled data for the most recent year are written to a file
#' with a name like /LengthWeightAge/LWAdata_2017.csv.
#' 
#' @param year An integer value specifying the year of data you would like to summarize.
#'
#' @export
#' @author Ian Taylor
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
#'   \item file: The file path used to save the \code{dat}.
#' }
#' 
wtatage_collate <- function(year = hakedata_year()) {
  info <- list()
  mydir <- hakedatawd()
  acdir <- file.path(mydir, "AcousticSurvey", "BioData", "csvFiles")
  
  lb2kg <- 0.453592

  # Determine if a survey occurred
  if (length(dir(acdir, pattern = as.character(year))) > 0) {
    Ac_survYear <- TRUE
    #Shimada
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
      data.frame
    haul <- readxl::read_excel(
      path = dir(
        acdir,
        pattern = glue::glue("{year}.+haul\\."),
        full.names = TRUE
      )
    ) %>%
      data.frame
    datUS <- merge(
      datUS,
      haul[, c("haul", "eq_date_time", "eq_latitude", "eq_longitude")],
      by = "haul", all.x = TRUE)
    datUS <- data.frame(Source="Acoustic U.S.", Weight_kg=datUS$weight, 
      Sex = datUS$Sex2, Age_yrs = datUS$age,
      Length_cm = datUS$length, 
      Month = as.numeric(format(as.Date(datUS$eq_date_time, "%m/%d/%y"), "%m")),
      Year = as.numeric(format(as.Date(datUS$eq_date_time, "%m/%d/%y"), "%Y"))
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
      )
    haul <- readxl::read_excel(
      path = dir(
        acdir,
        pattern = glue::glue("{year}.+biodata_haul_CAN\\."),
        full.names = TRUE
      )
    )
    if (!"eq_date_time" %in% colnames(haul) | all(is.na(haul$eq_date_time))) {
      haul$eq_date_time <- haul$hb_date_time
    }
    datCAN <- merge(
      datCAN,
      haul[, c("haul", "eq_date_time", "eq_latitude", "eq_longitude")],
      by = "haul", all.x = TRUE)
    if (grepl("^[0-9]{4}-", datCAN$eq_date_time[1])) {
      datCAN$eq_date_time <- as.Date(datCAN$eq_date_time, f = "%Y-%m-%d")
    } else {
      datCAN$eq_date_time <- as.Date(datCAN$eq_date_time, f = "%m/%d/%y")
    }
    datCAN <- data.frame(Source = "Acoustic Canada", Weight_kg = datCAN$weight, 
      Sex = datCAN$Sex2, Age_yrs = datCAN$age,
      Length_cm = datCAN$length, 
      Month = as.numeric(format(datCAN$eq_date_time, "%m")),
      Year = as.numeric(format(datCAN$eq_date_time, "%Y")))

    dat <- rbind(datUS, datCAN)
    dat <- dat[!(is.na(dat$Age_yrs) | is.na(dat$Weight_kg)), ]

    info$acousticmean <- tapply(dat$Weight_kg, list(dat$Age_yrs), mean)
  } else { 
    Ac_survYear <- FALSE
    info$acousticmean <- NULL
  }
  
  #US at sea fishery
  base::load(file.path(mydir, "extractedData", "atsea.ages.Rdat")) #atsea.ages
  tmp <- atsea.ages[
    !is.na(atsea.ages$AGE) &
    !is.na(atsea.ages$WEIGHT) &
    atsea.ages$Year %in% year, ]
  info$usatseamean <- tapply(tmp$WEIGHT, list(tmp$AGE), mean)
  tmp <- data.frame(Source = "ATSEA", Weight_kg = tmp$WEIGHT, 
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

  #US Shore-based fishery
  base::load(file.path(mydir, "extractedData", "page.Rdat"))
  page.worked <- page[
    !is.na(page$FISH_AGE_YEARS_FINAL) &
    !is.na(page$FISH_WEIGHT) &
    page$SAMPLE_YEAR %in% year, ]
  page.worked$SEX <- factor(page.worked$SEX)
  info$usshorebasedmean <- tapply(page.worked$FISH_WEIGHT / 1000, 
    list("AGE" = page.worked$FISH_AGE_YEARS_FINAL), mean)
  tmp <- data.frame(Source = "SHORE",
    Weight_kg = page.worked$FISH_WEIGHT / 1000,
    Sex = page.worked$SEX, Age_yrs = page.worked$FISH_AGE_YEARS_FINAL,
    Length_cm = page.worked$FISH_LENGTH / 10, 
    Month = page.worked$SAMPLE_MONTH, 
    Year = page.worked$SAMPLE_YEAR)

  dat <- rbind(dat, tmp)
  rm(tmp)

  fileout <- file.path(mydir, "LengthWeightAge", 
    paste0("LWAdata_", year, ".csv"))
  bad <- dat[dat$Weight_kg > 10, ]
  info$outliers <- NULL
  if (NROW(bad) > 0) {
    info$outliers <- bad
    dat[dat$Weight_kg < 10, ]
  }
  utils::write.csv(dat, file = fileout, row.names = FALSE)

  info$largefishtable <- aggregate(Weight_kg ~ Source + I(Weight_kg > 10),  
    data = dat, length)
  info$wtatage <- dat
  info$survey <- Ac_survYear
  info$year <- year
  info$file <- fileout
  invisible(info)
}




