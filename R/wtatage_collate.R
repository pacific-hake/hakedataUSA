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
    datUS <- utils::read.csv(
      file = dir(acdir, pattern = paste0(year, ".*S_biodata_specimen_AGES.csv"),
        full.names = TRUE))
    datUS$Sex2 <- "U"
    datUS$Sex2[datUS$Sex == 1] <- "M"
    datUS$Sex2[datUS$Sex == 2] <- "F"
    haul <- utils::read.csv(
      file = dir(acdir, pattern = paste0(year, ".*S_biodata_haul.csv"),
        full.names = TRUE),
      stringsAsFactors = FALSE)
    datUS <- merge(
      datUS,
      haul[, c("Haul", "EQ_Date_Time", "EQ_Latitude", "EQ_Longitude")],
      by = "Haul", all.x = TRUE)
    datUS <- data.frame(Source="Acoustic U.S.", Weight_kg=datUS$Weight, 
      Sex = datUS$Sex2, Age_yrs = datUS$Age,
      Length_cm = datUS$Length, 
      Month = as.numeric(format(as.Date(datUS$EQ_Date_Time, "%m/%d/%y"), "%m")),
      Year = as.numeric(format(as.Date(datUS$EQ_Date_Time, "%m/%d/%y"), "%Y")))

    datCAN <- utils::read.csv(
      file.path(acdir, paste0(year, "_biodata_specimen_CAN.csv")))
    datCAN$Sex2 <- "U"
    datCAN$Sex2[datCAN$Sex == 1] <- "M"
    datCAN$Sex2[datCAN$Sex == 2] <- "F"
    haul <- utils::read.csv(
      file.path(acdir, paste0(year, "_biodata_haul_CAN.csv")),
      stringsAsFactors = FALSE)
    if (!"EQ_Date_Time" %in% colnames(haul) | all(is.na(haul$EQ_Date_Time))) {
      haul$EQ_Date_Time <- haul$HB_Date_Time
    }
    datCAN <- merge(
      datCAN,
      haul[, c("Haul", "EQ_Date_Time", "EQ_Latitude", "EQ_Longitude")],
      by = "Haul", all.x = TRUE)
    if (grepl("^[0-9]{4}-", datCAN$EQ_Date_Time[1])) {
      datCAN$EQ_Date_Time <- as.Date(datCAN$EQ_Date_Time, "%Y-%m-%d")
    } else {
      datCAN$EQ_Date_Time <- as.Date(datCAN$EQ_Date_Time, "%m/%d/%y")
    }
    datCAN <- data.frame(Source = "Acoustic Canada", Weight_kg = datCAN$Weight, 
      Sex = datCAN$Sex2, Age_yrs = datCAN$Age,
      Length_cm = datCAN$Length, 
      Month = as.numeric(format(datCAN$EQ_Date_Time, "%m")),
      Year = as.numeric(format(datCAN$EQ_Date_Time, "%Y")))

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
    atsea.ages$YEAR %in% year, ]
  info$usatseamean <- tapply(tmp$WEIGHT, list(tmp$AGE), mean)
  tmp <- data.frame(Source = "ATSEA", Weight_kg = tmp$WEIGHT, 
    Sex = tmp$SEX, Age_yrs = tmp$AGE, 
    Length_cm = tmp$LENGTH, 
    Month = as.numeric(format(tmp$HAUL_OFFLOAD_DATE, "%m")), 
    Year = tmp$YEAR)

  if (Ac_survYear) {
    dat <- rbind(dat, tmp)
  } else {
    dat <- tmp
  }
  rm(tmp)

  #US Shore-based fishery
  base::load(file.path(mydir, "extractedData", "page.Rdat"))
  bds.fish.worked <- page[
    !is.na(page$FISH_AGE_YEARS_FINAL) &
    !is.na(page$FISH_WEIGHT) &
    page$SAMPLE_YEAR %in% year, ]
  bds.fish.worked$SEX <- factor(bds.fish.worked$SEX)
  info$usshorebasedmean <- tapply(bds.fish.worked$FISH_WEIGHT / 1000, 
    list("AGE" = bds.fish.worked$FISH_AGE_YEARS_FINAL), mean)
  tmp <- data.frame(Source = "SHORE",
    Weight_kg = bds.fish.worked$FISH_WEIGHT / 1000,
    Sex = bds.fish.worked$SEX, Age_yrs = bds.fish.worked$FISH_AGE_YEARS_FINAL,
    Length_cm = bds.fish.worked$FISH_LENGTH / 10, 
    Month = bds.fish.worked$SAMPLE_MONTH, 
    Year = bds.fish.worked$SAMPLE_YEAR)

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




