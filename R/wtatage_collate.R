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
    datCAN <- merge(
      datCAN,
      haul[, c("Haul", "EQ_Date_Time", "EQ_Latitude", "EQ_Longitude")],
      by = "Haul", all.x = TRUE)
    datCAN <- data.frame(Source = "Acoustic Canada", Weight_kg = datCAN$Weight, 
      Sex = datCAN$Sex2, Age_yrs = datCAN$Age,
      Length_cm = datCAN$Length, 
      Month = as.numeric(format(as.Date(datCAN$EQ_Date_Time, "%m/%d/%y"), "%m")),
      Year = as.numeric(format(as.Date(datCAN$EQ_Date_Time, "%m/%d/%y"), "%Y")))

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
  base::load(file.path(mydir, "extractedData", "pacfin_bds_age.Rdat"))
  base::load(file.path(mydir, "extractedData", "pacfin_bds_allsp_cluster.Rdat"))
  base::load(file.path(mydir, "extractedData", "pacfin_bds_fish.Rdat"))
  base::load(file.path(mydir, "extractedData", "pacfin_bds_sp_cluster.Rdat"))
  bds.fish.worked <- workupPacFinTablesBDS(bds_fish = bds.fish,
    age_temp = bds.age, sp_cluster = bds.sp.cluster,
    all_cluster = bds.allsp.cluster)
  bds.fish.worked$SEX <- factor(bds.fish.worked$SEX)
  tmp <- bds.fish.worked[
    !is.na(bds.fish.worked$FISH_AGE_YEARS_FINAL) &
    !is.na(bds.fish.worked$FISH_WEIGHT) & 
    bds.fish.worked$SAMPLE_YEAR %in% year, ]
  info$usshorebasedmean <- tapply(
    tmp$FISH_WEIGHT * lb2kg,
    list(tmp$FISH_AGE_YEARS_FINAL), mean)
  tmp <- data.frame(Source = "SHORE", Weight_kg = tmp$FISH_WEIGHT * lb2kg, 
    Sex = tmp$SEX, Age_yrs = tmp$FISH_AGE_YEARS_FINAL, 
    Length_cm = tmp$FISH_LENGTH / 10, 
    Month = tmp$SAMPLE_MONTH, 
    Year = tmp$SAMPLE_YEAR)

  dat <- rbind(dat, tmp)
  rm(tmp)

  fileout <- file.path(mydir, "LengthWeightAge", 
    paste0("LWAdata_", year, ".csv"))
  utils::write.csv(dat, file = fileout, row.names = FALSE)

  ### checking for source of outliers in 2017 data
  info$outliers <- table(
    bds.fish.worked$SAMPLE_AGENCY[
      !is.na(bds.fish.worked$FISH_AGE_YEARS_FINAL) &
      !is.na(bds.fish.worked$FISH_WEIGHT) &
      bds.fish.worked$FISH_WEIGHT > 10])
  ## CA OR PW  W 
  ##  0  0  0 40 
  info$largefishtable <- aggregate(Weight_kg ~ Source + I(Weight_kg > 10),  
    data = dat, length)
  info$wtatage <- dat
  info$survey <- Ac_survYear
  info$year <- year
  info$file <- fileout
  invisible(info)
}




