datatocomps <- function(dirdata, dirmod) {

  options(default.stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)
  
  can <- readLines(file.path(dirdata, "can-age-data.csv"))
  if (!grepl("\\d{4}", substring(can[2], 1, 2))) can <- can[-2]

  cansplit <- strsplit(can, ",")
  canlabel <- cansplit[which(sapply(cansplit, length) == 1)]
  canlabel <- gsub(".+Trawler.+", "CAN FreezerTrawl", canlabel)
  canlabel <- gsub(".+side.+", "CAN Shoreside", canlabel)
  canlabel <- gsub(".+Venture.+", "CAN JV", canlabel)
  temp <- which(sapply(cansplit, length) == 2)
  ncan <- data.frame(rep(canlabel[4:6], 
    diff(c(0, 
      which(!diff(temp) == 1), length(temp)))), 
      do.call("rbind", cansplit[temp]))
  colnames(ncan) <- c("Sector", "Year", "Nsamples")
  temp <- which(sapply(cansplit, length) > 2)
  cancomp <- data.frame(rep(canlabel[1:3], 
    diff(c(0, 
      which(!diff(temp) == 1), length(temp)))), 
      do.call("rbind", cansplit[temp]))
  colnames(cancomp) <- c("Sector", "Year", paste0("a", 3:ncol(cancomp) - 2))
  cancomp <- merge(cancomp, ncan, all.x = TRUE)
  usc <- utils::read.csv(file = file.path(dirdata, "us-cp-age-data.csv"),
    stringsAsFactors = FALSE)
  usm <- utils::read.csv(file = file.path(dirdata, "us-ms-age-data.csv"),
    stringsAsFactors = FALSE)
  uss <- utils::read.csv(file = file.path(dirdata, "us-shore-age-data.csv"),
    stringsAsFactors = FALSE)
  colnames(uss)[which(colnames(uss) == "n.trips")] <- "n.hauls"
  uscomp <- merge(merge(
    data.frame("Sector" = "U.S. at-sea CP", usc), 
    data.frame("Sector" = "U.S. at-sea MS", usm), all = TRUE), 
    data.frame("Sector" = "U.S. Shoreside", uss), all = TRUE)
  colnames(uscomp)[which(colnames(uscomp) == "n.hauls")] <- "Nsamples"
  colnames(uscomp)[which(colnames(uscomp) == "year")] <- "Year"
  comps <- merge(cancomp, uscomp, all = TRUE)
  
  catch <- utils::read.csv(file.path(dirdata, "landings-tac-history.csv"))
  
  temp <- c(
    "atSea_US_MS", "atSea_US_CP", "US_shore", 
    "CAN_JV", "CAN_Shoreside", "CAN_FreezeTrawl")
  catches <- reshape(catch[, c("Year", temp)], direction = "long", 
    v.names = "Catch", varying = temp,
    timevar = "Sector", times = temp)
  catches$Sector <- factor(catches$Sector, 
    levels = temp, 
    labels = c("U.S. at-sea MS", "U.S. at-sea CP", "U.S. Shoreside",
      "CAN JV", "CAN Shoreside", "CAN FreezerTrawl"))
  all <- merge(comps, catches, all.x = TRUE)
  cw <- apply(all[, grepl("^a", colnames(all))], 2, as.numeric) * 
    as.numeric(all$Catch)
  colnames(cw) <- gsub("^a", "cw", colnames(cw))
  sw <- apply(all[, grepl("^a", colnames(all))], 2, as.numeric) * 
    as.numeric(all$Nsamples)
  colnames(sw) <- gsub("^a", "sw", colnames(sw))

  # todo: change from hardcoding
  mnwtage <- c(0.0885, 0.2562, 0.3799, 0.4913, 0.5434, 0.5906, 0.6620,
    0.7215, 0.7910, 0.8629, 0.9315, 0.9681, 1.0751, 1.0016, 1.0202)
  nw <- apply(all[, grepl("^a", colnames(all))], 2, as.numeric) * 
    as.numeric(all$Catch) / 
    apply(apply(all[, grepl("^a", colnames(all))], 2, as.numeric), 1, 
    function(x) sum(x * mnwtage))
  colnames(nw) <- gsub("^a", "nw", colnames(nw))

  # todo: need wtatage per year so I can do nwy
  wtatage <- r4ss::SS_readwtatage(file.path(dirmod, "wtatage.ss"))
  colnames(wtatage)[colnames(wtatage) == "Yr"] <- "Year"
  colnames(wtatage)[colnames(wtatage) %in% 0:ncol(wtatage)] <- paste0("wtAtAge", 
  colnames(wtatage)[colnames(wtatage) %in% 0:ncol(wtatage)])
  all <- merge(all, wtatage[, c("Year", paste0("wtAtAge", 1:ncol(cw)))], 
    all.x = TRUE)
  all <- all[order(all$Sector, all$Year), ]
  samplesize <- tapply(all$Nsamples, all$Year, function(x) sum(as.numeric(x)))
  
  nwy <- apply(all[, grepl("^a", colnames(all))], 2, as.numeric) * 
    as.numeric(all$Catch) / sapply(1:nrow(all), 
    function(x) sum(as.numeric(all[x, grep("a\\d+", colnames(all))]) * 
      as.numeric(all[x, grep("wtAtAge\\d", colnames(all))])))
  final <- cbind(all[, c("Year", "Sector")], nwy)
  final <- reshape(final, direction = "long", 
    varying = colnames(nwy), times = colnames(nwy),
    v.names = "aa")
  final <- tapply(final$aa, list(final$Year, final$time), sum)
  final <- final[, match(gsub("cw", "a", colnames(cw)), colnames(final))]
  final <- t(apply(final, 1, function(x) 100 * x / sum(x)))
  final <- data.frame(
    "#year" = rownames(final),
    "Month" = 7, "Fleet" = 1, "Sex" = 0, "Partition" = 0,
    "AgeErr" = as.numeric(rownames(final)) - 1972,
    "LbinLo" = -1, "LbinHi" = -1,
    "nTrips" = samplesize[match(names(samplesize), rownames(final))],
    final)
  write.table(final, file.path(dirdata, "ForSS_marginalages.csv"),
    sep = ",", row.names = FALSE)
  return(final)
}

# datatocomps("c:/stockAssessment/hake-assessment/data",
#   dirmod = "c:/stockAssessment/hake-assessment/models/2019.03.00_base_model")

datatoassessmentcomp <- function(dircomp, dirassessment) {
  dirsave <- file.path(dirassessment, "data")
  aa <- utils::read.csv(file.path(dircomp, "CP.Age.Only", "comps.csv"))
  colnames(aa)[1:3] <- c("year", "n.fish", "n.hauls")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa, 
    file = file.path(dirsave, "us-cp-age-data.csv"), sep = ",",
    row.names = FALSE)
  aa <- utils::read.csv(file.path(dircomp, "MS.Age.Only", "comps.csv"))
  colnames(aa)[1:3] <- c("year", "n.fish", "n.hauls")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa, 
    file = file.path(dirsave, "us-ms-age-data.csv"), sep = ",",
    row.names = FALSE)
  aa <- utils::read.csv(file.path(dircomp, "Shoreside.Age.Only", "shoresideAgeComps.csv"))
  colnames(aa)[1:2] <- c("year", "n.trips")
  colnames(aa) <- gsub("Age", "a", colnames(aa))
  aa[is.na(aa)] <- 0
  utils::write.table(x = aa, 
    file = file.path(dirsave, "us-shore-age-data.csv"), sep = ",",
    row.names = FALSE)
}