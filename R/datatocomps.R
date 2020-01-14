datatocomps <- function(dirdata, dirmod) {

  options(default.stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)

  can.l <- lapply(
    dir(dirdata, "canada.*-age", full.names = TRUE),
    function(x) {
      out <- read.csv(x, header = TRUE, check.names = FALSE)
      out$Sector <- gsub(".*canada-([-a-z]*)-age.*", "\\1", x)
      out$Sector[out$Sector == "freezer-trawler"] <- "CAN_FreezeTrawl"
      out$Sector[out$Sector == "joint-venture"] <- "CAN_JV"
      out$Sector[out$Sector == "shoreside"] <- "CAN_Shoreside"
      colnames(out) <- gsub("^([1-9]{1})", "a\\1", colnames(out))
      colnames(out) <- gsub("^N$", "Nsamples", colnames(out))
      colnames(out)[1] <- "Year"
      out <- out[, c("Sector", "Year", 
        grep("a[0-9]", colnames(out), value = TRUE), "Nsamples")]
      return(out)
    })
  can.l <- do.call(rbind, can.l)
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
  comps <- merge(can.l, uscomp, all = TRUE)
  
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

  wtatage <- r4ss::SS_readwtatage(file = file.path(dirmod, "wtatage.ss"),
    verbose = FALSE)
  colnames(wtatage) <- gsub("^([0-9])", "wtAtAge\\1", colnames(wtatage))
  colnames(wtatage) <- gsub("^Yr", "Year", colnames(wtatage))
  all <- merge(all,
    wtatage[wtatage$Fleet == 1, c("Year", paste0("wtAtAge", 1:ncol(cw)))],
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

datatoassessment_comp <- function(dirassessment) {
  dircomp = file.path(hakedatawd(), "Catches", "Comps")
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
