datatocomps <- function(dirdata, dirmod, cohorts) {

  options(default.stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)

  canfile <- file.path(dirdata, "can-age-data.csv")
  if (file.exists(canfile)) {
    can <- readLines(canfile)
    cansplit <- strsplit(can, ",")
    cansplit <- lapply(cansplit, function(x){c(x[1],x[-1][!x[-1] == ""])})
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
      diff(c(1, 
        which(!diff(temp) == 1), length(temp)))), 
        do.call("rbind", cansplit[temp[-1]]))
    colnames(cancomp) <- c("Sector", "Year", paste0("a", 3:ncol(cancomp) - 2))
    can.l <- merge(cancomp, ncan, all.x = TRUE)
  } else {
    can.l <- lapply(
      dir(dirdata, "can[ada]*.*-age", full.names = TRUE),
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
  }
  can.l[, "Sector"] <- gsub("CAN ([A-Z])", "CAN_\\1", can.l$Sector)
  can.l[, "Sector"] <- gsub("FreezerTrawl", "FreezeTrawl", can.l$Sector)

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
      "CAN_JV", "CAN_Shoreside", "CAN_FreezeTrawl"))
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
  if (!file.exists(file.path(dirmod, "hake_data.ss"))) {
    warning("ss dat file doesn't exist in ", dirmod)
  }
  ssdat <- r4ss::SS_readdat(file.path(dirmod, "hake_data.ss"),
    verbose = FALSE, echoall = FALSE)
  ind <- !(ssdat$agecomp$Yr >= 2008 & ssdat$agecomp$FltSvy == 1)
  new <- setNames(final[final[, 1] >= 2008, ], colnames(ssdat$agecomp))
  ssdat$agecomp <- rbind(ssdat$agecomp[ind, ], new)
  # Increment ageing error matrix
  ssdat$N_ageerror_definitions <- ssdat$N_ageerror_definitions + 1
  ssdat$ageerror <- rbind(ssdat$ageerror, ageerror_new(cohorts))
  r4ss::SS_writedat(ssdat, file.path(dirmod, "hake_data.ss"),
    overwrite = TRUE, verbose = FALSE)
  return(final)
}
