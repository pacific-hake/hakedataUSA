#' Pull Information From NORPAC and PacFIN Databases
#' 
#' Extract catch and age data from NORPAC and PacFIN databases
#' for the hake stock assessment. 
#' 
#' @param database A vector of character values indicating 
#' which databases you want to pull information from.
#' Options include \code{c("NORPAC", "PacFIN")}, one or both
#' can be specified.
#' @param startyear An ordered list the same length as \code{database} with
#' at least one element per list entry specifying the start year for
#' each database. If only a single value per database is provided, then
#' then the number will be recycled for catches and ages. Otherwise,
#' the first entry is the start year for catches and the second is the 
#' start year for age compositions. 
#' @param endyear An integer value specifying the final year of data
#' to include in the extraction. The default will be to
#' end the data based on the current calendar year.
#' @param passwordfile A file path directing R to a file that contains
#' two lines, with the first being your NORPAC password and the
#' second being your PacFIN password without quotes.
#' If this argument is \code{NULL}, the user will be prompted
#' for their password. 
#' 
#' @export
#' @author Kelli Faye Johnson
#' @return An environment with several objects pulled from the 
#' NORPAC and PacFIN databases. Several \code{Rdat} files are
#' saved to the disk, i.e., one for each object and a summary 
#' file.
#' 
#' @examples
#' dataenv <- pulldatabase()
#' head(get("ncatch", envir = dataenv))
#' 
pulldatabase <- function(database = c("NORPAC", "PacFIN"), 
  startyear = list("NORPAC" = 2008, "PacFIN" = c(1980, 2008)),
  endyear = as.numeric(format(Sys.time(), "%Y")),
  passwordfile = "password.txt") {
  
  mydir <- hakedatawd()
  sqldir <- system.file("extdata", "sql", package = "hakedataUSA")
  if (!is.list(startyear)) {
    stop("startyear must be a list.")
  }
  if (length(startyear) != length(database)) {
    stop("At least one start year per database must be provided.")
  }
  for (ii in seq_along(startyear)) {
    if (length(startyear[[ii]]) == 1) {
      startyear[[ii]][2] <- startyear[[ii]][1]
    }
  }
  if (is.null(names(startyear))) {
    names(startyear) <- database
  } else {
    if (!all(names(startyear) == database)) {
      stop("Names of startyear do not match database entries.")
    }
  }

  summaryfile <- file.path(mydir, "extractedData", 
    paste0("summary_", format(Sys.time(), "%Y.%m.%d"), ".txt"))
  on.exit(sink(file = NULL))
  sink(summaryfile)
  cat("Summary of hake-data pull from", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  sink()
  info <- hakedatasqlpw(file = passwordfile)
  NORPAC.uid <- info[[1]][1]
  PacFIN.uid <- info[[1]][2]
  NORPAC.pw <- info[[2]][1]
  PacFIN.pw <- info[[2]][2]

  oldoptions <- options()
  on.exit(options(oldoptions), add = TRUE)
  # set this so that the full haul join number is displayed
  options(digits = 19)
  dir.create(file.path(mydir, "extractedData", "copies"), recursive = TRUE,
    showWarnings = FALSE)

  localsave <- function(data, trailingname, dir = hakedatawd()) {
    x <- deparse(substitute(data))
    assign(x, data)
    end <- paste0(trailingname, ".Rdat")
    save(list = x, 
      file = file.path(dir, "extractedData", end))
    ignore <- file.copy(from = file.path(dir, "extractedData", end),
      to = file.path(dir, "extractedData", "copies", 
        gsub("\\.", paste0("_", format(Sys.time(), "%Y.%m.%d"), "."), end)),
      overwrite = TRUE)
    if (!ignore) stop("Something went wrong copying the file to copies")
  }

  # NORPAC 
  if ("norpac" %in% tolower(database)) {
    # Catches
    ncatch <- queryDB(
      queryFilename = dir(sqldir, "NORPACdomesticCatch", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      start = startyear$NORPAC[1], end = endyear)
    localsave(ncatch, "NORPACdomesticCatch")
    # Age and weight data
    atsea.ageWt <- queryDB(
      queryFilename = dir(sqldir, "atseaAgeWeight", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      sp = "206", start = startyear$NORPAC[2], end = endyear)
    localsave(atsea.ageWt, "atsea.ageWt")
    # Age and weight data from squash table
    atsea.ages <- queryDB(
      queryFilename = dir(sqldir, "atSeaSquashTableAges", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      sp = "206", start = startyear$NORPAC[2], end = endyear)
    localsave(atsea.ages, "atsea.ages")
  }

  if ("pacfin" %in% tolower(database)) {
    # Catches
    # new method in 2017, which includes research catch and breaks out tribal catch
    # Remove XXX fleet (foreign catch?)
    pcatch <- queryDB(
      queryFilename = dir(sqldir, "comp_ft_taylor_aliased", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[1], end = endyear)
    localsave(pcatch, "Pacfincomp_ft_taylorCatch")
    # bds data
    # used to be bds.age.sql
    bds.age <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_age", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.age, "pacfin_bds_age")
    bds.allsp.cluster <- queryDB(
      queryFilename=dir(sqldir, "pacfin_bds_allsp_cluster", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.allsp.cluster, "pacfin_bds_allsp_cluster")
    bds.fish <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_fish", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear, 
      asis = c(rep(FALSE, 11), TRUE, rep(FALSE, 32)))
    localsave(bds.fish, "pacfin_bds_fish")
    bds.sp.cluster <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_sp_cluster", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.sp.cluster, "pacfin_bds_sp_cluster")
    pcatchatsea <- queryDB(
      queryFilename = dir(sqldir, "pacfin.atseabysector", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = 206, start = startyear$PacFIN[2], end = endyear)
    localsave(pcatchatsea, "pcatchatsea")
  }

  # Summaries
  # todo: Make summaries available if only one database is requested.
  # todo: Make csv or table for summary to easily compare across years
  #       instead of having to do manual comparisons.
  sink(summaryfile, append = TRUE)
  cat("look at file sizes\n")
  print(file.info(dir(file.path(mydir, "extractedData"), 
    full.names = TRUE, pattern = ".Rdat"))[, 1:2])
  cat("\n")
  sink()
  if (all(c("norpac", "pacfin") %in% tolower(database))) {
    sink(summaryfile, append = TRUE)
    cat("\nAges, otoliths, and samples (PacFIN only) by year\n",
      "where NA values are included for age\n")
    maxyears <- unique(unique(bds.age$SAMPLE_YEAR),
      unique(bds.fish$SAMPLE_YEAR),
      unique(atsea.ages$YEAR))
    maxyears <- maxyears[order(maxyears)]
    temp <- data.frame(
      "database" = c(rep("PacFIN", 3), rep("NORPAC", 2)),
      "data" = c("bds ages", "bds otoliths", "fish samples", 
        "ages", "otoliths"),
      rbind(
      table(factor(bds.age$SAMPLE_YEAR[!is.na(bds.age$AGE_YEARS)],
        levels = maxyears)),
      table(factor(bds.age$SAMPLE_YEAR, levels = maxyears)),
      table(factor(bds.fish$SAMPLE_YEAR, levels = maxyears)),
      table(factor(atsea.ages$YEAR[!is.na(atsea.ages$AGE)], 
        levels = maxyears)),
      table(factor(atsea.ages$YEAR, levels = maxyears))))
    colnames(temp) <- gsub("^X", "", colnames(temp))
    print(temp, row.names = FALSE)
    rm(temp)
    cat("\nAges by month and year.\n",
      "Information is separated by state for PacFIN.\n",
      "The number of NAs is summarized for NORPAC.\n")
    temp <- list()
    temp[[1]] <-   table(bds.age$SAMPLE_YEAR,
      factor(bds.age$SAMPLE_MONTH, levels = 1:12),
          bds.age$SAMPLE_AGID)
    temp[[2]] <- table(atsea.ages$YEAR,
      factor(as.numeric(format(as.Date(atsea.ages$HAUL_OFFLOAD_DATE),"%m")),levels = 1:12),
      is.na(atsea.ages$AGE))
    temp <- do.call("rbind", 
      lapply(temp, function(y) do.call("rbind", apply(y, 3, function(x) data.frame(x)))))
    temp <- data.frame(do.call("rbind", strsplit(rownames(temp), "\\.")), temp)
    colnames(temp)[1:2] <- c("state", "YEAR")
    colnames(temp) <- gsub("^X|X2\\.|", "", colnames(temp))
    temp$database <- ifelse(temp[, 1] %in% c("TRUE", "FALSE"), "NORPAC", "PacFIN")
    temp$NAinfo <- ifelse(temp$state == TRUE, TRUE, FALSE)
    temp$state <- ifelse(temp$state %in% c(TRUE, FALSE), "all", as.character(temp$state))
    rownames(temp) <- NULL
    print(temp, row.names = FALSE)
    rm(temp)
    sink()
  }
  if ("norpac" %in% tolower(database)) {
    sink(summaryfile, append = TRUE)
    cat("\nInvestigate unique trips from NORPAC data\n")
    cat("The following cruise numbers should be investigated:\n")
    badNORPAC <- names(which(apply(
      table(atsea.lenAge$CRUISE,atsea.lenAge$CRUISE_VESSEL_SEQ)>0, 1, sum) > 1))
    cat(paste(badNORPAC, collapse = "\n"), "\n")
    for (cruise in badNORPAC) {
      dat <- atsea.lenAge[atsea.lenAge$CRUISE == cruise, ]
      y1 <- paste(dat$CRUISE, dat$CRUISE_VESSEL_SEQ, dat$TRIP_SEQ, sep=".")
      y2 <- paste(dat$CRUISE, dat$TRIP_SEQ, sep=".")
      cat("Cruise and trip sequence on top vs. cruise",
       "cruise vessel sequence, and trip sequence on left\n")
      print(table(y1,y2))
      cat("\n\n")
    }
    yy <- atsea.ages[!is.na(atsea.ages$AGE),]
    xx <- atsea.lenAge[!is.na(atsea.lenAge$AGE),]
    xx$year <- substring(xx$RETRV_DATE_TIME,1,4)
    cat("In the NORPAC data,",
      "there are a different number of age and length NA values",
      "in the following years:\n")
    yyy <- table(yy$YEAR)
    xxx <- table(xx$year)
    if (!all(c(
      names(yyy) %in% names(xxx), 
      names(xxx) %in% names(yyy)))) {
      cat("Ages\n")
      print(table(yy$YEAR))
      cat("Lengths\n")
      print(table(xx$year))
    }
    print(merge(yyy, xxx, all = TRUE))
    sink()
  }
  if ("norpac" %in% tolower(database)) {
    sink(summaryfile, append = TRUE)
    cat("\nSummary of PacFIN catches by fleet across all years")
    cat("\nShould include 1, 2, and 3, with the latter being the smallest")
    colSums(table(
      substr(ncatch$RETRIEVAL_DATE, 1, 4),
      ncatch$VESSEL_TYPE))
    cat("\n\n")
    sink()
  }
  if ("pacfin" %in% tolower(database)) {
    sink(summaryfile, append = TRUE)
    cat("\nSummary of PacFIN catches by fleet across all years")
    cat("\nShould include LE, OA, R, TI, XX")
    colSums(table(pcatch$YEAR, pcatch$FLEET))
    cat("\n\n")
    sink()
  }
  sink(summaryfile, append = TRUE)
  cat("\n\nEND OF SUMMARY\n\n")
  sink()

  # todo: get IT to explain legacy code
  # Legacy summary code that I don't understand
  # y1 <- paste(atsea.lenAge$CRUISE, atsea.lenAge$TRIP_SEQ, sep=".")
  # y2 <- paste(atsea.lenAge$CRUISE, atsea.lenAge$CRUISE_VESSEL_SEQ, atsea.lenAge$TRIP_SEQ, sep=".")
  # x <- table(y1,y2)>0
  # table(apply(x,1,sum))
  # y1 <- paste(atsea.lenAge$CRUISE, sep=".")
  # y2 <- paste(atsea.lenAge$CRUISE, atsea.lenAge$TRIP_SEQ, sep=".")
  # x <- table(y1,y2)>0
  # table(apply(x,1,sum))
  # y1 <- paste(atsea.lenAge$TRIP_SEQ, sep=".")
  # y2 <- paste(atsea.lenAge$CRUISE, atsea.lenAge$TRIP_SEQ, sep=".")
  # x <- table(y1,y2)>0
  # table(apply(x,1,sum))
  # zz <- merge(xx,yy,by=c("SPECIMEN_NUMBER","BARCODE"),all=T)
  # sum(duplicated(xx$SPECIMEN_NUMBER))
  # sum(duplicated(yy$SPECIMEN_NUMBER))
  # sum(duplicated(zz$SPECIMEN_NUMBER))

  # sum(duplicated(paste(xx$year,xx$SPECIMEN_NUMBER)))
  # head(xx[duplicated(xx$SPECIMEN_NUMBER),])
  # xx[xx$SPECIMEN_NUMBER==632706,]
  # xx[xx$SPECIMEN_NUMBER==281,]
  # yy[yy$SPECIMEN_NUMBER==281,]

  # zz14 <- zz[zz$YEAR==2014,]
  # sum(duplicated(paste(zz14$SPECIMEN_NUMBER,zz14$BARCODE)))

  # zzz <- zz[!duplicated(paste(zz$SPECIMEN_NUMBER,zz$BARCODE)),]
  # table(zzz$YEAR)
  # table(xx$year)
  # table(yy$YEAR)

  # xxx <- xx[!duplicated(paste(xx$SPECIMEN_NUMBER,xx$BARCODE)),]
  e1 <- new.env()
  if ("norpac" %in% tolower(database)) {
    assign("atsea.ages", atsea.ages, envir = e1)
    assign("atsea.ageWt", atsea.ageWt, envir = e1)
    assign("atsea.lenAge", atsea.lenAge, envir = e1)
    assign("ncatch", ncatch, envir = e1)
  }
  if ("pacfin" %in% tolower(database)) {
    assign("bds.age", bds.age, envir = e1)
    assign("bds.allsp.cluster", bds.allsp.cluster, envir = e1)
    assign("bds.fish", bds.fish, envir = e1)
    assign("bds.sp.cluster", bds.sp.cluster, envir = e1)
    assign("pcatch", pcatch, envir = e1)
  }
  invisible(e1)
}
