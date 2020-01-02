#' Workup Shoreside Compsition Data
#' 
#' Provide composition data from the U.S. Shoreside
#' hake fishery. Written by Allan Hicks in 2016 and
#' revised in 2017 by Ian Taylor.
#' 
#' @template atsea.ages
#' @template bds.fish
#' @template bds.sp.cluster
#' @template bds.allsp.cluster
#' 
#' @export
#' @author Kelli Faye Johnson
#' @return todo: document the return
#' 
#' @examples
#' shorecomps(verbose = TRUE)
#' 
shorecomps <- function(bds.age = NULL, bds.fish = NULL,
  bds.sp.cluster = NULL, bds.allsp.cluster = NULL, 
  ages = 1:15, verbose = FALSE) {
  
  mydir <- hakedatawd()
  if (is.null(bds.age)) base::load(file.path(mydir, "extractedData", 
    "pacfin_bds_age.Rdat"))
  if (is.null(bds.allsp.cluster)) base::load(file.path(mydir, "extractedData", 
    "pacfin_bds_allsp_cluster.Rdat"))
  if (is.null(bds.fish)) base::load(file.path(mydir, "extractedData", 
    "pacfin_bds_fish.Rdat"))
  if (is.null(bds.sp.cluster)) base::load(file.path(mydir, "extractedData", 
    "pacfin_bds_sp_cluster.Rdat"))
  mydir <- hakedatawd()
  summaryfile <- file.path(mydir, "Catches", "Comps", "shoreagesSummary.txt")
  sink(file = summaryfile)
  on.exit(suppressWarnings(sink()), add = TRUE)
  cat("Summary of hake-data shoreside", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  sink()

  bds.fish.worked <- workupPacFinTablesBDS(bds_fish=bds.fish,
    age_temp = bds.age, sp_cluster = bds.sp.cluster, 
    all_cluster = bds.allsp.cluster)

  bad <- sum(is.na(bds.fish.worked$FISH_AGE_YEARS_FINAL))
  if (bad > 0) {
    if (verbose) warning("There are ", bad, 
      " NA ages in bds.fish.worked",
      "that were removed.")
    bds.fish.worked <- bds.fish.worked[
      !is.na(bds.fish.worked$FISH_AGE_YEARS_FINAL), ]
  }

  sink(summaryfile, append = TRUE)
  cat("\nThere were ", bad, "NA ages.\n")
  cat("\nSummary of Year.\n")
  print(table(bds.fish.worked$SAMPLE_YEAR))
  cat("\nSummary of Ages by Agency by Year:\n")
  print(table(
    bds.fish.worked$FISH_AGE_YEARS_FINAL, 
    bds.fish.worked$SAMPLE_AGENCY,
    bds.fish.worked$SAMPLE_YEAR
    ))
  cat("\nSummary of Grid by Month by Year:\n")
  print(table(
    bds.fish.worked$GRID, 
    bds.fish.worked$SAMPLE_MONTH,
    bds.fish.worked$SAMPLE_YEAR
    ))
  sink()

  bds.fish.worked$SEX <- factor(bds.fish.worked$SEX)
  dat <- SetUpHakeBDS.fn(bds.fish.worked, verbose = verbose,
    max.mmLength = 1000, dataTypes = c("C"),
    sampleMethods = c("R"), sampleTypes = c(NA, "", "C", "M"),
    states = c("CA", "OR", "WA", "PW"))

  nFishbygear <- table(dat$SAMPLE_YEAR,dat$gear)
  nSamp <- apply(
    table(dat$SAMPLE_YEAR,dat$SAMPLE_NO),
    1,
    function(x){sum(x>0)})
  nFish <- table(dat$SAMPLE_MONTH,dat$SAMPLE_YEAR)

  dat$state <- "PW"  #so that it doesn't need to expand up states and won't need a catch file
  out.lm <- lm(log(FISH_WEIGHT)~log(FISH_LENGTH/10), data = dat)
  # Must keep the next line as is, don't try to make it shorter!
  lw <- data.frame(OR=c(exp(out.lm$coefficients[1]),out.lm$coefficients[2]))

  LFs <- commLFs.fn(dat,lw,
    gear = NULL, state = "PW",
    catchFile = NULL,
    maxExpansion = 1e9, verbose = TRUE,
    loessSpan = 0.3, ageComp = TRUE)
  files <- sprintf(file.path(
    mydir, "Catches", "Comps", "Shoreside.Age.Only", "shorecommLFs_%d.pdf"),
    dev.list())
  for (ii in seq_along(files)) {
    suppressWarnings(grDevices::dev.print(pdf, files[ii]))
    dev.off()
  }

  tmp <- LFs$all$PW
  afs <- matrix(NA,
    ncol = length(ages) + 2, nrow = length(tmp),
    dimnames = list(NULL, c("Year", "nTrips", paste0("a", ages))))
  for(i in 1:length(tmp)) {
    tmp2 <- tmp[[i]]
    afs[i, "Year"] <- tmp2$year[1]
    afs[i, paste0("a", min(ages))] <- sum(tmp2[tmp2$age<=min(ages), "lf"])
    afs[i, paste0("a", max(ages))] <- sum(tmp2[tmp2$age>=max(ages), "lf"])
    for(j in (min(ages) + 1):(max(ages) - 1)) {
      if(sum(tmp2$age == j)) {
        afs[i, paste0("a", j)] <- tmp2[tmp2$age == j, "lf"]
      } else {
        afs[i, paste0("a", j)] <- 0
      }
    }
  }

  afs[, "nTrips"] <- nSamp[as.character(afs[, "Year"])]
  dir.create(file.path(mydir, "Catches",
    "Comps", "Shoreside.Age.Only"), recursive = TRUE, showWarnings = FALSE)
  write.csv(afs,
    file = file.path(mydir, "Catches", "Comps", "Shoreside.Age.Only",
      "shoresideAgeComps.csv"), row.names = FALSE)

  sink(summaryfile, append = TRUE)
  cat("\n\nPort Code IDs (PCID) by year:\n")
  pcidsum <- aggregate(totalWt ~ PCID + SAMPLE_YEAR, 
    data = dat, FUN = sum)
  print(pcidsum)
  sink()
  if (verbose) {
    testthat::expect_equal(
      pcidsum[pcidsum$SAMPLE_YEAR == 2017, "totalWt"],
      c(241587234, 25904000, 104190250, 123699900))
  }
  return(afs)
}
