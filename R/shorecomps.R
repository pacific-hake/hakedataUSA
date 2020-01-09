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
shorecomps <- function(
  page = NULL,
  ages = 1:15,
  verbose = FALSE) {
  
  mydir <- hakedatawd()
  dir.create(file.path(mydir, "Catches", "Comps", "Shoreside.Age.Only"),
    recursive = TRUE, showWarnings = FALSE)
  if (is.null(page)) base::load(file.path(mydir, "extractedData", "page.Rdat"))

  bds.fish.worked <- page[!is.na(page$AGE_YEARS), ]
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
  # Relationship assumes FISH_WEIGHT is in grams and FISH_LENGTH is cm
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
  utils::write.csv(afs,
    file = file.path(mydir, "Catches", "Comps", "Shoreside.Age.Only",
      "shoresideAgeComps.csv"), row.names = FALSE)
  utils::write.csv(row.names = FALSE,
    file = file.path(mydir, "Catches", "Comps", "shoreside_AGID_Age.csv"),
    reshape(aggregate(FREQ ~ SOURCE_AGID + FISH_AGE_YEARS_FINAL+ SAMPLE_YEAR ,
      data = bds.fish.worked, length),
      direction = "wide", timevar = "SOURCE_AGID",
      idvar = c("FISH_AGE_YEARS_FINAL", "SAMPLE_YEAR")))
  utils::write.csv(row.names = FALSE,
    file = file.path(mydir, "Catches", "Comps", "shoreside_AGID_Grid.csv"),
    reshape(aggregate(FREQ ~ GRID + FISH_AGE_YEARS_FINAL+ SAMPLE_YEAR ,
      data = bds.fish.worked, length),
      direction = "wide", timevar = "GRID",
      idvar = c("FISH_AGE_YEARS_FINAL", "SAMPLE_YEAR")))
  utils::write.csv(row.names = FALSE,
    file = file.path(mydir, "Catches", "Comps", "shoreside_PCID.csv"),
    aggregate(totalWt ~ PCID + SAMPLE_YEAR, data = dat, FUN = sum))

  return(afs)
}
