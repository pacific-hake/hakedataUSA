############################################################################
# atseaComp.yr.r
#
# Expand and summarize the size- or age-compositions for the hake at-sea fishery.
#
# first written by Andi Stephens, August 2010
# updated by Allan Hicks, December 2015
#### Fixed a few things and use new extraction from database after it changed
#### Assumes extraction is from SQUASH table from NORPAC database
#
# Filters the atsea bilogical data,
# expands it by tow,
# then summarizes the composition as specied
#
# This is a wrapper function that calls other functions
#
# Input:
#    a dataframe from an extraction from the NORPAC database SQUASH table
#    Specific columns must be provided:
###      CRUISE, HAUL_OFFLOAD, LENGTH, AGE, WEIGHT, SEX, YEAR
###      FREQUENCY if doing length comps
###   Assumes that only one species is present in both dat and ncatch
#
# Arguments:
#    BY_AGE        Logical.  Do age- rather than the default length-comps.
#    BY_MONTH      Logical.  Do monthly summaries.
#    BY_GENDER     Logical.  Summarize by gender
#    NO_LENGTH     Logical.  Summarize by age-only.
#    lbin.sizes    Vector, sorted smallest to largest.  Not required;
#                  default is 2-cm bins.
#    in.season     List whose elements describe the months to include
#                  in a SEASON for seasonal summaries.  Not required.
#                  Number and size of seasons is arbitrary.  See example
#                  in DEBUG section below.
#    in.pctl       Threshhold quantile for expansion factors.  Default:  0.95.
#    min_Haul      Numeric.  Minimum haul weight.
#    min_T_weight  Numeric.  Minimum total weight.
#    minSampleSize Numeric.  Minimum fish in a sample/haul.
#    remove_sparse Logical.  Remove samples without the minimum # fish. If they are not removed, the expansion factor is set to the median expansion factor
#    which         Output mode.  "pct", "num", or "both".  Default:  "pct".
#    in.filename   Data file.
#    out.filename  Results file.
#    rpt.filename  Report file.
#' @import stats
atseaComp.yr = function(dat,ncatch,BY_AGE=TRUE, BY_MONTH=FALSE, BY_GENDER=FALSE,
                       lbin.sizes=NULL, minAge=1, maxAge=15,
                       vesselType=NULL,
                       in.season=NULL, in.pctl=0.95,
                       which="pct", min_Haul=0, min_T_weight=0,
                       minSampleSize=1, remove_sparse=FALSE, NO_LENGTH=FALSE,
                       report.filename, DEBUG=FALSE) 
{

  #options(stringsAsFactors = FALSE) #in case there are issues with strings converted to factors (moreso if reading in csv)

  if (DEBUG) {
    cat("DEBUG is ON\n"); flush.console();
  } # End if DEBUG

  ##############################################################################
  # Perform preliminary processing.
  # Keep only those columns used in the analysis.
  # Remove bad records and record statistics.
  ##############################################################################

  report <- NULL # a list to hold the statistics to write in the report file
  if (length(unique(dat$YEAR)) > 1 ) {
    stop("\n\n *** atseaComp.yr should be called with a single year.\n\n")
  } # End if
  report$Year <- dat$YEAR[1]

  if (!is.null(lbin.sizes) ) { 
    Lbins = as.integer(lbin.sizes)
  } # End if

  BY_SEASON = (! is.null(in.season))
  if (BY_SEASON && BY_MONTH) {
    stop("Choose either monthly or seasonal summaries, not both!\n")
  } # End if

  TEMPORAL = FALSE
  if (BY_SEASON || BY_MONTH) {
    TEMPORAL = TRUE
  } # End if

  #Create some variables
  dat$Month <- as.numeric(substring(dat$HAUL_OFFLOAD_DATE,6,7))
  # Create trip, haul identifiers; use HAUL_JOIN b/c CRUISE is not unique to a
  # single trip but rather an observer deployment
  dat$HaulID <- paste(dat$HAUL_JOIN, dat$HAUL_OFFLOAD, sep = ".")  #can match with the ncatch file
  ncatch$HaulID <- paste(ncatch[, grep("HAUL_JOIN\\)", colnames(ncatch))], ncatch$HAUL, sep = ".") 
  if(sum(table(ncatch$HaulID)>1) > 1) {
    stop("The hauls in ncatch are not unique.\n",
      "Are you sure there is only one species in ", dat[1,"YEAR"], "?\n")
  }
  if (DEBUG) {print("created trip and haul identifiers")}

  if (BY_AGE) {
    report$Age_comps_generated <- TRUE
  } else {
    report$Length_comps_generated <- TRUE
  } # End if-else

  if (BY_GENDER) { report$Conditioned_on_SEX <- TRUE }
  if (BY_MONTH)  { report$Conditioned_on_SAMPLE_MONTH <- TRUE }
  if (BY_SEASON) {
    report$Conditioned_on_SEASON <- TRUE
    report$Seasons_were <- TRUE
    for ( i in 1:length(in.season) ) {
      report$Season_Months <- paste(i, in.season[[i]], ": ")
    } # End for
  } # End if

  # Collect initial summary data
  report$Original.N.records = nrow(dat)
  report$Original.sum.freq = sum(dat$FREQUENCY)
  report$pre_length.summary = summary(dat$LENGTH)
  report$pre_wgt.summary = summary(dat$WEIGHT)
  report$pre_age.summary = summary(dat$AGE)

  #Set up temporal
  if (TEMPORAL) {
    # Convert Month to SEASON
    if (BY_SEASON) {
      dat$SEASON = as.character(dat$Month)
      for ( i in 1:length(in.season) ) {
        dat$SEASON[dat$SEASON %in% in.season[[i]]] = paste("Season.", i, sep="")
      } # End for
    } else {
      dat$SEASON = as.numeric(dat$Month)
    } # End if-else
  } # End if

  # Flag LENGTH as NA for records we do not want to use.
  # Data without length is already NA
 	No.length = sum(is.na(dat$LENGTH)) #No length observation

  # Data without gender
  if (BY_GENDER) {
    dat$SEX[dat$SEX == 0 | dat$SEX == "U"] <- NA
    No.gender <- sum(is.na(dat$SEX))
    dat$LENGTH[is.na(dat$SEX)] <- NA
  } else {
    #set it all to "U" since we do not tabulate by sex
    dat$SEX <- "U"
  } # End if

  # Data without Date
  if (TEMPORAL) {
    dat$Month[dat$Month==""] = NA
    No.month = sum(is.na(dat$Month))
    dat$LENGTH[is.na(dat$Month)] <- NA
  } # End if TEMPORAL

  # Unaged fish
  if (BY_AGE) {
    dat$AGE[dat$AGE < 0] <- NA
    No.age = sum(is.na(dat$AGE))
    dat$LENGTH[is.na(dat$AGE)] <- NA
  } else {
    cat("WARNING: This function is written for creating age comps from the SQUASH table in the NORPAC database.\n")
    cat("This output does not contain the frequency of length observations.\n")
    cat("Therefore, this function needs to be checked that it works with FREQUENCY correctly, and\n")
    cat("Make sure that your extraction is correct.\n")
  }# End if BY_AGE

  # Now remove the flagged data.
  dat = dat[!is.na(dat$LENGTH), ]
   

  #########################################################################
  # Find too-sparse samples from a haul
  sampPerHaul <- table(dat$HaulID)
  sampPerHaul[sampPerHaul<minSampleSize] <- NA

  xx <- table(!is.na(sampPerHaul))
  propHaulsSmallSamp <- xx["FALSE"]/sum(xx)  #proportion of hauls to be removed
  if(is.na(propHaulsSmallSamp)) {propHaulsSmallSamp <- 0}

  if(propHaulsSmallSamp > 0) {
  	smallHauls <- names(sampPerHaul)[is.na(sampPerHaul)]
  	tmp <- nrow(dat)
    #remove rows with small samples
  	dat <- dat[!(dat$HaulID %in% smallHauls),]
  	numMinSampSize <- tmp-nrow(dat)  
  }

  ##############################################################################
  # Create extra rows representing multiple fish for LENGTH comps only (FREQUENCY)
  #Each row will be a single fish
  #Length comp only
  #NEEDS TO BE TESTED AND HAVE THE PROPER EXTRACTION
  #without the prope extraction it will summarize lengths for the age observations (SQUASH TABLE)
  if(!BY_AGE) {
    dat <- dat[rep(1:nrow(dat), dat$FREQUENCY),]
    dat$FREQUENCY <- 1
    N.Lengthed.fish = sum(dat$FREQUENCY)

    # Create Use_Length:  the length-bins used in the assessment.
    # If no length bins were read in, just use the rounded length
    # in 2-cm bins
    if ( is.null(lbin.sizes) ) {
      Use_Length = dat$LENGTH
      Use_Length[Use_Length <= 20] = 20
      Use_Length[Use_Length >= 70] = 70

      is.odd = Use_Length %% 2 > 0
      Use_Length[is.odd] = Use_Length[is.odd] - 1

      # Make sure all lengths show up in the table, even if not in the data
      report$post_length.summary = summary(Use_Length)

      # Needs to be factored in order to have unused levels show up in the tables.
      dat$Use_Length = factor(Use_Length, levels=seq(20,70,2))
    } else {
      Use_Length = rep(0, length(dat$LENGTH))
      # Iteratively replace zeros with next Lbin
      # Assumes Lbins are sorted least-greatest
      for (i in 2:length(Lbins)) {
        minsize = Lbins[i]  
        lesser = Lbins[i-1]
        Use_Length[Use_Length == 0 & LENGTH_SIZE < minsize] = lesser
      } # End for

      # Finish up largest bin
      Use_Length[Use_Length == 0] = Lbins[length(Lbins)]

      report$post_length.summary = summary(Use_Length * 10)

      # Make sure all lengths show up in the table, even if not
      # in the data
      dat$Use_Length = factor(Use_Length, levels=Lbins)
    } # End if
  }

  ##############################################################################
  # Calculate the total haul weight for hake only
  #use the catch file extracted from NORPAC (ncatch)
  #link ncatch hauls to dat hauls
  #EXTRAPOLATED_WEIGHT is the weight of hake in the haul

  dat <- merge(dat,ncatch[,c("HaulID","EXTRAPOLATED_WEIGHT","VESSEL_TYPE")],by="HaulID",all.x=T)

  #subset by vessel_type if desired
  if(!is.null(vesselType)) {
    report$vesselKept <- vesselType
    report$vesselObs <- table(dat$VESSEL_TYPE)
    dat <- dat[dat$VESSEL_TYPE %in% vesselType, ]
  }
  if (NROW(dat) == 0) return(NULL)
  #calculate sample weight by summing weights
  #first, fill in missing weights with median of weight-at-length or -age 
  ### do this by month first, if still missing weights, do it by year.

  if(BY_AGE) {
    colnm <- "AGE"
  } else {
    colnm <- "LENGTH"
  }

  #by Month
  avgWtAt <- aggregate(dat$WEIGHT,list(dat[,colnm],dat$Month),mean,na.rm=T)
  names(avgWtAt) <- c(colnm,"Month","avgWtMn")
  dat <- merge(dat,avgWtAt,by=c(colnm,"Month"),all.x=T)
  dat$WEIGHT[is.na(dat$WEIGHT)] <- dat$avgWtMn[is.na(dat$WEIGHT)]
  #by Year to fill in remaining
  avgWtAt <- aggregate(dat$WEIGHT,list(dat[,colnm]),mean,na.rm=T)
  names(avgWtAt) <- c(colnm,"avgWtYr")
  dat <- merge(dat,avgWtAt,by=c(colnm),all.x=T)
  dat$WEIGHT[is.na(dat$WEIGHT)] <- dat$avgWtYr[is.na(dat$WEIGHT)]
  #predict (simple) to fill in any other remaining
  avWt.lm <- lm(avgWtYr ~ AGE,data=avgWtAt)
  predAges <- 0:max(dat[,colnm])   
  avWtPred <- predict(avWt.lm,newdata=data.frame(AGE=predAges))
  names(avWtPred) <- predAges
  tmp <- avWtPred[as.character(dat$AGE)]

  dat$WEIGHT[is.na(dat$WEIGHT)] <- tmp[is.na(dat$WEIGHT)]

  sampWt <- aggregate(dat$WEIGHT,list(dat$HaulID),sum)
  names(sampWt) <- c("HaulID","sampWt")
  dat <- merge(dat,sampWt,by="HaulID",all.x=T)

  #Expansion factor 
  #Fill in missing values with median
  dat$expFactor <- dat$EXTRAPOLATED_WEIGHT/dat$sampWt
  pre_exp.factor.summary = summary(dat$expFactor)
  pctl = quantile(dat$expFactor, in.pctl, na.rm=T)

#return(pre_exp.factor.summary)

  medExpFact <- median(dat$expFactor,na.rm=T)
  dat$expFactor[is.na(dat$expFactor)] <- medExpFact

  #curtail large expansion factors by truncating at an upper percentile
  dat$expFactor[dat$expFactor > pctl] <- pctl

  # fix expasion factor at median for sparse samples
  if (!remove_sparse & propHaulsSmallSamp > 0) {
    dat$expFactor[dat$HaulID %in% smallHauls] <- medExpFact
  }

  report$post_exp.factor.summary = summary(dat$expFactor)



  if (BY_AGE) {
    #Create minus and plus groups for ages
    dat$AGE[dat$AGE < minAge] <- minAge
    dat$AGE[dat$AGE > maxAge] <- maxAge
    report$post_age.summary = summary(dat$AGE)

    # Make sure all ages show up in the table, as zeros if not represented in the data
    dat$AGE <- factor(dat$AGE, levels=minAge:maxAge)

    dat$AGE = addNA(dat$AGE, ifany=T)
  } # End if


  #############################################################################
  # Output comp data

  if (BY_AGE) { colnms <- c("AGE")}
  if (!BY_AGE) { colnms <- c("Use_Length")}
  colnms <- c(colnms,"SEX")
  ind <- 2   #for the apply to create proportions at age
  if (TEMPORAL) { colnms <- c(colnms,"SEASON"); ind <- c(ind,3) }
  comps <- tapply(dat$expFactor,dat[,colnms],sum)

  nFish <- nrow(dat)
  nHauls <- length(unique(dat$HaulID))

  out <- list(nHauls,nFish,comps)
  names(out) <- c("nHauls","nFish","comps")

  #write out report
  sink(file = report.filename)
  #cat("")
  for(i in 1:length(report)) {
    cat(names(report)[i],sep="\n")
    print(report[[i]])
    cat("\n")
  }
  cat("nHauls:",nHauls,"\n",sep=" ")
  cat("nFish:",nFish,"\n\n",sep=" ")
  cat("Comps as percentage:\n\n")
  print(round(apply(comps,ind,function(x){x/sum(x,na.rm=T)}),6))
  sink()

  return(out)
}


















if(F) {
writeReport <- function(report,outFile) {

  cat("\n\nYear:", YEAR[1], "\n")
  cat("\nOriginal dataset:", Original.N.records, "  records\n")
  cat("\nSum of FREQUENCY:", Original.sum.freq, "  records\n\n")

  cat("Run conditions:\n\n")

  if (BY_AGE) {

    cat("\tAge comps generated\n")
 
  } else {

    cat("\tLength comps generated\n")

  } # End if-else


  if (BY_GENDER) { cat("\tConditioned on SEX\n") }
  if (BY_MONTH)  { cat("\tConditioned on SAMPLE_MONTH\n") }
  if (BY_SEASON) {

    cat("\tConditioned on SEASON\n")
    cat("\n\tSeasons were:\n")

    for ( i in 1:length(in.season) ) {

      cat("\t\t", "Season", i, " Months", in.season[[i]], "\n")

    } # End for

  } # End if

  if (min_sample > 0) {

    cat("\nMinimum sample size:", min_sample, "\n")
    if (nrow(Too_few_fish) > 0) {

      cat("Smaller samples:\n")
      print(Too_few_fish)

    } else {

      cat("No samples were smaller than the minimum\n")

    } # End if-else

    cat("\n\n")

  } # End if

  if (min_Haul > 0) {

    cat("\nMinimum haul weight:", min_Haul, "\n")
    cat("Replaced  ", Replaced.with.min.haul_weight,
        "  haul weights with minimum\n")

  } # End if

  if (min_T_weight > 0) {

    cat("\nMinimum total weight:", min_T_weight, "\n")
    cat("Replaced  ", Replaced.with.min_T_weight,
        "  total weights with minimum\n")

  } # End if


  cat("\nPre-processing lengths (in mm):\n")
  print(pre_length.summary)

  cat("\nPost-processing lengths (in mm):\n")
  print(post_length.summary)

  cat("\nPre-processing observer estimates (in kg):\n")
  print(pre_obs_est.summary)

  cat("\nPost-processing observer estimates (in kg):\n")
  print(summary(OBSVR_EST_CATCH))

  cat("\nPre-processing vessel estimates (in kg):\n")
  print(pre_ves_est.summary)

  cat("\nPost-processing vessel estimates (in mtonnes):\n")
  print(summary(VESSEL_EST_CATCH))

  cat("\nPre-processing total weights (in kg):\n")
  print(pre_total.wgt.summary)

  cat("\nPost-processing total Effective weights (in mtonnes):\n")
  print(post_total.wgt.summary)

  if (BY_AGE) {

    cat("\nPre-truncation expansion factor summary:\n")
    print(pre_age_exp.factor.summary)

    cat("\nPost-truncation expansion factor summary:\n")
    print(post_age_exp.factor.summary)

    cat("\nPre-processing ages:\n")
    print(pre_age.summary)

    cat("\nPost-processing ages:\n")
    print(post_age.summary)

    cat("\nNumber of hauls with aged fish:  ", n.hauls.with.age, "\n")

    cat("\nAged sample weight\n")
    print(summary(aged_sample_weight))

  } else {

    cat("\nPre-truncation expansion factor summary:\n")
    print(pre_Len_exp.factor.summary)

    cat("\nPost-truncation expansion factor summary:\n")
    print(post_Len_exp.factor.summary)

    cat("Number of lengthed fish:  ", N.Lengthed.fish, "\n")

  } # End if-else

  cat("\nNumber removed:\n")

  if (BY_AGE) {

    cat("\nRemoved  ", No.age, "  records without AGE.\n")

  } # End if

  cat("Removed  ", No.length, 
                       "  records without recorded LENGTH_SIZE.\n",
                        append=T)

  if (BY_GENDER) { 

    cat("Removed  ", No.gender, "  records without SEX_CODE.\n")

  } # End if

  if (TEMPORAL) {

    cat("Removed  ", No.month, "  records without SAMPLE_MONTH.\n")

  } # End if

  if (remove_sparse) {

    cat("Removed  ", Removed.sparse, "  samples without", min_sample, "fish\n")

  } # End if

  cat("\nWeights replaced in remaining data:\n")

  cat("Replaced  ", Replaced.vessel.catch, "  observer estimates with vessel", 
                       "estimates.\n")
  cat("Replaced  ", Replaced.trip.median, "  individual weights with median",
                       "by-trip individual weight\n")
  cat("Replaced  ", got_fixed, "  individual weights with median",
                       "by-trip individual weight from preceeding or following day(s).\n")
  cat("Replaced  ", Replaced.with.Daily.miw, "  individual weights with median",
                       "daily weights\n")
  cat("Replaced  ", Replaced.with.Annual.miw, "  individual weights with median",
                       "annual weights\n")

  cat("\n\nDifference between the sum of species weight and SPECIES_WEIGHT:", sp_wt_diff[1], "\n")

  if (!remove_sparse) {

    if (BY_AGE) {


      cat("\nReplaced  ", Replaced.age.exp.median, "  Age expansion factors with median",
                        "in sparse samples\n")


    } else {

      cat("\nReplaced  ", Replaced.Len.exp.median, "  Length expansion factors with median",
                        "in sparse samples\n")

    } # End if-else BY_AGE

  } # End if sparse

  cat("\nRecords removed because expansion factors couldn't be generated\n")
                       
  cat("Removed  ", No.MEAN_Eff_sample_weight, "  records with no MEAN_Eff_sample_weight\n")
  cat("Removed  ", No.MEAN_Extrap_Indiv_Wt, "  records with no MEAN_Extrap_Indiv_Wt\n")
  cat("Removed  ", No.FREQUENCY, "  records with no FREQUENCY\n")

  cat("\nRecords used in final analysis:", N.lengthed.fish, "\n")


}
}

if(F) {





  ##############################################################################
  # ORIGINAL COMMENTS
  # First get per-haul expansion factor by weight.  This gives us an
  # estimate of the weight of the target species per haul.  This is
  # the expansion factor used for age composition.  The length-composition
  # expansion factor takes both this and the median individual weights
  # into account.
  # All WEIGHTS == 0 are Replaced with NA, to avoid skewing medians.
  #
  #     ORIGINAL CODE
  ### atsea.data$WEIGHT[WEIGHT == 0] = NA
  ### tmp.nlens = aggregate(FREQUENCY, list(Haul.ID), sum, na.rm=T)
  ### names(tmp.nlens) = c("Haul.ID", "N_lens")
  ### N_lens = find.matching.rows(atsea.data, tmp.nlens, "Haul.ID", "Haul.ID", "N_lens")
  ### tmp_species_weight = aggregate(SPECIES_WEIGHT, list(Haul.ID), sum, na.rm=T)
  ### tmp_total_sample_weight= aggregate(TOTAL_SAMPLE_WEIGHT, list(Haul.ID), sum, na.rm=T)
  ### names(tmp_species_weight) = c("Haul.ID", "sum_sp_weight")
  ### names(tmp_total_sample_weight) = c("Haul.ID", "sum_tot_weight")
  ### # Replicate per-haul values per-row.
  ### sum_byhaul_species_weight = find.matching.rows(atsea.data, tmp_species_weight,
  ###                                                "Haul.ID", "Haul.ID", "sum_sp_weight")[[1]]
  ### sum_byhaul_total_sample_weight = find.matching.rows(atsea.data, tmp_total_sample_weight,
  ###                                               "Haul.ID", "Haul.ID", "sum_tot_weight")[[1]]
  ### # Haul weight is either OBSVR_EST_CATCH or VESSEL_EST_CATCH*1000
  ### # OBSVR_EST_CATCH is in mtonnes; VESSEL_EST_CATCH is in kg.
  ### haul_weight = OBSVR_EST_CATCH
  ### tmp.replace = replace.zeros(haul_weight, VESSEL_EST_CATCH * 1000)
  ### haul_weight = tmp.replace[[1]]
  ### Replaced.vessel.catch = tmp.replace[[2]]
  ### # Create per-haul expansion factor
  ### tmp.sum_total = aggregate(TOTAL_SAMPLE_WEIGHT, list(Haul.ID), sum, na.rm=T)
  ### names(tmp.sum_total) = c("Haul.ID", "sum_total_sample_weight")
  ### sum_total_sample_weight = find.matching.rows(atsea.data, tmp.sum_total, "Haul.ID", "Haul.ID",
  ###                                              "sum_total_sample_weight")[[1]]
  ### tmp.sum_spp = aggregate(SPECIES_WEIGHT, list(Haul.ID), sum, na.rm=T)
  ### names(tmp.sum_total) = c("Haul.ID", "sum_species_weight")
  ### sum_species_weight = find.matching.rows(atsea.data, tmp.sum_total, "Haul.ID", "Haul.ID",
  ###                                         "sum_species_weight")[[1]]
  ### haul_exp_factor = haul_weight/sum_total_sample_weight;
  ### species_est_haul_weight = sum_species_weight*haul_exp_factor;
  #

  # NEW 2015
  #  Original expansion factor used "TOTAL_SAMPLE_WEIGHT" in denominator, which is the weight of fish from which the length or age subsample was taken.
  #  I feel that the expansion should expand from the weight actually measured, so I use "SPECIES_WEIGHT"/"HaulWt".
  #     SAMPLE_WEIGHT is an intermediary
  #  
  #  I also think that there was an error in the original version because the species and total sample weights were summed over rows,
  #     but each row represents the entire haul.  

  #  TOTAL_SAMPLE_WEIGHT is the weight of the species from which the sample was taken (typically about half the total haul weight, by design)
  #  SPECIES_WEIGHT seems to be the weight of the species actually measured, but there are some errors where it is close to TOTAL_SAMPLE_WEIGHT
  #  OBSVR_EST_CATCH is the weight of the haul (kg)
  #  VESSEL_EST_CATCH is weight of the haul from logbook (mt)
  ##############################################################################
  # Data without expansions are assigned a median











  tmp.nlens = aggregate(dat$FREQUENCY, by=list(dat$HaulID), sum, na.rm=T)
  names(tmp.nlens) = c("HaulID", "nLensHaul")
  dat = merge(dat,tmp.nlens,by="HaulID")


  # Create per-haul expansion factor






  table(round(100*dat$SPECIES_WEIGHT/dat$TOTAL_SAMPLE_WEIGHT),substring(dat$RETRV_DATE_TIME,1,4))
  table(round(dat$SPECIES_WEIGHT/1000))
  table(round(dat$SPECIES_WEIGHT/1000),substring(dat$RETRV_DATE_TIME,1,4))
       
  dat[round(dat$SPECIES_WEIGHT/1000)>65,]

  par(mfrow=c(2,1))
  hist(dat$SPECIES_WEIGHT)
  boxplot(dat$SPECIES_WEIGHT)
  plot(dat$SPECIES_WEIGHT)
  plot(dat$SPECIES_WEIGHT,ylim=c(0,1000),pch=20)
  plot(dat$TOTAL_SAMPLE_WEIGHT,dat$SPECIES_WEIGHT,ylim=c(0,1000),pch=20)
  abline(a=0,b=1)
  hist(dat$TOTAL_SAMPLE_WEIGHT)

}