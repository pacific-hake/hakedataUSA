#############################################################################
#
# Shoreside.comps.r
#
# Build the size- or age-compositions for the hake shoreside fishery.
#
# Andi Stephens, June 2010
# Allan Hicks, November 2011
#
# The output data are (optionally) copied from the inputs, so that
# "original.data" can always be examined for comparison (and reality checks)
# with intermediate and final products.
#
# Input files:  
#    input data directly (only a single year)
#    ##############bds.fish.r.out.csv  The pre_processed shoreside data.
#
# Arguments:
#
#    BY_AGE        Logical.  Do age- rather than the default length-comps.
#    NO_LENGTH     Logical.  Don't stratify BY-AGE summaries by length.
#    BY_MONTH      Logical.  Do monthly summaries.
#    BY_GENDER     Logical.  Summarize by gender
#    lbin.sizes    Vector, sorted smallest to largest.  Not required;
#                  default is 2-cm bins.
#    in.pctl       Threshhold value for expansion factor.  Default:  0.95.
#    in.season     List whose elements describe the months to include
#                  in a SEASON for seasonal summaries.  Not required.
#                  Number and size of seasons is arbitrary.  See example
#                  in DEBUG section below.
#    min_CL_weight Numeric.  Minimum cluster weight.
#    min_T_weight  Numeric.  Minimum total weight.
#    min_sample    Numeric.  Minimum fish in a sample.
#    remove_sparse Logical.  Remove samples without the minimum # fish
#    in.filename   Data file.
#    out.filename  Results file.
#    rpt.filename  Report file.
#    which         Controls output.  Values:  "both", "pct", "num".
#    DEBUG
#    verbose       Whether to output report details to screen (always outputs to report file)
#
#
# Process:
#
#    Evaluate input arguments
#    Extract the input columns of interest
#    Delete records containing missing or bad values
#    Use by-port median cluster weights to fill in missing cluster weights
#    Use annual median cluster weights to fill in those still missing
#    Develop the appropriate expansion factor to normalize sampling effort
#        (i.e. Some clusters are more extensively sampled than others).
#    Use the expansion factor on the input data to generate normalized values.
#    Tabulate these according to the summary-control arguments to the function.
#    Report summary statistics and write out results.
#
# NOTE:  Data exist for 1965-1989, but are NOT usable according to this method.
#        Ian is putting it through as a research recommendation (2011).
#
# Changes 2011:
#   Checks if FISH_LENGTH_TYPE is logical and changes it
#   Cluster wts with NA were removed. Code chagned to fill in annual median.
#   Modified the replace.zeros to deal with NA's properly
#   I removed changing NA's to zeros and worked directly with NA's
#
# TODO: Eliminate hte attach and detaches, and use calls to the dataframe to avoid potential errors
##############################################################################

Shore.comps = function(input.data,BY_AGE=FALSE, BY_MONTH=FALSE, BY_GENDER=FALSE,
                       lbin.sizes=NULL, in.season=NULL, in.pctl=0.95,
                       which="pct", min_CL_weight=0, min_T_weight=0,
                       min_sample=15, remove_sparse=FALSE, NO_LENGTH=FALSE,
                       out.filename, report.filename, DEBUG=F,verbose=T) {

    mt2lbs <- 2204.62

    #bin sizes
    if ( ! is.null(lbin.sizes) ) { 
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


    ##############################################################################
    # Read in utility functions unless they're already there (running in a loop).
    ##############################################################################
    if (!exists("try.detach")) {
      source("Functions.R")
      source("Get.age.or.length.r")
    }
    
    ##############################################################################
    # Perform preliminary processing.
    # Keep only those columns used in the analysis.
    # Remove bad records and record statistics.
    ##############################################################################
    
    # Before attaching things, make sure they're not already there.
    #try.detach(shore.data)
    #try.detach(input.data)
    
    # 'Attach' exports all the column names into the environment
    # so that we don't have to refer to each 'x' as input.data$x.
    # Note that you still assign to yyy$x, and after doing so,
    # you need to refer to yyy$x.  Do not ask me why this is so
    # damned screwy!  Actually, it is because "attach" shows you
    # a copy of the columns rather than the true ones.
    
    #Using attach can be dangerous and may not be the best way to go. It may mask variables and it attaches to the 2nd position so some variables may be masked.
    #This should be coded without hte attaches and detaches so that you are certain of the variable you are using
    #I have started removing the attaches

    YEAR <- sort(unique(input.data$SAMPLE_YEAR))
    
    if (length(YEAR) > 1 ) {
      stop("\n\n *** Fatal error: called Shore.comps with multi-year dataset.\nCall one year at a time\n\n")
    } # End if
    
    if(is.logical(input.data$FISH_LENGTH_TYPE)) {
        cat("Changing FISH_LENGTH_TYPE from logical to character\n")
        tmp <- rep(NA, length(input.data$FISH_LENGTH_TYPE))
        tmp[input.data$FISH_LENGTH_TYPE == FALSE] <- "F"
        tmp[input.data$FISH_LENGTH_TYPE == TRUE ] <- "T"
    
        input.data$FISH_LENGTH_TYPE <- tmp
    }

    shore.data <- data.frame(tmp, input.data[c("FISH_LENGTH_TYPE","FISH_LENGTH","FISH_WEIGHT","CLUSTER_WGT",
                            "SAMPLE_YEAR","PORT","TOTAL_WGT","FREQ",
                            "SAMPLE_NO","SEX","SAMPLE_MONTH","SAMPLE_AGENCY",
                            "FISH_AGE_YEARS_FINAL","age1","EXP_WT",
                            "DATA_TYPE","SAMPLE_TYPE")],stringsAsFactors=FALSE)
    

    Original.N.records = nrow(shore.data)
    
    # Oregon reports EXP_WT, and it is believed better than TOTAL_WGT.
    
    Use_EXP_WT = shore.data$EXP_WT
    tmp.replace = replace.zeros(Use_EXP_WT, shore.data$TOTAL_WGT) 
    Use_EXP_WT = tmp.replace[[1]]
    Replaced.EXP_WT = tmp.replace[[2]]   #This is previous to filtering hte data, so this number is meaningless to output
    shore.data$TOTAL_WGT = Use_EXP_WT
    #Some NA's may be left because both TOTAL_WGT and EXP_WT are NA
    
    
    # Collect initial summary data
    
    sum.FISH_LENGTH_TYPE = table(shore.data$FISH_LENGTH_TYPE)
    sum.DATA_TYPE = table(shore.data$DATA_TYPE)
    sum.SAMPLE_TYPE = table(shore.data$SAMPLE_TYPE)
    
    pre_length.summary = summary(shore.data$FISH_LENGTH)
    pre_cluster.summary = summary(shore.data$CLUSTER_WGT)
    pre_total.wgt.summary = summary(shore.data$TOTAL_WGT)
    pre_age.summary = summary(shore.data$FISH_AGE_YEARS_FINAL)
    
    # Convert to metric tonnes
    
    shore.data$FISH_WEIGHT = shore.data$FISH_WEIGHT / mt2lbs
    shore.data$CLUSTER_WGT = shore.data$CLUSTER_WGT / mt2lbs
    shore.data$TOTAL_WGT   = shore.data$TOTAL_WGT / mt2lbs
    
    
    # Convert mm to cm
    
    shore.data$FISH_LENGTH = shore.data$FISH_LENGTH / 10

    # Change the NA's in SEX to "U"
    shore.data$SEX[is.na(shore.data$SEX) | shore.data$SEX == 0] = "U"
    
print(table(shore.data$SEX))
    
    #attach(shore.data)
    
    # Set up seasons
    if (TEMPORAL) {
        # Convert SAMPLE_MONTH to SEASON
        if (BY_SEASON) {
            SEASON = as.character(shore.data$SAMPLE_MONTH)
            for ( i in 1:length(in.season) ) {
                SEASON[SEASON %in% in.season[[i]]] = paste("Season.", i, sep="")
            } # End for
        } else {
            SEASON = as.numeric(shore.data$SAMPLE_MONTH)
        } # End if-else
        shore.data = data.frame(shore.data, SEASON)
        
        No.month <- sum(is.na(shore.data$SAMPLE_MONTH))
        shore.data$FISH_LENGTH[is.na(shore.data$SAMPLE_MONTH)] <- NA
    } # End if

    # Flag bad records by setting their FISH_LENGTH to 0 for deletion
    # Data without length

    shore.data$FISH_LENGTH[is.na(shore.data$FISH_LENGTH)] = 0   #Why change it zero and not leave it NA????  NA indicates that no data, whereas zero could be accidently thought of data by R
    No.length = sum(shore.data$FISH_LENGTH == 0, na.rm=T)

    #Deal with Gender
    if (BY_GENDER) {
        ind <- !(shore.data$SEX == "F" | shore.data$SEX == "M")
        No.gender <- sum(ind, na.rm=T)  #This will always be zero because above you try to add 0 to a factor that does not include 0, thus gives NA, but shouldn't NA be included in sum of unsexed if there are any?
        withGender <- sum(!ind,na.rm=T)
        LensNoGender <- shore.data$FISH_LENGTH[ind]
        cat("There are",No.gender,"unsexed fish and",withGender,"sexed fish in year",YEAR,". Unsexed fish range in length:",range(LensNoGender[LensNoGender>0]),"\n")
        shore.data$FISH_LENGTH[shore.data$SEX == "U"] <- NA
    } else {
        # Make sure they'll be tallied in final comps
        shore.data$SEX[shore.data$SEX != "M"] <- "M"   #Call them all Male 
    }

    # Unaged fish
    if (BY_AGE) {
        # Try to get an age
        tmp.replace = replace.zeros(shore.data$FISH_AGE_YEARS_FINAL, shore.data$age1)
        shore.data$FISH_AGE_YEARS_FINAL = tmp.replace[[1]]
        Replaced.age = tmp.replace[[2]]

        # If there still isn't one, delete.
        No.age <- sum(is.na(shore.data$FISH_AGE_YEARS_FINAL))
        shore.data$FISH_LENGTH[is.na(shore.data$FISH_AGE_YEARS_FINAL)] <- NA
    } else {
        shore.data$FISH_AGE_YEARS_FINAL = factor(shore.data$FISH_AGE_YEARS_FINAL)
        shore.data$FISH_AGE_YEARS_FINAL = addNA(shore.data$FISH_AGE_YEARS_FINAL, ifany=T)
    } # End if-else BY_AGE

    # Now delete the flagged data.  Always detach when changing
    # the size or shape of attached datasets, or you'll be SORRY!
    # ACH: why attach and detach? Just use the dataframe
    #detach(shore.data)

    shore.data <- shore.data[!is.na(shore.data$FISH_LENGTH), ]
    if(nrow(shore.data)==0) {
        cat("No data for year",YEAR,"\n")
        return(NULL)
    }


    # Find too-sparse samples
    Too_few_fish <- NULL
    if (min_sample > 0) {
        samples <- table(as.character(shore.data$SAMPLE_NO)) #use as.character so that it doesn't summarize all levels (removed samples), but it doesn't matter since those will have zeros
        if(length(samples)>0) {
            Too_few_fish <- cbind(samples[samples < min_sample])
            colnames(Too_few_fish) <- "Sample_size"
        }
        
        #samples = sort(unique(shore.data$SAMPLE_NO))
        #sample_count = rep(0, length(samples))
        #for ( i in 1:length(samples) ) {
        #    sample_count[i] = sum(shore.data$SAMPLE_NO == samples[i], na.rm=T)
        #} # End for
        Removed.sparse <- 0
        if (remove_sparse & nrow(Too_few_fish)>0) {
            Removed.sparse <- nrow(Too_few_fish)
            shore.data <- shore.data[!shore.data$SAMPLE_NO %in% rownames(Too_few_fish),]
            Too_few_fish = NULL
        } # End if
    } # End if min_sample


    #############
    # ADDED by ach on 12/23/2013
    # Some agencies do not have a total weight or cluster weight (WA)
    # sum up FISH_WEIGHTS to get a total and cluster weight
    # coded real quick and sloppy for now
    # Fills in MALES_WGT, FEMALES_WGT, and CLUSTER_WGT with sum of individual fish weights
    ############
    for(iii in unique(shore.data$SAMPLE_NO)) {
        tmp <- shore.data[shore.data$SAMPLE_NO == iii,]
        if(is.na(sum(tmp$MALES_WGT))) {
            shore.data[shore.data$SAMPLE_NO == iii,"MALES_WGT"] <- sum(tmp$FISH_WEIGHT[tmp$SEX=="M"])
        }
        if(is.na(sum(tmp$MALES_NUM))) {
            shore.data[shore.data$SAMPLE_NO == iii,"MALES_NUM"] <- sum(tmp$SEX=="M")
        }
        if(is.na(sum(tmp$FEMALES_WGT))) {
            shore.data[shore.data$SAMPLE_NO == iii,"FEMALES_WGT"] <- sum(tmp$FISH_WEIGHT[tmp$SEX=="F"])
        }
        if(is.na(sum(tmp$FEMALES_NUM))) {
            shore.data[shore.data$SAMPLE_NO == iii,"FEMALES_NUM"] <- sum(tmp$SEX=="F")
        }
        if(is.na(sum(tmp$CLUSTER_WGT))) {
            shore.data[shore.data$SAMPLE_NO == iii,"CLUSTER_WGT"] <- sum(tmp$FISH_WEIGHT)
        }
    }
        


    ##############################################################################
    #
    # Data that come in without values can be assumed to approach the population
    # median.  The next section of code gets these median values and adjusts
    # for missing values.
    #
    ##############################################################################

#attach(shore.data)

    # First, try user-specified minimums
    if (min_CL_weight > 0) {
        Use_Annual_CL = shore.data$CLUSTER_WGT
        Use_Annual_CL[is.na(shore.data$CLUSTER_WGT)] = 0
        Replaced.with.min.CL = sum(Use_Annual_CL == 0, na.rm=T)
        Use_Annual_CL[Use_Annual_CL < min_CL_weight] = min_CL_weight

        Replaced.with.Port.CW = 0
        Replaced.with.Annual.CW = 0
    } else {
        # Try median values by PORT, then overall.  Calculate medians
        # BEFORE replacing NA's with zeros, to avoid skewing values.
        Annual.Median.CL = stats::median(shore.data$CLUSTER_WGT, na.rm=T)

        # Early years of data had no PORT information

        if (length(unique(shore.data$PORT)) > 0 ) {
            # Get median values for CLUSTER_WGT by PORT
            tmp.agg = aggregate(shore.data$CLUSTER_WGT, list(shore.data$PORT), median, na.rm=T)
            colnames(tmp.agg) = c("PORT","Med_CL_Wgt")

            # Find corresponding median for each sample
            Medians.by.Port =  find.matching.rows(shore.data, tmp.agg, "PORT", "PORT", "Med_CL_Wgt")

            # Find.matching.rows returns a list.  We only want the vector of values
            Medians.by.Port = Medians.by.Port[[1]]

            # We will use the medians in the cases where the CLUSTER_WGT is zero or NA

            Use_Port_CL_Wgt = shore.data$CLUSTER_WGT

            tmp.replace = replace.zeros(Use_Port_CL_Wgt, Medians.by.Port)  #The Medians.by.Port may have an NA for a port with no clusterwts
            Use_Port_CL_Wgt = tmp.replace[[1]]

            Replaced.with.Port.CW = sum(!is.na(Use_Port_CL_Wgt))#tmp.replace[[2]] 
        } else {
            Use_Port_CL_Wgt = shore.data$CLUSTER_WGT
            Use_Port_CL_Wgt[is.na(Use_Port_CL_Wgt)] = 0
            Replaced.with.Port.CW = 0
        } # End if-else no PORT data

        # Now use annual medians calculated above.

        # Use the cluster weights we already adjusted but replace zeros/na's that
        # may have come from aggregating by PORT

        Use_Annual_CL = Use_Port_CL_Wgt
        #Replaced.with.Annual.CW = sum(Use_Annual_CL == 0, na.rm=T)
        Replaced.with.Annual.CW = sum(Use_Annual_CL == 0 | is.na(Use_Annual_CL))

        #Use_Annual_CL[Use_Annual_CL == 0] = Annual.Median.CL 
        Use_Annual_CL[Use_Annual_CL == 0 | is.na(Use_Annual_CL)] = Annual.Median.CL 

    } # End if-else use min_CL_weight


# Need to get median value for TOTAL_WGT. Use to fill in missing values.
# Again, adjusting for NA and zeros.

    if (min_T_weight > 0) {
        Use_Annual_Total = shore.data$TOTAL_WGT

        Use_Annual_Total[is.na(shore.data$TOTAL_WGT)] = 0
        Replaced.with.min_T_weight = sum(Use_Annual_Total == 0, na.rm=T)
        Use_Annual_Total[Use_Annual_Total < min_T_weight] = min_T_weight

        Replaced.with.Annual.Total = 0
    } else {
        Annual.Median.Total = stats::median(shore.data$TOTAL_WGT, na.rm=T)
        Annual.Median.Total = rep(Annual.Median.Total, length(shore.data$TOTAL_WGT))

        Use_Annual_Total = shore.data$TOTAL_WGT

        tmp.replace = replace.zeros(Use_Annual_Total, Annual.Median.Total)
        Use_Annual_Total = tmp.replace[[1]]

        Replaced.with.Annual.Total = tmp.replace[[2]]
    } # End if-else



    ##############################################################################
    #
    # Now get means and sums of various quantities needed to create summary tables.
    #
    ##############################################################################

    # Get the means of the Use_Annual_Total and Use_Annual_CL, and the sum of FREQ
    # for each SAMPLE_NO

    toagg = cbind(Use_Annual_Total, Use_Annual_CL, shore.data$FREQ)

    tmp.agg = aggregate(toagg, list(shore.data$SAMPLE_NO), mean, na.rm=T)
    colnames(tmp.agg) = c("SAMPLE_NO","MEAN_TOTAL_WGT", "MEAN_Use_Ann_CL")
    tmp.agg2 = aggregate(toagg, list(shore.data$SAMPLE_NO), sum, na.rm=T)

    tmp.agg = cbind(tmp.agg[,1:3],tmp.agg2[,4])
    colnames(tmp.agg)[4] = "SUM_FREQ"

    targets = c("MEAN_TOTAL_WGT", "MEAN_Use_Ann_CL", "SUM_FREQ")

    Summary_data = find.matching.rows(shore.data, tmp.agg, "SAMPLE_NO", "SAMPLE_NO", targets)

    ##############################################################################
    #
    # Detach while changing the dataset.  Add the columns we need to save,
    # and remove them from the environment.  This ensures that as we remove
    # bad records the column lengths stay the same.
    #
    ##############################################################################

#detach(shore.data)

    shore.data = cbind(shore.data, Summary_data, Use_Annual_Total, Use_Annual_CL,
                   Use_Port_CL_Wgt)



    ##############################################################################
    #
    # Record and flag for removal those records for which we couldn't generate
    # enough summary data
    #
    ##############################################################################

#attach(shore.data)
cat(sum(is.na(shore.data$FISH_LENGTH)),"NAs in fish_length\n")

    shore.data$FISH_LENGTH[is.na(shore.data$MEAN_TOTAL_WGT)] <- NA
    No.MEAN_TOTAL_WGT = sum(is.na(shore.data$MEAN_TOTAL_WGT))

cat(sum(is.na(shore.data$FISH_LENGTH)),"NAs in fish_length\n")

    shore.data$FISH_LENGTH[is.na(shore.data$MEAN_Use_Ann_CL)] <- NA
    No.MEAN_Use_Ann_CL = sum(is.na(shore.data$MEAN_Use_Ann_CL))
                         

    shore.data$FISH_LENGTH[is.na(shore.data$FREQ)] <- NA
    No.FREQ = sum(is.na(shore.data$FREQ))

cat(sum(is.na(shore.data$FISH_LENGTH)),"NAs in fish_length\n")
print(unique(shore.data$FREQ))

#detach(shore.data)


    shore.data = shore.data[!is.na(shore.data$FISH_LENGTH), ] 

print("HERE1")

#attach(shore.data)

    # How many records now?
    N.lengthed.fish = nrow(shore.data)

    ##############################################################################
    #
    # Create expansion factor.
    # After the deletions above, exp_factor should never be NA, zero or Inf
    #
    ##############################################################################

    exp_factor = shore.data$MEAN_TOTAL_WGT/shore.data$MEAN_Use_Ann_CL * shore.data$FREQ
    pre_exp.factor.summary = summary(exp_factor)

    # Reduce the impact of very-very large catches with small samples by setting max to some percentile
    pctl = stats::quantile(exp_factor, in.pctl, na.rm=T)
    exp_factor[shore.data$SUM_FREQ < 40 & exp_factor > pctl] = pctl    #WHERE IS THE 40 COMING FROM
    Maxed.exp.factor <- sum(shore.data$SUM_FREQ < 40 & exp_factor > pctl)
    
    # Deal with sparse samples
    if (!remove_sparse) {
        median_exp_factor = stats::median(exp_factor, na.rm=T)
        exp_factor[shore.data$SAMPLE_NO %in% rownames(Too_few_fish)] = median_exp_factor
        Replaced.exp.median = nrow(Too_few_fish)
    } # End if


    post_exp.factor.summary = summary(exp_factor)
print("HERE2")

    ##############################################################################
    #
    # Create Use_Length:  the length-bins used in the assessment.
    #
    # If no length bins were read in, just use the rounded length
    # in 2-cm bins
    #
    ##############################################################################

    if ( is.null(lbin.sizes) ) {
        Use_Length = round(shore.data$FISH_LENGTH)
        Use_Length[Use_Length <= 20] = 20
        Use_Length[Use_Length >= 70] = 70

        is.odd = Use_Length %% 2 > 0
        Use_Length[is.odd] = Use_Length[is.odd] - 1

        # Make sure all lengths show up in the table, even if not
        # in the data

        post_length.summary = summary(Use_Length * 10)

        #  Needs to be factored in order to have unused levels
        # show up in the tables.

        Use_Length = factor(Use_Length, levels=seq(20,70,2))
    } else {

    Use_Length = rep(0, length(shore.data$FISH_LENGTH))

    # Iteratively replace zeros with next Lbin
    # Assumes Lbins are sorted least-greatest
        for (i in 2:length(Lbins)) {
    
        minsize = Lbins[i]  
        lesser = Lbins[i-1]
    
        Use_Length[Use_Length == 0 & shore.data$FISH_LENGTH < minsize] = lesser
    
        } # End for
    
        # Finish up largest bin 
        Use_Length[Use_Length == 0] = Lbins[i]
        post_length.summary = summary(Use_Length * 10)
    
        # Make sure all lengths show up in the table, even if not in the data
        Use_Length = factor(Use_Length, levels=Lbins)

    } # End if

#detach(shore.data)
    shore.data = cbind(shore.data, Use_Length, exp_factor)
print("HERE3")

#attach(shore.data)

    if (BY_AGE) {
        # We're doing age comps
        shore.data$FISH_AGE_YEARS_FINAL[shore.data$FISH_AGE_YEARS_FINAL < 1] = 1
        shore.data$FISH_AGE_YEARS_FINAL[shore.data$FISH_AGE_YEARS_FINAL > 15] = 15

        # Make sure all ages show up in the table, as zeros if not represented in the data
        post_age.summary = summary(shore.data$FISH_AGE_YEARS_FINAL)
        shore.data$FISH_AGE_YEARS_FINAL = factor(shore.data$FISH_AGE_YEARS_FINAL,
                                           levels=seq(1,15))
#    detach(shore.data)
    } # End if


print("HERE5")

    ##############################################################################
    #
    # Output data
    #
    ##############################################################################

    # Size comps by number and percentage
    # The order in which factors are added matters!
    by.factors = NULL

    if (TEMPORAL) { by.factors =  "SEASON + " }  
    by.factors = paste(by.factors, "SEX + Use_Length + FISH_AGE_YEARS_FINAL", sep="")
  
    form = stats::as.formula(paste("exp_factor ~ ", by.factors, sep=""))
    fish.form = stats::as.formula(paste("FREQ ~ ", by.factors, sep=""))
    trips.form = stats::as.formula(paste("FREQ ~ ", "SAMPLE_NO + ", by.factors, sep=""))
    
    #try.detach(shore.data)
    #attach(shore.data)

    shore.comps.num = round(stats::xtabs(form,data=shore.data))
    fish.num = round(stats::xtabs(fish.form,data=shore.data))
    trips.num = round(stats::xtabs(trips.form,data=shore.data))
print("HERE6")

    ##############################################################################
    #
    #  Final SUMMARY STATISTICS
    #
    # Number of sample trips for length comps this year
    # Total weight of fish sampled this year
    # Number of individuals lengthed in sample trips this year 
    #
    ##############################################################################

    # Helper function for counting trips

    #nfacts = function(x) { nlevels(factor(x)) }   #Bad idea to count levels because you can have levels that do not appear in data

    # Collect Weight (that is, MEAN_TOTAL_WGT) used for each SAMPLE
    # NB:  Use of as.character and as.numeric because R insists on converting
    # things to goddamn FACTORS!
    #I believe this is a problem in the way things are coded without thinking of the proper data types
    SAMPLES = sort(unique(shore.data$SAMPLE_NO))

    Weight = matrix(nrow = length(SAMPLES), ncol=2, 0)
    colnames(Weight) = c("W","M_or_S")

    #can probably do the same thing with duplicated
    for ( i in 1:length(SAMPLES) ) {
        Weight[i,1] = shore.data$MEAN_TOTAL_WGT[shore.data$SAMPLE_NO == SAMPLES[i]][1]
        if (TEMPORAL) {
            Weight[i,2] = shore.data$SEASON[shore.data$SAMPLE_NO == SAMPLES[i]][1]
        } # End if
    } # End for

    Weight = data.frame(Weight)  #Why not just create a data.frame to begin with?
print("HERE7")

    if (TEMPORAL) {
        Trips = aggregate(shore.data$SAMPLE_NO, list(shore.data$SEASON), FUN=function(x){length(unique(x))})
        print("here7.1")
        Rpt.Weight = aggregate(as.numeric(Weight$W),
                           list(Weight$M_or_S), sum, na.rm=T)
        print("here7.2")
        N.Fish = aggregate(shore.data$FREQ, list(shore.data$SEASON), sum, na.rm=T)
        print("here7.3")
        sampled = cbind(Rpt.Weight, Trips[,2], N.Fish[,2])
        print("here7.4")

        if (BY_MONTH) {
            colnames(sampled) = c("Month","Metric.Tonnes","Trips","N.Fish")
            sampled[,1] = month.abb[as.numeric(sampled[,1])]
        } else {
            colnames(sampled) = c("Season","Metric.Tonnes","Trips","N.Fish")
        } # End if-else BY_MONTH
    } else {
        Trips = length(unique(shore.data$SAMPLE_NO))
        Rpt.Weight = sum(Weight[,1])
        N.Fish = sum(shore.data$FREQ, na.rm=T)

        # Staple them together
        sampled = c(Rpt.Weight, Trips, N.Fish)
        dim(sampled) = c(1,3)
        colnames(sampled) = c("Metric.Tonnes", "Trips", "N.Fish")
    } # End if-else

    post_cluster.summary = summary(shore.data$Use_Annual_CL * mt2lbs)
    post_total.wgt.summary = summary(shore.data$Use_Annual_Total * mt2lbs)
print("HERE8")

    ##############################################################################
    #
    #  Write out data
    #
    #  Each of these tables has to be converted to a matrix in order to be properly
    #  formatted in output.  Data can be output as percentages, numbers (weight) or
    #  both.
    #
    ##############################################################################

#options(warn=-1) turning off warnings can be dangerous

    outfname = out.filename

    if (BY_AGE) {
        cat(file=outfname, "\n\nShore age comps, Year:", YEAR, "\n\n", append=T)
        cat(file=outfname, "Aged Fish\n\n", append=T)
    } else {
        cat(file=outfname, "\n\nShore size comps, Year:", YEAR, "\n\n", append=T)
        cat(file=outfname, "Lengthed Fish\n\n", append=T)
    } # End if-else BY_AGE

    # Write out the summary data
    outdata = sampled
    write.table(file=outfname, x=outdata, sep=",", col.names=T, row.names=FALSE, append=T)
    if (TEMPORAL) {
        Mynames = attributes(shore.comps.num)$dimnames$SEASON   #Interesting?
        if (BY_MONTH) {
            Mynames = month.abb[as.numeric(Mynames)]
        } # End if BY_MONTH
    } # End if TEMPORAL

    while ( TRUE ) {
        target = shore.comps.num

        # By percent?
        if (which == "num") {
            by_pct = FALSE
            cat(file=outfname, "\nBy weight\n\n", append = T)
        } else {
            by_pct = TRUE
            cat(file=outfname, "\nAs percentages\n\n", append = T)
        } # End if-else which

        # Output summary control
        selection = "length.only"
print("HERE8.1")

        if (BY_AGE) {
            if (NO_LENGTH) {
                selection = "age.only"
            } else {
                selection = "age.and.length"
            } # End if-else NO_LENGTH
        } # End if BY_AGE
print("HERE8.2")
 
        if (TEMPORAL) {
            # Need to rebuild the tables for this case.
            outdata = NULL
            
            # Use m to index SEASON
            for ( m in 1:attributes(target)$dim[1] ) {
                outdata = get.age.or.length(target[m,,,], trips.num[,m,,,], fish.num[m,,,],
                                      output=selection, by.sex=BY_GENDER, pct=by_pct)
                if (BY_AGE) {
                    cat(file=outfname, "\n", Mynames[m], ",,,,,AGE\n", append=T)
                } else {
                    cat(file=outfname, "\n", Mynames[m], ",,,,,LENGTH BIN\n", append=T)
                } # End if
    
                write.table(file=outfname, x=outdata, col.names=T, row.names=F, sep=",", append=T)
    
            } # End for m
        } else {
print("HERE8.2a")
            # Not summarizing by Month/Season
            outdata = get.age.or.length(target, trips.num, fish.num, output=selection,
                                    by.sex=BY_GENDER, pct=by_pct)
print("HERE8.21")
            write.table(file=outfname, x=outdata, col.names=T, row.names=F, sep=",", append=T)
        } # End if-else TEMPORAL
print("HERE8.3")
   
        # Go through once more?
        if ( which == "both") {
            which = "num"
        } else {
            break  #Why do this?????????
        } # End if-else which
    } # End while
print("HERE9")

    ##############################################################################
    #   
    #  Write out the error statistics
    #
    ##############################################################################

    outfile = file(report.filename, open="wt")

    if(verbose) {
        sink(outfile, split=T, append=T, type="output")
    } else {
        sink(outfile, split=F, append=T, type="output")
    }
    
    cat("\n\nYear:", YEAR, "\n")
    cat("\nOriginal dataset:", Original.N.records, "  records\n\n")

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

    if (min_CL_weight > 0) {
        cat("\nMinimum cluster weight:", min_CL_weight, "\n")
        cat("Replaced  ", Replaced.with.min_CL_weight,"  cluster weights with minimum\n")
    } # End if
    
    if (min_T_weight > 0) {
        cat("\nMinimum total weight:", min_T_weight, "\n")
        cat("Replaced  ", Replaced.with.min_T_weight,"  total weights with minimum\n")
    } # End if

    if (BY_AGE) {
        cat("Replaced  ", Replaced.age, "  FISH_AGE_YEARS_FINAL with age1\n")
    } # End if

    cat("Replaced  ", Replaced.EXP_WT, "  TOTAL_WGTs with EXP_WT before filtering\n") 

    cat("\nPre-processing lengths (in mm):\n")
    print(pre_length.summary)

    cat("\nPost-processing lengths (in mm):\n")
    print(post_length.summary)

    cat("\nPre-processing cluster weights (in lbs):\n")
    print(pre_cluster.summary)

    cat("\nPost-processing cluster weights (in lbs):\n")
    print(post_cluster.summary)

    cat("\nPre-processing total weights (in lbs):\n")
    print(pre_total.wgt.summary)

    cat("\nPost-processing total weights (in lbs):\n")
    print(post_total.wgt.summary)

    cat("\nPre-truncation expansion factor summary:\n")
    print(pre_exp.factor.summary)

    cat("\nPost-truncation expansion factor summary:\n")
    print(post_exp.factor.summary)

    if (BY_AGE) { 
        cat("\nPre-processing ages:\n")
        print(pre_age.summary)

        cat("\nPost-processing ages:\n")
        print(post_age.summary)
    } # End if

    cat("\nNumber removed:\n")

    if (BY_AGE) {
        cat("\nRemoved  ", No.age, "  records without AGE.\n")
    } # End if

    cat("Removed  ", No.length,"  records without recorded FISH_LENGTH.\n",append=T)
    
    if (BY_GENDER) { 
        cat("Removed  ", No.gender, "  records without SEX.\n")
    } # End if

    if (TEMPORAL) {
        cat("Removed  ", No.month, "  records without SAMPLE_MONTH.\n")
    } # End if

    if (remove_sparse) {
        cat("Removed  ", Removed.sparse, "  samples without", min_sample, "fish\n")
    } # End if

    cat("\nWeights replaced in remaining data:\n")
    cat("Replaced  ", Replaced.with.Port.CW, "  cluster weights with median", 
                       "by-Port cluster weight\n")
    cat("Replaced  ", Replaced.with.Annual.CW, "  cluster weights with median",
                       "by-Year cluster weight\n")
    cat("Replaced  ", Replaced.with.Annual.Total, "  total weights with median",
                       "by-Year total weight\n\n")

    if (!remove_sparse) {
        cat("Replaced  ", Replaced.exp.median, "  expansion factors with median",
                        "in sparse samples\n")
    } # End if
    cat("Replaced  ", Maxed.exp.factor, "  expansion factors with maximum quantile of",pctl,"\n")

    cat("\nRecords removed because expansion factors couldn't be generated\n")
                       
    cat("Removed  ", No.MEAN_TOTAL_WGT, "  records with no MEAN_TOTAL_WGT\n")
    cat("Removed  ", No.MEAN_Use_Ann_CL, "  records with no MEAN_Use_Ann_CL\n")
    cat("Removed  ", No.FREQ, "  records with no FREQ\n")

    cat("\nRecords used in final analysis:", N.lengthed.fish, "\n")

    cat("\nSummary of FISH_LENGTH_TYPE\n")
    print(sum.FISH_LENGTH_TYPE)

    cat("\nSummary of DATA_TYPE\n")
    print(sum.DATA_TYPE)

    cat("\nSummary of SAMPLE_TYPE\n")
    print(sum.SAMPLE_TYPE)

  sample_sizes = stats::xtabs(stats::as.formula(FREQ ~ SAMPLE_NO + SAMPLE_AGENCY),data=shore.data)
  
  cat("\nSample sizes\n")
  print(sample_sizes)

    sink()
    close(outfile)

    # Clean up

#detach(shore.data)
#options(warn=0)

} # End function shore.comps
