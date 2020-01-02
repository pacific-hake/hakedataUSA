############################################################################
# atseaComps.fn.r
#
# Expand and summarize the size- or age-compositions for the hake at-sea fishery.
#
# first written by Andi Stephens, August 2010
# updated by Allan Hicks, December 2015
#
# Filters the atsea bilogical data,
# expands it by tow,
# then summarizes the composition as specied
#
# This is a wrapper function that calls other functions
#
# Input:
#    a dataframe from an extraction from the NORPAC database
#    Specific columns must be provided
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
#    min_sample    Numeric.  Minimum fish in a sample.
#    remove_sparse Logical.  Remove samples without the minimum # fish.
#    which         Output mode.  "pct", "num", or "both".  Default:  "pct".
#    in.filename   Data file.
#    out.filename  Results file.
#    rpt.filename  Report file.

Atsea.comps = function(dat,BY_AGE=FALSE, BY_MONTH=FALSE, BY_GENDER=FALSE,
                       lbin.sizes=NULL, in.season=NULL, in.pctl=0.95,
                       which="pct", min_Haul=0, min_T_weight=0,
                       min_sample=15, remove_sparse=FALSE, NO_LENGTH=FALSE,
                       out.filename, report.filename, DEBUG=FALSE) {

#options(stringsAsFactors = FALSE) #in case there are issues with strings converted to factors (moreso if reading in csv)

if (DEBUG) {
  cat("DEBUG is ON\n"); flush.console();
} # End if DEBUG

##############################################################################
# Perform preliminary processing.
# Keep only those columns used in the analysis.
# Remove bad records and record statistics.
##############################################################################

Month <- as.numeric(substring(dat$RETRV_DATE_TIME,6,7))
YEAR <- as.numeric(substring(dat$RETRV_DATE_TIME,1,4))

if (length(unique(YEAR)) > 1 ) {
  stop("\n\n *** Fatal error: called Atsea.comps with multi-year dataset.\n\n")
} # End if



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
}
