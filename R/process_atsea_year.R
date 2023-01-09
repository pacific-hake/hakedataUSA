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
#    in.pctl       Threshhold quantile for expansion factors.  Default:  0.95.
#    in.filename   Data file.
#    out.filename  Results file.
#    rpt.filename  Report file.
#' @import stats
process_atsea_year <- function(dat,
                        ncatch,
                        minAge = 1,
                        maxAge = 15,
                        vesselType = c(1, 2),
                        in.pctl = 0.95){
  stopifnot(length(unique(ncatch[["SPECIES"]])) == 1)
  # Create some variables
  # Create trip, haul identifiers; use HAUL_JOIN b/c CRUISE is not unique to a
  # single trip but rather an observer deployment
  # Calculate the total haul weight for hake only use the catch file extracted
  # from NORPAC (ncatch) link ncatch hauls to dat hauls EXTRAPOLATED_WEIGHT is
  # the weight of hake in the haul
  # subset by vessel_type if desired
  colname_haul <- grep("HAUL[_]{0,1}JOIN", colnames(ncatch), value = TRUE)
  dat <- dat %>%
    dplyr::mutate(
      SEX = "U",
      AGE = ifelse(AGE < 0, NA_integer_, AGE)
    ) %>%
    dplyr::filter(!is.na(AGE)) %>%
    dplyr::left_join(
      y = dplyr::select(
        .data = ncatch,
        dplyr::matches("EXTRAPOLATED|VESSEL_TYPE|HAUL[_]{0,1}JOIN|HAUL$")
      ),
      by = c(HAUL_JOIN = colname_haul, HAUL_OFFLOAD = "HAUL")
    ) %>%
    dplyr::filter(VESSEL_TYPE %in% vesselType)
  if (NROW(dat) == 0) return(NULL)

  # calculate sample weight by summing weights first, fill in missing weights
  # with median of weight-at-length or -age do this by month first, if still
  # missing weights, do it by year.
  # Calculate lm before filling with means
  max_age <- max(dat[["AGE"]], na.rm = TRUE)
  lm_results <- stats::lm(WEIGHT ~ Year + AGE, data = dat)
  newdata <- tidyr::expand(dat, Year, AGE)
  lm_predict <- cbind(
    newdata,
    WEIGHT = stats::predict(
      lm_results,
      newdata = newdata
    )
  )
  # Fill with age-specific weight-at-age means for Month, then Year
  dat <- dat %>%
    dplyr::group_by(AGE, Year, Month) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = WEIGHT,
        .fns = ~ tidyr::replace_na(., mean(., na.rm = TRUE)),
      )
    ) %>%
    dplyr::group_by(AGE, Year) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = WEIGHT,
        .fns = ~ tidyr::replace_na(., mean(., na.rm = TRUE)),
      )
    ) %>%
    dplyr::ungroup()

  # Fill in remaining missing weights with results from linear model
  dat[["WEIGHT"]][is.na(dat[["WEIGHT"]])] <-
    dplyr::left_join(
      dat[is.na(dat[["WEIGHT"]]), ],
      lm_predict,
      by = c("Year", "AGE")
    )[["WEIGHT.y"]]
  # Expansion factor
  # Sum up weight per group
  # Fill in missing values with median
  dat <- dat %>%
    dplyr::group_by(YEAR, HAUL_JOIN, HAUL_OFFLOAD) %>%
    dplyr::mutate(
      sampWt = sum(WEIGHT),
      expFactor = EXTRAPOLATED_WEIGHT / sampWt
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(
      pctl = quantile(expFactor, in.pctl, na.rm = TRUE),
      medExpFact = median(expFactor, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      expFactor = ifelse(is.na(expFactor), medExpFact, expFactor),
      expFactor = ifelse(expFactor > pctl, pctl, expFactor),
      AGE = ifelse(AGE < minAge, minAge, AGE),
      AGE = ifelse(AGE > maxAge, maxAge, AGE)
    )

  # Output comp data
  out <- dplyr::full_join(
    dat %>%
      dplyr::mutate(id = paste(CRUISE, PERMIT, HAUL_OFFLOAD)) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        `n.fish` = n(),
        `n.hauls` = length(unique(id))
      ) %>%
      dplyr::ungroup(),
    dat %>%
      dplyr::group_by(Year, AGE = factor(AGE, levels = minAge:maxAge)) %>%
      dplyr::summarise(comp = sum(expFactor)) %>%
      tidyr::complete(AGE, fill = list(comp = 0)) %>%
      dplyr::relocate(starts_with("n"), .after = Year) %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(comp = comp / sum(comp, na.rm = TRUE)) %>%
      dplyr::ungroup(),
    by = "Year"
  ) %>%
  dplyr::rename(year = "Year")
}
