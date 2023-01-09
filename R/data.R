#' Proportion mature at each age
#'
#' A vector of maturity values from the maturity ogive. The length of the vector
#' is the same as the number of ages in the model, not the number of ages in the
#' data.
#'
#' @details
#' The data are stored in a data object that can be updated/built by running
#' the script in `data-raw/maturity_at_age.R`.
#'
#' @format ## `maturity_at_age`
#' A vector with 20 observations, one for each modelled age starting with age
#' zero.
#' @source Melissa Head
"maturity_at_age"

#' Quota information for U.S. sectors of Pacific Hake fishery
#' @format ## `who`
#' A data frame with 5 rows and many columns:
#' \describe{
#'   \item{Fleet}{Names of sectors that receive allocation}
#'   \item{2008,...}{Year-specific allocations}
#'   ...
#' }
"quotas"
