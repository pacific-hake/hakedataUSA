#' @param breaks A vector of numeric values that will be used as 
#' lower boundaries for categorizing data into bins. Thus, if you wish
#' the first bin to be a catch-all for all values smaller than the first
#' bin use `c(-Inf, 3, 4, 5)` or what have you such that any number smaller
#' than three here would be included in the first bin labeled `-Inf`.
#' For a plus group, see the argument `includeplusgroup` or add `Inf` to
#' your bin vector.
