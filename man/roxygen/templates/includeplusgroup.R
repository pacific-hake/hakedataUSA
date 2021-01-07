#' @param includeplusgroup A logical that specifies if bin sizes larger than
#' the maximum value in `breaks` should be included in the final bin? This
#' argument is available in multiple functions, but it ultimately is passed
#' to [comps_bin]. If `TRUE`, which is the default, then the right border of
#' the maximum bin will be `Inf`.
