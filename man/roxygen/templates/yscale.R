#' @param yscale The desired scale for the y axis. Allowable values
#' are all of those accepted in the `trans` argument of
#' [ggplot2::scale_y_continuous]. The default, `identity`, leads
#' to nothing being changed. Users will typically want to change
#' the value to `log`, which doesn't transform the data just the
#' distance between the labels on the y axis.
