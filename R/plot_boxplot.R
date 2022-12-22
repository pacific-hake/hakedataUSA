#' Plot a box plot with user-defined probabilities
#'
#' Plot a box plot with user-defined probabilities using
#' [ggplot2::geom_boxplot] and [ggplot2::facet_wrap] if
#' more than one `xvar` is supplied.
#'
#' @template data
#' @template xvar
#' @template yvar
#' @template file
#' @param probabilities A vector of five probabilities,
#' starting with the lower limit of the whisker,
#' then the lower edge of the box,
#' the middle of the box,
#' the upper edge of the box, and
#' the upper limit of the whisker.
#' @param showmedian A logical, specifying if a line should be plotted
#' for the third entry in `probabilities`, which will be the middle of
#' the box. The default, `FALSE`, leads to no median/middle line.
#' @template mlab
#' @template xlab
#' @template ylab
#' @template yscale
#' @param incolor If missing, no colors will be used for `aes(fill = )` in
#' the boxplots. Otherwise, a vector of column names can be used, which
#' will lead to [interaction] being called and the resulting factor
#' will determine the color categories. Colors will be determined
#' using [plotcolour]; but, note that this internal function can only
#' handle a limited number of categories.
#' @param scales A character value supplied to [ggplot2::facet_wrap] that
#' defines how the scales of the axes are determined across facets.
#' The default of `fixed` causes all axes to be the same across panels.
#' The most-likely value besides the default will be `free` allowing axes to
#' be specific to each panel.
#' @param nrow,ncol Number of rows and columns passed to
#' [ggplot2::facet_wrap]. The default values of `NULL` allow the function to
#' determine the appropriate number of rows and columns. If changed from the
#' default, use integer values to predetermine the number of rows or columns.
#' @param legend.position Coordinates for where you would like the legend
#'   to be placed.
#' @param legend.direction `"vertical"` or `"horizontal",
#'   where the default is the former.
#' @param ... Arguments passed to [ggplot2::ggsave] other than `filename`
#' which is passed using `file`.
#'
#' @return A `ggplot2` object is invisibly returned. If `file` is supplied,
#' then the figure is also saved based on the entry (i.e., either relative
#' to your current working directory or based on the full path if specified).
#' The returned object contains a data frame of the data used in the figure
#' and not the data frame supplied to the figure. This object `returned[["data"]]`
#' is helpful when trying to determine quantiles for text in a document.
#'
#' @examples
#' testdata <- data.frame(
#'   yvar = rnorm(100),
#'   xvar1 = rep(c("Good", "Bad", "Bad", "Good"), length.out = 100),
#'   xvar2 = rep(c("Apples", "Bananas", "Oranges", "Carrots"), length.out = 100))
#' gg <- plot_boxplot(data = testdata, xvar = c("xvar1"), yvar = "yvar",
#'   showmedian = TRUE, ylab = "Best fruit", incolor = "xvar2",
#'   legend.position = "right")
#' \dontrun{
#' print(gg)
#' grDevices::dev.off()
#' rm(testdata, gg)
#' }
plot_boxplot <- function(data,
  xvar,
  yvar,
  file,
  probabilities = c(0.025, 0.25, 0.5, 0.75, 0.975),
  showmedian = FALSE,
  mlab = "Box plot with 95 percent quantiles",
  xlab = tools::toTitleCase(xvar[1]),
  ylab = "",
  yscale = "identity",
  incolor,
  scales = "fixed",
  nrow = NULL, ncol = NULL,
  legend.position,
  legend.direction = "vertical",
  ...
  ) {

  #### Set up the variables and perform checks
  stopifnot(length(probabilities) == 5)
  stopifnot(length(yvar) == 1)
  if (!showmedian) {
    fatten <- NULL
  } else {
    fatten <- as.numeric(showmedian)
  }
  label <- c("lower95", "lowerhinge",
    "median", "upperhinge", "upper95")

  #### Manipulate the data, calculate probabilities
  if (missing(incolor)) {
    cols2keep <- xvar
  } else {
    cols2keep <- unique(c(xvar, incolor))
  }
  p.temp <- plyr::ddply(data, cols2keep,
    plyr::numcolwise(quantile, probs = probabilities, na.rm = TRUE))
  p.dist <- transform(p.temp[, c(cols2keep, yvar)],
    probs = probabilities)
  p.formula <- stats::as.formula(
    paste(paste(cols2keep, collapse = " + "), "~", "probabilities")
    )
  p.dist <- tidyr::pivot_wider(
    p.dist, names_from = probs, values_from = yvar
  )
  colnames(p.dist) <- c(cols2keep, label)
  p.dist[, "inter"] <- interaction(p.dist[, xvar], drop = FALSE)
  if (!missing(incolor)) {
    p.dist[, "color"] <- interaction(p.dist[, incolor], drop = FALSE)
  }

  #### Create the figure
  gg <- ggplot2::ggplot(p.dist,
    ggplot2::aes(x = .data[[xvar[1]]], y = .data[[label[1]]],
      group = .data[["inter"]],
      ymin = .data[[label[1]]], lower = .data[[label[2]]],
      middle = .data[[label[3]]], upper = .data[[label[4]]],
      ymax = .data[[label[5]]])) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::ggtitle(mlab) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::scale_y_continuous(trans = yscale)
  if (!missing(incolor)) {
    colors <- plotcolour(length(levels(p.dist$color)))
    border <- ifelse(any(c("black", "#000000") %in% colors),
      "gray", "black")
    gg <- gg +
      ggplot2::geom_boxplot(stat = "identity", fatten = fatten,
        position = position_dodge(preserve = "single"),
        aes(
          fill = .data[["color"]],
          group = interaction(.data[["color"]], .data[["inter"]])
          ),
        colour = border
        ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme(legend.position = "none")
      if (!missing(legend.position)) {
        gg <- gg + 
          ggplot2::theme(
            legend.direction = legend.direction,
            legend.position = legend.position,
            legend.background = element_rect(fill = "transparent")
          ) +
          ggplot2::labs(
            fill = tools::toTitleCase(paste(collapse = " x ", incolor))
            )
      }
  } else {
    gg <- gg +
      ggplot2::geom_boxplot(stat = "identity", fatten = fatten,
        position = position_dodge(preserve = "single"))
  }
  if (length(xvar) > 1) {
    gg <- gg +
     ggplot2::facet_wrap(xvar[-1], scales = scales, nrow = nrow, ncol = ncol)
  }
  if (!missing(file)) {
    suppressMessages(ggplot2::ggsave(plot = gg, filename = file, ...))
  }

  return(invisible(gg))
}
