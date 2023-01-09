#' Workup At-Sea and PacFIN lengths and weights
#'
#' @template atsea.ages
#' @template ncatch
#' @param years A vector of years that you want plotted. The default is to plot
#'   the most recent five years.
#'
#' @return Four figures are saved to the disk. Figures are of length and weight
#' distributions for the at-sea and shoreside sectors.
#' @export
#' @import ggplot2
#' @author Kelli F. Johnson
#'
plot_raw_age <- function(atsea.ages = get_local("atsea.ages.Rdat"),
                         page = get_local("page.Rdat"),
                         years = as.numeric(format(Sys.time(), "%Y")) - 0:4,
                         xlim_weight = c(0, 3),
                         xlim_length = c(0, 70)) {
  gg <- ggplot(atsea.ages[atsea.ages$YEAR %in% years,],
    aes(group = factor(.data[["YEAR"]]),  na.rm = TRUE)) +
    ylab("At-Sea") +
    labs(col = "Year") +
    plot_theme() +
    theme(legend.position = c(0.85, 0.55)) +
    scale_color_manual(values = plot_colour(n = length(years)))
  g1 <- gg +
    geom_line(stat = "density",
      aes(x = .data[["LENGTH"]], y = after_stat(scaled),
        col = factor(.data[["YEAR"]]))) +
    xlim(xlim_length) +
    xlab("Length (cm)")
  g2 <- gg +
    geom_line(stat = "density",
      aes(x = .data[["WEIGHT"]], y = after_stat(scaled),
        col = factor(.data[["YEAR"]]))) +
    xlim(xlim_weight) +
    xlab("Weight (kg)")

  gg <- ggplot(page[page$SAMPLE_YEAR %in% years,],
    aes(group = factor(.data[["SAMPLE_YEAR"]]), na.rm = TRUE)) +
    ylab("Shoreside") +
    labs(col = "Year") +
    plot_theme() + theme(legend.position = c(0.85, 0.55)) +
    scale_color_manual(values = plot_colour(n = length(years)))
  g3 <- gg +
    geom_line(stat = "density", 
      aes(x = .data[["FISH_LENGTH"]] / 10, y = after_stat(scaled),
        col = factor(.data[["SAMPLE_YEAR"]]))) +
    xlim(xlim_length) +
    xlab("Length (cm)")
  g4 <- gg +
    geom_line(stat = "density",
      aes(x = .data[["FISH_WEIGHT"]] / 1000, y = after_stat(scaled),
        col = factor(.data[["SAMPLE_YEAR"]]))) +
    xlim(xlim_weight) +
    xlab("Weight (kg)")

  ignore <- mapply(
    ggplot2::ggsave,
    filename = file.path(hakedata_wd(), "Figures",
      c("raw_length_AtSea.png", "raw_weight_AtSea.png",
        "raw_length_shore.png", "raw_weight_shore.png")),
    plot = list(g1, g2, g3, g4),
    MoreArgs = list(width = 7, height = 2.5)
  )

}
