#' Workup At-Sea Composition Data for Pacific Hake
#'
#' todo: add more information about this function here. 
#' 
#' @template atsea.ages
#' @template ncatch
#' @param years A vector of years that you want plotted.
#' 
#' @return #todo: document return
#' @export 
#' @import ggplot2
#' @author Kelli Faye Johnson
#' 
plot_rawmeasure <- function(atsea.ages = NULL, page = NULL, years = 2017) {
  x_weight <- c(0, 3)
  x_length <- c(0, 70)
  mydir <- hakedatawd()
  if (is.null(atsea.ages)) {
    base::load(file.path(mydir, "extractedData", "atsea.ageWt.Rdat"))
    base::load(file.path(mydir, "extractedData", "atsea.ages.Rdat"))
    base::load(file.path(mydir, "extractedData", "page.Rdat"))
  }
  g1 <- ggplot(atsea.ages[atsea.ages$YEAR %in% years,],
    aes(LENGTH, group = factor(YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(YEAR))) +
    xlim(x_length) +
    xlab("Length (cm)") +
    ylab("At-Sea") +
    labs(col = "Year") +
    plottheme() + theme(legend.position = c(0.85, 0.55))
    scale_color_manual(values=plotcolour(n = length(years)))
  g2 <- ggplot(atsea.ages[atsea.ages$YEAR %in% years,],
    aes(WEIGHT, group = factor(YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(YEAR))) +
    xlim(x_weight) +
    xlab("Weight (kg)") +
    ylab("At-Sea") +
    labs(col = "Year") +
    plottheme() + theme(legend.position = c(0.85, 0.55))
    scale_color_manual(values=plotcolour(n = length(years)))
  g3 <- ggplot(page[page$SAMPLE_YEAR %in% years,],
    aes(FISH_LENGTH/10, group = factor(SAMPLE_YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(SAMPLE_YEAR))) +
    xlim(x_length) +
    xlab("Length (cm)") +
    ylab("Shoreside") +
    labs(col = "Year") +
    plottheme() + theme(legend.position = c(0.85, 0.55))
    scale_color_manual(values=plotcolour(n = length(years)))
  g4 <- ggplot(page[page$SAMPLE_YEAR %in% years,],
    aes(FISH_WEIGHT / 1000,
      group = factor(SAMPLE_YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(SAMPLE_YEAR))) +
    xlim(x_weight) +
    xlab("Weight (kg)") +
    ylab("Shoreside") +
    labs(col = "Year") +
    plottheme() + theme(legend.position = c(0.85, 0.55))
    scale_color_manual(values=plotcolour(n = length(years)))
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_length_AtSea.png"),
    g1, width = 7, height = 2.5
  )
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_weight_AtSea.png"),
    g2, width = 7, height = 2.5
  )
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_length_shore.png"),
    g3, width = 7, height = 2.5
  )
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_weight_shore.png"),
    g4, width = 7, height = 2.5
  )
}
