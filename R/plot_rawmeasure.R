#' Workup At-Sea Composition Data for Pacific Hake
#'
#' todo: add more information about this function here. 
#' 
#' @template atsea.ages
#' @template ncatch
#' @param years
#' 
#' @return 
#' @export 
#' @import ggplot2
#' @author Kelli Faye Johnson
#' 
plot_rawmeasure <- function(atsea.ages = NULL, years = 2017) {
  x_weight <- c(0, 3)
  x_length <- c(0, 70)
  mydir <- hakedatawd()
  if (is.null(atsea.ages)) {
    base::load(file.path(mydir, "extractedData", "atsea.ageWt.Rdat"))
    base::load(file.path(mydir, "extractedData", "atsea.lenAge.Rdat"))
    base::load(file.path(mydir, "extractedData", "atsea.ages.Rdat"))
    base::load(file.path(mydir, "extractedData", "pacfin_bds_fish.Rdat"))
  }
  if (!"Year" %in% colnames(atsea.lenAge)) {
    atsea.lenAge$Year <- format(atsea.lenAge$RETRV_DATE_TIME, "%Y")
    atsea.ages$Year <- format(atsea.ages$RETRV_DATE_TIME, "%Y")
    atsea.ages$Year <- atsea.ages$YEAR
  }
  g1 <- ggplot(atsea.lenAge[atsea.lenAge$Year %in% years,],
    aes(LENGTH_SIZE, group = factor(Year), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(Year))) + 
    xlim(x_length) + 
    xlab("Length (cm)") +
    ylab("At-Sea")+
    labs(col = "Year") + 
    hakedata:::plottheme() + 
    scale_color_manual(values=plotcolour(n = length(years)))
  g2 <- ggplot(atsea.ages[atsea.ages$Year %in% years,],
    aes(WEIGHT, group = factor(Year), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(Year))) + 
    xlim(x_weight) + 
    xlab("Weight (kg)") +
    ylab("At-Sea")+
    labs(col = "Year") + 
    hakedata:::plottheme() + 
    scale_color_manual(values=plotcolour(n = length(years)))
  bds.fish <- bds.fish[-which(bds.fish$SAMPLE_NO == "OR1972474" & bds.fish$FISH_NO == 70), ]
  g3 <- ggplot(bds.fish[bds.fish$SAMPLE_YEAR %in% years,],
    aes(FISH_LENGTH/10, group = factor(SAMPLE_YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(SAMPLE_YEAR))) + 
    xlim(x_length) + 
    xlab("Length (cm)") +
    ylab("Shoreside") +
    labs(col = "Year") + 
    hakedata:::plottheme() + 
    scale_color_manual(values=plotcolour(n = length(years)))
  g4 <- ggplot(bds.fish[bds.fish$SAMPLE_YEAR %in% years &
    !bds.fish$SAMPLE_NO %in% c(20173204010038, 20173204010040),],
    aes(FISH_WEIGHT * 0.453592, 
      group = factor(SAMPLE_YEAR), na.rm = TRUE)) +
    geom_line(stat = "density", aes(y = ..density.., col = factor(SAMPLE_YEAR))) + 
    xlim(x_weight) + 
    xlab("Weight (kg)") +
    ylab("Shoreside") +
    labs(col = "Year") + 
    hakedata:::plottheme() + 
    scale_color_manual(values=plotcolour(n = length(years)))
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_length_AtSea.png"), g1, width = 7, height = 3)
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_weight_AtSea.png"), g2, width = 7, height = 3)
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_length_shore.png"), g3, width = 7, height = 3)
  ggplot2::ggsave(filename = file.path(mydir, "Figures", "raw_weight_shore.png"), g4, width = 7, height = 3)
}
