#' Summarize Bycatch in the Pacific Hake Fishery With Barplots
#' 
#' Create barplots of bycatch by species for the Pacific hake fishery.
#' Data is summarized from the NORPAC database only.
#' 
#' @template ncatch 
#' @param nspecies A data frame of species names from NORPAC.
#' @param fileout A file path to a \code{.pdf} extension to specify where
#' the plots should be saved. A single \code{.pdf} can be created or multiple
#' \code{.png} files will be created.
#' 
#' @import graphics grDevices
#' @importFrom stats reshape
#' 
#' @return A data frame of bycatch by species, year, and sector.
#' @export
#' @author Kelli Faye Johnson
#'
workupNorpacCatchSpecies <- function(ncatch = NULL, nspecies = NULL,
  fileout = "bycatchsummary.pdf") {
  
  oldoptions <- options()
  options(digits = 19)
  on.exit(options(oldoptions))
  on.exit(graphics.off(), add = TRUE)
  file <- "NORPACdomesticCatch.Rdat"

  if (is.null(ncatch)) {
    filepath <- ifelse(file.exists(file), file,
      file.path(hakedatawd(), "extractedData", file))
    if (!file.exists(filepath)) stop("The file ", 
    	filepath, " does not exist.")
    base::load(filepath)
  }
  if (is.null(nspecies)) {
    file <- "NORPACspecies.Rdata"
    filepath <- ifelse(file.exists(file), file,
      file.path(hakedatawd(), "extractedData", file))
    if (!file.exists(filepath)) stop("The file ",
      filepath, " does not exist.")
    base::load(filepath)
  }

  spp <- setNames(nspecies$SPECIES_NO, nspecies$SPECIES_NAME)
  spp <- spp[spp %in% ncatch$SPECIES]
  out.yr <- do.call("rbind", lapply(seq_along(spp), function(x) {
  	out <- processNorpacCatch(ncatch, species = spp[x])
  	if(is.null(out)) return()
    out.yr <- data.frame(aggregate(out$Catch,list(out$Sector,out$Year),sum),
  	  "Species" = names(spp[x]))
    names(out.yr) <- c("Sector","Year","Catch.MT", "Species")
    return(out.yr)
  }))
	forbplot <- stats::reshape(out.yr[, c("Year", "Catch.MT", "Species")], 
		direction = "wide",
		idvar = "Year", timevar = "Species")
	forbplot[is.na(forbplot)] <- 0
	colnames(forbplot) <- gsub("Catch.MT.", "", colnames(forbplot))

	barplotbycatch <- function(use, data, withhake = TRUE) {
		if (withhake) use <- c("Pacific hake", use)
		use.colors <- grDevices::topo.colors(n = length(use))
		use.data <- t(as.matrix(data[, toupper(use)], 
			ncol = length(use), byrow = TRUE))
		graphics::barplot(use.data,
			beside = TRUE, names = data$Year, 
			xlab = "Year", ylab = "Catch (mt)",
			col = use.colors)
		legend(x = "topleft", legend = use, bty = "n", fill = use.colors)
	}
	use <- list("squid" = c("Humboldt squid", "Squid unidentified"),
		"rockfish" = c("Silvergray rockfish", "Aurora rockfish",
		"Rougheye rockfish", "Shortraker rockfish", "Shortraker/Rougheye rockfish"), 
		"red banded" = c("Red banded rockfish"),
		"mackerel" = grep("mackerel", names(spp), value = TRUE, ignore.case = TRUE),
		"sardine" = "Pacific sardine")
	ext <- tail(strsplit(basename(fileout), split="\\.")[[1]], 1)
	if (ext == "pdf") pdf(fileout)
  if (ext == "png") png(filename = gsub("\\.png", "hake%03d.png", fileout))
	ignore <- sapply(use, barplotbycatch, data = forbplot, withhake = FALSE)
  if (ext == "png") png(filename = gsub("\\.png", "%03d.png", fileout))
	ignore <- sapply(use, barplotbycatch, data = forbplot)
	graphics.off()

  return(out.yr)
}
