#' Make \code{.csv} File of Canadian Weight-At-Age Data
#' 
#' Take input data provided by the Canadian JTC and make
#' a non-duplicated file to be read in for weight-at-age
#' calculations. 
#' 
#' @param file_in An absolute or relative path to the input file.
#' @param file_out An absolute or relative path name to save the
#' output data frame to the disk.
#' @param verbose A logical value specifying if output should be
#' printed to the screen for debugging. The default is \code{TRUE}.
#' @author Kelli Faye Johnson
#' @return A data frame of Canadian weight-at-age values with acoustic
#' data post the 2017 removed, data from the research tows removed,
#' and fishery data landed during January-March removed.
#' 
wtatage_can <- function(file_in, file_out = NULL, verbose = FALSE) {
  if (!file.exists(file_in)) {
    stop("The file ", file_in, "was not found.")
  }
  data <- utils::read.csv(file_in, header = TRUE,
    stringsAsFactors = FALSE)
  data <- data[!grepl("US|Poland", data[, "Source"]), ]
  nosurvey <- c(1996, 1997, 1999:2000, seq(2002, 2010, by = 2),
    seq(2014, 2018, by = 2))
  bad <- rep(FALSE, NROW(data))
  bad[data[, "Year"] > 2017 &
    grepl("Can.Acoustic", data[, "Source"], ignore.case = TRUE)] <- TRUE
  bad[data[, "Year"] < 1995 &
    grepl("Can.Acoustic", data[, "Source"], ignore.case = TRUE)] <- TRUE
  bad[data[, "Year"] %in% nosurvey &
    grepl("Can.Acoustic", data[, "Source"], ignore.case = TRUE)] <- TRUE
  bad[data[, "Source"] == "CAN_domestic" & data[, "Month"] < 4] <- TRUE
  bad[data[, "Source"] == "CAN_shoreside" & data[, "Month"] < 4] <- TRUE
  table(data[bad, "Year"], data[bad, "Source"])
  outdata <- data[!bad, ]
  if (!is.null(file_out)) {
    utils::write.csv(outdata)
  }
  return(outdata)
}
