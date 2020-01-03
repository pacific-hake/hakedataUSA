#' Pull Information From NORPAC and PacFIN Databases
#' 
#' Extract catch, weight and age data from NORPAC and PacFIN databases
#' for the hake stock assessment.
#' 
#' @details
#' There are many detailed parts to \code{pulldatabase} that lead to many files
#' being saved to the disk. The steps are outlined below:
#' \enumerate{
#'   \item The folder where the data should be saved is found.
#'   \item A summary file is saved to the disk.
#'   \item Extract data from NORPAC
#'     \enumerate{
#'       \item Catch data
#'       \item Weight and age data
#'       \item Squash table of ages (that also includes lengths)
#'     }
#'   \item Extract data from PacFIN
#'     \enumerate{
#'       \item Catch data
#'       \item Age data from bds table
#'       \item Species cluster
#'       \item Bds fish table
#'       \item Bds species cluster
#'       \item At sea sector data
#'     }
#'   \item Save each item to the disk in the "extractedData" folder.
#' }
#' @param database A vector of character values indicating 
#' which databases you want to pull information from.
#' Options include \code{c("NORPAC", "PacFIN")}, one or both
#' can be specified.
#' @param startyear An ordered list the same length as \code{database} with
#' at least one element per list entry specifying the start year for
#' each database. If only a single value per database is provided, then
#' then the number will be recycled for catches and ages. Otherwise,
#' the first entry is the start year for catches and the second is the 
#' start year for age compositions. 
#' @param endyear An integer value specifying the final year of data
#' to include in the extraction. The default will be to
#' end the data based on the current calendar year.
#' @param passwordfile A file path directing R to a file that contains
#' two lines, with the first being your NORPAC password and the
#' second being your PacFIN password without quotes.
#' If this argument is \code{NULL}, the user will be prompted
#' for their password. 
#' 
#' @export
#' @author Kelli Faye Johnson
#' @return An environment with several objects pulled from the 
#' NORPAC and PacFIN databases. Several \code{Rdat} files are
#' saved to the disk, i.e., one for each object and a summary 
#' file.
#' 
#' @examples
#' \dontrun{
#' dataenv <- pulldatabase()
#' head(get("ncatch", envir = dataenv))
#' }
#' 
pulldatabase <- function(database = c("NORPAC", "PacFIN"), 
  startyear = list("NORPAC" = 2008, "PacFIN" = c(1980, 2008)),
  endyear = as.numeric(format(Sys.time(), "%Y")),
  passwordfile = "password.txt") {
  
  mydir <- hakedatawd()
  sqldir <- system.file("extdata", "sql", package = "hakedataUSA")
  if (!is.list(startyear)) {
    stop("startyear must be a list.")
  }
  if (length(startyear) != length(database)) {
    stop("At least one start year per database must be provided.")
  }
  for (ii in seq_along(startyear)) {
    if (length(startyear[[ii]]) == 1) {
      startyear[[ii]][2] <- startyear[[ii]][1]
    }
  }
  if (is.null(names(startyear))) {
    names(startyear) <- database
  } else {
    if (!all(names(startyear) == database)) {
      stop("Names of startyear do not match database entries.")
    }
  }

  info <- hakedatasqlpw(file = passwordfile)
  NORPAC.uid <- info[[1]][1]
  PacFIN.uid <- info[[1]][2]
  NORPAC.pw <- info[[2]][1]
  PacFIN.pw <- info[[2]][2]

  oldoptions <- options()
  on.exit(options(oldoptions), add = TRUE)
  # set this so that the full haul join number is displayed
  options(digits = 19)
  dir.create(file.path(mydir, "extractedData", "copies"), recursive = TRUE,
    showWarnings = FALSE)

  localsave <- function(data, trailingname, dir = hakedatawd()) {
    x <- deparse(substitute(data))
    assign(x, data)
    end <- paste0(trailingname, ".Rdat")
    save(list = x, 
      file = file.path(dir, "extractedData", end))
    ignore <- file.copy(from = file.path(dir, "extractedData", end),
      to = file.path(dir, "extractedData", "copies", 
        gsub("\\.", paste0("_", format(Sys.time(), "%Y.%m.%d"), "."), end)),
      overwrite = TRUE)
    if (!ignore) stop("Something went wrong copying the file to copies")
  }

  # NORPAC 
  if ("norpac" %in% tolower(database)) {
    # Catches
    ncatch <- queryDB(
      queryFilename = dir(sqldir, "NORPACdomesticCatch", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      start = startyear$NORPAC[1], end = endyear)
    localsave(ncatch, "NORPACdomesticCatch")
    # Age and weight data
    atsea.ageWt <- queryDB(
      queryFilename = dir(sqldir, "atseaAgeWeight", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      sp = "206", start = startyear$NORPAC[2], end = endyear)
    localsave(atsea.ageWt, "atsea.ageWt")
    # Age and weight data from squash table
    atsea.ages <- queryDB(
      queryFilename = dir(sqldir, "atSeaSquashTableAges", full.names = TRUE),
      db = "NORPAC", uid = NORPAC.uid, pw = NORPAC.pw, 
      sp = "206", start = startyear$NORPAC[2], end = endyear)
    localsave(atsea.ages, "atsea.ages")
  }

  if ("pacfin" %in% tolower(database)) {
    # Catches
    # Remove XXX fleet (foreign catch?)
    pcatch <- queryDB(
      queryFilename = dir(sqldir, "comp_ft_taylor_aliased", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[1], end = endyear)
    localsave(pcatch, "Pacfincomp_ft_taylorCatch")
    # bds data
    bds.age <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_age", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.age, "pacfin_bds_age")
    bds.allsp.cluster <- queryDB(
      queryFilename=dir(sqldir, "pacfin_bds_allsp_cluster", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.allsp.cluster, "pacfin_bds_allsp_cluster")
    bds.fish <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_fish", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw, 
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear, 
      asis = c(rep(FALSE, 11), TRUE, rep(FALSE, 32)))
    localsave(bds.fish, "pacfin_bds_fish")
    bds.sp.cluster <- queryDB(
      queryFilename = dir(sqldir, "pacfin_bds_sp_cluster", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = "PWHT", start = startyear$PacFIN[2], end = endyear)
    localsave(bds.sp.cluster, "pacfin_bds_sp_cluster")
    pcatchatsea <- queryDB(
      queryFilename = dir(sqldir, "pacfin.atseabysector", full.names = TRUE),
      db = "PACFIN", uid = PacFIN.uid, pw = PacFIN.pw,
      sp = 206, start = startyear$PacFIN[2], end = endyear)
    localsave(pcatchatsea, "pcatchatsea")
  }

  e1 <- new.env()
  if ("norpac" %in% tolower(database)) {
    assign("atsea.ages", atsea.ages, envir = e1)
    assign("atsea.ageWt", atsea.ageWt, envir = e1)
    assign("atsea.lenAge", atsea.lenAge, envir = e1)
    assign("ncatch", ncatch, envir = e1)
  }
  if ("pacfin" %in% tolower(database)) {
    assign("bds.age", bds.age, envir = e1)
    assign("bds.allsp.cluster", bds.allsp.cluster, envir = e1)
    assign("bds.fish", bds.fish, envir = e1)
    assign("bds.sp.cluster", bds.sp.cluster, envir = e1)
    assign("pcatch", pcatch, envir = e1)
  }
  invisible(e1)
}
