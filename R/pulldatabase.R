#' Pull data from databases for Pacific Hake
#'
#' Extraction of
#' * catch data,
#' * weight measurements,
#' * length measurements,
#' * age reads, and
#' * management quantities
#' from NORPAC and PacFIN databases for the Pacific Hake assessment.
#' Other, additional, databases may be added in the future through the
#' `database =` argument.
#'
#' @details
#' `pulldatabase()` includes several steps, outlined below,
#' and leads to many files being saved to your computer.
#' Originally, files were **NOT** overwritten. In 2022, Kelli F. Johnson
#' changed the code to not save previous downloads to
#' save on disk space, reduce the storing of redunant information, and
#' simplify the code base that needed maintenance.
#' * Find the folder where the data should be saved with [hakedatawd()].
#' * Extract data from NORPAC.
#'   * Catch data
#'   * Weight and age data
#'   * Squash table of ages (that also includes lengths)
#'   * Foreign ages
#'   * Species list
#' * Extract data from PacFIN.
#'   * Catch data
#'   * Age, length, and weight data from bds table
#'   * At-sea data
#'   * Management quantities such as allowable catch limits (ACL) for the
#'     Pacific Fisheries Management Council, i.e., U.S. quantities of interest.
#' * Save each object to the disk in the "extractedData" directory in
#'   [hakedatawd()].
#'
#' @param database A vector of character strings indicating
#'   which databases you want to pull information from.
#'   Options include `c("NORPAC", "PacFIN")`,
#'   one or both (the default) can be specified.
#'   Note that case matters here.
#' @param startyear An ordered list the same length as `database` with
#'   at least one integer entry per list item specifying the
#'   first year that you want data for.
#'   If only a single value per database is provided, then
#'   then the year will be recycled for catch and composition information.
#'   Otherwise,
#'   the first entry is the start year for catches and
#'   the second is the start year for the biological data.
#'   Names of the list must match entries in `database` or be `NULL`.
#' @param endyear An integer value specifying the final year of data
#'   to include in the extraction. The default uses
#'   `hakedata_year()` to determine the terminal year.
#' @param passwordfile A file path directing R to a file that contains
#'   a password for each database listed in `database`.
#'   Each password should be on its own line. The default case would place
#'   your NORPAC password on the first line and the
#'   second line would be your PacFIN password without quotes.
#'   If this argument is `NULL`, users will be prompted
#'   for their passwords.
#'   Passwords are needed because the databases store confidential data.
#' @template savedir
#' @template verbose
#'
#' @seealso
#' * [hakedata_year()]
#' * [hakedatawd()]
#' @export
#' @author Kelli F. Johnson
#' @return An environment or `list` with several objects pulled from the
#' NORPAC and PacFIN databases. Several `Rdat` files are
#' saved to the disk in the extractedData folder,
#' one file for each object and a summary file.
#'
#' @examples
#' \dontrun{
#' # An environment with objects is returned
#' dataenv <- pulldatabase()
#' # Access individual objects using `get()`
#' head(get("ncatch", envir = dataenv))
#' # Access individual objects pretending the environment is a list
#' dataenv[["ncatch"]][1:5, ]
#' }
#'
pulldatabase <- function(database = c("NORPAC", "PacFIN"),
                         startyear = list(
                           "NORPAC" = 2008,
                           "PacFIN" = c(1980, 2008)
                         ),
                         endyear = hakedata_year(),
                         passwordfile = "password.txt",
                         savedir = hakedatawd(),
                         verbose = FALSE) {
  # File management
  sqldir <- system.file("extdata", "sql", package = "hakedataUSA")
  info <- hakedatasqlpw(file = passwordfile)
  final_dir <- file.path(savedir, "extractedData")
  fs::dir_create(path = final_dir, recurse = TRUE)
  if (verbose) {
    message(final_dir, "\nwas created if it did not already exist.")
  }

  # Checks regarding startyear
  stopifnot(is.list(startyear))
  stopifnot(length(startyear) == length(database))
  for (ii in seq_along(startyear)) {
    if (length(startyear[[ii]]) == 1) {
      startyear[[ii]][2] <- startyear[[ii]][1]
    }
  }
  if (is.null(names(startyear))) {
    names(startyear) <- database
  } else {
    stopifnot(all(names(startyear) == database))
  }

  # Set digits so that the full haul join number is displayed
  oldoptions <- options()
  on.exit(options(oldoptions), add = TRUE)
  options(digits = 19)

  # Creat local function
  localsave <- function(data, trailingname, dir) {
    x <- deparse(substitute(data))
    assign(x, data)
    end <- paste0(trailingname, ".Rdat")
    save(
      list = x,
      file = file.path(dir, "extractedData", end)
    )
  }

  # NORPAC
  if ("NORPAC" %in% database) {
    # Catches
    ncatch <- queryDB(
      queryFilename = dir(sqldir, "NORPACdomesticCatch", full.names = TRUE),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      start = startyear$NORPAC[1], end = endyear
    )
    localsave(ncatch, "NORPACdomesticCatch", savedir)
    # Age and weight data
    atsea.ageWt <- queryDB(
      queryFilename = dir(sqldir, "atseaAgeWeight", full.names = TRUE),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206", start = startyear$NORPAC[2], end = endyear
    )
    localsave(atsea.ageWt, "atsea.ageWt", savedir)
    # Age and weight data from squash table
    atsea.ages <- queryDB(
      queryFilename = dir(sqldir, "atSeaSquashTableAges", full.names = TRUE),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206",
      start = startyear$NORPAC[2],
      end = endyear
    ) %>%
      dplyr::mutate(
        Month = format(HAUL_OFFLOAD_DATE, "%m"),
        Year = format(HAUL_OFFLOAD_DATE, "%Y")
      )
    localsave(atsea.ages, "atsea.ages", savedir)
    atsea.foreign <- queryDB(
      queryFilename = dir(sqldir, "atsea_foreign_ages", full.names = TRUE),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206",
      start = startyear$NORPAC[2],
      end = endyear
    )
    localsave(atsea.foreign, "atsea.foreign", savedir)
    # Get species list
    nspecies <- queryDB(
      queryFilename = file.path(sqldir, "NORPACspecies.query"),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      start = startyear$NORPAC[2],
      end = endyear
    )
    localsave(nspecies, "NORPACspecies", savedir)
  }

  if ("pacfin" %in% tolower(database)) {
    # Catches
    # Remove XXX fleet (foreign catch?)
    pcatch <- queryDB(
      queryFilename = dir(sqldir, "comp_ft_taylor_aliased", full.names = TRUE),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = startyear$PacFIN[1],
      end = endyear
    ) %>%
      dplyr::mutate(
        Date = as.Date(TDATE),
        month = get_date(Date, "%m"),
        year = YEAR,
        sector = ifelse(grepl("^R", FLEET), "USresearch", "USshore")
      ) %>%
      dplyr::arrange(Date)
    localsave(pcatch, "Pacfincomp_ft_taylorCatch", savedir)
    # bds data
    pcatchatsea <- queryDB(
      queryFilename = dir(sqldir, "pacfin.atseabysector", full.names = TRUE),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = 206,
      start = startyear$PacFIN[2],
      end = endyear
    )
    localsave(pcatchatsea, "pcatchatsea", savedir)
    page <- queryDB(
      queryFilename = dir(
        path = sqldir,
        pattern = "pacfin_comprehensive_bds",
        full.names = TRUE
      ),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = startyear$PacFIN[2],
      end = endyear
    )
    # Fix weights to be in grams and lengths to be in mm
    page$FISH_WEIGHT <- page$FISH_WEIGHT * ifelse(
      test = page$FISH_WEIGHT_UNITS %in% c("LBS", "P"),
      yes = pound_to_gram,
      no = 1
    )
    page$FISH_LENGTH <- page$FISH_LENGTH * ifelse(
      test = page$FISH_LENGTH_UNITS %in% c("CM"),
      yes = 10,
      no = 1
    )
    localsave(page, "page", savedir)
    pspec <- queryDB(
      queryFilename = dir(sqldir, "pacfin_spec", full.names = TRUE),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = startyear$PacFIN[1],
      end = endyear
    )
    if (NCOL(pspec) != 1) {
      pspec <- pspec[!duplicated(pspec[, "YEAR"]), ]
    }
  }

  e1 <- new.env()
  if ("norpac" %in% tolower(database)) {
    assign("atsea.ages", atsea.ages, envir = e1)
    assign("atsea.ageWt", atsea.ageWt, envir = e1)
    assign("atsea.foreign", atsea.foreign, envir = e1)
    assign("ncatch", ncatch, envir = e1)
    assign("nspecies", nspecies, envir = e1)
  }
  if ("pacfin" %in% tolower(database)) {
    assign("pcatch", pcatch, envir = e1)
    assign("pcatchatsea", pcatchatsea, envir = e1)
    assign("page", page, envir = e1)
    assign("pspec", pspec, envir = e1)
  }
  invisible(e1)
}
