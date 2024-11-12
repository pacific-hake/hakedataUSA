#' Pull data from U.S. databases for Pacific Hake
#'
#' Extraction of
#' * catch data,
#' * weight measurements,
#' * length measurements,
#' * age reads, and
#' * management quantities
#' from NORPAC and PacFIN databases for the Pacific Hake assessment. Other,
#' additional, databases may be added in the future through the `database =`
#' argument.
#'
#' @details
#' `pull_US_data()` includes several steps, outlined below, and leads to many
#' files being saved to your computer. Originally, files were **NOT**
#' overwritten. In 2022, Kelli F. Johnson changed the code to not save previous
#' downloads to save on disk space, reduce the storing of redundant information,
#' and simplify the code base that needed maintenance.
#' * Find the folder where the data should be saved with [hakedata_wd()].
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
#'   [hakedata_wd()].
#'
#' @param password_file A file path specifying where to find the passwords. The
#'   path can be full or relative to your current working directory. If a path
#'   is provided, the file must be a text file with one password per line for
#'   each database in the `database` argument and in that order. The default for
#'   `database` means that the file has two lines, where the first line is the
#'   NORPAC password and the second line is the PacFIN password. These passwords
#'   should not be surrounded with quotes. If a file name is not provided, which
#'   is the default behavior, then the user will be prompted for their
#'   passwords. This also happens if the file cannot be found given the path
#'   provided. These passwords are needed because the databases store
#'   confidential information.
#' @param database A vector of character strings indicating which databases you
#'   want to pull information from. Options include `c("NORPAC", "PacFIN")`, one
#'   or both (the default) can be specified. Note that case matters here.
#' @param start_year An integer value specifying the start year of data to
#'   include in the extraction. The default is 1900 to include all the data
#'   possible, including years where only a foreign fleet fished.
#' @param end_year An integer value specifying the final year of data to include
#'   in the extraction. The default uses [hakedata_year()] to determine the
#'   terminal year.
#' @param save_dir A string specifying the full path to a directory where you
#'   want files saved. The default is to save them in a folder found using
#'   [hakedata_wd()], which returns a path specific to the personal preferences
#'   of JTC members.
#'
#' @seealso
#' * [hakedata_year()]
#' * [hakedata_wd()]
#' @export
#' @author Kelli F. Johnson
#' @return
#' An environment or `list` with several objects pulled from the desired
#' databases are invisibly returned. Additionally, several `.Rdat` files are
#' saved to the disk in a newly created folder called `extractedData` inside of
#' the directory passed to save_dir. The following files are saved:
#' * nages.Rdat
#' * nweight.Rdat
#' * ncatch.Rdat
#' * pcatch.Rdat
#' * page.Rdat
#'
#' @examples
#' \dontrun{
#' # An environment with objects is returned
#' dataenv <- pull_US_data()
#' # Access individual objects using `get()`
#' head(get("ncatch", envir = dataenv))
#' # Access individual objects pretending the environment is a list
#' dataenv[["ncatch"]][1:5, ]
#' }
#'
pull_US_data <- function(password_file,
                         database = c("NORPAC", "PacFIN"),
                         start_year = 1900,
                         end_year = hakedata_year(),
                         save_dir = hakedata_wd()) {
  # Ensure that the database names are spelled appropriately with the right case
  database <- match.arg(database, several.ok = TRUE)

  # File management
  sql_dir <- system.file("extdata", "sql", package = "hakedataUSA")
  info <- hakedata_sql_password(
    password_file = password_file,
    database = database
  )
  final_dir <- file.path(save_dir, "extractedData")
  fs::dir_create(path = final_dir, recurse = TRUE)

  # Set digits so that the full haul join number is displayed
  oldoptions <- options()
  on.exit(options(oldoptions), add = TRUE)
  options(digits = 19)

  # Create local function
  localsave <- function(data, trailing_name, dir) {
    x <- deparse(substitute(data))
    assign(x, data)
    end <- paste0(trailing_name, ".Rdat")
    save(
      list = x,
      file = file.path(dir, "extractedData", end)
    )
  }

  # NORPAC
  if ("NORPAC" %in% database) {
    # Catches
    ncatch <- queryDB(
      queryFilename = fs::path(sql_dir, "ncatch.sql"),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      start = start_year, end = end_year
    )
    localsave(ncatch, "ncatch", save_dir)
    # Age and weight data
    # TODO: determine if nweight is used at all
    nweight <- queryDB(
      queryFilename = fs::path(sql_dir, "nweight.sql"),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206", start = start_year, end = end_year
    )
    localsave(nweight, "nweight", save_dir)
    # Age and weight data from squash table
    atsea.ages <- queryDB(
      queryFilename = fs::path(sql_dir, "nages_domestic.sql"),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206",
      start = start_year,
      end = end_year
    ) |>
      dplyr::mutate(
        Month = format(HAUL_OFFLOAD_DATE, "%m"),
        Year = as.integer(format(HAUL_OFFLOAD_DATE, "%Y")),
        .after = HAUL_JOIN
      )
    atsea.foreign <- queryDB(
      queryFilename = fs::path(sql_dir, "nages_foreign.sql"),
      db = "NORPAC",
      uid = info[["username"]][["NORPAC"]],
      pw = info[["password"]][["NORPAC"]],
      sp = "206",
      start = start_year,
      end = end_year
    ) |>
      dplyr::rename(
        Year = YEAR
      )
    nages <- dplyr::full_join(
      x = atsea.ages,
      y = atsea.foreign,
      by = colnames(atsea.foreign)
    )
    rm(atsea.ages, atsea.foreign)
    localsave(nages, "nages", save_dir)
  }

  if ("PacFIN" %in% database) {
    # Catches
    # Remove XXX fleet (foreign catch?)
    pcatch <- queryDB(
      queryFilename = fs::path(sql_dir, "pcatch.sql"),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = start_year,
      end = end_year
    ) |>
      dplyr::mutate(
        Date = as.Date(LANDING_DATE),
        month = f_date(Date, "%m"),
        # Entries are LE, OA, R, TI, and XX where R is research
        sector = ifelse(grepl("^R", FLEET), "USresearch", "USshore")
      ) |>
      dplyr::rename(year = YEAR) |>
      dplyr::select(-LANDING_DATE) |>
      dplyr::arrange(Date)
    localsave(pcatch, "pcatch", save_dir)
    # bds data
    page <- queryDB(
      queryFilename = dir(
        path = sql_dir,
        pattern = "pacfin_comprehensive_bds",
        full.names = TRUE
      ),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = start_year,
      end = end_year
    ) |>
      dplyr::mutate(
        # Convert weight to g
        FISH_WEIGHT = FISH_WEIGHT * ifelse(
          test = FISH_WEIGHT_UNITS %in% c("LBS", "P"),
          yes = pound_to_gram,
          no = 1
        ),
        FISH_WEIGHT = ifelse(FISH_WEIGHT == 0, NA, FISH_WEIGHT),
        FISH_LENGTH = FISH_LENGTH * ifelse(
          test = FISH_LENGTH_UNITS %in% c("CM"),
          yes = 10,
          no = 1
        ),
        FISH_WEIGHT_UNITS = ifelse(
          test = FISH_WEIGHT_UNITS %in% c("LBS", "P"),
          yes = "G",
          no = FISH_WEIGHT_UNITS
        ),
        FISH_LENGTH_UNITS = ifelse(
          test = FISH_LENGTH_UNITS == "CM",
          yes = "MM",
          no = FISH_LENGTH_UNITS
        )
      )
    localsave(page, "page", save_dir)
    pspec <- queryDB(
      queryFilename = fs::path(sql_dir, "pspec.sql"),
      db = "PACFIN",
      uid = info[["username"]][["PacFIN"]],
      pw = info[["password"]][["PacFIN"]],
      sp = "PWHT",
      start = start_year,
      end = end_year
    )
    if (NCOL(pspec) != 1) {
      pspec <- pspec[!duplicated(pspec[, "YEAR"]), ]
    }
    localsave(pspec, "pspec", save_dir)
  }

  e1 <- new.env()
  if ("norpac" %in% tolower(database)) {
    assign("nages", nages, envir = e1)
    assign("nweight", nweight, envir = e1)
    assign("ncatch", ncatch, envir = e1)
  }
  if ("pacfin" %in% tolower(database)) {
    assign("pcatch", pcatch, envir = e1)
    assign("page", page, envir = e1)
    assign("pspec", pspec, envir = e1)
  }
  invisible(e1)
}
