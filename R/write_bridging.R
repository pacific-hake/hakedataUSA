#' Write a set of Stock Synthesis input files to a directory for bridging
#'
#' Write a set of Stock Synthesis input files to a directory after modifying
#' them with new data for the year. Children functions manipulates specific
#' parts of the data or control file and the wrapper function ensures that they
#' are written in the correct order and saved using a standard naming protocol.
#'
#' @details
#' * Catch: The catches are taken from data/landings-tac-history.csv, which is
#'   updated in [process_catch()].
#' * Weight-At-Age: The weight-at-age information is stored in data/wtatage.ss,
#'   which is updated in [process_weight_at_age()].
#' @param input A list read in using [r4ss::SS_read()] that contains all of the
#'   information necessary to write a set of output files. The information will
#'   typically be pulled from the previous year's base model, which represents
#'   the first model in a bridging set.
#' @param dir_output A string that specifies the path to a directory that may
#'   or may not already exist that will be used to save the output.
#' @param CPUE A data frame of catch-per-unit-effort information with the
#'   following columns:
#'   * `year`
#'   * `seas`
#'   * `index`
#'   * `obs`
#'   * `se_log`
#' @param agecomp_survey A data frame of age-composition information for the
#'   surveys that is formatted to use in the Stock Synthesis data file.
#'
#' @author Kelli F. Johnson
#' @return An invisible list of modified Stock Synthesis input files. A
#' directory is also created in `dir_output` with multiple directories, each
#' storing a bridging step.
#' @export
write_bridging <- function(dir_input,
                           dir_output) {
  input <- r4ss::SS_read(dir_input, ss_new = TRUE, verbose = FALSE)
  # 01 new executable
  output_01 <- r4ss::SS_write(
    input,
    dir = fs::path(dir_output, "01_exe"),
    overwrite = TRUE
  )
  # 02 catch
  output_02 <- write_bridging_catch(
    input = input,
    dir_output = dir_output
  )
  # 03 weight-at-age data
  output_03 <- write_bridging_weight_at_age(
    input = output_02,
    dir_output = dir_output
  )
  # 04 survey age-2+ fish
  output_04 <- write_bridging_other(
    input = output_03,
    dir_output = dir_output
    # CPUE = ,
    # agecomp_survey =
  )
  # 05 survey age-1 fish
  output_05 <- write_bridging_other(
    input = output_04,
    dir_output = dir_output
    # CPUE =
  )
  # 06 fishery
  output_06 <- write_bridging_other(
    input = output_05,
    dir_output = dir_output,
    fishery = TRUE
  )

  return(invisible(output_06))
}

#' @describeIn write_bridging Bridge catch data
#' @export
write_bridging_catch <- function(input, dir_output) {
  repo_catch <- utils::read.csv(
    file = fs::path(hakedata_wd(), "landings-tac-history.csv"),
  )
  input[["dat"]][["catch"]] <- rbind(
    input[["dat"]][["catch"]] %>%
      dplyr::filter(year <= 0),
    data.frame(
      "year" = repo_catch$Year,
      "seas" = 1,
      "fleet" = 1,
      catch = repo_catch$TOTAL,
      catch_se = 0.01
    )
  )
  input[["dat"]][["endyr"]] <- max(repo_catch$Year)
  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, "02_catch"),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}

#' @describeIn write_bridging Bridge weight-at-age data
#' @export
write_bridging_weight_at_age <- function(input, dir_output) {
  # Need to add input weight-at-age data
  weight_at_age <- r4ss::SS_readwtatage(
    file = fs::path(hakedata_wd(), "wtatage.ss"),
    verbose = FALSE
  )
  input[["wtatage"]] <- weight_at_age
  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, "03_weight_at_age"),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}

#' @describeIn write_bridging Bridge CPUE and/or composition data
#' @export
write_bridging_other <- function(input,
                                 dir_output,
                                 CPUE,
                                 agecomp,
                                 fishery = FALSE) {
  # Local function
  make_survey <- function(dir, suffix) {
    sprintf(
      "%02d_%s",
      as.numeric(
        gsub(
          "^([0-9]+)_.+$",
          "\\1",
          basename(tail(fs::dir_ls(dir, type = "directory"), 1))
        )
      ) + 1,
      suffix
    )
  }

  # Deal with new data
  if (!missing(CPUE)) {
    input[["dat"]][["CPUE"]] <- rbind(
      input[["dat"]][["CPUE"]] %>%
        dplyr::filter(!index %in% CPUE[["index"]]),
      CPUE
    )
    survey <- switch(unique(CPUE[["index"]]) == 1 ~ "04_survey_age_2_plus",
      unique(CPUE[["index"]]) == 3 ~ "05_survey_age_1",
      TRUE ~ make_survey(dir_output, "survey")
    )
  }
  if (!missing(agecomp)) {
    input[["dat"]][["agecomp"]] <- rbind(
      input[["dat"]][["agecomp"]] %>%
        dplyr::filter(!FltSvy %in% agecomp[["FltSvy"]]),
      agecomp
    )
  }
  if (missing(CPUE) & missing(agecomp) & fishery) {
    survey <- "06_fishery_age"
    agecomp <- load_age_fishery()
    input[["dat"]][["agecomp"]] <- rbind(
      dplyr::filter(input[["dat"]][["agecomp"]], Yr < 2008 | FltSvy != 1),
      setNames(
        dplyr::filter(agecomp, year >= 2008),
        colnames(input[["dat"]][["agecomp"]])
      )
    )
  }

  # Standard inputs that do not depend on new data
  # Check selectivity deviation year
  input[["ctl"]] <- update_ss3_selectivity(input_list = input)
  # Check recruitment year
  input[["ctl"]] <- update_ss3_recruitment_year(input_list = input)
  # Check ageing error matrix
  input[["dat"]] <- update_ss3_ageing_error(input_list = input)

  # Write the files and return the list
  if (!exists("survey")) {
    survey <- make_survey(dir_output, "data")
  }
  input[["par"]] <- NULL
  r4ss::SS_write(
    inputlist = input,
    dir = fs::path(dir_output, survey),
    overwrite = TRUE,
    verbose = FALSE
  )
  return(invisible(input))
}
