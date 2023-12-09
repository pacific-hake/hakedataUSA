#' Process age data from the Pacific Hake U.S. at-sea fishery
#'
#' @template atsea.ages
#' @template ncatch
#' @param years A vector of integers specifying the years of data that you wish
#'   to process. Typically all years prior to 2008 are left static despite
#'   updates to the data.
#' @param ages A vector of ages to be included in the composition data.
#' The default is to include ages from one to fifteen.
#' @param files File paths to the exported csv files. Must have lower case
#'   `"cp"` or `"ms"` in the file name so the correct vessel type is chosen.
#' @param write A logical specifying if the data should be written to the disk.
#'   The default is `TRUE`.
#' @return A list object with summary information regrading the composition
#' data. Multiple files are saved to the disk as well. These saved files
#' include summary information in ...report.txt files and comps.csv files.
#' @export
#' @author Kelli F. Johnson
#'
process_age_sea <- function(atsea.ages = get_local(file = "atsea.ages.Rdat"),
                            ncatch = get_local(file = "norpac_catch.Rdat"),
                            years = 2008:hakedata_year(),
                            ages = 1:15,
                            files = fs::path(
                              hakedata_wd(),
                              paste0(
                                "us-",
                                c("cp", "ms"),
                                "-age-proportions.csv"
                              )
                            ),
                            write = TRUE) {
  daterange <- atsea.ages %>%
    dplyr::filter(!is.na(AGE)) %>%
    dplyr::pull(HAUL_OFFLOAD_DATE) %>%
    format("%Y-%m-%d") %>%
    range()
  if (daterange[1] != "2008-05-15") {
    cli::cli_abort(
      message = c(
        "The ages in atsea.ages should start in 2008-05-15",
        "x" = "Not in {daterange[1]}"
      )
    )
  }

  # Output raw ages
  raw_ages <- dplyr::filter(
    atsea.ages,
    YEAR %in% years,
    !is.na(AGE)
  ) |>
    dplyr::left_join(
      y = ncatch |>
        dplyr::filter(SPECIES == 206) |>
        dplyr::select(HAULJOIN, HAUL, vesseltype),
      by = c(HAUL_JOIN = "HAULJOIN", HAUL_OFFLOAD = "HAUL")
    ) |>
    dplyr::mutate(
      AGE = ifelse(AGE > 15, 15, AGE)
    ) |>
    dplyr::rename(year = YEAR) |>
    dplyr::group_by(year, AGE, vesseltype) |>
    dplyr::count() |>
    tidyr::pivot_wider(
      names_from = AGE,
      values_from = n
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      .fns = \(x) tidyr::replace_na(x, 0)
    ))|>
    tidyr::nest(gg = -"vesseltype") |>
    dplyr::mutate(
      lower_name = tolower(vesseltype),
      name = fs::path(
        dirname(files)[1],
        glue::glue("us-{lower_name}-age-raw.csv")
      ),
      purrr::walk2(
        .x = gg,
        .y = name,
        .f = \(x, y) utils::write.csv(x, y, row.names = FALSE, quote = FALSE)
      )
    )

  processed <- purrr::map(
    .x = ifelse(test = grepl("ms", files), yes = 2, no = 1),
    .f = ~ process_atsea_year(
      dat = atsea.ages,
      ncatch = ncatch %>% dplyr::filter(SPECIES == 206),
      minAge = 1,
      maxAge = 15,
      vesselType = .x,
      in.pctl = 0.95
    ) %>%
      tidyr::pivot_wider(
        id_cols = year,
        names_from = AGE,
        values_from = comp,
        unused_fn = max
      ) %>%
      dplyr::relocate(`num_fish`, `num_samples`, .after = year)
  )
  purrr::walk2(
    .x = processed,
    .y = files,
    .f = ~ utils::write.csv(
      x = .x,
      file = .y,
      quote = FALSE,
      row.names = FALSE
    )
  )
  return(processed)
}

#' Workup shoreside composition data
#'
#' Provide composition data from the U.S. Shoreside
#' hake fishery. Written by Allan Hicks in 2016 and
#' revised in 2017 by Ian G. Taylor.
#'
#' @template page
#' @param ages Vector of ages to keep.
#'
#' @export
#' @author Kelli F. Johnson
#' @return todo: document the return
#'
process_age_shore <- function(page = get_local("page.Rdat"),
                              ages = 1:15) {
  page.worked <- page[!is.na(page$AGE), ]
  page.worked$SEX <- factor(page.worked$SEX)
  dat <- SetUpHakeBDS.fn(page.worked,
    verbose = FALSE,
    max.mmLength = 1000, dataTypes = c("C"),
    sampleMethods = c("R"), sampleTypes = c(NA, "", "C", "M"),
    states = c("CA", "OR", "WA", "PW")
  )

  raw_ages <- page.worked |>
    dplyr::group_by(SAMPLE_YEAR, AGE) |>
    dplyr::mutate(
      AGE = ifelse(AGE > 15, 15, AGE)
    ) |>
    dplyr::count() |>
    tidyr::pivot_wider(
      names_from = AGE,
      values_from = n
    ) |>
    dplyr::rename(year = SAMPLE_YEAR) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      .fns = \(x) tidyr::replace_na(x, 0)
    ))|>
    utils::write.csv(
      file = fs::path(hakedata_wd(), "us-sb-age-raw.csv"),
      row.names = FALSE,
      quote = FALSE
    )

  nFishbygear <- table(dat$SAMPLE_YEAR, dat$gear)
  nSamp <- apply(
    table(dat$SAMPLE_YEAR, dat$SAMPLE_NO),
    1,
    function(x) {
      sum(x > 0)
    }
  )
  nFish <- table(dat$SAMPLE_YEAR, useNA = "ifany")

  dat$state <- "PW" # so that it doesn't need to expand up states and won't need a catch file
  # Relationship assumes FISH_WEIGHT is in grams and FISH_LENGTH is cm
  out.lm <- lm(log(FISH_WEIGHT) ~ log(FISH_LENGTH / 10), data = dat)
  # Must keep the next line as is, don't try to make it shorter!
  lw <- data.frame(OR = c(exp(out.lm$coefficients[1]), out.lm$coefficients[2]))

  LFs <- commLFs.fn(dat, lw,
    gear = NULL, state = "PW",
    catchFile = NULL,
    maxExpansion = 1e9, verbose = FALSE,
    loessSpan = 0.3, ageComp = TRUE
  )

  tmp <- LFs$all$PW
  afs <- matrix(NA,
    ncol = length(ages) + 3, nrow = length(tmp),
    dimnames = list(NULL, c("year", "num_fish", "num_samples", paste0(ages)))
  )
  for (i in 1:length(tmp)) {
    tmp2 <- tmp[[i]]
    afs[i, "year"] <- tmp2$year[1]
    afs[i, paste0(min(ages))] <- sum(tmp2[tmp2$age <= min(ages), "lf"])
    afs[i, paste0(max(ages))] <- sum(tmp2[tmp2$age >= max(ages), "lf"])
    for (j in (min(ages) + 1):(max(ages) - 1)) {
      if (sum(tmp2$age == j)) {
        afs[i, paste0(j)] <- tmp2[tmp2$age == j, "lf"]
      } else {
        afs[i, paste0(j)] <- 0
      }
    }
  }
  while (length(dev.list()) > 0) {
    dev.off()
  }

  afs[, "num_samples"] <- nSamp[as.character(afs[, "year"])]
  afs[, "num_fish"] <- nFish[as.character(afs[, "year"])]
  # Deal with not wanting scientific notation in output
  oldoptions <- options()
  options(scipen = 999)
  on.exit(options(sciepen = oldoptions[["scipen"]]), add = TRUE)
  utils::write.csv(
    afs,
    file = fs::path(hakedata_wd(), "us-sb-age-proportions.csv"),
    row.names = FALSE,
    quote = FALSE
  )

  return(afs)
}
