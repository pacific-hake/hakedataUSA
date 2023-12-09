#' Process the raw data from [pull_database()]
#' @export
#' @family process
process_database <- function() {
  # Catch
  processed_catch_norpac <- process_catch_norpac()
  process_catch_pacfin()
  commit_catch()

  # Age composition
  age_norpac <- process_age_sea(ncatch = processed_catch_norpac)
  age_shore <- process_age_shore()
  plot_raw_age()

  # Weight at age
  process_weight_at_age_survey()
  process_weight_at_age_us()
  withforecast <- process_weight_at_age()
}
