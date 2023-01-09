update_ss3_recruitment_year <- function(input_list) {
  input_list[["ctl"]][["MainRdevYrLast"]] <- dplyr::filter(
    input_list[["dat"]][["CPUE"]],
    index == 3
  ) %>%
    dplyr::pull(year) %>%
    max() - 1
  return(input_list[["ctl"]])
}
