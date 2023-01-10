
#' Run tests for historical weight-at-age data
#'
#' @param data A data frame of weight-at-age data.
#' @author Kelli F. Johnson
#' @export
#' @return A logical of `TRUE` if it reaches the end without error.
#' @family weight-at-age
test_weight_at_age <- function(data) {
  pre_2008 <- c(
    # TODO: check the following
    # samples added Dec 2018 / Jan 2019 but aren't in data
    # "Acoustic Poland" = 2094,
    "Acoustic Poland" = 2092,
    CAN_acoustic = 5719,
    CAN_JV = 136,
    CAN_polish = 487,
    CAN_shoreside = 636,
    US_acoustic = 15718,
    US_atsea = 27939,
    # samples added Dec 2018 / Jan 2019 but aren't in data
    # US_FOREIGN = 29778,
    US_foreign = 29767,
    # samples added Dec 2018 / Jan 2019 but aren't in data
    # US_JV = 29431,
    US_JV = 29421,
    US_shore = 35824
  )
  data_pre_2008 <- data %>%
    dplyr::filter(Year < 2008) %>%
    dplyr::group_by(Source) %>%
    dplyr::count()
  testthat::expect_equal(
    names(pre_2008),
    data_pre_2008 %>% dplyr::pull(Source)
  )
  testthat::expect_equivalent(
    pre_2008,
    data_pre_2008 %>% dplyr::pull(n)
  )
  invisible(TRUE)
}
