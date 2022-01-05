#' Get number of unique values per grouping
#'
#' Get the number of unique records per group, where the groups are defined
#' using the [interaction] of all columns listed in `xvar`.
#' Typically, less than three entries leads to the exposure of
#' confidential data. This function allows one to check to see the
#' number of unique labels (e.g., vessel names) within various
#' categories (e.g., combinations of month and year) is less than three.
#' Data can then be subset based on the new column called `ngroups`.
#'
#' @details
#' If you want to use two variables for `yvar`, then you will need to pre-process
#' your data. The [interaction] function will be helpful, in that it can take
#' any number of columns and create a new column that is combination of everything
#' that you want. This option is better than using paste to combine columns.
#' You will not always want the interaction though, and sometimes you might want
#' to use [ifelse] calls to combine columns.
#' For example, with hake, catcher boats deliver to motherships and each type of
#' vessel has its own name in the data frame. If the catcher boat column entry is
#' `NA` then you would assume that a name would be present in the mothership column
#' because it might be a catcher-processor vessel that doesn't need a catcher boat.
#' So, if `is.na(data[, "catcherboat"])` then `data[, "mothership"]`.
#'
#' @template data
#' @template xvar
#' @template yvar
#'
#' @author Kelli Faye Johnson
#' @return A new column is added to the input data frame
#' called `ngroups` that provides the number of unique yvar entries
#' for each grouping.
#'
get_confidential <- function(data,
  xvar,
  yvar = "VESSEL"
  ) {

  #### Input checks
  stopifnot(length(yvar) == 1)
  stopifnot(all(xvar %in% colnames(data)))
  stopifnot(yvar %in% colnames(data))

  #### Find the number of unique yvar entries per grouping
  # xvar defines the columns to use as groups
  # a new column named 'ngroups' will be added to the returned data frame
  data %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(ngroups = dplyr::n_distinct(vcolumn, na.rm = TRUE)) %>%
    dplyr::ungroup()

}
