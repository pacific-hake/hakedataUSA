#' Mutating join
#'
#' A join that adds information from matching columns from `y` to `x`. If the
#' value in `y` is `NA`, then the value from `x` will be used. Thus, all new
#' information can be used to overwrite the old information.
#'
#' @inheritParams dplyr::full_join
#' @param join The {dplyr} function, or function from any other package, that
#'   should be used to join `x` and `y`. The default is to perform a full join,
#'   i.e., [dplyr::full_join()] between the two data frames.
#' @param ... Any additional arguments you wish to supply to the function
#'   specified in `join`.
#'
#' @author Edward Visel with some changes from Kelli F. Johnson.
#' @references \url{https://alistaire.rbind.io/blog/coalescing-joins/} 
#'
#' @export
coalesce_join <- function(x,
                          y, 
                          by = NULL,
                          suffix = c(".x", ".y"), 
                          join = dplyr::full_join,
                          ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(
    gsub(paste0("\\", suffix, collapse = "|"), "", to_coalesce)
  )
  names(to_coalesce) <- to_coalesce

  coalesced <- purrr::map_dfc(
    to_coalesce,
    ~ dplyr::coalesce(
      joined[[paste0(.x, suffix[2])]], 
      joined[[paste0(.x, suffix[1])]]
    )
  )
  names(coalesced) <- to_coalesce

  return(dplyr::bind_cols(joined, coalesced)[names(x)])
}
