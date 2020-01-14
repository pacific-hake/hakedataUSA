#' Calculate Ageing Error Matrix for Additional Year
#' 
#' Ageing error is based on an overall ageing error and strong cohorts.
#' The cohorts lead to a decreased error using a multiplier. There is
#' one value of ageing error per age in the population. Pacific Hake
#' has 20 ages in the population starting at age 0. Thus, the ageing-error
#' matrix starts with age 0.5 and goes to age 20.5.
#' 
#' @param ages The ages that have strong cohorts and decreased ageing error.
#' @param x A vector of ageing error used for each definition of ageing
#' error in the stock assessment model.
#' @param multiplier A single value that is multiplied times certain
#' entries of \code{x} to account for cohort effects. The default for Pacific
#' Hake is 0.55.
#' 
#' @return A matrix of ageing error with two rows, where the top row is a
#' vector of ages at the mid point of the year and the second row is a
#' vector of ageing error. The two vectors can be used as a new definition
#' of ageing error in Stock Synthesis.
#'
#' @examples
#' # 2019 ageing error was
#' error <- ageerror_new(ages = c(5.5, 9.5, 20.5))
#' \dontshow{
#'   test_that("2019 ageing error", {
#'   expect_equal(error[2, 6], c("age5" = 0.2354495),
#'     label = "2019 ageing error for age 5")
#' })
#' }
#'
ageerror_new <- function(ages, 
  x = c(0.329242, 0.329242, 0.346917, 0.368632, 0.395312, 
        0.42809, 0.468362, 0.517841, 0.57863, 0.653316, 
        0.745076, 0.857813, 0.996322, 1.1665, 1.37557, 
        1.63244, 1.858, 2.172, 2.53, 2.934, 3.388),
  multiplier = 0.55) {
  names <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5,
    11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5)
  names(x) <- names
  x[names(x) %in% ages] <- x[names(x) %in% ages] * multiplier
  out <- rbind(names, x)
  rownames(out) <- NULL
  colnames(out) <- paste0("age", floor(names))
  return(out)
}
