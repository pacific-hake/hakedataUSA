#' Converts the rows of a matrix to percentages cleanly as.pctages
#'
#' @param x A matrix
as.pctages <- function(x) {
  # If it's a vector, convert to a matrix.
  if (is.null(dim(x))) {
    dim(x) <- c(1, length(x))
  } # End if

  return.val <- NULL

  for (i in 1:nrow(x)) {
    if (sum(x[i, ]) == 0) {
      tmp <- rep(0, ncol(x))
    } else {
      tmp <- x[i, ] / sum(x[i, ])
    } # End if-else
    return.val <- rbind(return.val, tmp)
  } # End for i

  return(return.val)
} # End function as.pctages
