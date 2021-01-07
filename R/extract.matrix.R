#' Extract.matrix extracts a matrix from a table object
#'
#' @param x A table object.
#'
extract.matrix <- function(x) {

  return.val = NULL
  vector.names = NULL

  # Iterate over the rows of data,
  # binding them into a new matrix.
  tmp = attributes(x)$dim

  if (length(tmp) == 0) {
    # It's a vector, not a table
    return(x)
  } # End if

  if (length(tmp) > 1) {
    for ( i in 1:tmp[1] ) {
      return.val = rbind(return.val, x[i,])
    } # End for
    # Grab the row names.  The column names came along
    # automatically.
    rownames(return.val) = attributes(x)$dimnames[[1]]
  } else {
    # The table contains a vector of values (a one-D table)
    for ( i in 1:tmp[1] ) {
      return.val = c(return.val, x[[i]])
      vector.names = c(vector.names, attributes(x[i])$names)
    } # End for
    names(return.val) = vector.names
  } # End if-else

  return(return.val)
} # End extract.matrix
