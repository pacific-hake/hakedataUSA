#' Format `VESSEL_TYPE` as a character
#'
#' `VESSEL_TYPE` in the NORPAC data base are integers and this function
#' changes these integer values to character strings that provide meaning.
#' The following integers are the available options in NORPAC to indicate
#' whether the vessel processes fish or delivers it to a processing plant:
#' 1. a catcher processor (CP) vessel;
#' 2. a mothership (MS) or ship that receives unsorted codends from
#'   other vessels;
#' 3. a catcher only vessel that delivers unprocessed fish to shoreside,
#'   floating plant, or vessel this only occurred in 2009 and 2010 with a
#'   single vessel, see the notes in the code for more details;
#' 4. a MS that receives sorted codends;
#' 5. a vessel that sells the majority of their catch over the side to other
#'   fishing vessels who will utilize the fish for bait, and
#' 6. vessels that discard all catch from a haul.
#'
#' @param x A vector of integers between one and six.
#'
#' @author Kelli F. Johnson
#' @return A vector of combined integer values and character strings.
#'
f_vessel_type <- function(x) {
  if (!any(x %in% 1:6)) {
    stop("All values in x must be between one and six.")
  }
  x[x == 1] <- "CP"
  x[x == 2] <- "MS"
  # A catcher vessel (A709) named the Stormy C (A709) that did minimal
  # processing at sea and had to have an observer on board.
  # x[x == 3] <- "StormyC"
  return(x)
}
