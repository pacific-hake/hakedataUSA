#' Update the catches in a Stock Synthesis file
#'
#' @inheritParams commit_catch
#' @param file_ss3 A file path to the Stock Synthesis data file that you want to
#'   integrate catches into.
#' @author Kelli F. Johnson
#' @return A Stock Synthesis data file is augmented and overwritten.
#' @export
update_ss3_catch <- function(dir_data, file_ss3) {
  file_lan <- fs::path(dir_data, "landings-tac-history.csv")
  lan <- utils::read.csv(file_lan)
  dat <- r4ss::SS_readdat(file_ss3, verbose = FALSE)
  dat[["catch"]] <- rbind(
    dplyr::filter(dat[["catch"]], year < 0),
    data.frame(
      "year" = lan$Year,
      "seas" = 1,
      "fleet" = 1,
      catch = lan$TOTAL,
      catch_se = 0.01
    )
  )
  dat[["endyr"]] <- max(lan$Year)
  r4ss::SS_writedat(dat, file_ss3, overwrite = TRUE, verbose = FALSE)
}
