context("Catches equal across databases")

test_that("Catches are within 4 mt", {
  ncatch_month <- read.csv(
    file.path(dirname(test_dir_data), 
      "Catches", "NorpacDomesticCatchesByMonth.csv"))
  pcatch_month <- 
  diffs <- merge(
    aggregate(MT ~ YEAR + MONTH, data = pcatchatsea, sum),
    ncatch_month,
    by.x = c("YEAR", "MONTH"), by.y = c("Year", "Month"), all = TRUE)
  diff <- diffs[order(diffs$YEAR, diffs$MONTH), ]
  diffs$diff <- diffs$MT - diffs$Catch.MT
  expect_lt(max(abs(diffs$diff)), 4.0, 
    label = "Maximum abs(difference) in catches")
})
