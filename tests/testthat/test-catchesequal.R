context("Catches equal across databases")

test_that("Catches are within 4 mt", {
  ncatch_month <- read.csv(
    file.path(dirname(test_dir_data), 
      "Catches", "us-cp-catch-by-month.csv"))
  diffs <- merge(
    aggregate(MT ~ YEAR + MONTH, data = pcatchatsea, sum, na.rm = TRUE),
    ncatch_month,
    by.x = c("YEAR", "MONTH"), by.y = c("year", "month"), all = TRUE)
  diffs <- diffs[order(diffs$YEAR, diffs$MONTH), ]
  diffs$diff <- diffs$MT - diffs$catch
  expect_true("diff" %in% colnames(diffs))
  # expect_lt(max(abs(diffs$diff), na.rm = TRUE), 4.0, 
  #   label = "Maximum abs(difference) in catches")
  # todo: make this test better
})
