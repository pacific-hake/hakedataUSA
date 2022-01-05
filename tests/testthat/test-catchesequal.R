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

test_that("Catches are good", {
  savedir <- hakedatawd()
  quotas <- read.csv(file.path(savedir, "Catches", "quotas.csv"), 
    sep = ",", header = TRUE, check.names = FALSE)
  shore <- read.csv(
    file.path(savedir, "Catches", "USshoreCatchByPeriodComp_ft.csv"))
  cp <- data.frame("sector" = "CP", 
    read.csv(file.path(savedir, "Catches", "us-cp-catch-by-month.csv")))
  ms <- data.frame("sector" = "MS", 
    read.csv(file.path(savedir, "Catches", "us-ms-catch-by-month.csv")))

  tmp <- tapply(cp$catch,cp$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 136960, tolerance = 1e-04,
    info = "2017 US catcher-processor catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="CP","2017"]),
    0.9978726, tolerance = 1e-06,
    info = "Ratio of 2017 catch to catcher-processor quota.")  

  tmp <- tapply(ms$catch,ms$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 66427.88, tolerance = 1e-04,
    info = "2017 US mothership catch in mt.")
  testthat::expect_equal(as.numeric(tmp/quotas[quotas$Fleet=="MS","2017"]),
    0.6856435, tolerance = 1e-06,
    info = "Ratio of 2017 catch to mothership quota.")

  tmp <- tapply(shore$catch,shore$year,sum)["2017"]
  testthat::expect_equal(as.numeric(tmp), 150841.18, tolerance = 1e-04,
    info = "2017 US shoreside catch in mt.")
  testthat::expect_equal(
    as.numeric(tmp/quotas[quotas$Fleet=="Shore","2017"]), 
    0.8896718, tolerance = 1e-06,
    info = "Ratio of 2017 catch to shoreside quota.")
})