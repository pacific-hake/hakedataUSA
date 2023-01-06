context("PacFIN catches")

dir_catches <- file.path(hakedatawd(), "Catches")

test_that("PacFIN catches since 2008", {
  yearly <- utils::read.table(file.path(dir_catches, "us-shore-catch-by-month.csv"),
    sep = ",", header = TRUE
  )
  expect_true(all(1994:hakedata_year() %in% yearly$year))
})

test_that("Summary of PacFIN catches by year and fleet", {
  fleetsum <- utils::read.table(file.path(dir_catches, "PacFIN_Fleet.csv"),
    sep = ",", header = TRUE
  )
  noforeign <- fleetsum$XX[fleetsum$X %in% 1994:max(fleetsum$X)]
  expect_true(all(is.na(noforeign)),
    label = "All foreign (i.e., XX) catches are pre 1994"
  )
  notribal <- fleetsum$R[fleetsum$X %in% 2002:min(fleetsum$X)]
  expect_true(all(is.na(notribal)),
    label = "All tribal (i.e., TI) catches are post 2002"
  )
  expect_lt(max(fleetsum$OA, na.rm = TRUE), 150,
    label = "All open access (i.e., OA) catches"
  )
  expect_lt(max(fleetsum$R, na.rm = TRUE), 1100,
    label = "All research catches"
  )
})

rm(dir_catches)
