context("Pull from databases")

test_that("Files have positive size", {
  expect_true(
    all(file.size(dir(test_dir_data,
      full.names = TRUE, pattern = "[a-z].Rdat"
    )) > 1000),
    label = "All extracted Rdat files have large file size"
  )
})

test_that("Age structures without an age", {
  # todo: find a way to get number of unaged structures from PacFIN
  # expect_equal(sum(is.na(page$AGE)), 0,
  #   label = "Number of PacFIN age structures without an age is")
  # todo(eachyear): add historic info to compare to
  history_badages <- c(
    "2008" = 10, "2009" = 11, "2010" = 76,
    "2011" = 5, "2012" = 84, "2013" = 5, "2014" = 17, "2015" = 175,
    "2016" = 52, "2017" = 8
  )
  new_badages <- table(atsea.ages$YEAR[is.na(atsea.ages$AGE)])
  expect_true(all(new_badages[names(history_badages)] - history_badages == 0),
    label = "Number of NORPAC age structures w/o an age remained the same"
  )
})

test_that("Data is available for every year.", {
  maxyears <- unique(c(page$SAMPLE_YEAR, atsea.ages$YEAR))
  maxyears <- maxyears[order(maxyears)]
  temp <- data.frame(
    "database" = c(rep("PacFIN", 1), rep("NORPAC", 1)),
    "data" = c("otoliths", "otoliths"),
    rbind(
      table(factor(page$SAMPLE_YEAR, levels = maxyears)),
      table(factor(atsea.ages$YEAR[!is.na(atsea.ages$AGE)], levels = maxyears))
    ),
    check.names = FALSE
  )
  expect_true(all(temp[, -c(1:2, NCOL(temp))] > 1200),
    label = "At least 1200 age samples per year per database"
  )
  utils::write.csv(temp, file = file.path(test_dir_data, "summary_nsamples_year.csv"))
})


test_that("", {
  atsea.ages$Month <- factor(as.numeric(format(as.Date(
    atsea.ages$HAUL_OFFLOAD_DATE
  ), "%m")), levels = 1:12)
  temp <- merge(
    all = TRUE,
    aggregate(list("Ages" = !is.na(page$AGE)),
      by = list(
        "Month" = factor(page$SAMPLE_MONTH, levels = 1:12),
        "Year" = page$SAMPLE_YEAR,
        "State" = page$SOURCE_AGID
      ), FUN = sum
    ),
    data.frame(
      aggregate(AGE ~ YEAR + Month,
        data = atsea.ages[!is.na(atsea.ages$AGE), ],
        FUN = length
      ),
      "State" = "atsea"
    ),
    by.x = c("Year", "Month", "State", "Ages"),
    by.y = c("YEAR", "Month", "State", "AGE")
  )
  temp <- reshape(temp,
    direction = "wide", idvar = c("Year", "Month"),
    timevar = "State"
  )
  expect_equal(sum(temp$Ages.C > 0, na.rm = TRUE), 2,
    label = "N years that California provided age data"
  )
})

test_that("Fleet XXX in PacFIN has no recent data", {
  temp <- table(pcatch[pcatch$FLEET == "XX", "YEAR"])
  expect_lt(max(as.numeric(names(temp))), 1994,
    label = "Maximum year of catch from FLEET XX"
  )
})

test_that("Research catch is small", {
  expect_lt(
    max(aggregate(MT ~ YEAR,
      FUN = sum,
      data = pcatch[pcatch$FLEET == "R", c("YEAR", "MT")]
    )$MT),
    1050,
    label = "Yearly research catch"
  )
})

test_that("No tribal catch before 2003", {
  temp <- aggregate(MT ~ YEAR,
    FUN = sum,
    data = pcatch[pcatch$FLEET == "TI", c("YEAR", "MT")]
  )
  expect_true(all(temp$MT > 0), label = "Recent tribal catch every year")
  expect_gt(min(temp$YEAR), 2002, label = "First year of tribal catch")
})

test_that("Positive catches in Limited Entry", {
  temp <- aggregate(MT ~ YEAR,
    FUN = sum,
    data = pcatch[pcatch$FLEET == "LE", c("YEAR", "MT")]
  )
  expect_true(all(temp$MT > 0), label = "Recent LE catch every year")
  expect_gt(min(temp$YEAR), 1993, label = "First year of LE catch")
})

test_that("NORPAC VESSEL_TYPE == 3 only in 2009 and 2010", {
  temp <- substr(ncatch[ncatch$VESSEL_TYPE == 3, "RETRIEVAL_DATE"], 1, 4)
  expect_true(all(temp %in% 2009:2010), label = "VESSEL_TYPE 3 in 2009:2010")
  expect_true(all(ncatch[ncatch$VESSEL_TYPE == 3, "PERMITT"] == 353),
    label = "VESSEL_TYPE 3 PERMITT"
  )
  expect_true(all(ncatch[ncatch$VESSEL_TYPE == 3, "VESSEL"] == "A709"),
    label = "VESSEL_TYPE 3 VESSEL"
  )
})

test_that("Tribal catch is mothership catch", {
  expect_true(all(ncatch[ncatch$CDQ_CODE %in% "M01", "VESSEL_TYPE", drop = TRUE] == 2),
    label = "Makah catch is from mothership in NORPAC"
  )
})

test_that("Tribal catch is same as before", {
  expect_equal(unique(na.omit(as.character(ncatch$CDQ_CODE))), "M01",
    label = "Unique CDQ_CODE"
  )
  cdqcodedate <- table(ncatch$CDQ_CODE, format(ncatch$RETRIEVAL_DATE, "%Y"))
  expect_equal(nrow(cdqcodedate), 1,
    label = "cdq codes in the data indicate Makah catch and it"
  )
  expect_equal(cdqcodedate[, "2008"], 1938, label = "2008 Makah catch")
  expect_equal(cdqcodedate[, "2009"], 2065, label = "2009 Makah catch")
  expect_equal(cdqcodedate[, "2010"], 2496, label = "2010 Makah catch")
  expect_equal(cdqcodedate[, "2011"], 1636, label = "2011 Makah catch")
  expect_equal(cdqcodedate[, "2012"], 19, label = "2012 Makah catch")
  expect_equal(cdqcodedate[, "2013"], 0, label = "2013 Makah catch")
  expect_equal(cdqcodedate[, "2014"], 0, label = "2014 Makah catch")
  expect_equal(cdqcodedate[, "2015"], 0, label = "2015 Makah catch")
  expect_equal(cdqcodedate[, "2016"], 0, label = "2016 Makah catch")
  expect_equal(cdqcodedate[, "2017"], 0, label = "2017 Makah catch")
  expect_equal(cdqcodedate[, "2018"], 0, label = "2018 Makah catch")
  expect_equal(cdqcodedate[, "2019"], 0, label = "2019 Makah catch")
  # todo(eachyear): add new year equal to zero
})
