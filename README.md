
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hakedataUSA

<!-- badges: start -->
<!-- badges: end -->

The goal of {hakedataUSA} is to provide code to extract and workup the
U.S. data for the assessment of Pacific Hake.

## Installation

You can install the development version of {hakedataUSA} from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pacific-hake/hakedataUSA")
# or if getwd() is a clone of this repository
# pak::local_install()
library(hakedataUSA)
```

## Instructions

``` r
# Customize the next two lines
local.model <- fs::path("2023", "test")

pull_database()
process_database()

write_bridging(
  dir_input = fs::path(dirname(hakedata_wd()), "models", "2022.01.10_base"),
  dir_output = fs::path(dirname(hakedata_wd()), "models", "2023", "01-version", "02-bridging-models")
)
```

``` r
rmarkdown::render("inst/extdata/beamer/hake_jtc_data_us.Rmd")
```

## Issues

Please contact <kelli.johnson@noaa.gov> if there are issues with the
code. Note that the databases will only be accessible to members of the
U.S. JTC.
