# hakedataUSA

<!-- badges: start -->

<!-- badges: end -->

The goal of {hakedataUSA} is to provide code to extract and workup the
U.S. data for the assessment of Pacific Hake.

## Instructions

1.  First, you must update `data-raw/quotas.csv` to include the
    sector-specific quotas. These values are used when processing the
    data, mainly for the creation of figures. Then, from within R,
    source `data-raw/quotas.R` and the internal data object will be
    updated and ready for use. Commit both `data-raw/quotas.csv` and
    `data-quotas.rda` to the repository and push.

2.  Next, load the package. This can be accomplished through GitHub
    (first chunk) or using a local clone (second chunk).
    
    ``` r
    chooseCRANmirror(ind = 1)
    # install.packages("pak")
    pak::pak("pacific-hake/hakedataUSA")
    library(hakedataUSA)
    ```
    
    ``` r
    chooseCRANmirror(ind = 1)
    stopifnot(basename(getwd()) == "hakedataUSA")
    devtools::load_all()
    ```

3.  The path to where the raw output will be saved is stored in an
    internal function, i.e., `hakedata_wd()`. Try it out, see if it
    works for you. If it does not work, then you will need to alter the
    function, which is stored in `R/hakedata-R`. The function should
    result in a path ending with `data-tables` inside of your cloned
    version of
    [pacific-hake/hake-assessment](www.github.com/pacific-hake/hake-assessment).

4.  Check that the correct year will be pulled for the data of interest
    by running `hakedata_year()`. This will be the last year of data.

5.  To pull the data for the U.S.A., run the following code:
    
    ``` r
    pull_US_data()
    ```

6.  To process the recently pulled data run the following code:
    
    ``` r
    process_database()
    
    write_bridging(
      dir_input = fs::path(dirname(hakedata_wd()), "models", "2022.01.10_base"),
      dir_output = fs::path(dirname(hakedata_wd()), "models", "2023", "01-version", "02-bridging-models")
    )
    ```

7.  This process must be augmented if it is a survey year to get the
    survey data from the server and process it accordingly. More
    information on that process is to come later.
