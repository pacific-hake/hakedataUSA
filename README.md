# hakedataUSA

Code to extract and workup the U.S. data for the
assessment of Pacific Hake.

# Current assessment

To extract code for this year's assessment open a new R session and run
```
# The following two lines will need to be altered for each user
local.assess <- file.path("c:", "stockAssessment", "hake-assessment")
local.model <- "2020.01.03_data"

# Install the package hakedataUSA
devtools::install_github("pacific-hake/hakedataUSA")
# or load_all() if you are in the hakedataUSA directory

# Pull data, manipulate columns, and write csvs and figures
hakedata <- pulldatabase()
norpaccatches(hakedata$ncatch)
pacfincatches(hakedata$pcatch)

# Make composition data
age_norpac <- atseacomps(hakedata$atsea.ages, hakedata$ncatch)
age_shore <- shorecomps(hakedata$page, verbose = TRUE)
age_yearlyweights <- mappingagesamples(hakedata$atsea.ages, hakedata$ncatch, savepng = TRUE)
jtcdecotoliths <- agedotoliths(hakedata$atsea.ages)
plot_rawmeasure(hakedata$atsea.ages, years = 2020:(2020-4))

# Send catches to hake-assessment/data
datatoassessment_catch(file.path(local.assess, "data"))
# fix end year and catches
new_catch(dirout = file.path(local.assess, "data"), year = hakedata_year(),
  filedat = file.path(local.assess, "models", local.model, "hake_data.ss"))
# Send compositions to hake-assessment/data
datatoassessment_comp(file.path(local.assess, "data"))
compdata <- datatocomps(dirdata = file.path(local.assess, "data"),
  dirmod = file.path(local.assess, "models", local.model),
  cohorts = c(7, 11))

# Weight-at-age
wtatage_collate()
data_wtatage(dir = file.path(local.assess, "data", "LengthWeightAge"))
render("inst/extdata/beamer/hake_jtc_data_us.Rmd")
```

# Issues

Please contact kelli.johnson@noaa.gov if there are issues with the code.
Note that the databases will only be accessible to members of the U.S. JTC.
