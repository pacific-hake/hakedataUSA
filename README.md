# hakedataUSA
Code to extract and workup the US data for the Pacific hake assessment.

# Current assessment
To extract code for this year's assessment open a new R session and run the following:
```
local.assess <- file.path("c:", "stockAssessment", "hake-assessment")
local.model <- "2020.01.03_data"

library(devtools)
devtools::install_github("pacific-hake/hakedataUSA")
hakedata <- pulldatabase()
norpaccatches(hakedata$ncatch, nyears = 5)
# todo: remove the added catches for 2020 in 2021
catch_pacfin <- pacfincatches(hakedata$pcatch,
  addtribal = 133.19)
hake_catchPlots(doPNG = TRUE, nyears = 5)

age_norpac <- atseacomps(hakedata$atsea.ages, hakedata$ncatch)
age_shore <- shorecomps(verbose = TRUE)
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
Please contact kelli.johnson@noaa.gov if there are any issues in running the code, though the databases will only be accessible to members of the US JTC.
